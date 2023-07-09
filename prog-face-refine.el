;;; prog-face-refine.el --- Refine faces for programming modes -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2023  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-prog-face-refine
;; Version: 0.1
;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;; Refine faces for comments & strings.

;;; Usage

;; ;; Refine emacs-lisp-mode comment faces.
;; (add-hook
;;  'emacs-lisp-mode-hook
;;  (lambda ()
;;    (setq prog-face-refine-list
;;          (list
;;           (list ";[[:blank:]]" 'comment '((t (:inherit font-lock-comment-face :weight bold))))
;;           (list ";;;[[:blank:]]" 'comment '((t (:inherit warning :weight bold))))))
;;    ;; Activate in the current buffer.
;;    (prog-face-refine-mode)))

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup prog-face-refine nil
  "Refine faces for programming modes."
  :group 'convenience)

(defcustom prog-face-refine-list nil
  "A list of matches for comments, each item must be a list where:

- First item is a match, either a regexp string or a function.
  The function must take a single `state' argument
  (in the format returned by `syntax-ppss'),
  a non-nil return value causes the match to succeed.
- The second item is a symbol, either `comment' or `string'.
- The third argument is a face.

Any changes to this value must run `prog-face-refine-refresh' for them to be taken into account."
  :type '(repeat (symbol (choice regexp function) face)))


;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local prog-face-refine--list-comment nil)
(defvar-local prog-face-refine--list-string nil)

;; ---------------------------------------------------------------------------
;; Internal Validation
;;
;; These only run when enabling the mode to validate `prog-face-refine-list'.
;; Validation could be skipped, it's just a way to avoid unexpected errors during
;; font-locking, a time when errors are often difficult to trouble-shoot.

(defun prog-face-refine--message (fmt &rest args)
  "Message FMT with ARGS (using prefix)."
  (apply #'message (concat "prog-face-refine: " fmt) args))

(defun prog-face-refine--regexp-valid-or-warn (re)
  "Return nil if RE is a valid regexp, otherwise the error string."
  (condition-case err
      (prog1 nil
        (string-match-p re ""))
    (error (error-message-string err))))

(defun prog-face-refine--valid-match-or-warn (index var match)
  "Ensure MATCH can be used to match text at INDEX in VAR."
  (cond
   ((stringp match)
    (let ((err (prog-face-refine--regexp-valid-or-warn match)))
      (cond
       (err
        (prog-face-refine--message "%S is not a valid REGEXP (1st item, %d index in %s)."
                                   match
                                   index
                                   var)
        t)
       (t
        nil))
      t))
   ((functionp match)
    t)
   (t
    (prog-face-refine--message "%S is not a string or function (1st item, %d index in %s)."
                               match
                               index
                               var)
    nil)))

(defun prog-face-refine--valid-type-or-warn (index var ty)
  "Ensure TY is a valid type at INDEX in VAR."
  (cond
   ((memq ty (list 'comment 'string))
    t)
   (t
    (prog-face-refine--message
     "%S is not valid type, expected 'comment or 'string (2nd item, %d index in %s)."
     ty index var)
    nil)))

(defun prog-face-refine--valid-face-or-warn (index var face)
  "Ensure FACE can be used as a text face property at INDEX in VAR."
  (cond
   ((facep face)
    t)
   ((listp face)
    ;; Assume the contents is valid since list faces can be complex.
    t)
   (t
    (prog-face-refine--message "%S is not valid face (3rd item, %d index in %s)." face index var)
    nil)))


;; ---------------------------------------------------------------------------
;; Internal API

(defun prog-face-refine--match-impl (state match-list)
  "Match the comment at POINT for STATE using MATCH-LIST.
Return the face or NIL for default behavior."
  (let ((result nil))
    (while match-list
      (let ((item (pop match-list)))
        (pcase-let ((`(,match . ,face) item))
          (cond
           ((stringp match)
            (when (looking-at-p match)
              (setq match-list nil) ;; Break.
              (setq result face)))
           (t
            (when (funcall match state)
              (setq match-list nil) ;; Break.
              (setq result face)))))))
    result))

(defsubst prog-face-refine--match-string (state)
  "Return a string match from STATE or nil."
  (save-excursion
    (goto-char (nth 8 state))
    (prog-face-refine--match-impl state prog-face-refine--list-string)))

(defsubst prog-face-refine--match-comment (state)
  "Return a comment match from STATE or nil."
  (save-excursion
    (goto-char (nth 8 state))
    (prog-face-refine--match-impl state prog-face-refine--list-comment)))


(defun prog-face-refine--is-enabled ()
  "Return t when highlighting is in use, otherwise nil."
  (cond
   ((or prog-face-refine--list-comment prog-face-refine--list-string)
    t)
   (t
    nil)))

;; ---------------------------------------------------------------------------
;; Internal Wrapper Function

(defun prog-face-refine--font-lock-syntax-face-function-comment-and-string (orig-fn state)
  "Wrapper for `font-lock-syntactic-face-function'.
Run ORIG-FN with STATE."
  (or (cond
       ((nth 3 state)
        (prog-face-refine--match-string state))
       ((nth 4 state)
        (prog-face-refine--match-comment state)))
      ;; Default behavior.
      (funcall orig-fn state)))

(defun prog-face-refine--font-lock-syntax-face-function-comment (orig-fn state)
  "Wrapper for `font-lock-syntactic-face-function'.
Run ORIG-FN with STATE."
  (or (cond
       ((nth 4 state)
        (prog-face-refine--match-comment state)))
      ;; Default behavior.
      (funcall orig-fn state)))

(defun prog-face-refine--font-lock-syntax-face-function-string (orig-fn state)
  "Wrapper for `font-lock-syntactic-face-function'.
Run ORIG-FN with STATE."
  (or (cond
       ((nth 3 state)
        (prog-face-refine--match-string state)))
      ;; Default behavior.
      (funcall orig-fn state)))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun prog-face-refine--clear ()
  "Clear local variables and state.

Return nil if the state is known not to have changed."
  (let ((is-enabled-prev (prog-face-refine--is-enabled))
        (local-vars (list 'prog-face-refine--list-comment 'prog-face-refine--list-string))
        (local-fn-wrappers
         (list
          #'prog-face-refine--font-lock-syntax-face-function-comment-and-string
          #'prog-face-refine--font-lock-syntax-face-function-comment
          #'prog-face-refine--font-lock-syntax-face-function-string)))

    (dolist (var local-vars)
      (kill-local-variable var))

    (dolist (fn-wrap local-fn-wrappers)
      (remove-function (local 'font-lock-syntactic-face-function) fn-wrap))

    is-enabled-prev))

(defun prog-face-refine--refresh ()
  "Refresh the local variables and state.

Return nil if the state is known not to have changed."
  (let ((is-enabled-prev (prog-face-refine--is-enabled))
        (is-enabled-next nil)
        (index 0)
        (index-var "prog-face-refine-list"))
    (prog-face-refine--clear)
    (pcase-dolist (`(,match ,ty ,face) prog-face-refine-list)
      (when (and (prog-face-refine--valid-match-or-warn index index-var match)
                 (prog-face-refine--valid-type-or-warn index index-var ty)
                 (prog-face-refine--valid-face-or-warn index index-var face))
        (cond
         ((eq ty 'comment)
          (push (cons match face) prog-face-refine--list-comment))
         ((eq ty 'string)
          (push (cons match face) prog-face-refine--list-string))
         (t ;; `prog-face-refine--valid-type-or-warn' must catch this.
          (error "Internal error"))))
      (setq index (1+ index)))

    (let ((fn
           (cond
            ((and prog-face-refine--list-comment prog-face-refine--list-string)
             #'prog-face-refine--font-lock-syntax-face-function-comment-and-string)
            (prog-face-refine--list-comment
             #'prog-face-refine--font-lock-syntax-face-function-comment)
            (prog-face-refine--list-string
             #'prog-face-refine--font-lock-syntax-face-function-string)
            (t
             nil))))
      (when fn
        (add-function :around (local 'font-lock-syntactic-face-function) fn))
      (setq is-enabled-next (prog-face-refine--is-enabled)))

    (or is-enabled-prev is-enabled-next)))

(defun prog-face-refine--mode-enable ()
  "Turn on `prog-face-refine-mode' for the current buffer."
  (when (prog-face-refine--refresh)
    (font-lock-flush)))

(defun prog-face-refine--mode-disable ()
  "Turn off `prog-face-refine-mode' for the current buffer."
  (when (prog-face-refine--clear)
    (font-lock-flush)))


;; ---------------------------------------------------------------------------
;; Public API

(defun prog-face-refine-refresh ()
  "Update change from custom settings."
  ;; Check the mode as refresh isn't needed unless the mode is active.
  (when (bound-and-true-p prog-face-refine-mode)
    (when (prog-face-refine--refresh)
      ;; Only flush on change as it's relatively expensive.
      (font-lock-flush))))

;;;###autoload
(define-minor-mode prog-face-refine-mode
  "Refine faces for programming modes."
  :global nil
  :lighter
  prog-face-refine-lighter
  (cond
   (prog-face-refine-mode
    (prog-face-refine--mode-enable))
   (t
    (prog-face-refine--mode-disable))))

(provide 'prog-face-refine)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; prog-face-refine.el ends here
