;;; prog-face-refine.el --- Refine faces for programming modes -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2023  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-prog-face-refine
;; Keywords: faces, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;; Refine faces for comments and strings.

;; Usage:

;; ;; Refine emacs-lisp-mode comment and string faces.
;; (add-hook
;;  'emacs-lisp-mode-hook
;;  (lambda ()
;;    (setq-local prog-face-refine-list
;;                (list
;;                 (list ";[[:blank:]]" 'comment '((t (:inherit font-lock-comment-face :weight bold))))
;;                 (list ";;;[[:blank:]]" 'comment '((t (:inherit warning :weight bold))))
;;                 (list "\"\\\\n" 'string '((t (:inherit font-lock-string-face :slant italic))))))
;;    ;; Enable the mode in the current buffer.
;;    (prog-face-refine-mode)))

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))


;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(- ,getter ,(or delta 1)))))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup prog-face-refine nil
  "Refine faces for programming modes."
  :group 'convenience)

(defcustom prog-face-refine-list nil
  "A list of matches for comments and strings, each item must be a list where:

- The first item is a match, either a regexp string or a function.
  The function must take a single `state' argument
  (in the format returned by `syntax-ppss'),
  a non-nil return value causes the match to succeed.
- The second item is a symbol, either `comment' or `string'.
- The third item is a face.

Changes to this variable require calling `prog-face-refine-refresh' to take effect."
  :type
  '(repeat (list (choice regexp function)
                 (choice (const comment) (const string))
                 face)))

(defcustom prog-face-refine-lighter nil
  "Mode-line lighter for `prog-face-refine-mode'."
  :type '(choice (const :tag "None" nil) string))


;; ---------------------------------------------------------------------------
;; Internal Variables

;; Processed list of comment match patterns and their faces.
(defvar-local prog-face-refine--list-comment nil)
;; Processed list of string match patterns and their faces.
(defvar-local prog-face-refine--list-string nil)


;; ---------------------------------------------------------------------------
;; Internal Validation
;;
;; These only run when enabling the mode to validate `prog-face-refine-list'.
;; Validation could be skipped, it's just a way to avoid unexpected errors during
;; font-locking, a time when errors are often difficult to troubleshoot.

(defun prog-face-refine--message (fmt &rest args)
  "Display a message with FMT and ARGS, prefixed with the package name."
  (apply #'message (concat "prog-face-refine: " fmt) args))

(defun prog-face-refine--regexp-valid-or-error-as-string (re)
  "Return nil if RE is a valid regexp, otherwise the error string."
  (condition-case err
      (prog1 nil
        (string-match-p re ""))
    (error
     (error-message-string err))))

(defun prog-face-refine--valid-match-or-warn (index var match)
  "Check if MATCH can be used to match text at INDEX in VAR.
Return non-nil on success, nil if invalid (with a warning message)."
  (cond
   ((stringp match)
    (let ((err (prog-face-refine--regexp-valid-or-error-as-string match)))
      (cond
       (err
        (prog-face-refine--message "%S is not a valid regexp (1st item, at index %d in %s)."
                                   match
                                   index
                                   var)
        nil)
       (t
        t))))
   ((functionp match)
    t)
   (t
    (prog-face-refine--message "%S is not a string or function (1st item, at index %d in %s)."
                               match
                               index
                               var)
    nil)))

(defun prog-face-refine--valid-type-or-warn (index var ty)
  "Check if TY is a valid type at INDEX in VAR.
Return non-nil on success, nil if invalid (with a warning message)."
  (cond
   ((memq ty '(comment string))
    t)
   (t
    (prog-face-refine--message
     "%S is not a valid type, expected `comment' or `string' (2nd item, at index %d in %s)."
     ty index var)
    nil)))

(defun prog-face-refine--valid-face-or-warn (index var face)
  "Check if FACE can be used as a text face property at INDEX in VAR.
Return non-nil on success, nil if invalid (with a warning message)."
  (cond
   ((facep face)
    t)
   ((and face (listp face))
    ;; Assume the contents are valid since list faces can be complex.
    t)
   (t
    (prog-face-refine--message "%S is not a valid face (3rd item, at index %d in %s)."
                               face
                               index
                               var)
    nil)))


;; ---------------------------------------------------------------------------
;; Internal API

(defun prog-face-refine--match-impl (state match-list)
  "Match the syntax element at POINT for STATE using MATCH-LIST.
Return the face or nil for default behavior."
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
  "Return the face for a matched string from STATE, or nil."
  (save-excursion
    (goto-char (nth 8 state))
    (prog-face-refine--match-impl state prog-face-refine--list-string)))

(defsubst prog-face-refine--match-comment (state)
  "Return the face for a matched comment from STATE, or nil."
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
;; Internal Wrapper Functions

(defun prog-face-refine--font-lock-syntax-face-function-comment-and-string (orig-fn state)
  "Wrapper that applies custom faces to both comments and strings.
Falls back to ORIG-FN with STATE, or default faces if ORIG-FN is nil."
  (or (cond
       ((nth 3 state)
        (prog-face-refine--match-string state))
       ((nth 4 state)
        (prog-face-refine--match-comment state)))
      ;; Default behavior.
      (cond
       (orig-fn
        (funcall orig-fn state))
       ((nth 3 state)
        font-lock-string-face)
       (t
        font-lock-comment-face))))

(defun prog-face-refine--font-lock-syntax-face-function-comment (orig-fn state)
  "Wrapper that applies custom faces to comments only.
Falls back to ORIG-FN with STATE, or the default face if ORIG-FN is nil."
  (or (cond
       ((nth 4 state)
        (prog-face-refine--match-comment state)))
      ;; Default behavior.
      (cond
       (orig-fn
        (funcall orig-fn state))
       (t
        font-lock-comment-face))))

(defun prog-face-refine--font-lock-syntax-face-function-string (orig-fn state)
  "Wrapper that applies custom faces to strings only.
Falls back to ORIG-FN with STATE, or the default face if ORIG-FN is nil."
  (or (cond
       ((nth 3 state)
        (prog-face-refine--match-string state)))
      ;; Default behavior.
      (cond
       (orig-fn
        (funcall orig-fn state))
       (t
        font-lock-string-face))))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun prog-face-refine--clear ()
  "Clear local variables and state.

Return non-nil if highlighting was previously enabled."
  (let ((is-enabled-prev (prog-face-refine--is-enabled))
        (local-vars '(prog-face-refine--list-comment prog-face-refine--list-string))
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

Return non-nil if highlighting was or is now enabled."
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
         (t ;; prog-face-refine--valid-type-or-warn must catch this.
          (error "Internal error"))))
      (incf index))

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
  "Apply changes from custom settings when `prog-face-refine-mode' is active."
  (when (bound-and-true-p prog-face-refine-mode)
    (when (prog-face-refine--refresh)
      ;; Only flush on change as it's relatively expensive.
      (font-lock-flush))))

;;;###autoload
(define-minor-mode prog-face-refine-mode
  "Toggle refined face highlighting for comments and strings.
When enabled, applies custom faces based on `prog-face-refine-list'."
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
