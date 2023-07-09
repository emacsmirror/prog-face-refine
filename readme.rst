################
Prog Face Refine
################

Support for refining comment & string faces based on their contents.

Available via `melpa <https://melpa.org/#/prog-face-refine>`__.


Motivation
==========

For code-comments or strings, you may wish to have more control over their faces.

For example, with this package it's possible to use different faces for:

- ``/* Comment. */``
- ``/** Comment. */``
- ``// Comment.``
- ``//! Comment.``

The same is possible for strings.


Usage
=====

This package exposes the following functions.

``prog-face-refine-mode``
   Toggles face refine mode.
``prog-face-refine-refresh``
   Run this comment after changes to custom-settings.


Customization
-------------

``prog-face-refine-list``: ``nil``
   A list of items, each item is a match with the following format.

   The items are formatters as follows.

   - First is a match, either a REGEXP string or a function.

     When a function is used it takes a single argument
     representing the syntax state (in the format returned by ``(syntax-ppss)``).
     A non-nil return value causes the match to succeed.

   - Second is the type in (``'comment``, ``'string``).
   - Third is the face to use.


Example
-------

This example shows doc-string faces being used for ``/** */`` comments, and markup face used for
single line ``//`` comments.

.. code-block::

   (setq prog-face-refine-list
         (list
          (list (concat (regexp-quote "/**") "[^*]") 'comment 'font-lock-doc-face)
          (list (concat (regexp-quote "//") "[^/]") 'comment 'font-lock-doc-markup-face)))
   (prog-face-refine-refresh)

You may also specify faces that inherit from others. This example highlights ``;`` and ``;;;`` prefixed comments
differently in emacs-lisp.

.. code-block::

   (setq prog-face-refine-list
         (list
          (list ";[[:blank:]]" 'comment '((t (:inherit font-lock-comment-face :weight bold))))
          (list ";;;[[:blank:]]" 'comment '((t (:inherit warning :weight bold))))))
   (prog-face-refine-refresh)

This examples shows how ``# ~`` and ``# !`` prefixed comments can be highlighted in Python.

.. code-block::

   (setq prog-face-refine-list
         (list (list "# ~" 'comment 'shadow)
               (list "# !" 'comment 'warning)))
   (prog-face-refine-refresh)


Installation
============

The package is `available in melpa <https://melpa.org/#/prog-face-refine>`__ as ``prog-face-refine``.

If you wish to configure ``prog-face-refine-list`` separately, this is all that is needed.

.. code-block:: elisp

   (use-package
    prog-face-refine
    :commands (prog-face-refine-mode))


This examples shows faces configured for modes using ``use-package`` hooks.

.. code-block:: elisp

   (use-package
    prog-face-refine
    :commands (prog-face-refine-mode)
    :hook
    ((emacs-lisp-mode)
     .
     (lambda ()
       (setq prog-face-refine-list
             (list
              (list
               ";[[:blank:]]" 'comment '((t (:inherit font-lock-comment-face :weight bold))))
              (list ";;;[[:blank:]]" 'comment '((t (:inherit warning :weight bold))))))
       (prog-face-refine-mode)))
    ((c-mode c++-mode)
     .
     (lambda ()
       (setq prog-face-refine-list
             (list
              (list (concat (regexp-quote "/**") "[^*]") 'comment 'font-lock-doc-face)
              (list (concat (regexp-quote "//") "[^/]") 'comment 'font-lock-doc-markup-face)))
       (prog-face-refine-mode))))


Other Packages
==============

`hl-prog-extra <https://codeberg.org/ideasman42/emacs-hl-prog-extra>`__
   This package provides a way to highlight keywords within comments & strings
   and can be used as a companion to this package.

   While there are no inter-dependencies, both packages support using user-configuration
   to extend comment highlighting.

- `sidecar-locals <https://codeberg.org/ideasman42/emacs-sidecar-locals>`__
  provides out-of-source configuration, this is a convenient alternative to file or directory-locals
  that makes it convenient to add project-specific highlighting.
