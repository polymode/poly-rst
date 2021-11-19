;;; poly-rst.el --- poly-rst-mode polymode -*- lexical-binding: t -*-
;;
;; Author: Gustaf Waldemarson, Vitalie Spinu
;; Copyright (C) 2018 Gustaf Waldemarson, Vitalie Spinu
;; Version: 0.2.2
;; Package-Requires: ((emacs "25") (polymode "0.2.2"))
;; URL: https://github.com/polymode/poly-rst
;; Keywords: languages, multi-modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Polymode for working with files using ReStructured Text (ReST or rst) markup
;; syntax.  This allows the user to work in regions marked with code using the
;; appropriate Emacs major mode.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'rst)

(define-obsolete-variable-alias 'pm-host/rst 'poly-rst-hostmode "v0.2")
(define-obsolete-variable-alias 'pm-inner/rst 'poly-rst-innermode "v0.2")

(defvar poly-rst-code-tags '("code" "code-block" "sourcecode" "highlight")
  "List of rst code block tags.")

(defvar poly-rst-head-start-regexp (format "\.\. +%s::[^:]" (regexp-opt poly-rst-code-tags))
  "Regexp to match start of the header.")

(defvar poly-rst-code-lang-regexp "\.\. .*:: +\\(.+\\)"
  "Regexp to match program languages from the start of the header.")

;; This is based on `markdown-code-lang-modes' from markdown-mode.el
(defcustom poly-rst-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
    ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
    ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
    ("bash" . sh-mode))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the major mode.
This mapping is useful to map programming language whose major mode name
is not directly related to the language name. For example, the major mode
for OCaml is `tuareg-mode'."
  :group 'poply-rst
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode"))))

(defun poly-rst-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (list (cdr (assoc lang poly-rst-code-lang-modes))
         (cdr (assoc (downcase lang) poly-rst-code-lang-modes))
         (intern (concat lang "-mode"))
         (intern (concat (downcase lang) "-mode")))))

(defun poly-rst-head-matcher (ahead)
  "ReST heads end with an empty line.
Find the ReST header that are AHEAD or -AHEAD number of headers
away from the current location."
  (when (re-search-forward poly-rst-head-start-regexp nil t ahead)
    (cons (match-beginning 0)
          (if (re-search-forward "^[ \t]*\n" nil t)
              (match-end 0)
            (point-max)))))

(defun poly-rst-mode-matcher ()
  (when (re-search-forward poly-rst-code-lang-regexp nil t 1)
    (let* ((lang (buffer-substring (match-beginning 1) (match-end 1)))
           (mode (poly-rst-get-lang-mode lang)))
      mode)))

(define-hostmode poly-rst-hostmode nil
  "ReSTructured text hostmode."
  :mode 'rst-mode)

(define-auto-innermode poly-rst-innermode nil
  "ReSTructured text innermode."
  :head-matcher #'poly-rst-head-matcher
  :tail-matcher #'pm-same-indent-tail-matcher
  :mode 'host
  :head-mode 'host
  :indent-offset 4
  :mode-matcher #'poly-rst-mode-matcher)

;;;###autoload (autoload #'poly-rst-mode "poly-rst")
(define-polymode poly-rst-mode
  :hostmode 'poly-rst-hostmode
  :innermodes '(poly-rst-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.re?st\\'" . poly-rst-mode))

(provide 'poly-rst)
;;; poly-rst.el ends here
