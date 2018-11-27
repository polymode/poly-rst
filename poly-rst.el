;;; poly-rst.el --- poly-rst-mode polymode -*- lexical-binding: t -*-
;;
;; Author: Gustaf Waldemarson, Vitalie Spinu
;; Copyright (C) 2018 Gustaf Waldemarson, Vitalie Spinu
;; Version: 0.1.5
;; Package-Requires: ((emacs "25") (polymode "0.1.5"))
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'rst)

(defvar poly-rst-code-tags '("code" "code-block" "sourcecode" "highlight")
  "List of rst code block tags.")

(defvar poly-rst-head-start-regexp (format "\.\. +%s::[^:]" (regexp-opt poly-rst-code-tags))
  "Regexp to match start of the header.")

(defun poly-rst-head-matcher (ahead)
  "ReST heads end with an empty line."
  (when (re-search-forward poly-rst-head-start-regexp nil t ahead)
    (cons (match-beginning 0)
          (if (re-search-forward "^[ \t]*\n" nil t)
              (match-end 0)
            (point-max)))))

(defcustom pm-host/rst
  (pm-host-chunkmode :name "rst"
                     :mode 'rst-mode)
  "ReSTructured text hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/rst
  (pm-inner-auto-chunkmode :name "rst-code-block"
                           :head-matcher #'poly-rst-head-matcher
                           :tail-matcher #'pm-same-indent-tail-matcher
                           :mode 'host
                           :head-mode 'host
                           :indent-offset 4
                           :mode-matcher (cons "\.\. .*:: +\\(.+\\)" 1))
  "ReSTructured text innermode."
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload #'poly-rst-mode "poly-python")
(define-polymode poly-rst-mode
  :hostmode 'pm-host/rst
  :innermodes '(pm-inner/rst))

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rst$" . poly-rst-mode))

(provide 'poly-rst)
;;; poly-rst.el ends here
