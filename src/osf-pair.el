;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(electric-pair-mode)

(straight-use-package 'puni)
(require 'puni)
(osf-keymap-set puni-mode-map
  "DEL" nil
  "C-d" nil)
(puni-global-mode)

(defvar-keymap osf-puni-lisp-sexp-edit-map
  "r" #'puni-raise
  "s" #'puni-split
  "S" #'puni-splice
  "." #'puni-slurp-forward
  "," #'puni-barf-forward
  "\<" #'puni-slurp-backward
  "\>" #'puni-barf-backward
  )
(fset #'osf-puni-lisp-sexp-edit-map osf-puni-lisp-sexp-edit-map)

(defun osf-puni-setup-lisp-sexp-edit-map-locally ()
  (unless (bound-and-true-p puni-mode)
    (error "`puni-mode' not enabled"))
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (osf-keymap-set map
      "M-e" 'osf-puni-lisp-sexp-edit-map)
    (use-local-map map)))

(defun osf--puni-setup-lisp-sexp-edit-map-locally-check-modes ()
  (when (memq major-mode '(emacs-lisp-mode
                           lisp-mode
                           lisp-data-mode
                           scheme-mode))
    (osf-puni-setup-lisp-sexp-edit-map-locally)))

(add-hook 'puni-mode-hook
          #'osf--puni-setup-lisp-sexp-edit-map-locally-check-modes)
(add-hook 'eval-expression-minibuffer-setup-hook
          #'osf-puni-setup-lisp-sexp-edit-map-locally)

(provide 'osf-pair)
