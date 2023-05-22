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
(puni-global-mode)

(defvar-keymap puni-lisp-sexp-edit-map
  "r" #'puni-raise
  "s" #'puni-split
  "S" #'puni-splice
  "\)" #'puni-slurp-forward
  "\(" #'puni-barf-forward
  "\<" #'puni-slurp-backward
  "\>" #'puni-barf-backward
  )
(fset #'puni-lisp-sexp-edit-map puni-lisp-sexp-edit-map)

(defun osf-puni-setup-lisp-sexp-edit-map-locally ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (osf-keymap-set map
      "M-e" 'puni-lisp-sexp-edit-map)
    (use-local-map map)))
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-data-mode-hook
                scheme-mode-hook))
  (add-hook hook #'osf-puni-setup-lisp-sexp-edit-map-locally))
(add-hook 'eval-expression-minibuffer-setup-hook
          #'osf-puni-setup-lisp-sexp-edit-map-locally)

(provide 'osf-pair)
