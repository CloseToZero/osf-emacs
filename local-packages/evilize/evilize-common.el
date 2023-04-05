;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs

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

(require 'cl-lib)
(require 'evil)

(defun evilize--normalize-bindings (bindings)
  (mapcan (lambda (binding)
            (list (kbd (cl-first binding))
                  (cl-second binding)))
          bindings))

(defun evilize--evil-normalize-keymaps-av (&rest _)
  (evil-normalize-keymaps))

(defun evilize-define-key (state keymap &rest bindings)
  "Like `evil-define-key*', but wrap each key in `kbd'.
NOTE: this function can be called with empty bindings."
  (declare (indent defun))
  (when (and (eq keymap 'local)
             (or (not (boundp 'evil-normal-state-local-map))
                 (not (keymapp evil-normal-state-local-map))))
    (evil-normalize-keymaps))
  (when bindings
    (setq bindings
          (cl-loop for (key def) on bindings by #'cddr
                   collect (kbd key)
                   collect def))
    (apply #'evil-define-key* state keymap bindings)))

(provide 'evilize-common)
