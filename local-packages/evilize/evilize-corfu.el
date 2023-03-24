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

(require 'evilize-common)

(defvar corfu-map)

(declare-function corfu--setup "ext:corfu")
(declare-function corfu--teardown "ext:corfu")

(let ((bindings '(("C-j" corfu-next)
                  ("C-k" corfu-previous)
                  ("C-d" corfu-scroll-up)
                  ("C-u" corfu-scroll-down)
                  ("C-a" corfu-prompt-beginning)
                  ("C-e" corfu-prompt-end)
                  ("C-g" corfu-quit)
                  ("M-<" corfu-first)
                  ("M->" corfu-last)
                  ("<tab>" corfu-complete)
                  ("RET" corfu-insert)
                  ("M-SPC" corfu-insert-separator)
                  )))
  (apply #'evil-define-key* 'insert corfu-map
         (evilize-normalize-bindings bindings)))

(advice-add #'corfu--setup :after #'evilize--evil-normalize-keymaps-av)
(advice-add #'corfu--teardown :after #'evilize--evil-normalize-keymaps-av)

(provide 'evilize-corfu)
