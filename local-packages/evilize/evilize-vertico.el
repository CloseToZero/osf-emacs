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
(defvar vertico-map)

(pcase-dolist
    (`(,state ,key ,cmd)
     '((normal "]" vertico-next-group)
       (normal "[" vertico-previous-group)
       (normal "C-u" vertico-scroll-down)
       (normal "C-d" vertico-scroll-up)
       (insert "C-j" vertico-next)
       (insert "C-k" vertico-previous)
       (normal "j" vertico-next)
       (normal "k" vertico-previous)
       (normal "g g" vertico-first)
       (normal "G" vertico-last)
       (insert "M-<" vertico-first)
       (insert "M->" vertico-last)
       ((insert normal) "RET" vertico-exit)
       ((insert normal) "M-RET" vertico-exit-input)
       (normal "y y" vertico-save)
       (insert "M-w" vertico-save)
       ))
  (evil-define-key* state vertico-map (kbd key) cmd))

(provide 'evilize-vertico)
