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

(declare-function wdired-finish-edit "ext:wdired")
(declare-function wdired-abort-changes "ext:wdired")

;; Keep in normal state after entered wdired-mode, so we can use ciw
;; etc to change the file name.
(evil-set-initial-state 'wdired-mode 'normal)

(defun evilize--wdired-evil-write-finish-edit-ad (fn &rest args)
  (if (and (eq major-mode 'wdired-mode)
           (seq-every-p #'null args))
      (wdired-finish-edit)
    (apply fn args)))
(defun evilize--wdired-evil-quit-abort-changes-ad (fn &rest args)
  (if (and (eq major-mode 'wdired-mode)
           (seq-every-p #'null args))
      (wdired-abort-changes)
    (apply fn args)))
(advice-add #'evil-write
            :around #'evilize--wdired-evil-write-finish-edit-ad)
(advice-add #'evil-quit
            :around #'evilize--wdired-evil-quit-abort-changes-ad)

(defun evilize--wdired-show-key-bindings-ad ()
  (message "Press %s when finished or %s to abort changes"
           (propertize ":w RET" 'face 'help-key-binding)
           (propertize ":q RET" 'face 'help-key-binding)))
(advice-add 'wdired-change-to-wdired-mode
            :after #'evilize--wdired-show-key-bindings-ad)

(provide 'evilize-wdired)
