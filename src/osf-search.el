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

;; Don't terminate the search if we just pressed a control character.
(setq search-exit-option 'edit
      isearch-wrap-pause 'no)

;; Swap the bindings of DEL and C-M-w, so that DEL always delete a
;; character rather than undo the previous search action.
(osf-keymap-set isearch-mode-map
  "DEL" #'isearch-del-char
  "C-M-w" #'isearch-delete-char)

(straight-use-package 'deadgrep)
(keymap-global-set "M-s r" #'deadgrep)

(when (executable-find "rg")
  (with-eval-after-load 'project
    (when (boundp 'project-prefix-map)
      (keymap-set project-prefix-map "g" #'deadgrep)
      (setq project-switch-commands
            (cl-delete-if (lambda (e)
                            (eq (car e) 'project-find-regexp))
                          project-switch-commands))
      (add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t))))

(with-eval-after-load 'deadgrep
  (defun osf-deadgrep-edit ()
    (interactive)
    (deadgrep-edit-mode)
    (message
     "%s"
     (substitute-command-keys
      "\\[osf-deadgrep-edit-abort] to abort the changes, \
\\[osf-deadgrep-edit-exit] to exit the edit")))

  (defun osf-deadgrep-edit-exit ()
    (interactive)
    (when (yes-or-no-p "Exit the edit? (the changes won't be rolled back) ")
      (deadgrep-mode)))

  (defvar osf--fake-undo-entry '(apply osf--fake-undo-command))
  (defun osf--fake-undo-command ()
    (push osf--fake-undo-entry buffer-undo-list))
  (defun osf---deadgrep-edit-mark-undo-point ()
    (osf--fake-undo-command))
  (add-hook 'deadgrep-edit-mode-hook #'osf---deadgrep-edit-mark-undo-point)

  (defun osf-deadgrep-edit-abort ()
    (interactive)
    (unless (eq major-mode 'deadgrep-edit-mode)
      (user-error "Current major-mode is not `deadgrep-edit-mode'"))
    (when (yes-or-no-p "Roll back changes? ")
      (unless (equal (car buffer-undo-list) osf--fake-undo-entry)
        (undo-start)
        (while (and (listp pending-undo-list)
                    (not (equal (car buffer-undo-list) osf--fake-undo-entry)))
          (undo-more 1)))
      (deadgrep-mode)))

  (osf-keymap-set deadgrep-edit-mode-map
    "C-c C-k" #'osf-deadgrep-edit-abort
    "C-c C-c" #'osf-deadgrep-edit-exit)

  (osf-keymap-set deadgrep-mode-map
    "C-c w" #'osf-deadgrep-edit))

(provide 'osf-search)
