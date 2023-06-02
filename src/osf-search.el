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
      isearch-wrap-pause 'no
      isearch-lazy-count t)

;; Swap the bindings of DEL and C-M-w, so that DEL always delete a
;; character rather than undo the previous search action.
(osf-keymap-set isearch-mode-map
  "DEL" #'isearch-del-char
  "C-M-w" #'isearch-delete-char)

(straight-use-package 'deadgrep)

(defun osf-deadgrep (search-term &optional directory)
  "Just like `deadgrep', but handle prefix argument in a different way.
When a positive prefix argument is given, create the results buffer but
don’t actually start the search.
When a negative prefix argument is given, start search at `default-directory'
instead of the directory determined by `deadgrep-project-root-function'.
When a prefix argument with numeric value zero is given, the effect is the
combination of positive and negative prefix arguments."
  (interactive
   (list (deadgrep--read-search-term)
         (if (and current-prefix-arg
                  (<= (prefix-numeric-value current-prefix-arg) 0))
             default-directory
           (funcall deadgrep-project-root-function))))
  (if (and current-prefix-arg
           (>= (prefix-numeric-value current-prefix-arg) 0))
      ;; Keep `current-prefix-arg' for `deadgrep'.
      (deadgrep search-term directory)
    (let ((current-prefix-arg nil))
      (deadgrep search-term directory))))

(osf-leader-define-key 'global
  "/ r" #'osf-deadgrep)

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

  (defun osf-deadgrep-visit-result-other-window ()
    "Like `deadgrep-visit-result-other-window', but stay at the same window."
    (interactive)
    (let ((old-window (selected-window)))
      (deadgrep-visit-result-other-window)
      (select-window old-window)))

  (osf-evil-define-key 'normal deadgrep-edit-mode-map
    "Z Z" #'osf-deadgrep-edit-exit
    "Z Q" #'osf-deadgrep-edit-abort)

  (osf-evil-define-key 'normal deadgrep-mode-map
    "i" #'osf-deadgrep-edit
    "M-<return>" #'osf-deadgrep-visit-result-other-window)
  )

(provide 'osf-search)
