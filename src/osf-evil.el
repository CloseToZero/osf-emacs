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

(straight-use-package 'undo-tree)
(require 'undo-tree)
(osf-inhibit-message #'undo-tree-save-history)
(global-undo-tree-mode)

(straight-use-package 'evil)
(setq evil-search-module 'evil-search
      evil-symbol-word-search t
      evil-mouse-word 'evil-WORD
      evil-want-Y-yank-to-eol t
      evil-want-C-u-scroll t
      evil-want-C-u-delete t
      evil-want-abbrev-expand-on-insert-exit nil
      evil-jumps-ignored-file-patterns nil
      evil--jumps-buffer-targets
      (rx
       (or
        (seq string-start "*"
             (or "new" "scratch" "eww" "Help")
             "*" string-end)
        (seq string-start "*Org Src " (+ anychar) "*"))))
(require 'evil)
(customize-set-variable 'evil-undo-system 'undo-tree)
(evil-mode)

(evil-define-command osf-evil-force-normal-state ()
  "Like `evil-force-normal-state', but also clear the search highlightings."
  :repeat abort
  :suppress-operator t
  (evil-normal-state)
  (evil-ex-nohighlight))

(evil-define-key* 'normal 'global
  (kbd "<escape>") #'osf-evil-force-normal-state)

(straight-use-package
 '(double-trigger
   :type git
   :repo "git@github.com:CloseToZero/double-trigger.git"))

(double-trigger-mode)

(defun osf-double-trigger-fn ()
  (when (eq evil-state 'insert)
    (evil-repeat-stop)
    (setq this-command #'evil-normal-state
          this-original-command #'evil-normal-state)))

(setq double-trigger-fn #'osf-double-trigger-fn)

(straight-use-package 'evil-visualstar)
(global-evil-visualstar-mode)

(straight-use-package
 `(evilize
   :type nil
   :local-repo
   ,(expand-file-name
     "evilize" osf-local-packages-dir)))
(require 'evilize)

(provide 'osf-evil)
