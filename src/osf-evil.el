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

(customize-set-variable 'evil-undo-system 'undo-redo)

(defun osf-evil-define-key (state keymap key def &rest bindings)
  "Like `evil-define-key*', but wrap every keys in `kbd'."
  (declare (indent defun))
  ;; Check if `evil-normal-state-local-map' have not been initialized,
  ;; if so, call `evil-normalize-keymaps'.
  (when (and (eq keymap 'local)
             (or (not (boundp 'evil-normal-state-local-map))
                 (not (keymapp evil-normal-state-local-map))))
    (evil-normalize-keymaps))
  (let ((new-bindings (list (kbd key) def)))
    (setq new-bindings
          (append new-bindings
                  (cl-loop for (key def) on bindings by #'cddr
                           nconc (list (kbd key) def))))
    (apply #'evil-define-key* state keymap new-bindings)))

(evil-define-command osf-evil-force-normal-state ()
  "Like `evil-force-normal-state', but also clear search highlights.
NOTE: only clear search highlights when the `evil-search-module' is 'evil-search."
  :repeat abort
  :suppress-operator t
  (when (eq evil-search-module 'evil-search)
    (evil-ex-nohighlight))
  (evil-normal-state))

(osf-evil-define-key 'normal 'global
  "<escape>" #'osf-evil-force-normal-state)

(evil-mode)

(provide 'osf-evil)
