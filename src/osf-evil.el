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
(osf-inhibit-message #'undo-tree-load-history)
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

(defun osf-evil-define-key (state keymap &rest bindings)
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

(straight-use-package 'better-jumper)
(better-jumper-mode)
(osf-evil-define-key 'motion 'global
  "C-o" #'better-jumper-jump-backward
  "C-i" #'better-jumper-jump-forward)

(straight-use-package 'evil-visualstar)
(global-evil-visualstar-mode)

(straight-use-package
 `(evilize
   :type nil
   :local-repo
   ,(expand-file-name
     "evilize" osf-local-packages-dir)))
(setq evilize-want-jk-visual-lines t)
(require 'evilize)

(defun osf--evil-move-visual-line-set-jump-ad (&optional count)
  "Advice for `evil-next-visual-line' and `evil-previous-visual-line'
to set jump if the COUNT argument is non-nil (invoked by digit
argument, like 3j), only set jump if called interactively."
  (when (and (called-interactively-p 'any) count)
    (evil-set-jump)))
(dolist (fn '(evil-next-visual-line evil-previous-visual-line))
  (advice-add fn :before #'osf--evil-move-visual-line-set-jump-ad))

(osf-evil-define-key 'motion 'global
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "g j" #'evil-next-line
  "g k" #'evil-previous-line)

(defvar osf-leader-key "SPC"
  "Leader key (take effect in `osf-leader-key-states').")

(defvar osf-aux-leader-key "M-SPC"
  "Auxiliary leader key (take effect in `osf-aux-leader-key-states').")

(defvar osf-leader-key-states '(motion normal visual))

(defvar osf-aux-leader-key-states '(insert))

(defvar osf-local-leader-key (concat osf-leader-key " m")
  "Local leader key (take effect in `osf-local-leader-key-states').")

(defvar osf-aux-local-leader-key (concat osf-aux-leader-key " m")
  "Auxiliary local leader key (take effect in
`osf-aux-local-leader-key-states').")

(defvar osf-local-leader-key-states osf-leader-key-states)

(defvar osf-aux-local-leader-key-states osf-aux-leader-key-states)

(defun osf--leader-define-key (keymap local &rest bindings)
  (when bindings
    (let* ((leader-key (if local osf-local-leader-key osf-leader-key))
           (aux-leader-key
            (if local osf-aux-local-leader-key osf-aux-leader-key))
           (leader-key-bindings
            (cl-loop for (key def) on bindings by #'cddr
                     collect (concat leader-key " " key)
                     and collect def))
           (aux-leader-key-bindings
            (cl-loop for (key def) on bindings by #'cddr
                     collect (concat aux-leader-key " " key)
                     and collect def)))
      (apply #'osf-evil-define-key
             (if local osf-local-leader-key-states
               osf-leader-key-states)
             keymap leader-key-bindings)
      (apply #'osf-evil-define-key
             (if local osf-aux-local-leader-key-states
               osf-aux-leader-key-states)
             keymap aux-leader-key-bindings))))

(defun osf-leader-define-key (keymap &rest bindings)
  "Define leader key bindings."
  (declare (indent defun))
  (apply #'osf--leader-define-key keymap nil bindings))

(defun osf-local-leader-define-key (keymap &rest bindings)
  "Define local leader key bindings."
  (declare (indent defun))
  (apply #'osf--leader-define-key keymap t bindings))

(defun osf-release-leader-key (keymaps)
  (mapc (lambda (keymap)
          (osf-evil-define-key osf-leader-key-states
            keymap osf-leader-key nil)
          (osf-evil-define-key osf-aux-leader-key-states
            keymap osf-aux-leader-key nil)
          (when (keymapp keymap)
            (osf-define-key keymap osf-leader-key nil)
            (osf-define-key keymap osf-aux-leader-key nil)))
        (if (and (listp keymaps) (not (keymapp keymaps)))
            keymaps
          (list keymaps))))

(osf-release-leader-key 'global)
(with-eval-after-load 'magit
  (osf-release-leader-key (list magit-mode-map
                                magit-status-mode-map
                                magit-diff-mode-map
                                magit-blame-read-only-mode-map)))
(with-eval-after-load 'info
  (osf-release-leader-key Info-mode-map))
(with-eval-after-load 'dired
  (osf-release-leader-key dired-mode-map))

(provide 'osf-evil)
