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
      evil-jumps-ignored-file-patterns nil)

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

(defun osf--evil-move-line-set-jump-ad (&optional count)
  "Advice for `evil-next-visual-line', `evil-previous-visual-line',
`evil-next-line' and `evil-previous-line' to set jump if the COUNT
argument is non-nil (invoked by digit argument, like 3j), only set
jump if called interactively."
  (when (and (called-interactively-p 'any) count)
    (evil-set-jump)))
(dolist (fn '(evil-next-visual-line
              evil-previous-visual-line
              evil-next-line
              evil-previous-line))
  (advice-add fn :before #'osf--evil-move-line-set-jump-ad))

(osf-evil-define-key 'motion 'global
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "g j" #'evil-next-line
  "g k" #'evil-previous-line)

(require 'osf-evil-leader-key)
(require 'osf-evilize)

(evil-mode)

(straight-use-package
 '(double-trigger
   :inherit nil
   :type git
   :repo "git@github.com:CloseToZero/double-trigger.git"))

(require 'double-trigger)

(defun osf-double-trigger-fn ()
  (when (eq evil-state 'insert)
    (evil-repeat-stop)
    (setq this-command #'evil-normal-state
          this-original-command #'evil-normal-state)))

(setq double-trigger-fn #'osf-double-trigger-fn
      double-trigger-keys "ii")

(double-trigger-mode)

(straight-use-package 'better-jumper)

(setq better-jumper-ignored-file-patterns evil-jumps-ignored-file-patterns)

(require 'better-jumper)

(defun osf--better-jumper--jump (idx shift &optional context)
  "Fix `better-jumper--jump'.
Don't use `better-jumper--buffer-targets' to determine whether the
place is a buffer or file, first check if the place is a buffer, then
check if the place is a existing file, if the place it neither a
buffer or a file, don't jump."
  (let ((jump-list (better-jumper--get-jump-list context)))
    (setq idx (+ idx shift))
    (let* ((size (ring-length jump-list)))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'better-jumper-pre-jump-hook)
        (let* ((marker-table (better-jumper--get-marker-table context))
               (place (ring-ref jump-list idx))
               (file-name (nth 0 place))
               (pos (nth 1 place))
               (marker-key (nth 2 place))
               (marker (gethash marker-key marker-table)))
          (setq better-jumper--jumping t)
          (when better-jumper-use-evil-jump-advice
            (setq evil--jumps-jumping-backward t))
          (let ((switched nil))
            (cond ((get-buffer file-name)
                   (switch-to-buffer file-name)
                   (setq switched t))
                  ((file-exists-p file-name)
                   (find-file file-name)
                   (setq switched t)))
            (when switched
              (if (and marker (marker-position marker))
                  (goto-char marker)
                (goto-char pos)
                (puthash marker-key (point-marker) marker-table))))
          (setf (better-jumper-jump-list-struct-idx (better-jumper--get-struct context)) idx)
          (setq better-jumper--jumping nil)
          (run-hooks 'better-jumper-post-jump-hook))))))

(advice-add #'better-jumper--jump :override #'osf--better-jumper--jump)

;; Ensure `better-jumper--push' push position even when there is not
;; buffer-file-name and don't use `better-jumper--buffer-targets'.
(setq better-jumper--buffer-targets (rx (* anychar)))

(better-jumper-mode)

(osf-evil-define-key 'motion 'global
  "C-o" #'better-jumper-jump-backward
  "C-i" #'better-jumper-jump-forward)

(straight-use-package 'evil-visualstar)
(global-evil-visualstar-mode)

(straight-use-package 'evil-exchange)
(evil-exchange-install)

(straight-use-package
 '(evil-snipe
   :inherit nil
   :type git
   :repo "git@github.com:CloseToZero/evil-snipe.git"))

(setq evil-snipe-aliases
      (append
       '((?\" "[\"“”]")
         (?\\ "[\\、]")
         (?, "[,，]")
         (?\( "[(（]")
         (?\) "[)）]")
         (?: "[:：]")
         (?. "[.。◦]")
         (?a "[aAα]")
         (?x "[xX×]")
         (?u "[uU∪]")
         (?n "[nN∩]")
         (?c "[cC∈ε]")
         (?> "[>≥]")
         (?< "[<≤]")
         (?- "[-−→]"))
       (mapcar (lambda (ch)
                 `(,ch ,(concat "["
                                (string ch)
                                (string (upcase ch))
                                "]")))
               (number-sequence ?a ?z)))
      evil-snipe-aliases
      (mapcan (lambda (item)
                (let ((ch (car item)))
                  (if (= (downcase ch) (upcase ch))
                      (list item)
                    (list item (cons (upcase ch) (cdr item))))))
              evil-snipe-aliases))

(evil-snipe-mode)
(evil-snipe-override-mode)

(provide 'osf-evil)
