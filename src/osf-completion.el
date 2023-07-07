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

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(with-eval-after-load 'minibuffer
  ;; Unbind "SPC" from `minibuffer-complete-word',
  ;; Bind `minibuffer-complete-word' to M-SPC instead.
  (osf-keymap-set minibuffer-local-completion-map
    "SPC" nil
    "M-SPC" #'minibuffer-complete-word))

(straight-use-package 'orderless)

(setq completion-styles '(orderless basic partial-completion)
      orderless-matching-styles '(orderless-literal)
      orderless-style-dispatchers '(orderless-affix-dispatch)
      orderless-affix-dispatch-alist
      `((?# . ,#'orderless-regexp)
        (?! . ,#'orderless-without-literal)))

(defvar osf-default-completion-styles
  (let ((sv (get 'completion-styles 'standard-value)))
    (and (consp sv)
         (condition-case nil
             (eval (car sv) t)
           (error completion-styles)))))

(with-eval-after-load 'company
  (defun osf--disable-orderless-in-company (orig-fun &rest args)
    "Diable orderless completion style when company is doing the completion."
    (let ((completion-styles osf-default-completion-styles))
      (apply orig-fun args)))
  (advice-add #'company--perform :around #'osf--disable-orderless-in-company))

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-indexed)))

(setq vertico-multiline
      (cons #("\\\\n" 0 3 (face vertico-multiline))
            #("..." 0 3 (face vertico-multiline))))

(require 'vertico)

(osf-indexed-setup-keymap vertico-map)

(vertico-mode)

(vertico-indexed-mode)

(straight-use-package 'marginalia)

(marginalia-mode)

(osf-keymap-set minibuffer-local-map
  "M-A" #'marginalia-cycle)

(straight-use-package 'consult)

(with-eval-after-load 'consult
  (defun consult--buffer-sort-identically-but-current (buffers)
    "Sort identically but exclude the current buffer.
Most importantly, don't put visible buffers in the bottom of the list."
    ;; Rationale: split the current window into windows A and B, both
    ;; windows now display the same buffer B.
    ;; In window A, Invoke `consult-buffer' then press RET immediately
    ;; to switch to the most recent buffer R, view some content, then
    ;; use `consult-buffer' RET again to switch to the previous buffer
    ;; B. Without looking at the window B (focusing on window A), I
    ;; would expect that I would be switched to the buffer B, but
    ;; since the buffer B is visible right now in window B, I would
    ;; actucally go into a unexpected buffer.
    (let ((current (current-buffer)))
      (nconc (delq current buffers) (list current))))

  (plist-put consult--source-buffer :items
             (lambda ()
               (consult--buffer-query
                :sort 'identically-but-current
                :as #'buffer-name)))
  )

(osf-leader-define-key 'global
  "SPC" #'execute-extended-command
  "b b" #'consult-buffer)

(osf-evil-define-key 'insert 'global
  "M-y" #'consult-yank-pop)

(defun osf-completion-in-region-function (&rest args)
  (apply (if vertico-mode
             #'consult-completion-in-region
           #'completion--in-region)
         args))
(setq completion-in-region-function #'osf-completion-in-region-function)

(straight-use-package 'company)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-show-quick-access t
      company-dabbrev-ignore-case t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-buffers (rx (or (regexp "\\`[ *]")
                                             (seq ".pdf" string-end)))
      company-tooltip-align-annotations t
      company-require-match 'never
      company-global-modes
      '(not eshell-mode shell-mode term-mode)
      company-backends '(company-capf
                         company-cmake
                         company-files
                         (company-dabbrev-code
                          company-gtags
                          company-etags
                          company-keywords)
                         company-dabbrev))

(require 'company)

(evil-declare-change-repeat #'company-complete-quick-access)

(defun osf--ensure-company-keymaps-not-been-overridden ()
  ;; Note:
  ;; Check if `company-emulation-alist' is in
  ;; `emulation-mode-map-alists', if true, call
  ;; `company-ensure-emulation-alist' to ensure
  ;; `company-emulation-alist' is the first item of
  ;; `emulation-mode-map-alists', thus has a higher
  ;; priority than keymaps of evil-mode.
  ;; We raise the priority of company-mode keymaps
  ;; unconditionally even when completion is not
  ;; activated. This should not cause problems,
  ;; because when completion is activated, the value of
  ;; `company-emulation-alist' is ((t . company-my-keymap)),
  ;; when completion is not activated, the value is ((t . nil)).
  (when (memq 'company-emulation-alist emulation-mode-map-alists)
    (company-ensure-emulation-alist)))

(add-hook 'evil-local-mode-hook
          #'osf--ensure-company-keymaps-not-been-overridden)

(defun osf-company-complete-by-completing-read ()
  "Complete current company candidates by `completing-read'."
  (interactive)
  (unless company-candidates (user-error "No company candidates avaiable"))
  (when-let (cand (completing-read "Candidate: " company-candidates))
    (company-finish cand)))

(osf-keymap-set company-active-map
  "M-<" #'company-select-first
  "M->" #'company-select-last
  "M-\\" #'osf-company-complete-by-completing-read)

(global-company-mode 1)

(defun osf-switch-to-completions-or-scroll-down (&optional arg)
  (interactive "^P")
  (cond (arg (scroll-down-command arg))
        (t (if-let ((window (get-buffer-window "*Completions*" 0)))
               (if (eq (selected-window) window)
                   (scroll-down-command)
                 (switch-to-completions))
             (scroll-down-command)))))

(osf-keymap-global-set
  "M-v" #'osf-switch-to-completions-or-scroll-down)

(provide 'osf-completion)
