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

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-indexed)))
(setq vertico-multiline
      (cons #("\\\\n" 0 3 (face vertico-multiline))
            #("..." 0 3 (face vertico-multiline))))
(vertico-mode)

(defun osf-vertico-goto-index-and-exit (n)
  (vertico--goto n)
  (vertico-exit))
(dotimes (i 10)
  (let ((fn (intern
             (concat "osf-vertico-goto-" (prin1-to-string i) "-and-exit"))))
    (defalias fn
      (lambda ()
        (interactive)
        (osf-vertico-goto-index-and-exit i)))
    (osf-define-key vertico-map
      (concat "M-" (prin1-to-string i)) fn)))
(vertico-indexed-mode)

(straight-use-package 'marginalia)
(marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)

(straight-use-package 'orderless)
;; The basic style is for commands that rely on dynamic completion
;; tables like `completion-table-dynamic' and
;; `completion-table-in-turn'.
(setq completion-styles '(orderless basic)
      orderless-matching-styles '(orderless-literal)
      orderless-style-dispatchers '(orderless-affix-dispatch))
;; Workaround for TRAMP hostname completion to work, no need for
;; Emacs 30+.
(when (< emacs-major-version 30)
  (setq completion-category-overrides
        '((file (styles basic partial-completion)))))

(straight-use-package '(corfu :files (:defaults "extensions/*")
                              :includes (corfu-indexed)))
(global-corfu-mode)
(setq tab-always-indent 'complete)

(defun osf-corfu-goto-index-and-insert (n)
  (corfu--goto n)
  (corfu-insert))
(dotimes (i 10)
  (let ((fn (intern
             (concat "osf-corfu-goto-" (prin1-to-string i) "-and-insert"))))
    (defalias fn
      (lambda ()
        (interactive)
        (osf-corfu-goto-index-and-insert i)))
    (osf-define-key corfu-map
      (concat "M-" (prin1-to-string i)) fn)))

(corfu-indexed-mode)

(defun osf--enable-corfu-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'osf--enable-corfu-in-minibuffer)

(straight-use-package 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

(straight-use-package 'consult)
(setq consult-async-min-input 2)

(osf-leader-define-key 'global
  "b b" #'consult-buffer

  "/ r" #'consult-ripgrep)
(osf-evil-define-key 'insert 'global
  "M-y" #'consult-yank-from-kill-ring)

(with-eval-after-load 'comint
  (osf-evil-define-key '(normal insert) comint-mode-map
    "M-r" #'consult-history))
(defun osf-bind-consult-history ()
  (osf-evil-define-key '(normal insert) 'local
    "M-r" #'consult-history))
(add-hook 'eshell-mode-hook #'osf-bind-consult-history)
(with-eval-after-load 'consult
  (push '(elime-repl-mode slime-repl-input-history)
        consult-mode-histories))
(with-eval-after-load 'slime-repl
  (osf-evil-define-key '(normal insert) slime-repl-mode-map
    "M-r" #'consult-history))

(provide 'osf-completion)
