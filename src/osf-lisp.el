;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2024 Zhexuan Chen <2915234902@qq.com>

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

(straight-use-package 'paredit)

(setq paredit-space-for-delimiter-predicates (list #'ignore))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                lisp-data-mode-hook
                scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))

(with-eval-after-load 'paredit
  (osf-evil-define-key 'normal paredit-mode-map
    ">" #'paredit-forward-slurp-sexp
    "<" #'paredit-forward-barf-sexp
    "[" #'paredit-backward-slurp-sexp
    "]" #'paredit-backward-barf-sexp
    "M-r" #'paredit-raise-sexp
    "M-s" #'paredit-splice-sexp
    "M-S" #'paredit-split-sexp
    "M-(" #'paredit-wrap-round
    "M-q" #'paredit-reindent-defun
    "D" #'paredit-kill)

  (osf-evil-define-key 'insert paredit-mode-map
    "(" #'paredit-open-round
    ")" #'paredit-close-round
    "[" #'paredit-open-square
    "]" #'paredit-close-square
    "\\" #'paredit-backslash
    "\"" #'paredit-doublequote
    "DEL" #'paredit-backward-delete
    "C-d" #'paredit-forward-delete
    "RET" #'paredit-RET)

  (osf-evil-define-key '(normal insert) paredit-mode-map
    "M-;" #'paredit-comment-dwim))

(defun osf--enable-paredit-for-eval-expression ()
  (when (eq this-command 'eval-expression)
    ;; Use `lisp-indent-line' to fix the weird indentation.
    (setq-local indent-line-function #'lisp-indent-line)
    (enable-paredit-mode)))
(add-hook 'minibuffer-setup-hook #'osf--enable-paredit-for-eval-expression)

(defun osf--allow-paredit-eval-expression-at-end (fn &rest args)
  (if (and (minibufferp)
           (or (evil-normal-state-p)
               (and (evil-insert-state-p)
                    (= (point) (point-max)))))
      (exit-minibuffer)
    (apply fn args)))

(advice-add #'paredit-RET :around #'osf--allow-paredit-eval-expression-at-end)

(provide 'osf-lisp)
