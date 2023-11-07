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

(straight-use-package 'slime)

(setq slime-net-coding-system 'utf-8-unix
      slime-contribs '(slime-fancy))

(with-eval-after-load 'slime
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
    (setq slime-default-lisp 'sbcl))

  (when (executable-find "ros")
    (add-to-list 'slime-lisp-implementations '(roswell ("ros" "run")))
    (setq slime-default-lisp 'roswell))

  (defun osf--slime-auto-select-popup-buffer ()
    (add-hook
     'window-buffer-change-functions
     #'osf--slime-auto-select-popup-buffer-change-function nil t))
  (add-hook 'slime-popup-buffer-mode-hook #'osf--slime-auto-select-popup-buffer)

  (defun osf--slime-auto-select-popup-buffer-change-function (window)
    (unless (eq (selected-window) window)
      (select-window window))
    (remove-hook
     'window-buffer-change-functions
     #'osf--slime-auto-select-popup-buffer-change-function t)))

(straight-use-package 'slime-company)
(setq slime-company-completion 'fuzzy)
(add-to-list 'slime-contribs 'slime-company)

(defun osf--slime-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
    "e e" #'osf-slime-eval-last-expression-in-repl
    "e d" #'osf-slime-eval-defun-in-repl

    "h f" #'slime-describe-function
    "h s" #'slime-describe-symbol
    "h h" #'slime-documentation-lookup
    "h H" #'slime-hyperspec-lookup
    "h ~" #'common-lisp-hyperspec-format
    "h g" #'common-lisp-hyperspec-glossary-term
    "h #" #'common-lisp-hyperspec-lookup-reader-macro
    "h a a" #'slime-apropos
    "h a A" #'slime-apropos-all
    "h a p" #'slime-apropos-package

    "q q" #'slime-quit-lisp
    "q r" #'slime-restart-inferior-lisp))

(defun osf--slime-repl-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
    "c b" #'slime-repl-clear-buffer
    "c o" #'slime-repl-clear-output
    "p s" #'slime-repl-set-package))

(with-eval-after-load 'slime
  (osf--slime-setup-leader-key-bindings slime-mode-map))

(with-eval-after-load 'slime-repl
  (osf--slime-setup-leader-key-bindings slime-repl-mode-map)
  (osf--slime-repl-setup-leader-key-bindings slime-repl-mode-map))

(provide 'osf-common-lisp)
