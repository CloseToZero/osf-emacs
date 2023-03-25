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

(setq slime-net-coding-system 'utf-8-unix)

(with-eval-after-load 'slime
  (when (executable-find "sbcl")
    (push '(sbcl ("sbcl")) slime-lisp-implementations)
    (setq slime-default-lisp 'sbcl))
  (when (executable-find "ros")
    (push '(roswell ("ros" "run")) slime-lisp-implementations)
    (setq slime-default-lisp 'roswell)))

(defun osf--slime-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
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
    "c o" #'slime-repl-clear-output))

(with-eval-after-load 'slime
  (osf--slime-setup-leader-key-bindings slime-mode-map))

(with-eval-after-load 'slime-repl
  (osf--slime-setup-leader-key-bindings slime-repl-mode-map)
  (osf--slime-repl-setup-leader-key-bindings slime-repl-mode-map))

(with-eval-after-load 'slime-repl
  (defalias #'osf-slime-repl-return-eval-at-end
    (osf-lisp-gen-repl-return-eval-at-end-fn #'slime-repl-return))

  (defun osf-slime-smart-repl-return ()
    "If around a presentation, inspect the presentation,
otherwise, fallback to `osf-slime-repl-return-eval-at-end'."
    (interactive)
    (call-interactively
     (if (slime-presentation-around-or-before-point-p)
         #'slime-inspect-presentation-at-point
       #'osf-slime-repl-return-eval-at-end)))

  (osf-evil-define-key '(normal insert) slime-repl-mode-map
    "RET" #'osf-slime-smart-repl-return))

(with-eval-after-load 'slime
  ;; The way `slime--completion-at-point' does the completions is
  ;; obsoleted.
  (defun osf--slime-completion-at-point ()
    (let ((slime-current-thread :repl-thread)
          (package (slime-current-package)))
      (pcase-let ((`(,beg . ,end)
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
        (list beg end
              (car (slime-eval
                    ;; Or swank:simple-completions
                    `(swank:fuzzy-completions
                      ,(thing-at-point 'symbol) ',package)))))))
  (advice-add #'slime--completion-at-point
              :override #'osf--slime-completion-at-point))

(provide 'osf-common-lisp)
