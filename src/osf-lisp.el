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

(straight-use-package 'lispy)

(setq-default lispy-no-space t)

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                inferior-emacs-lisp-mode-hook
                lisp-mode-hook
                slime-repl-mode-hook
                lisp-data-mode-hook
                scheme-mode-hook))
  (add-hook hook #'lispy-mode))

(defun osf--enable-lispy-for-eval-expression ()
  (when (eq this-command 'eval-expression)
    ;; Use `lisp-indent-line' to fix the weird indentation.
    (setq-local indent-line-function #'lisp-indent-line)
    ;; Start the code from the second line, so press "i" to indent
    ;; the code won't trigger the error "Text is read-only".
    (insert "\n")
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook #'osf--enable-lispy-for-eval-expression)

(with-eval-after-load 'lispy
  ;; Support the evaluation of `lisp-data-mode'.
  (setq lispy-eval-alist
        (mapcar (lambda (handler)
                  (if (eq (cl-third handler) 'lispy--eval-elisp)
                      (cons (cons 'lisp-data-mode (cl-first handler))
                            (cl-rest handler))
                    handler))
                lispy-eval-alist))

  (defvar osf-lispy-repl-eval
    '((minibufferp . read--expression-try-read)
      ((mode . slime-repl-mode) . slime-repl-return)
      ((mode . sly-mrepl-mode) . osf-sly-mrepl-return)
      ((mode . inferior-emacs-lisp-mode) . ielm-return))
    "A alist to determine whether a buffer is a REPL buffer \
and how to evaluate its expressions.
Each element is of the form (MODE . EVAL-FUN).
MODE can be a cons of the form (mode . MAJOR-MODE), the car is the
symbol 'mode, the cdr specify the major-mode of a REPL buffer.
MODE can also be a function, the function will be called with 0 argument
in a buffer and it should return non-nil if the buffer is a repl buffer.
EVAL-FUN should be a function, and it should try to eval the expression
at/around the point.")

  (defun osf--repl-buffer-eval-fun ()
    "Return the REPL evaluation function of the current buffer.
If current buffer is not a REPL buffer, return nil.
See `osf-lispy-repl-eval'."
    (cl-some (lambda (mode-eval)
               (let ((mode (car mode-eval))
                     (eval-fun (cdr mode-eval)))
                 (cond ((and (consp mode)
                             (eq (car mode) 'mode)
                             (eq (cdr mode) major-mode))
                        eval-fun)
                       ((functionp mode)
                        (when (funcall mode) eval-fun)))))
             osf-lispy-repl-eval))

  (defun osf-lispy-enhance-return-for-repl (orig-fun &rest args)
    "Enhance RET for REPL buffers.

Whenever press RET in REPL buffers,
Try to evaluate the expression if any of the following conditions is true:
1. In motion state.
2. In normal state.
3. In insert state and the point is at the end of buffer.
Otherwise, call `newline-and-indent'.

Whether a buffer is a REPL buffer and the corresponding evaluation function
are controlled by `osf-lispy-repl-eval'."
    (let ((eval-fun (osf--repl-buffer-eval-fun)))
      (cond (eval-fun
             (if (or (evil-motion-state-p) (evil-normal-state-p)
                     (and (evil-insert-state-p) (= (point) (point-max))))
                 (funcall eval-fun)
               (newline-and-indent)))
            (t (apply orig-fun args)))))

  (advice-add #'lispy-newline-and-indent-plain :around #'osf-lispy-enhance-return-for-repl)

  (defun osf--lispy-bind-return-in-repl ()
    "Bind RET in motion and normal state to `lispy-newline-and-indent-plain' \
in REPL buffers.

Whether a buffer is a REPL buffer is controlled in `osf-lispy-repl-eval'."
    (when (osf--repl-buffer-eval-fun)
      (osf-evil-define-key '(motion normal insert) 'local
        "RET" #'lispy-newline-and-indent-plain
        "<return>" #'lispy-newline-and-indent-plain)))

  (defun osf--lispy-cleanup-lispy-bindings-in-repl ()
    (when (osf--repl-buffer-eval-fun)
      (osf-evil-define-key '(motion normal insert) 'local
        "RET" nil
        "<return>" nil)))
  (add-hook 'lispy-mode-hook #'osf--lispy-bind-return-in-repl)
  ;; The minibuffer won't be recreated, thus the bindings setup by
  ;; osf--lispy-bind-return-in-repl will be remained and we need to
  ;; clear the bindings explicitly.
  (add-hook 'minibuffer-exit-hook #'osf--lispy-cleanup-lispy-bindings-in-repl)

  (with-eval-after-load 'sly-mrepl
    (defun osf-sly-mrepl-return (&optional end-of-input)
      "Like `sly-mrepl-return', but go to the end of buffer after \
eval the expression."
      (interactive "P")
      (let ((goto-end
             (and (not sly-mrepl--read-mark)
                  (or (sly-input-complete-p (sly-mrepl--mark) (point-max))
                      end-of-input))))
        (sly-mrepl-return end-of-input)
        (when goto-end (goto-char (point-max))))))

  (defun osf--lispy-mark-symbol-dont-include-tail-char (&rest _)
    (when (and (eq this-command #'lispy-mark-symbol)
               (or (evil-motion-state-p) (evil-normal-state-p)))
      (backward-char)))

  (advice-add #'lispy-mark-symbol
              :after #'osf--lispy-mark-symbol-dont-include-tail-char))

(straight-use-package 'lispyville)

(add-hook 'lispy-mode-hook #'lispyville-mode)

(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(operators
     c-w c-u prettify text-objects additional-movement
     (commentary normal visual) additional slurp/barf-lispy)))

(provide 'osf-lisp)
