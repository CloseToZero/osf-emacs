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
    (setq slime-default-lisp 'roswell))

  (let ((local-hyperspec-dir (expand-file-name "~/HyperSpec/")))
    (when (file-directory-p local-hyperspec-dir)
      (setq common-lisp-hyperspec-root
            (concat "file:///" local-hyperspec-dir))))

  (defun osf-slime-last-expression ()
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (slime-last-expression))
      (slime-last-expression)))

  (defun osf-gen-slime-eval-in-repl-fn (form-fn)
    "Return a function like `slime-eval-last-expression-in-repl',
but keep in the same window.
The returned function call FORM-FN to get the form to be evaluated."
    (lambda (prefix)
      (interactive "P")
      (let ((form (funcall form-fn))
            (buffer-name (buffer-name (current-buffer)))
            (new-package (slime-current-package))
            (old-package (slime-lisp-package))
            (slime-repl-suppress-prompt t)
            (yank-back nil))
        (with-current-buffer (slime-output-buffer)
          (unless (eq (current-buffer) (window-buffer))
            (display-buffer (current-buffer) '(display-buffer-reuse-window)))
          (goto-char (point-max))
          ;; Kill pending input in the REPL
          (when (< (marker-position slime-repl-input-start-mark) (point))
            (kill-region slime-repl-input-start-mark (point))
            (setq yank-back t))
          (unwind-protect
              (progn
                (insert-before-markers (format "\n;;; from %s\n" buffer-name))
                (when new-package
                  (slime-repl-set-package new-package))
                (let ((slime-repl-suppress-prompt nil))
                  (slime-repl-insert-prompt))
                (insert form)
                (slime-repl-return))
            (unless (or prefix (equal (slime-lisp-package) old-package))
              ;; Switch back.
              (slime-repl-set-package old-package)
              (let ((slime-repl-suppress-prompt nil))
                (slime-repl-insert-prompt))))
          ;; Put pending input back.
          (when yank-back
            (yank))))))

  (defalias 'osf-slime-eval-last-expression-in-repl
    (osf-gen-slime-eval-in-repl-fn #'osf-slime-last-expression))

  (defalias 'osf-slime-eval-defun-in-repl
    (osf-gen-slime-eval-in-repl-fn #'slime-defun-at-point))
  )

(defun osf--slime-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
    "e r e" #'osf-slime-eval-last-expression-in-repl
    "e r d" #'osf-slime-eval-defun-in-repl

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

(with-eval-after-load 'slime-repl
  (defalias 'osf-slime-repl-return-eval-at-end
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
      (when-let ((symbol (thing-at-point 'symbol)))
        (pcase-let ((`(,beg . ,end)
                     (bounds-of-thing-at-point 'symbol)))
          (list beg end
                (car (slime-eval
                      ;; Or swank:simple-completions
                      `(swank:fuzzy-completions
                        ,(substring-no-properties symbol) ',package))))))))
  (advice-add #'slime--completion-at-point
              :override #'osf--slime-completion-at-point))

(provide 'osf-common-lisp)
