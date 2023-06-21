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

(straight-use-package 'smartparens)

(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

(require 'smartparens-config)

(sp-with-modes '(
                 js-mode
                 css-mode
                 zig-mode
                 java-mode
                 )
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

(with-eval-after-load 'smartparens-org
  (defun osf-sp-point-after-right-pair (id action _context)
    (when (memq action '(insert escape))
      (sp--looking-back-p (rx (or ")" "}" "]") (literal id)))))
  (sp-with-modes 'org-mode
    (sp-local-pair
     "_" "_"
     :unless '(sp-point-after-word-p osf-sp-point-after-right-pair))))

(smartparens-global-mode)

(straight-use-package 'puni)
(require 'puni)

(defun osf-puni-kill-line (&optional n)
  "Like `puni-kill-line', but won't try to delete whitespace."
  (interactive "P")
  (let* ((from (point))
         to)
    (if (and n (< n 0))
        (puni-backward-kill-line (- n))
      (setq to (save-excursion (forward-line (or n 1))
                               (point)))
      (unless (or kill-whole-line
                  ;; This is default behavior of Emacs: When the prefix
                  ;; argument is specified, always kill whole line.
                  n
                  ;; This means we started from the end of a line, and the
                  ;; following newline char should be killed.
                  (eq to (1+ from))
                  (save-excursion (goto-char to)
                                  (and (eobp) (eolp))))
        (setq to (1- to)))
      (puni-soft-delete from to 'strict-sexp 'beyond 'kill))))

(defun osf-puni-evil-mark-sexp-at-point ()
  "Like `puni-mark-sexp-at-point', but fix the issue that
the command marks an extra character at the end of the region
in evil visual state."
  (interactive)
  (puni-mark-sexp-at-point)
  (when (and (bound-and-true-p evil-local-mode)
             (use-region-p))
    (puni--mark-region (region-beginning) (1- (region-end)))))

(defun osf-puni-evil-puni-backward-sexp (&optional n)
  "Like `puni-backward-sexp', but fix the movement for non-insert states."
  (interactive "^p")
  (when (and (bound-and-true-p evil-local-mode)
             (not evil-move-beyond-eol)
             (not (evil-insert-state-p)))
    (forward-char))
  (puni-backward-sexp n))

(defvar-keymap osf-puni-map
  "r" #'puni-raise
  "s" #'puni-split
  "S" #'puni-splice
  "." #'puni-slurp-forward
  "," #'puni-barf-forward
  "<" #'puni-slurp-backward
  ">" #'puni-barf-backward
  "[" #'osf-puni-evil-puni-backward-sexp
  "]" #'puni-forward-sexp
  "m" #'osf-puni-evil-mark-sexp-at-point
  "k" #'puni-kill-line
  )
(fset #'osf-puni-map osf-puni-map)

(osf-keymap-set puni-mode-map
  "DEL" nil
  "C-d" nil
  "M-e" 'osf-puni-map)

(puni-global-mode)

(provide 'osf-pair)
