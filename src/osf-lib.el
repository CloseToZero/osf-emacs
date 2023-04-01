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

(defun osf-ensure-is-list (x)
  (if (listp x) x (list x)))

(defun osf-define-key (keymap &rest bindings)
  "Like `define-key', but can define multiple bindings at once.
NOTE: each key in a binding will be wrapped inside `kbd'."
  (declare (indent defun))
  (cl-loop for (key def) on bindings by #'cddr
           do (define-key keymap (kbd key) def)))

(defun osf-global-define-key (&rest bindings)
  "Use `osf-define-key' to define key bindings within `current-global-map'."
  (apply #'osf-define-key (current-global-map) bindings))

(defun osf--ad-inhibit-message (fn &rest args)
  (let ((inhibit-message (not (called-interactively-p 'interactive))))
    (apply fn args)))

(defun osf-inhibit-message (fn)
  (advice-add fn :around #'osf--ad-inhibit-message))

(defun osf-edit-config ()
  (interactive)
  (find-file user-init-file))

(defvar osf-create-src-hist nil)
(defun osf-create-src (basename)
  (interactive
   (list (read-string "Name: " nil 'osf-create-src-hist)))
  (let* ((feature (concat "osf-" basename))
	 (filepath (expand-file-name (concat feature ".el") osf-src-dir)))
    (when (file-exists-p filepath)
      (user-error "File ~A already exists" filepath))
    (find-file filepath)
    (setq-local lexical-binding t)
    (insert ";;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs \"28.1\"))

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

")
    (save-excursion
      (insert "

(provide '" feature ")"))
    (save-buffer)))

(defun osf-indexed-reset-prefix-arg ()
  (interactive)
  ;; We don't need to set the `current-prefix-arg' to nil,
  ;; the prefix is already consumed by this command.
  (minibuffer-message "Prefix reset"))

(defun osf--indexed-print-selected-candidate ()
  ;; It's not clear from the documentation what's the relationship
  ;; between `prefix-arg' and `current-prefix-arg', but I found
  ;; `current-prefix-arg' may not up-to-date after invoked
  ;; `digit-argument', `negative-argument' etc, and sometimes
  ;; `prefix-arg' is nil but `current-prefix-arg' is non-nil or vice
  ;; verse, prefer `prefix-arg' and use `current-prefix-arg' as
  ;; fallback seems to give the correct up-to-date prefix.
  (if (and (null prefix-arg) (null current-prefix-arg))
      (minibuffer-message "Prefix arg not used")
    (minibuffer-message
     "Select %sth candidate"
     (prefix-numeric-value (or prefix-arg current-prefix-arg)))))

(defun osf-indexed-show-prefix-arg (arg)
  (interactive "P")
  (osf--indexed-print-selected-candidate)
  ;; Keep the consumed prefix arg.
  (setq prefix-arg arg))

(defun osf-indexed-digit-argument (arg)
  (interactive "P")
  (digit-argument arg)
  (osf--indexed-print-selected-candidate))

;; It's meaningless to select the negative-th candidate, we keep the
;; following command just for consistency (e.g. print prefix arg).
(defun osf-indexed-negative-argument (arg)
  (interactive "P")
  (negative-argument arg)
  (osf--indexed-print-selected-candidate))

(defun osf-indexed-setup-keymap (keymap)
  (osf-define-key keymap
    "M-=" #'osf-indexed-show-prefix-arg
    "M-DEL" #'osf-indexed-reset-prefix-arg
    "M-1" #'osf-indexed-digit-argument
    "M-2" #'osf-indexed-digit-argument
    "M-3" #'osf-indexed-digit-argument
    "M-4" #'osf-indexed-digit-argument
    "M-5" #'osf-indexed-digit-argument
    "M-6" #'osf-indexed-digit-argument
    "M-7" #'osf-indexed-digit-argument
    "M-8" #'osf-indexed-digit-argument
    "M-9" #'osf-indexed-digit-argument
    "M-0" #'osf-indexed-digit-argument
    "M--" #'osf-indexed-negative-argument))

(provide 'osf-lib)
