;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs

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

(straight-use-package 'compat)
(require 'compat)

(straight-use-package 'posframe)

(defun osf-nthcdr (list n)
  (when (>= n 0)
    (cl-loop repeat n
             for cons on list by #'cdr
             finally (return cons))))

(defun osf-truncate-list! (list n)
  "Truncate the LIST to the max length of N, return the truncated list.

Example:
(osf-truncate-list! '(1 2 3) 4) => '(1 2 3)
(osf-truncate-list! '(1 2 3 4) 4) => '(1 2 3 4)
(osf-truncate-list! '(1 2 3 4 5) 4) => '(1 2 3 4)"
  (cond ((zerop n) nil)
        (t (when-let ((cons (osf-nthcdr list (- n 1))))
             (setcdr cons nil))
           list)))

(defun osf-edit-config ()
  (interactive)
  (find-file user-init-file))

(defvar osf-create-src-hist nil)
(osf-add-saved-vars 'osf-create-src-hist)
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

(defun osf--ad-inhibit-message (fn &rest args)
  (let ((inhibit-message (not (called-interactively-p 'interactive))))
    (apply fn args)))

(defun osf-inhibit-message (fn)
  (advice-add fn :around #'osf--ad-inhibit-message))

(defun osf-keymap-set (keymap &rest bindings)
  "Like `keymap-set', but allow define multiple bindings together.
Example:
(osf-keymap-set keymap1
  \"a\" #'command1
  \"b\" #'command2)"
  (declare (indent defun))
  (cl-loop for (key def) on bindings by #'cddr
           do
           (if (eq def :remove)
               (keymap-unset keymap key t)
             (keymap-set keymap key def))))

(defun osf-keymap-global-set (&rest bindings)
  "Like `keymap-global-set', but allow define multiple bindings together.
Example:
(osf-keymap-global-set
  \"a\" #'command1
  \"b\" #'command2)"
  (declare (indent defun))
  (cl-loop for (key def) on bindings by #'cddr
           do (keymap-global-set key def)))

(defun osf-open-by-system-default-app (file)
  (interactive "fOpen: ")
  (if (eq osf-system-type 'windows)
      (w32-shell-execute "open" (expand-file-name file))
    (call-process (pcase osf-system-type
                    ('mac "open")
                    ((or 'linux 'bsd) "xdg-open"))
                  nil 0 nil (expand-file-name file))))

(defun osf-ff-find-other-file-ignore-include-line
    (&optional in-other-window event)
  (interactive (list current-prefix-arg last-nonmenu-event))
  (ff-find-other-file in-other-window event))

(defun osf-random-alphanum ()
  (let* ((alphanum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alphanum))))
    (elt alphanum i)))

(defun osf-random-text (n)
  (cl-loop with text = (make-string n 0)
           for i from 0 below n
           do (aset text i (osf-random-alphanum))
           finally (return text)))

(defun osf-insert-random-text (n)
  "Insert n characters random text."
  (interactive "nN: ")
  (insert (osf-random-text n)))

(defun osf-message-without-logging (format-string &rest args)
  (let ((message-log-max nil))
    (funcall #'message format-string args)))

(defmacro osf-annotate-within-function (fn &optional var-sym)
  "Advice the function FN to tell others we are within the function FN
by the variable SYM.
This function will define the variable VAR-SYM
by `defvar' and use it to inform others let we are within the function
FN.
By default, VAR-SYM will named osf-within-FN? (NOTE: the FN need to
be a symbol.)"
  (let ((var-sym
         (or var-sym (intern (format "osf-within-%s?" (symbol-name fn)))))
        (advice-sym
         (intern (format "osf--annotate-within-function-%s" (symbol-name fn)))))
    `(progn
       (defvar ,var-sym nil)
       (defun ,advice-sym (fn &rest args)
         (let ((,var-sym t))
           (apply fn args)))
       (advice-add ',fn :around ',advice-sym))))

(provide 'osf-lib)
