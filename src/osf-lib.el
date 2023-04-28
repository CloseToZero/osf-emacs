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

(defun osf-ff-find-other-file-ignore-include-line
    (&optional in-other-window event)
  (interactive (list current-prefix-arg last-nonmenu-event))
  (ff-find-other-file in-other-window event))

(provide 'osf-lib)
