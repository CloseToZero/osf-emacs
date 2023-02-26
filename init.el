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

(defvar osf/cache-dir
  (expand-file-name ".cache" user-emacs-directory))

(defvar osf/backup-dir
  (expand-file-name "backup" osf/cache-dir))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq backup-directory-alist `(("." . ,osf/backup-dir)))

(setq auto-save-list-file-prefix
      (expand-file-name
       "auto-saves-"
       (expand-file-name
	"auto-save-list" osf/cache-dir)))

(setq org-persist-directory
      (file-name-as-directory
       (expand-file-name "org-persist" osf/cache-dir)))

(global-set-key (kbd "C-x f") #'find-file)

(load custom-file t t)
