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

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" osf-cache-dir))))

(setq auto-save-list-file-prefix
      (expand-file-name
       "auto-saves-"
       (expand-file-name
	    "auto-save-list" osf-cache-dir)))

(setq savehist-file (expand-file-name "saved-history" osf-cache-dir))

(setq org-id-locations-file
      (expand-file-name ".org-id-locations" osf-cache-dir))
(setq org-persist-directory
      (file-name-as-directory
       (expand-file-name "org-persist" osf-cache-dir)))

(setq project-list-file (expand-file-name "projects" osf-cache-dir))

(setq server-auth-dir
      (file-name-as-directory
       (expand-file-name
        "server" osf-cache-dir)))

(provide 'osf-clean-dir)
