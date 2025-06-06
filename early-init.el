;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

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

(defvar osf-early-custom-file
  (expand-file-name "early-custom.el" user-emacs-directory))

(when (>= emacs-major-version 27)
  (setq package-enable-at-startup nil))

(defvar osf-fundamental-mode-hook nil)

(defun osf--run-fundamental-mode-hook ()
  (when (eq major-mode 'fundamental-mode)
    (run-hooks 'osf-fundamental-mode-hook)))

(add-hook 'after-change-major-mode-hook
          #'osf--run-fundamental-mode-hook)

(load osf-early-custom-file t t)
