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

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq ring-bell-function #'ignore)

(push '(fullscreen . fullboth) default-frame-alist)

(setq column-number-indicator-zero-based nil)
(column-number-mode)

(setq display-line-numbers-type 'visual
      display-line-numbers-grow-only t)
(global-display-line-numbers-mode)

(defun osf-turn-off-display-line-numbers-mode ()
  (display-line-numbers-mode -1))

(add-hook 'image-mode-hook #'osf-turn-off-display-line-numbers-mode)

(setq whitespace-style '(face lines-tail empty)
      whitespace-line-column 80)

(provide 'osf-ui)
