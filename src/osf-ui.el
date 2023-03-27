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

(setq window-resize-pixelwise t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq ring-bell-function #'ignore)

(push '(fullscreen . fullboth) default-frame-alist)

(setq column-number-indicator-zero-based nil)
(column-number-mode)

(setq display-line-numbers-grow-only t
      display-line-numbers-type 'visual)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(defun osf--show-line-numbers-in-scratch-buffer ()
  (let ((target-buffer-name "*scratch*"))
    (when (string= (buffer-name) target-buffer-name)
      (with-current-buffer target-buffer-name
        (display-line-numbers-mode)))))
;; We assume `initial-major-mode' is fundamental-mode.
(add-hook 'osf-fundamental-mode-hook
          #'osf--show-line-numbers-in-scratch-buffer)

(setq whitespace-style '(face empty lines-tail trailing)
      whitespace-line-column 80)

(straight-use-package 'modus-themes)
(require 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-intense)
(load-theme 'modus-vivendi t)

(provide 'osf-ui)
