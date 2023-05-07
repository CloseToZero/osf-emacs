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

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(push '(fullscreen . fullboth) default-frame-alist)

(setq ring-bell-function #'ignore)

(column-number-mode)
(setq column-number-indicator-zero-based nil)

(straight-use-package 'modus-themes)

(setq modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-org-blocks nil
      modus-themes-completions '((t . (extrabold)))
      modus-themes-prompts nil)

(defun osf-make-invisible-window-dividers ()
  "Make window dividers invisible."
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'modus-themes-after-load-theme-hook #'osf-make-invisible-window-dividers)

(require 'modus-themes)
(modus-themes-load-theme 'modus-operandi)

(provide 'osf-ui)
