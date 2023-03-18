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

(push '(fullscreen . maximized) default-frame-alist)

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
(defun osf--whitespace-mode-enable-predicate ()
  (and (cond
        ((eq whitespace-global-modes t) t)
        ((listp whitespace-global-modes)
         (if (eq (car-safe whitespace-global-modes) 'not)
             (not (apply #'derived-mode-p (cdr whitespace-global-modes)))
           (apply #'derived-mode-p whitespace-global-modes)))
        (t nil))
       ;; ...we have a display (not running a batch job)
       (not noninteractive)
       ;; ...the buffer is not internal (name starts with a space)
       (not (eq (aref (buffer-name) 0) ?\ ))
       ;; ...the buffer is not special (name starts with *)
       (or (not (or (eq (aref (buffer-name) 0) ?*)
                    (string-prefix-p "magit-" (symbol-name major-mode))))
           ;; except the scratch buffer.
           (string= (buffer-name) "*scratch*"))))
(setq whitespace-enable-predicate
      #'osf--whitespace-mode-enable-predicate)
(global-whitespace-mode)

(provide 'osf-ui)
