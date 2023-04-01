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

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message "")

(setq native-comp-async-report-warnings-errors 'silent)

(setq enable-recursive-minibuffers t)

(setq save-interprogram-paste-before-kill t)

(setq history-delete-duplicates t)

(setq system-time-locale "C")

(when (boundp 'word-wrap-by-category)
  (customize-set-variable 'word-wrap-by-category t))

(setq vc-follow-symlinks t)

(setq inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      read-process-output-max (* 1024 1024))

(defun osf--ad-crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple'.
Display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple
            :filter-args #'osf--ad-crm-indicator)

(setq recentf-max-saved-items 5000
      recentf-max-menu-items 0)
(recentf-mode)
(run-at-time t (* 5 60) #'recentf-save-list)
(osf-inhibit-message #'recentf-save-list)
(when (fboundp #'recentf-open)
  (osf-leader-define-key 'global
    "f R" #'recentf-open))

(global-so-long-mode)

(provide 'osf-misc)
