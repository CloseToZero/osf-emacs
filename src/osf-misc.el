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

(setq native-comp-async-report-warnings-errors 'silent)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq initial-major-mode #'fundamental-mode
      initial-scratch-message "")

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setq help-window-select t)

(setq sentence-end-double-space nil)

(when (boundp 'word-wrap-by-category)
  (customize-set-variable 'word-wrap-by-category t))

(setq save-interprogram-paste-before-kill t)

(setq inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      read-process-output-max (* 1024 1024))

(when (eq osf-system-type 'windows)
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 1024 1024)))

(provide 'osf-misc)
