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

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(straight-use-package '(google-c-style :source emacsmirror-mirror))

(with-eval-after-load 'cc-mode
  (require 'google-c-style)
  (c-add-style "google" google-c-style)
  (setcdr (assoc 'other c-default-style) "google"))

(straight-use-package '(cmake-mode :source emacsmirror-mirror))

(with-eval-after-load 'cmake-mode
  (osf-local-leader-define-key cmake-mode-map
    "h h" #'cmake-help
    "h c" #'cmake-help-command
    "h v" #'cmake-help-variable
    "h p" #'cmake-help-property
    "h m" #'cmake-help-module
    "h l" #'cmake-help-list-commands))

(straight-use-package 'cmake-font-lock)

(provide 'osf-c-cpp)
