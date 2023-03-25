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

(defvar osf-cache-dir
  (expand-file-name ".cache" user-emacs-directory))

(defvar osf-src-dir
  (expand-file-name "src" user-emacs-directory))

(defvar osf-local-packages-dir
  (expand-file-name "local-packages" user-emacs-directory))

(defvar osf-system-type
  (cond ((memq system-type '(cygwin windows-nt ms-dos)) 'windows)
        ((eq system-type 'darwin) 'mac)
        ((eq system-type 'gnu/linux) 'linux)
        ((eq system-type 'berkeley-unix) 'bsd)
        (t system-type)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(push osf-src-dir load-path)
(require 'osf-clean-dir)
(require 'osf-core-lib)
(require 'osf-coding-system)
(require 'osf-package)
(require 'osf-lib)
(require 'osf-clean-mode-line)
(require 'osf-indent)
(require 'osf-evil)
(require 'osf-completion)
(require 'osf-misc)
(require 'osf-ui)
(require 'osf-pair)
(require 'osf-key)
(require 'osf-vc)
(require 'osf-project)
(require 'osf-template)
(require 'osf-org)
(require 'osf-markdown)
(require 'osf-zig)
(require 'osf-lisp)
(require 'osf-common-lisp)
(require 'osf-server)

(load custom-file t t)
