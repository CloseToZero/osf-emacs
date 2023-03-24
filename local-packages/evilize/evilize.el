;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (evil "1.15.0"))

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

(defgroup evilize nil
  "Evilize the key bindings of variout modes."
  :prefix "evilize-"
  :group 'evil)

(defcustom evilize-want-jk-visual-lines nil
  "Bind j/k to move by visual lines when appropriate."
  :type 'boolean)

(require 'evilize-minibuffer)
(with-eval-after-load 'magit
  (require 'evilize-magit))
(with-eval-after-load 'vertico
  (require 'evilize-vertico))
(with-eval-after-load 'info
  (require 'evilize-info))
(with-eval-after-load 'dired
  (require 'evilize-dired))
(with-eval-after-load 'corfu
  (require 'evilize-corfu))

(provide 'evilize)
