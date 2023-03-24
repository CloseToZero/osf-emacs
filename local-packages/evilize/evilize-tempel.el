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

(require 'evilize-common)

(defvar tempel-map)

(declare-function tempel--insert "ext:tempel")
(declare-function tempel--done "ext:tempel")
(declare-function tempel--abort "ext:tempel")

(let ((bindings '(
                 ("C-j" tempel-next)
                 ("C-k" tempel-previous)
                 ("C-l" tempel-kill)
                 ("C-a" tempel-beginning)
                 ("C-e" tempel-end)
                 ("M-RET" tempel-done)
                 ("C-g" tempel-abort)
                 )))
  (apply #'evil-define-key* '(insert normal) tempel-map
         (evilize--normalize-bindings bindings)))

(advice-add #'tempel--insert :after #'evilize--evil-normalize-keymaps-av)
(advice-add #'tempel--done :after #'evilize--evil-normalize-keymaps-av)
(advice-add #'tempel--abort :after #'evilize--evil-normalize-keymaps-av)

(provide 'evilize-tempel)
