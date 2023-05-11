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

;; Don't terminate the search if we just pressed a control character.
(setq search-exit-option 'edit)

;; Swap the bindings of DEL and C-M-w, so that DEL always delete a
;; character rather than undo the previous search action.
(osf-keymap-set isearch-mode-map
  "DEL" #'isearch-del-char
  "C-M-w" #'isearch-delete-char)

(straight-use-package 'deadgrep)
(keymap-global-set "M-s r" #'deadgrep)

(when (executable-find "rg")
  (with-eval-after-load 'project
    (when (boundp 'project-prefix-map)
      (keymap-set project-prefix-map "g" #'deadgrep)
      (setq project-switch-commands
            (cl-delete-if (lambda (e)
                            (eq (car e) 'project-find-regexp))
                          project-switch-commands))
      (add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t))))

(provide 'osf-search)
