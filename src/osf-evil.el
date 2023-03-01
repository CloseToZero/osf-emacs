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

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(straight-use-package 'evil)
(setq evil-symbol-word-search t
      evil-mouse-word 'evil-WORD
      evil-want-Y-yank-to-eol t
      evil-want-C-u-scroll t
      evil-want-C-u-delete t
      evil-want-abbrev-expand-on-insert-exit nil
      evil-jumps-ignored-file-patterns nil
      evil--jumps-buffer-targets
      (rx
       (or
        (seq string-start "*"
             (or "new" "scratch" "eww" "Help")
             "*" string-end)
        (seq string-start "*Org Src " (+ anychar) "*"))))
(require 'evil)
(customize-set-variable 'evil-undo-system 'undo-tree)
(evil-mode)

(provide 'osf-evil)
