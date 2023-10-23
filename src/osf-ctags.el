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

(straight-use-package 'citre)

(setq citre-peek-file-content-height 15
      citre-peek-backward-in-chain-after-jump t)

;; lazy load
(require 'citre-config)

(with-eval-after-load 'citre
  (osf-leader-define-key citre-mode-map
    "c j" #'citre-jump
    "c J" #'citre-jump-back
    "c p" #'citre-ace-peek
    "c u" #'citre-update-this-tags-file
    "c r" #'citre-peek-restore)
  (add-hook 'citre-mode-hook #'evil-normalize-keymaps)

  (osf-evil-define-key 'normal citre-peek-keymap
    "M-j" #'citre-peek-next-line
    "M-k" #'citre-peek-prev-line

    "M-J" #'citre-peek-next-tag
    "M-K" #'citre-peek-prev-tag

    "M-l" #'citre-peek-chain-forward
    "M-h" #'citre-peek-chain-backward
    "M-L" #'citre-peek-next-branch
    "M-H" #'citre-peek-prev-branch

    "M-f p" #'citre-peek-through
    "M-f r" #'citre-peek-through-reference
    "M-f d" #'citre-peek-delete-branch
    "M-f D" #'citre-peek-delete-branches

    "C-M-S-j" #'citre-peek-move-current-tag-down
    "C-M-S-k" #'citre-peek-move-current-tag-up
    "C-M-S-u" #'citre-peek-make-current-tag-first

    "M-f j" #'citre-peek-jump

    "<remap> <keyboard-quit>" #'citre-peek-abort)
  (add-hook 'citre-peek--mode-hook #'evil-normalize-keymaps)

  (dolist (cmd '(citre-jump citre-jump-back))
    (evil-set-command-property cmd :jump t)))

(provide 'osf-ctags)
