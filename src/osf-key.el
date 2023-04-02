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

(osf-leader-define-key 'global
  "SPC" #'execute-extended-command

  "h" help-map

  "b x" #'kill-current-buffer
  "b X" #'kill-buffer

  "w" evil-window-map
  "w -" #'evil-window-split
  "w \\" #'evil-window-vsplit

  "f f" #'find-file
  "f r" #'revert-buffer
  "f c" #'osf-edit-config

  "q q" #'save-buffers-kill-terminal
  )

(provide 'osf-key)
