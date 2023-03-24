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

(require 'evilize-common)

(defvar Info-mode-map)

(evil-set-initial-state 'Info-mode 'normal)

(let ((bindings '(("<tab>" Info-next-reference)
                  ("S-<tab>" Info-prev-reference)
                  ("RET" Info-follow-nearest-node)
                  ("d" Info-directory)
                  ("i" Info-index)
                  ("I" Info-virtual-index)
                  ("a" info-apropos)
                  ("C-o" Info-history-back)
                  ("C-i" Info-history-forward)
                  ("C-j" Info-forward-node)
                  ("C-k" Info-backward-node)
                  ("g j" Info-next)
                  ("g k" Info-prev)
                  ("g m" Info-menu)
                  ("g t" Info-top-node)
                  ("g T" Info-toc)
                  ("g u" Info-up)
                  ("g f" Info-follow-reference)
                  )))
  (apply #'evil-define-key* 'normal Info-mode-map
         (evilize--normalize-bindings bindings)))

(provide 'evilize-info)
