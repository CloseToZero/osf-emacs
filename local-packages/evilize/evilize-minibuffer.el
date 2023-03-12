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

(defun evilize-minibuffer-setup ()
  (set (make-local-variable 'evil-echo-state) nil)
  (evil-insert 1))
(add-hook 'minibuffer-setup-hook #'evilize-minibuffer-setup)

(pcase-dolist (`(,state ,key ,cmd)
               '((normal "<escape>" abort-recursive-edit)
                 (normal "RET" exit-minibuffer)
                 (insert "C-p" previous-complete-history-element)
                 (insert "C-n" next-complete-history-element)
                 (normal "C-p" previous-history-element)
                 (normal "C-n" next-history-element)))
  (dolist (map (list minibuffer-local-map
                     minibuffer-local-ns-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-isearch-map
                     evil-ex-completion-map))
    (evil-define-key* state map (kbd key) cmd)))

(provide 'evilize-minibuffer)
