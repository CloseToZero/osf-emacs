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

(setq tab-bar-show nil)

(tab-bar-mode)

(defun osf-select-tab-by-number (&optional n)
  "Select Nth (1-based) tab, if N is out of the range, don't select a tab."
  (interactive "p")
  (if (<= 1 n (length (funcall tab-bar-tabs-function)))
      (tab-bar-select-tab n)
    (error "There is not %dth tab" n)))

(defun osf-new-tab-with-name (&optional arg name)
  "Create a tab ARG positions to the right with the specified NAME."
  (interactive (list current-prefix-arg
                     (read-string "Tab name (leave blank for automatic naming): "
                                  nil 'osf-new-tab-with-name-history)))
  (let ((exiting-names (mapcar (apply-partially #'alist-get 'name)
                               (funcall tab-bar-tabs-function nil))))
    (when (seq-contains-p exiting-names name #'string=)
      (error "The tab name \"%s\" already exists" name)))
  (tab-bar-new-tab arg)
  (tab-bar-rename-tab name))

(osf-keymap-global-set
  "C-<tab>" nil
  "C-<tab> n" #'osf-new-tab-with-name
  "C-<tab> c" #'tab-bar-close-tab
  "C-<tab> C-<tab>" #'tab-bar-switch-to-recent-tab
  "C-<tab> j" #'tab-bar-switch-to-next-tab
  "C-<tab> k" #'tab-bar-switch-to-prev-tab
  "C-<tab> l" #'tab-bar-switch-to-tab
  "C-<tab> 1" (lambda () (interactive) (osf-select-tab-by-number 1))
  "C-<tab> 2" (lambda () (interactive) (osf-select-tab-by-number 2))
  "C-<tab> 3" (lambda () (interactive) (osf-select-tab-by-number 3))
  "C-<tab> 4" (lambda () (interactive) (osf-select-tab-by-number 4))
  "C-<tab> 5" (lambda () (interactive) (osf-select-tab-by-number 5))
  "C-<tab> 6" (lambda () (interactive) (osf-select-tab-by-number 6))
  "C-<tab> 7" (lambda () (interactive) (osf-select-tab-by-number 7))
  "C-<tab> 8" (lambda () (interactive) (osf-select-tab-by-number 8))
  "C-<tab> 9" (lambda () (interactive) (osf-select-tab-by-number 9))
  "C-<tab> >" #'tab-bar-move-tab
  "C-<tab> <" (lambda (&optional n) (interactive "p") (tab-bar-move-tab (- n))))

(provide 'osf-tab)
