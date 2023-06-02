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

(defun osf-helm-delete-minibuffer-contents (&optional arg)
  "Like `helm-delete-minibuffer-contents', but treat
  `helm-delete-minibuffer-contents-from-point' as nil."
  (interactive "P")
  (let ((helm-delete-minibuffer-contents-from-point nil))
    (helm-delete-minibuffer-contents arg)))

(osf-evil-define-key 'normal helm-map
  "j" #'helm-next-line
  "k" #'helm-previous-line
  "J" #'helm-follow-action-forward
  "K" #'helm-follow-action-backward
  "[" #'helm-previous-source
  "]" #'helm-next-source
  "C-d" #'helm-scroll-down
  "C-u" #'helm-scroll-up
  "g g" #'helm-beginning-of-buffer
  "G" #'helm-end-of-buffer
  "d d" #'osf-helm-delete-minibuffer-contents
  "m" #'helm-toggle-visible-mark-forward
  "M-m" #'helm-toggle-visible-mark-backward
  "t a" #'helm-toggle-all-marks
  "M" #'helm-mark-all
  "U" #'helm-unmark-all
  "(" #'helm-prev-visible-mark
  ")" #'helm-next-visible-mark
  "s s" #'helm-show-all-candidates-in-source
  "s a" #'helm-display-all-sources
  "t F" #'helm-toggle-full-frame
  "t >" #'helm-toggle-truncate-line
  "t l" #'helm-display-line-numbers-mode
  )

(osf-evil-define-key 'insert helm-map
  "C-j" #'helm-next-line
  "C-k" #'helm-previous-line
  "M-j" #'helm-follow-action-forward
  "M-k" #'helm-follow-action-backward
  "M-<" #'helm-beginning-of-buffer
  "M->" #'helm-end-of-buffer
  "M-K" #'osf-helm-delete-minibuffer-contents
  )

(osf-evil-define-key '(normal insert) helm-map
  "RET" #'helm-maybe-exit-minibuffer
  "M-<return>" #'helm-execute-persistent-action
  "<right>" #'helm-next-source
  "<left>" #'helm-previous-source
  "C-g" #'helm-keyboard-quit
  "<tab>" #'helm-select-action
  )

(provide 'osf-evilize-helm)
