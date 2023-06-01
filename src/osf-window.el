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

(defun osf-select-mru-window ()
  (interactive)
  (when-let ((mru-window (get-mru-window nil t t nil)))
    (select-window mru-window)))

(defvar osf-act-on-window-posframes nil)
(defvar osf-act-on-window-posframes-limit 10)
(defvar osf-act-on-window-actions
  `(
    ("Select window" ,#'osf-act-on-window-action-select)
    ("Delete window" ,#'osf-act-on-window-action-delete)
    ("Swap window" ,#'osf-act-on-window-action-swap)
    ("Split window right" ,#'osf-act-on-window-action-split-right)
    ("Split window below" ,#'osf-act-on-window-action-split-below)
    ))

(defface osf-act-on-window-tag-face
  '((t (:foreground "#d00000" :underline nil :height 2.0)))
  "Tag face for the posframes of `osf-act-on-window'.")

(defun osf-act-on-window ()
  (interactive)
  (let* ((windows
          (sort (sort (window-list)
                      (lambda (window1 window2)
                        (<= (window-pixel-top window1)
                            (window-pixel-top window2))))
                (lambda (window1 window2)
                  (and (<= (window-pixel-top window1)
                           (window-pixel-top window2))
                       (<= (window-pixel-left window1)
                           (window-pixel-left window2))))))
         (windows-with-tag
          (cl-loop for window in windows
                   for tag from 1
                   collect (list :window window
                                 :tag (number-to-string tag))))
         (show-tags-action
          (lambda ()
            (cl-loop for window-with-tag in windows-with-tag
                     for i from 0
                     for window = (plist-get window-with-tag :window)
                     for tag = (plist-get window-with-tag :tag)
                     do
                     (let ((buffer (format " *osf-act-on-window-%d*" i)))
                       (unless (> (length osf-act-on-window-posframes) i)
                         (setq osf-act-on-window-posframes
                               (nconc osf-act-on-window-posframes
                                      (list (get-buffer-create buffer)))))
                       (with-selected-window window
                         (posframe-show
                          buffer
                          :string tag
                          :poshandler #'posframe-poshandler-window-center
                          :font (face-font 'osf-act-on-window-tag-face)
                          :background-color (face-background 'osf-act-on-window-tag-face nil t)
                          :foreground-color (face-foreground 'osf-act-on-window-tag-face nil t)))))))
         (reposition-timer nil))
    (unwind-protect
        (progn
          ;; Constantly adjust posframe's height for dynamic minibuffer resizing.
          (setq reposition-timer (run-with-timer nil 0.25 show-tags-action))
          (let* ((helm-split-window-inside-p t)
                 (selected-tag
                  (progn
                    ;; Show tags after `completing-read' to count for minibuffer's height.
                    (run-with-timer 0 nil show-tags-action)
                    (completing-read "Window: "
                                     (mapcar (lambda (window-with-tag)
                                               (plist-get window-with-tag :tag))
                                             windows-with-tag)
                                     nil t nil t)))
                 (current-window (selected-window))
                 (selected-window
                  (plist-get
                   (cl-find-if (lambda (window-with-tag)
                                 (string= (plist-get window-with-tag :tag) selected-tag))
                               windows-with-tag)
                   :window))
                 (selected-action
                  (let* ((action-names (mapcar #'cl-first osf-act-on-window-actions))
                         (selected-action-name
                          (progn
                            (run-with-timer 0 nil show-tags-action)
                            (completing-read "Action: "
                                             action-names nil t nil t action-names))))
                    (cl-second (assoc selected-action-name osf-act-on-window-actions)))))
            (funcall selected-action selected-window current-window))
          (when reposition-timer (cancel-timer reposition-timer))
          (osf--cleanup-posframes))
      (when reposition-timer (cancel-timer reposition-timer))
      (osf--cleanup-posframes))))

(defun osf--cleanup-posframes ()
  (cl-loop for i from 0
           for buffer in osf-act-on-window-posframes
           do
           (condition-case nil
               (if (>= i osf-act-on-window-posframes-limit)
                   (posframe-delete buffer)
                 (posframe-hide buffer))
             (error nil)))
  (setq osf-act-on-window-posframes
        (osf-truncate-list! osf-act-on-window-posframes
                            osf-act-on-window-posframes-limit)))

(defun osf-act-on-window-action-select (target-window current-window)
  (select-window target-window))

(defun osf-act-on-window-action-delete (target-window current-window)
  (delete-window target-window))

(defun osf-act-on-window-action-swap (target-window current-window)
  (let ((target-buffer (window-buffer target-window))
        (current-buffer (window-buffer current-window)))
    (when (window-dedicated-p target-window)
      (user-error
       "Cannot swap the target window %S, it's a dedicated window"
       target-window))
    (when (window-dedicated-p current-window)
      (user-error
       "Cannot swap the current window %S, it's a dedicated window"
       current-window))
    (set-window-buffer target-window current-buffer)
    (set-window-buffer current-window target-buffer)))

(defun osf-act-on-window-action-split-right (target-window current-winow)
  (select-window (split-window-right nil target-window)))

(defun osf-act-on-window-action-split-below (target-window current-winow)
  (select-window (split-window-below nil target-window)))

(osf-leader-define-key 'global
  "w" evil-window-map
  "w -" #'evil-window-split
  "w \\" #'evil-window-vsplit
  "w a" #'osf-act-on-window
  "w p" #'osf-select-mru-window)
                   
(provide 'osf-window)
