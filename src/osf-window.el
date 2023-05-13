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

(defvar osf-select-window-posframes nil)
(defvar osf-select-window-posframes-limit 10)

(defface osf-select-window-tag-face
  '((t (:foreground "red" :underline nil :height 2.0)))
  "Tag face for the posframes of `osf-select-window'.")

(defun osf-select-window ()
  (interactive)
  (let* ((windows (cl-remove (selected-window) (window-list)))
         (windows-with-tag
          (cl-loop for window in windows
                   for tag from 1
                   collect (list :window window
                                 :tag (number-to-string tag)))))
    (unwind-protect
        (progn
          (cl-loop for window-with-tag in windows-with-tag
                   for i from 0
                   for window = (plist-get window-with-tag :window)
                   for tag = (plist-get window-with-tag :tag)
                   do
                   (let ((buffer (format " *osf-select-window-%d*" i)))
                     (unless (> (length osf-select-window-posframes) i)
                       (setq osf-select-window-posframes
                             (nconc osf-select-window-posframes
                                    (list (get-buffer-create buffer)))))
                     (with-selected-window window
                       (posframe-show
                        buffer
                        :string tag
                        :poshandler #'posframe-poshandler-window-center
                        :font (face-font 'osf-select-window-tag-face)
                        :background-color (face-background 'osf-select-window-tag-face nil t)
                        :foreground-color (face-foreground 'osf-select-window-tag-face nil t)))))
          (let* ((selected-tag
                  (completing-read "window: "
                                   (mapcar (lambda (window-with-tag)
                                             (plist-get window-with-tag :tag))
                                           windows-with-tag)
                                   nil t nil t))
                 (selected-window
                  (plist-get
                   (cl-find-if (lambda (window-with-tag)
                                 (string= (plist-get window-with-tag :tag) selected-tag))
                               windows-with-tag)
                   :window)))
            (when selected-window
              (select-window selected-window)))
          (osf--cleanup-posframes))
      (osf--cleanup-posframes))))

(defun osf--cleanup-posframes ()
  (cl-loop for i from 0
           for buffer in osf-select-window-posframes
           do
           (condition-case nil
               (if (>= i osf-select-window-posframes-limit)
                   (posframe-delete buffer)
                 (posframe-hide buffer))
             (error nil)))
  (setq osf-select-window-posframes
        (osf-truncate-list! osf-select-window-posframes
                            osf-select-window-posframes-limit)))

(defvar-keymap osf-window-map
  "s" #'osf-select-window
  "M-o" #'osf-select-mru-window)
(fset #'osf-window-map osf-window-map)

(osf-keymap-global-set
  "M-o" 'osf-window-map)
                   
(provide 'osf-window)
