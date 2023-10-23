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

(defun osf-make-frame (name)
  (interactive
   (let ((default (cdr (assq 'name (frame-parameters)))))
     (list (read-string (format-prompt "Frame name" default) nil nil
                        default))))
  (let ((frame (make-frame `((name . ,name)))))
    (unless (display-graphic-p)
      (select-frame frame))))

;; Adapted from
;; https://emacs.stackexchange.com/questions/21365/how-can-i-switch-to-the-most-recent-frame-with-other-frame
(defvar osf-recent-frames nil)

(defun osf--frame-record-selected ()
  "Record the currently selected frame. Add this to the `focus-in-hook'."
  (let ((f (selected-frame)))
    (setq osf-recent-frames (cons f (remove f osf-recent-frames)))))

(add-hook 'focus-in-hook #'osf--frame-record-selected)

(defun osf-mru-frame ()
  (car (cdr osf-recent-frames)))

(defun osf-select-mru-frame ()
  "Select the most recently used frame."
  (interactive)
  (when-let ((mru-frame (osf-mru-frame)))
    (select-frame-set-input-focus mru-frame)))

(defun osf-show-frame-name ()
  (interactive)
  (message "Frame name: %s" (cdr (assq 'name (frame-parameters)))))

(osf-leader-define-key 'global
  "TAB c" #'osf-make-frame
  "TAB s" #'select-frame-by-name
  "TAB TAB" #'osf-select-mru-frame
  "TAB x" #'delete-frame
  "TAB n" #'set-frame-name
  "TAB N" #'osf-show-frame-name)

(provide 'osf-frame)
