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

(defvar osf-recent-frames nil)

(defun osf--frame-record-recent (frame)
  (setq osf-recent-frames (cons frame (remove frame osf-recent-frames))))

(defun osf--frame-record-selected ()
  ;; Within `after-focus-change-function', the selected frame may just
  ;; get lost focus, and its `frame-focus-state' may be nil.
  (when (frame-focus-state (selected-frame))
    (osf--frame-record-recent (selected-frame))))

(add-function :after after-focus-change-function #'osf--frame-record-selected)

(defun osf--frame-record-selected-after-make-frame (new-frame)
  ;; New created frame seems not trigger `after-focus-change-function'
  ;; and `focus-in-hook' in GUI Emacs.
  (when (display-graphic-p)
    (osf--frame-record-recent new-frame)))

(add-hook 'after-make-frame-functions
          #'osf--frame-record-selected-after-make-frame)

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
