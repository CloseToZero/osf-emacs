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

(straight-use-package 'pdf-tools)

(add-hook 'pdf-view-mode-hook #'osf-hide-cursor)

(pdf-loader-install)

(defun osf--dont-ask-if-pdf-file-too-large (fn size op-type filename &optional offer-raw)
  (cond ((string-match-p (rx "." (regexp (osf-regexp-literal-any-case "pdf")) string-end) filename)
         (let ((large-file-warning-threshold nil))
           (funcall fn size op-type filename offer-raw)))
        (t (funcall fn size op-type filename offer-raw))))

(advice-add #'abort-if-file-too-large :around #'osf--dont-ask-if-pdf-file-too-large)

(defun osf-pdf-view-kill-outline-buffer (&optional pdf-buffer)
  "Kill the outline buffer of PDF-BUFFER (defaulted to the current buffer)."
  (when-let ((buffer-name (pdf-outline-buffer-name pdf-buffer))
             (buffer (get-buffer buffer-name)))
    (kill-buffer buffer)))

(defun osf--pdf-view-setup-kill-outline-buffer-hook ()
    (add-hook 'kill-buffer-hook #'osf-pdf-view-kill-outline-buffer nil t))
(add-hook 'pdf-view-mode-hook #'osf--pdf-view-setup-kill-outline-buffer-hook)

(straight-use-package 'saveplace-pdf-view)

;; Need enable save-place-mode in the other place.
(with-eval-after-load 'saveplace
  (with-eval-after-load 'pdf-tools
    (require 'saveplace-pdf-view)))

(with-eval-after-load 'saveplace-pdf-view
  (defun osf--saveplace-pdf-view-dont-save-if-no-built (fn &rest args)
    "Don't save the place of pdf-view buffers if pdf-tools haven't been installed.
Otherwise, we will lose the save-place history of pdf-view buffers."
    (cond ((derived-mode-p 'pdf-view-mode)
           (when (ignore-errors (pdf-info-check-epdfinfo) t)
             (apply fn args)))
          (t (apply fn args))))
  (advice-add #'saveplace-pdf-view-to-alist-advice :around
              #'osf--saveplace-pdf-view-dont-save-if-no-built)

  (defun osf--saveplace-pdf-view-restore-history-after-rebuilt
      (fn
       target-directory
       &optional
       skip-dependencies-p
       force-dependencies-p
       callback
       build-directory)
    "Restore pdf-view buffers' save-place history after pdf-tools installed."
    (apply
     fn
     target-directory
     skip-dependencies-p
     force-dependencies-p
     (lambda (executable &rest args)
       (prog1 (and callback (apply callback executable args))
         (when executable
           (dolist (buf (buffer-list))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (derived-mode-p 'pdf-view-mode)
                   ;; Switch to buf's window, avoid `pdf-view-bookmark-jump'
                   ;; switch the buffer of current window to buf.
                   (with-selected-window (or (get-buffer-window buf) (selected-window))
                     (save-place-find-file-hook)))))))))
     build-directory))
  (advice-add #'pdf-tools-build-server
              :around #'osf--saveplace-pdf-view-restore-history-after-rebuilt))

(provide 'osf-pdf)
