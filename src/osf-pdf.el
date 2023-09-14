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

(defun osf-pdf-view-kill-outline-buffer (&optional pdf-buffer)
  "Kill the outline buffer of PDF-BUFFER (defaulted to the current buffer)."
  (when-let ((buffer-name (pdf-outline-buffer-name pdf-buffer))
             (buffer (get-buffer buffer-name)))
    (kill-buffer buffer)))

(defun osf--pdf-view-setup-kill-outline-buffer-hook ()
    (add-hook 'kill-buffer-hook #'osf-pdf-view-kill-outline-buffer nil t))
(add-hook 'pdf-view-mode-hook #'osf--pdf-view-setup-kill-outline-buffer-hook)

(provide 'osf-pdf)
