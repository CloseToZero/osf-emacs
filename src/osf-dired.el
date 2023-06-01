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

(setq dired-dwim-target t)

(with-eval-after-load 'dired
  (defun osf-dired-copy-path ()
    (interactive)
    (dired-copy-filename-as-kill 0))

  (defun osf-dired-open-by-system-default-app ()
    (interactive)
    (let ((cur-file-name (dired-get-filename t t)))
      (unless cur-file-name
        (user-error "No file on this line"))
      (osf-open-by-system-default-app cur-file-name)))

  (osf-evil-define-key 'normal dired-mode-map
    "y p" #'osf-dired-copy-path
    "M-S-<return>" #'osf-dired-open-by-system-default-app)
  (osf-keymap-set dired-mode-map
    "M-RET" #'osf-dired-open-by-system-default-app
    "C-t A" #'image-dired-show-all-from-dir
    "C-c w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'image-dired-external
  (when (executable-find "magick")
    (setq image-dired-cmd-create-thumbnail-program "magick")
    (unless (member "convert" image-dired-cmd-create-thumbnail-options)
      (setq image-dired-cmd-create-thumbnail-options
            (cons "convert" image-dired-cmd-create-thumbnail-options)))))

(provide 'osf-dired)
