;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2024 Zhexuan Chen <2915234902@qq.com>

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

(setq comint-prompt-read-only t)

;; Add the following command to deal with the case that the eshell
;; buffer has fired the error 'Text is read-only', so I can't use the
;; 'clear' eshell command to clear the buffer and simply can't do
;; anything to fix the eshell buffer.
(defun osf-eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(with-eval-after-load 'esh-mode
  (osf-evil-define-key '(normal insert) eshell-mode-map
    "C-j" #'eshell-next-prompt
    "C-k" #'eshell-previous-prompt
    "RET" #'eshell-send-input)

  ;; Taken from Doom Emacs
  (evil-define-operator osf-eshell-evil-delete (beg end type register yank-handler)
    "Like `evil-delete' but will not delete/copy the prompt."
    (interactive "<R><x><y>")
    (save-restriction
      (narrow-to-region eshell-last-output-end (point-max))
      (evil-delete (if beg (max beg (point-min)) (point-min))
                   (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                   type register yank-handler)))

  ;; Taken from Doom Emacs
  (evil-define-operator osf-eshell-evil-delete-line (beg end type register yank-handler)
    "Change to end of line."
    :motion nil
    :keep-visual t
    (interactive "<R><x>")
    (osf-eshell-evil-delete (point) end type register yank-handler))

  (osf-evil-define-key 'normal eshell-mode-map
    "d" #'osf-eshell-evil-delete
    "D" #'osf-eshell-evil-delete-line))

(straight-use-package 'eshell-z)

(with-eval-after-load 'eshell
  (require 'eshell-z))

(provide 'osf-shell)
