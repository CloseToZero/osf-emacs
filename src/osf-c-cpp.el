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

(with-eval-after-load 'cc-mode
  (defun osf-c-indent-line-or-region-or-complete (&optional arg region)
    (interactive
     (list current-prefix-arg (c-region-is-active-p)))
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (c-indent-line-or-region arg region)
      (when (and (eq tab-always-indent 'complete)
                 (eql old-point (point))
                 (eql old-tick (buffer-chars-modified-tick))
                 (or (null tab-first-completion)
                     (eq last-command this-command)
                     (and (eq tab-first-completion 'eol)
                          (eolp))
                     (and (memq tab-first-completion
                                '(word word-or-paren word-or-paren-or-punct))
                          (not (eql 2 syn)))
                     (and (memq tab-first-completion
                                '(word-or-paren word-or-paren-or-punct))
                          (not (or (eql 4 syn)
                                   (eql 5 syn))))
                     (and (eq tab-first-completion 'word-or-paren-or-punct)
                          (not (eql 1 syn))))
                 (completion-at-point)))))
  (osf-evil-define-key 'insert c-mode-base-map
    "<tab>" #'osf-c-indent-line-or-region-or-complete))

(provide 'osf-c-cpp)
