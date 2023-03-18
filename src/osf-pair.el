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

(straight-use-package 'smartparens)
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)
(smartparens-global-mode)
(dolist (open-pair '("(" "[" "{"))
  (dolist (mode '(fundamental-mode
                  text-mode
                  prog-mode
                  ))
    (sp-local-pair mode open-pair nil
                   :post-handlers
                   '(:add ("||\n[i]" "RET")))))
(sp-with-modes '(emacs-lisp-mode lisp-mode scheme-mode)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p sp-in-comment-p))
  )

(provide 'osf-pair)