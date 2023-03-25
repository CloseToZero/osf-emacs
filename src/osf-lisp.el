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

;; FIXME not flexible
(defun osf-lisp-gen-repl-return-eval-at-end-fn (eval-fn)
  `(lambda ()
     ,(concat
       (if (symbolp eval-fn)
           (concat "Like `" (symbol-name eval-fn) "'")
         "The REPL evalutaion command")
       ",
but only eval the form if the current evil state is in normal state or
the current point is at the end of the repl buffer. Otherwise, just
`newline-and-indent'.")
     (interactive)
     (call-interactively (if (or (eq evil-state 'normal)
                                 (= (point) (point-max)))
                             #',eval-fn
                           #'newline-and-indent))))

(provide 'osf-lisp)
