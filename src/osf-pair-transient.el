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

(transient-define-prefix osf-transient-smartparens ()
  [("q" "Quit" transient-quit-all)
   (">" "Right expand" sp-forward-slurp-sexp :transient t)
   ("." "Right shrink" sp-forward-barf-sexp :transient t)
   ("<" "Left expand" sp-backward-slurp-sexp :transient t)
   ("," "Left shrink" sp-backward-barf-sexp :transient t)
   ("r" "Raise" sp-raise-sexp)
   ("s" "Split" sp-split-sexp)
   ("\(" "Wrap" sp-wrap-round)
   ("J" "Join" sp-join-sexp)
   ("i" "Indent defun" sp-indent-defun)
   ("m" "Mark" sp-mark-sexp)
   ("j" "Next" sp-beginning-of-next-sexp :transient t)
   ("k" "Previous" sp-beginning-of-previous-sexp :transient t)
   ("C-j" "Next" sp-next-sexp :transient t)
   ("C-k" "Previous" sp-previous-sexp :transient t)
   ("h" "Up" sp-backward-up-sexp :transient t)
   ("l" "Down" sp-down-sexp :transient t)
   ("f" "Forward" sp-forward-sexp :transient t)
   ("b" "Backward" sp-backward-sexp :transient t)
   ("^" "Beginning" sp-beginning-of-sexp :transient t)
   ("$" "End" sp-end-of-sexp :transient t)
   ])

(provide 'osf-pair-transient)
