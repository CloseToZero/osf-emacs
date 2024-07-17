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

(straight-use-package 'dape)

(setq dape-breakpoints
      (expand-file-name "dape-breakpoints" osf-cache-dir))

(defhydra osf-hydra-dape (:color red :hint nil)
  "
Start:      _d_ dape
Control:    _j_ step across  _J_ step into  _K_ step out _c_ continue _P_ pause
Stack:      _>_ down _<_ up _s_ select
Brackpoint: _b_ toggle _e_ expression _B_ remove all _h_ hits _l_ log
Thread:     _t_ select
Expression: _w_ watch _x_ eval
View:       _m_ memory _R_ repl _i_ info

Session:    _q_ quit _r_ restart _D_ detach
Quit:       _RET_ quit hydra"
  ("d" dape)
  ("j" dape-next)
  ("J" dape-step-in)
  ("K" dape-step-out)
  ("c" dape-continue)
  ("P" dape-pause)

  (">" dape-stack-select-up)
  ("<" dape-stack-select-down)
  ("s" dape-select-stack)

  ("b" dape-breakpoint-toggle)
  ("e" dape-breakpoint-expression)
  ("B" dape-breakpoint-remove-all)
  ("h" dape-breakpoint-hits)
  ("l" dape-breakpoint-log)

  ("t" dape-select-thread)

  ("w" dape-watch-dwim)
  ("x" dape-evaluate-expression)

  ("m" dape-read-memory)
  ("R" dape-repl)
  ("i" dape-info)

  ("q" dape-quit :exit t)
  ("r" dape-restart)
  ("D" dape-disconnect-quit)
  ("RET" nil))

(osf-leader-define-key 'global
  "d d" #'osf-hydra-dape/body)

(with-eval-after-load 'dape
  (osf-evil-define-key 'normal dape-info-scope-mode-map
    "TAB" #'dape-info-scope-toggle)

  (osf-local-leader-define-key dape-info-scope-mode-map
    "w" #'dape-info-scope-watch-dwim
    "b" #'dape-info-scope-data-breakpoint
    "e" #'dape-info-variable-edit))

(provide 'osf-dape)
