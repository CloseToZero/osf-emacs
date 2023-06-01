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

(osf-evil-define-key 'normal org-mode-map
  "M-j" #'org-metadown
  "M-k" #'org-metaup
  "M-h" #'org-metaleft
  "M-l" #'org-metaright
  "C-j" #'outline-forward-same-level
  "C-k" #'outline-backward-same-level
  "g j" #'org-next-visible-heading
  "g k" #'org-previous-visible-heading
  "H" #'org-up-element
  "L" #'org-down-element)

(evil-set-initial-state 'org-mode 'normal)

(provide 'osf-evilize-org)
