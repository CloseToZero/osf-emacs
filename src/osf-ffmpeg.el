;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

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

(defun osf-ffmpeg-compress-video (src dst)
  (interactive "fVideo file: \nFDst video file: ")
  (async-shell-command
   (format "ffmpeg -i %s -c:v libx264 -crf 18 -preset veryslow -c:a copy %s"
           (shell-quote-argument src)
           (shell-quote-argument dst))
    "osf-ffmpeg-compress-video"))

(defun osf-ffmpeg-cut-video (src time dst)
  (interactive "fVideo file: \nsTime (like hh:mm:ss-hh:mm:ss): \nFDst video file: ")
  (split-string "" "-")
  (let* ((time-pair (split-string time "-"))
         (from-time (cl-first time-pair))
         (to-time (cl-second time-pair)))
    (async-shell-command
     (format "ffmpeg -ss %s -to %s -i %s -c copy %s"
             from-time
             to-time
             (shell-quote-argument src)
             (shell-quote-argument dst))
     "osf-ffmpeg-cut-video")))

(provide 'osf-ffmpeg)
