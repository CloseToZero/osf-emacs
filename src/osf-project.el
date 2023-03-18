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

(defvar osf-project-dirs nil
  "A list of directories for `osf-find-projects' to search for projects.")

(autoload #'project-remember-projects-under "project")
(defun osf-find-projects ()
  "Search projects under the all directories of `osf-project-dirs'.
Use `project-remember-projects-under' to search projects,
only searching the top level directories within each directory."
  (interactive)
  (cl-block nil
    (when (and (not osf-project-dirs)
               (called-interactively-p 'interactive))
      (message "`osf-project-dirs' is empty, set it before invoke the command")
      (cl-return))
    (let ((queue nil))
      (dolist (dir osf-project-dirs)
        (when (file-directory-p dir)
          (setq queue
                (nconc queue
                       (directory-files
                        dir t directory-files-no-dot-files-regexp t)))))
      (let ((count 0))
        (dolist (file queue)
          (when (file-directory-p file)
            (message "Processing %s" file)
            (let ((inhibit-message t))
              (cl-incf count (project-remember-projects-under file)))))
        (if (zerop count)
            (message "Total: No projects were found")
          (message "Total: %d project%s were found"
                   count (if (= count 1) "" "s")))))))

(defun osf-project-shell ()
  "On Windows, run `project-shell', otherwise, run `project-eshell'."
  (interactive)
  (call-interactively
   (if (eq osf-system-type 'windows)
       #'project-shell #'project-eshell)))

(osf-leader-define-key 'global
  "p" project-prefix-map)

(osf-define-key project-prefix-map
  "R" #'osf-find-projects
  "s" #'osf-project-shell)

(provide 'osf-project)
