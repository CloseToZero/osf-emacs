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

(defun osf-mru-window ()
  (get-mru-window nil t t nil))

(defun osf-select-mru-window ()
  (interactive)
  (when-let ((mru-window (osf-mru-window)))
    (select-window mru-window)))

(defface osf-mru-window-mode-line-face
  '((t (:foreground "#8A2BE2")))
  "Face for the mru window indicator shown in the mode-line.")

(defun osf-mru-window-mode-line-string (mru-window)
  ;; Emacs will switch window to refresh mode-lines and mru-window
  ;; will changed, so we need the caller to pass recorded mru-window.
  (if (eq mru-window (get-buffer-window))
      (propertize " MRU " 'face 'osf-mru-window-mode-line-face)
    ""))

(defvar-local osf-mru-window-mode-line "")
(put 'osf-mru-window-mode-line 'risky-local-variable t)

(defun osf-add-mru-window-mode-line ()
  (let ((mru-window-mode-line 'osf-mru-window-mode-line))
    (setq global-mode-string (or global-mode-string '("")))
    (setq global-mode-string (delete mru-window-mode-line global-mode-string))
    (setq global-mode-string
          (cons (car global-mode-string)
                (cons mru-window-mode-line (cdr global-mode-string))))))

(defun osf-update-mru-window-mode-line (buffer mru-window)
  (with-current-buffer buffer
    (setq osf-mru-window-mode-line (osf-mru-window-mode-line-string mru-window))
    (force-mode-line-update)))

(defun osf-update-mru-window-mode-line-all-windows (&rest args)
  (let ((mru-window (osf-mru-window)))
    (dolist (buffer (mapcar #'window-buffer (window-list)))
      (osf-update-mru-window-mode-line buffer mru-window))))

(osf-add-mru-window-mode-line)
(add-hook 'window-selection-change-functions #'osf-update-mru-window-mode-line-all-windows)

(defvar osf-act-on-window-posframes nil)
(defvar osf-act-on-window-posframes-limit 10)
(defvar osf-act-on-window-actions
  `(("Select window" ,#'osf-act-on-window-action-select)
    ("Delete window" ,#'osf-act-on-window-action-delete)
    ("Swap window" ,#'osf-act-on-window-action-swap)
    ("Split window right" ,#'osf-act-on-window-action-split-right)
    ("Split window below" ,#'osf-act-on-window-action-split-below)))

(defface osf-act-on-window-tag-face
  '((t (:foreground "#D00000" :underline nil :height 2.0)))
  "Tag face for the posframes of `osf-act-on-window'.")

(defun osf-act-on-window ()
  (interactive)
  (let* ((windows
          (sort (sort (window-list)
                      (lambda (window1 window2)
                        (<= (window-pixel-top window1)
                            (window-pixel-top window2))))
                (lambda (window1 window2)
                  (and (<= (window-pixel-top window1)
                           (window-pixel-top window2))
                       (<= (window-pixel-left window1)
                           (window-pixel-left window2))))))
         (windows-with-tag
          (cl-loop for window in windows
                   for tag from 1
                   collect (list :window window
                                 :tag (number-to-string tag))))
         (show-tags-action
          (lambda ()
            (cl-loop for window-with-tag in windows-with-tag
                     for i from 0
                     for window = (plist-get window-with-tag :window)
                     for tag = (plist-get window-with-tag :tag)
                     do
                     (let ((buffer (format " *osf-act-on-window-%d*" i)))
                       (unless (> (length osf-act-on-window-posframes) i)
                         (setq osf-act-on-window-posframes
                               (nconc osf-act-on-window-posframes
                                      (list (get-buffer-create buffer)))))
                       (with-selected-window window
                         (posframe-show
                          buffer
                          :string tag
                          :poshandler #'posframe-poshandler-window-center
                          :font (face-font 'osf-act-on-window-tag-face)
                          :background-color (face-background 'osf-act-on-window-tag-face nil t)
                          :foreground-color (face-foreground 'osf-act-on-window-tag-face nil t)))))))
         (reposition-timer nil))
    (unwind-protect
        (progn
          ;; Constantly adjust posframe's height for dynamic minibuffer resizing.
          (setq reposition-timer (run-with-timer nil 0.25 show-tags-action))
          (let* ((selected-tag
                  (progn
                    ;; Show tags after `completing-read' to count for minibuffer's height.
                    (run-with-timer 0 nil show-tags-action)
                    (completing-read "Window: "
                                     (mapcar (lambda (window-with-tag)
                                               (plist-get window-with-tag :tag))
                                             windows-with-tag)
                                     nil t nil t)))
                 (current-window (selected-window))
                 (selected-window
                  (plist-get
                   (cl-find-if (lambda (window-with-tag)
                                 (string= (plist-get window-with-tag :tag) selected-tag))
                               windows-with-tag)
                   :window))
                 (selected-action
                  (let* ((action-names (mapcar #'cl-first osf-act-on-window-actions))
                         (selected-action-name
                          (progn
                            (run-with-timer 0 nil show-tags-action)
                            (completing-read "Action: "
                                             action-names nil t nil t action-names))))
                    (cl-second (assoc selected-action-name osf-act-on-window-actions)))))
            (funcall selected-action selected-window current-window))
          (when reposition-timer (cancel-timer reposition-timer))
          (osf--cleanup-posframes))
      (when reposition-timer (cancel-timer reposition-timer))
      (osf--cleanup-posframes))))

(defun osf--cleanup-posframes ()
  (cl-loop for i from 0
           for buffer in osf-act-on-window-posframes
           do
           (condition-case nil
               (if (>= i osf-act-on-window-posframes-limit)
                   (posframe-delete buffer)
                 (posframe-hide buffer))
             (error nil)))
  (setq osf-act-on-window-posframes
        (osf-truncate-list! osf-act-on-window-posframes
                            osf-act-on-window-posframes-limit)))

(defun osf-act-on-window-action-select (target-window current-window)
  (select-window target-window))

(defun osf-act-on-window-action-delete (target-window current-window)
  (delete-window target-window))

(defun osf-act-on-window-action-swap (target-window current-window)
  (let ((target-buffer (window-buffer target-window))
        (current-buffer (window-buffer current-window)))
    (when (window-dedicated-p target-window)
      (user-error
       "Cannot swap the target window %S, it's a dedicated window"
       target-window))
    (when (window-dedicated-p current-window)
      (user-error
       "Cannot swap the current window %S, it's a dedicated window"
       current-window))
    (set-window-buffer target-window current-buffer)
    (set-window-buffer current-window target-buffer)))

(defun osf-act-on-window-action-split-right (target-window current-winow)
  (select-window (split-window-right nil target-window)))

(defun osf-act-on-window-action-split-below (target-window current-winow)
  (select-window (split-window-below nil target-window)))

(winner-mode)

(straight-use-package 'hydra)

(defvar osf-default-resize-window-step 4)

(straight-use-package 'buffer-move)

(require 'windmove)

(defun osf-resize-window-step (prefix)
  (if prefix
      (prefix-numeric-value prefix)
    osf-default-resize-window-step))

(defun osf-move-splitter-left (arg)
  "Move the window splitter left."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally resize-step)
      (enlarge-window-horizontally resize-step))))

(defun osf-move-splitter-right (arg)
  "Move the window splitter right."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally resize-step)
      (shrink-window-horizontally resize-step))))

(defun osf-move-splitter-up (arg)
  "Move the window splitter up."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'up))
        (enlarge-window resize-step)
      (shrink-window resize-step))))

(defun osf-move-splitter-down (arg)
  "Move the window splitter down."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'up))
        (shrink-window resize-step)
      (enlarge-window resize-step))))

(defhydra osf-manage-window (:color red :hint nil)
  "
Window Move: _h_ left  _l_ right  _j_ down  _k_ up
Buffer Move: _C-h_ left  _C-l_ right  _C-j_ down  _C-k_ up
     Resize: _H_ left  _L_ right  _J_ down  _K_ up
Resize Step: _1_ _2_ _3_ _4_ _5_  current step: %`osf-default-resize-window-step
       Quit: _q_ quit"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("C-h" buf-move-left)
  ("C-l" buf-move-right)
  ("C-j" buf-move-down)
  ("C-k" buf-move-up)
  ("H" osf-move-splitter-left)
  ("L" osf-move-splitter-right)
  ("J" osf-move-splitter-down)
  ("K" osf-move-splitter-up)
  ("1" (lambda () (interactive) (setq osf-default-resize-window-step 1)))
  ("2" (lambda () (interactive) (setq osf-default-resize-window-step 2)))
  ("3" (lambda () (interactive) (setq osf-default-resize-window-step 3)))
  ("4" (lambda () (interactive) (setq osf-default-resize-window-step 4)))
  ("5" (lambda () (interactive) (setq osf-default-resize-window-step 5)))
  ("q" nil))

(defun osf-evil-window-split (&optional arg)
  (interactive "P")
  (cond (arg
         (select-window
          (split-window (frame-root-window) nil
                        (if evil-split-window-below 'below 'above)))
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-split))))

(defun osf-evil-window-split-below-if-root (&optional arg)
  (interactive "P")
  (cond (arg
         (split-window (frame-root-window) nil 'below)
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-split))))

(defun osf-evil-window-vsplit (&optional arg)
  (interactive "P")
  (cond (arg
         (select-window
          (split-window (frame-root-window) nil
                        (if evil-split-window-right 'right 'left)))
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-vsplit))))

(osf-leader-define-key 'global
  "w" evil-window-map
  "w -" #'osf-evil-window-split-below-if-root
  "w \\" #'osf-evil-window-vsplit
  "w a" #'osf-act-on-window
  "w p" #'osf-select-mru-window
  "w u" #'winner-undo
  "w x" #'evil-window-delete
  "w C-r" #'winner-redo
  "w M" #'osf-manage-window/body)

(provide 'osf-window)
