;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs

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

(require 'evilize-common)

(defvar evilize-want-jk-visual-lines)

(defvar magit-mode-map)
(defvar magit-section-mode-map)
(defvar magit-blame-mode-map)
(defvar magit-blame-read-only-mode-map)
(defvar magit-blob-mode-map)
(defvar magit-cherry-mode-map)
(defvar magit-diff-mode-map)
(defvar magit-file-section-map)
(defvar magit-hunk-section-map)
(defvar magit-log-mode-map)
(defvar magit-log-read-revs-map)
(defvar magit-log-select-mode-map)
(defvar magit-process-mode-map)
(defvar magit-reflog-mode-map)
(defvar magit-refs-mode-map)
(defvar magit-repolist-mode-map)
(defvar magit-status-mode-map)
(defvar magit-submodule-list-mode-map)

(declare-function magit-refresh "ext:magit-mode")
(declare-function transient-suffix-put "ext:transient")

(evil-define-operator evilize-magit-evil-yank-line (beg end type register)
  "Save whole lines into the kill-ring."
  ;; We can't reuse evil-yank-line since its :motion property may
  ;; changed with `evil-want-Y-yank-to-eol'.
  :motion evil-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (evil-yank beg end type register))

(let ((specs
       `(((normal visual) (,magit-mode-map)
          (("g" nil)
           ("h" evil-backward-char)
           ("l" evil-forward-char)
           ("H" magit-dispatch)
           ("L" magit-log)
           ("C-l" magit-log-refresh)
           ,@(cond (evilize-want-jk-visual-lines
                    '(("j" evil-next-visual-line)
                      ("k" evil-previous-visual-line)))
                    (t
                     '(("j" evil-next-line)
                       ("k" evil-previous-line))))
           ("g g" evil-goto-first-line)
           ("G" evil-goto-line)
           ("C-d" evil-scroll-down)
           (":" evil-ex)
           ("q" magit-mode-bury-buffer)
           ("g r" magit-refresh)
           ("g R" magit-refresh-all)
           ("x" magit-delete-thing)
           ("X" magit-file-untrack)
           ("p" magit-push)
           ("o" magit-reset-quickly)
           ("O" magit-reset)
           ("-" magit-revert-no-commit)
           ("_" magit-revert)
           ("=" magit-diff-less-context)
           ("+" magit-diff-more-context)
           ("M-+" magit-diff-default-context)
           ("'" magit-submodule)
           ("\"" magit-subtree)
           (,evil-toggle-key evil-emacs-state)
           ("Z" magit-stash)
           ("v" evil-visual-line)
           ("V" evil-visual-line)
           ("C-w" evil-window-map)
           ("y" nil)
           ("y y" evilize-magit-evil-yank-line)
           ("y s" magit-copy-section-value)
           ("y b" magit-copy-buffer-revision)
           ("g '" magit-show-refs)
           ("0" evil-beginning-of-line)
           ("$" evil-end-of-line)
           ("`" magit-process-buffer)
           ,@(when evil-want-C-u-scroll
               '(("C-u" evil-scroll-up)))
           ,@(cond ((eq evil-search-module 'evil-search)
                    '(("/" evil-ex-search-forward)
                      ("?" evil-ex-search-backward)
                      ("n" evil-ex-search-next)
                      ("N" evil-ex-search-previous)))
                   (t
                    '(("/" evil-search-forward)
                      ("?" evil-search-backward)
                      ("n" evil-search-next)
                      ("N" evil-search-previous))))
           ))
         ((normal visual) (,magit-status-mode-map)
          (("g s" magit-jump-to-staged)
           ("g u" magit-jump-to-unstaged)
           ("g t" magit-jump-to-tracked)
           ("g n" magit-jump-to-untracked)
           ("g z" magit-jump-to-stashes)
           ("g f u" magit-jump-to-unpulled-from-upstream)
           ("g f p" magit-jump-to-unpulled-from-pushremote)
           ("g p u" magit-jump-to-unpushed-to-pushremote)
           ("g p p" magit-jump-to-unpushed-to-pushremote)
           ("g d" magit-jump-to-diffstat-or-diff)
           ))
         ((normal visual) (,magit-section-mode-map)
          (("TAB" magit-section-toggle)
           ("<backtab>" magit-section-cycle-global)
           ("<C-tab>" magit-section-cycle)
           ("C-j" magit-section-forward)
           ("C-k" magit-section-backward)
           ("]" magit-section-forward-sibling)
           ("[" magit-section-backward-sibling)
           ("g h" magit-section-up)
           ("z 1" magit-section-show-level-1-all)
           ("z 2" magit-section-show-level-2-all)
           ("z 3" magit-section-show-level-3-all)
           ("z 4" magit-section-show-level-4-all)
           ("z a" magit-section-toggle)
           ("z c" magit-section-hide)
           ("z C" magit-section-hide-children)
           ("z o" magit-section-show)
           ("z O" magit-section-show-children)
           ))
         ((normal visual) (,magit-blob-mode-map)
          (("C-j" magit-blob-next)
           ("C-k" magit-blob-previous)
           ))
         ((normal) (,magit-blame-read-only-mode-map)
          (,@(cond (evilize-want-jk-visual-lines
                    '(("j" evil-next-visual-line)
                      ("k" evil-previous-visual-line)))
                    (t
                     '(("j" evil-next-line)
                       ("k" evil-previous-line))))
           ("C-j" magit-blame-next-chunk)
           ("C-k" magit-blame-previous-chunk)
           ("M-j" magit-blame-next-chunk-same-commit)
           ("M-k" magit-blame-previous-chunk-same-commit)
           ))
         ((normal visual) (,magit-blame-read-only-mode-map)
          (,@(cond ((eq evil-search-module 'evil-search)
                    '(("/" evil-ex-search-forward)
                      ("?" evil-ex-search-backward)
                      ("n" evil-ex-search-next)
                      ("N" evil-ex-search-previous)))
                   (t
                    '(("/" evil-search-forward)
                      ("?" evil-search-backward)
                      ("n" evil-search-next)
                      ("N" evil-search-previous))))
           ))
         ((normal) (,magit-blame-mode-map ,magit-blame-read-only-mode-map)
          (("q" magit-blame-quit)
           )))
       ))
  ;; We also need to define key bindings in visual state, otherwise,
  ;; overriding maps will take precedence.
  (pcase-dolist (`(,states ,keymaps ,bindings) specs)
    (dolist (keymap keymaps)
      (apply #'evil-define-key* states keymap
             (evilize--normalize-bindings bindings)))))

(defvar evilize-magit-popup-keys-remapped nil)
(unless evilize-magit-popup-keys-remapped
  (let ((popup-remaps
         '((magit-branch "x" "X")
           (magit-branch "k" "x")
           (magit-dispatch "o" "'")
           (magit-dispatch "O" "\"")
           (magit-dispatch "V" "_")
           (magit-dispatch "X" "O")
           (magit-dispatch "v" "-")
           (magit-dispatch "k" "x")
           (magit-remote "k" "x")
           ;; FIXME: Two actions with same keys but with different apply
           ;; conditions, how to remap properly?
           (magit-revert "V" "_")
           (magit-revert "V" "_")
           (magit-tag "k" "x"))))
    (pcase-dolist (`(,popup ,from ,to) popup-remaps)
      (transient-suffix-put popup from :key to)))
  (setq evilize-magit-popup-keys-remapped t))

(dolist (map (list magit-mode-map
                   magit-status-mode-map
                   magit-blob-mode-map
                   magit-diff-mode-map
                   magit-cherry-mode-map
                   magit-log-mode-map
                   magit-log-select-mode-map
                   magit-log-read-revs-map
                   magit-refs-mode-map
                   magit-reflog-mode-map
                   magit-process-mode-map
                   ))
  (evil-make-overriding-map map))

(evil-make-overriding-map magit-blame-read-only-mode-map 'normal)

(dolist (mode '(git-rebase-mode
                magit-mode
                magit-status-mode
                magit-stash-mode
                magit-stashes-mode
                magit-diff-mode
                magit-cherry-mode
                magit-log-mode
                magit-log-select-mode
                magit-process-mode
                magit-refs-mode
                magit-reflog-mode
                magit-revision-mode
                magit-repolist-mode
                ))
  (evil-set-initial-state mode 'normal))

(dolist (cmd '(magit-section-forward-sibling
               magit-section-forward
               magit-section-backward-sibling
               magit-section-backward
               magit-section-up))
  (evil-set-command-property cmd :keep-visual t))

;; Need to refresh evil keymaps when blame mode is entered.
(add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps)

(define-minor-mode evilize-magit-toggle-text-minor-mode
  "Minor mode used to enabled toggle key in `text-mode' after
using `evilize-magit-toggle-text-mode'."
  :keymap (make-sparse-keymap))

(defvar evilize-magit-last-mode nil
  "Used to store last magit mode before entering text mode using
`evilize-magit-toggle-text-mode'.")

(defun evilize-magit-toggle-text-mode ()
  "Switch to `text-mode' and back from magit buffers."
  (interactive)
  (cond
   ((derived-mode-p 'magit-mode)
    (setq evilize-magit-last-mode major-mode)
    (message "Switching to text-mode")
    (text-mode)
    (evilize-magit-toggle-text-minor-mode 1)
    (evil-normalize-keymaps))
   ((and (eq major-mode 'text-mode)
         (functionp evilize-magit-last-mode))
    (message "Switching to %s" evilize-magit-last-mode)
    (evilize-magit-toggle-text-minor-mode -1)
    (evil-normalize-keymaps)
    (funcall evilize-magit-last-mode)
    (magit-refresh)
    (evil-change-state 'normal))
   (t
    (user-error "evilize-magit-toggle-text-mode unexpected state"))))

(evil-define-key* 'normal evilize-magit-toggle-text-minor-mode-map
  (kbd "C-t") 'evilize-magit-toggle-text-mode)
(evil-define-key* 'normal magit-mode-map
  (kbd "C-t") 'evilize-magit-toggle-text-mode)

(evil-set-initial-state 'magit-repolist-mode 'normal)
(let ((bindings '(("q" quit-window)
                  ("m" magit-repolist-mark)
                  ("u" magit-repolist-unmark)
                  ("f" magit-repolist-fetch)
                  ("RET" magit-repolist-status)
                  ("g r"  magit-list-repositories)
                  )))
  (apply #'evil-define-key* 'normal magit-repolist-mode-map
         (evilize--normalize-bindings bindings)))
(add-hook 'magit-repolist-mode-hook #'evil-normalize-keymaps)

(evil-set-initial-state 'magit-submodule-list-mode 'normal)
(let ((bindings '(("q" quit-window)
                  ("RET" 'magit-repolist-status)
                  ("g r"  'magit-list-submodules)
                  )))
  (apply #'evil-define-key* 'normal magit-submodule-list-mode-map
         (evilize--normalize-bindings bindings)))
(add-hook 'magit-submodule-list-mode-hook #'evil-normalize-keymaps)

(define-key magit-file-section-map (kbd "C-j")  nil)
(define-key magit-hunk-section-map (kbd "C-j")  nil)

(with-eval-after-load 'git-rebase
  (defvar git-rebase-mode-map)
  (defvar git-rebase-show-instructions)
  (defvar git-rebase-comment-re)
  (declare-function git-rebase-mode-show-keybindings "ext:git-rebase")

  (defvar evilize-magit-git-rebase-mode-key-binding-specs
    '(("p" git-rebase-pick "pick = use commit")
      ("r" git-rebase-reword "reword = use commit, but edit the commit message")
      ("e" git-rebase-edit "edit = use commit, but stop for amending")
      ("s" git-rebase-squash "squash = use commit, but meld into previous commit")
      ("f" git-rebase-fixup "fixup = like \"squash\", but discard this commit's log message")
      ("x" git-rebase-exec "exec = run command (the rest of the line) using shell")
      ("d" git-rebase-kill-line "drop = remove commit")
      ("u" git-rebase-undo "undo last change")
      (nil with-editor-finish "tell Git to make it happen")
      (nil with-editor-cancel "tell Git that you changed your mind, i.e. abort")
      ,@(cond (evilize-want-jk-visual-lines
               '(("j" evil-next-visual-line "move point to next line")
                 ("k" evil-previous-visual-line "move point to previous line")))
              (t
               '(("j" evil-next-line "move point to next line")
                 ("k" evil-previous-line "move point to previous line"))))
      ("M-k" git-rebase-move-line-up "move the commit at point up")
      ("M-j" git-rebase-move-line-down "move the commit at point down")
      (nil git-rebase-show-commit "show the commit at point in another buffer")
      ))

  (pcase-dolist
      (`(,key ,cmd, _) evilize-magit-git-rebase-mode-key-binding-specs)
    (when key
      (evil-define-key* 'normal git-rebase-mode-map
        (kbd key) cmd)))

  (evil-make-overriding-map git-rebase-mode-map 'normal)

  (defun evilize-magit-git-rebase-mode-show-keybindings ()
    "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
    (let ((inhibit-read-only t)
          (aux-map (evil-get-auxiliary-keymap git-rebase-mode-map 'normal)))
      (save-excursion
        (goto-char (point-min))
        (when (and git-rebase-show-instructions
                   (re-search-forward
                    (concat git-rebase-comment-re "\\s-+p, pick")
                    nil t))
          (goto-char (line-beginning-position))
          (flush-lines (concat "^" (regexp-quote comment-start) ".+ = "))
          (pcase-dolist
              (`(,key ,cmd ,desc)
               evilize-magit-git-rebase-mode-key-binding-specs)
            (insert
             (format (propertize "%s %s %s\n"
                                 'font-lock-face 'font-lock-comment-face)
                     comment-start
                     (string-pad
                      (if (and key
                               (eq (lookup-key aux-map (kbd key)) cmd))
                          key
                        (replace-regexp-in-string
                         "<normal-state> " ""
                         (substitute-command-keys
                          (format "\\[%s]" cmd))))
                      8)
                     desc)))))))
  (remove-hook 'git-rebase-mode-hook
               #'git-rebase-mode-show-keybindings)
  (add-hook 'git-rebase-mode-hook
            'evilize-magit-git-rebase-mode-show-keybindings))

(provide 'evilize-magit)
