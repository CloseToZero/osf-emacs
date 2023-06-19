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

(when (and (fboundp #'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'cpp))
  (with-eval-after-load 'cc-mode
    (defun osf-cpp-definitions-for-declarations (beg end)
      (require 'treesit)
      (let* ((field-decl-nodes
              (treesit-query-capture
               'cpp
               '((field_declaration
                  (function_declarator
                   (field_identifier) @field-id))
                 @field-decl)
               beg end t))
             (list-of-field-decl-id
              ;; A list of pairs of the form (field-decl-node field-id-node)
              (seq-partition field-decl-nodes 2))
             (class-spec-node
              (and
               field-decl-nodes
               (treesit-parent-until
                (car field-decl-nodes)
                (lambda (node)
                  (string= (treesit-node-type node) "class_specifier")))))
             (class-name-node
              (and
               class-spec-node
               (car
                (treesit-query-capture
                 class-spec-node
                 '((class_specifier (type_identifier) @class-name)) nil nil t))))
             (class-name
              (and class-name-node (treesit-node-text class-name-node t))))
        (unless list-of-field-decl-id
          (error "Cannot find function declarations"))
        (unless class-name
          (error "Cannot find class name"))
        (cl-loop for (field-decl-node field-id-node) in list-of-field-decl-id
                 for field-name-pos-within-field-decl =
                 (- (treesit-node-start field-id-node)
                    (treesit-node-start field-decl-node))
                 for field-decl-with-class-name =
                 (concat (substring (treesit-node-text field-decl-node t)
                                    0 field-name-pos-within-field-decl)
                         class-name
                         "::"
                         (substring (treesit-node-text field-decl-node t)
                                    field-name-pos-within-field-decl))
                 collect
                 (concat (if (string-suffix-p ";" field-decl-with-class-name)
                             (substring field-decl-with-class-name 0 -1)
                           field-decl-with-class-name)
                         "\n{\n}"))))

    (defun osf-cpp-copy-definitions-for-declarations ()
      (interactive)
      (let* ((beg (if (use-region-p) (region-beginning) (point)))
             (end (if (use-region-p) (region-end) (point)))
             (definitions
              (string-join (osf-cpp-definitions-for-declarations beg end) "\n\n")))
        (if (fboundp #'evil-yank-lines)
            (with-temp-buffer
              (insert definitions)
              (evil-yank-lines (point-min) (point-max)))
          (kill-new definitions))
        (cond ((and (bound-and-true-p evil-mode) (evil-visual-state-p))
               (evil-force-normal-state))
              ((region-active-p) (deactivate-mark)))
        (message "Definitions copied")))

    (osf-local-leader-define-key c++-mode-map
      "a d" #'osf-cpp-copy-definitions-for-declarations)
    ))

(provide 'osf-c-cpp)
