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


(with-eval-after-load 'cc-mode
  (defun osf-cpp-definition-for-declaration (&optional point)
    (interactive)
    (let* ((point (or point (point)))
           (field-id-node (treesit-node-at point 'cpp))
           (field-id-node?
            (and field-id-node
                 (string= (treesit-node-type field-id-node)
                          "field_identifier")))
           (field-decl-node
            (and field-id-node?
                 (treesit-parent-until
                  field-id-node
                  (lambda (node)
                    (string= (treesit-node-type node)
                             "field_declaration")))))
           (class-specifier-node
            (and field-decl-node
                 (treesit-parent-until
                  field-decl-node
                  (lambda (node)
                    (string= (treesit-node-type node)
                             "class_specifier")))))
           (class-name-node
            (and class-specifier-node
                 (treesit-node-child-by-field-name
                  class-specifier-node "name")))
           (class-name
            (and class-name-node
                 (treesit-node-text class-name-node))))
      (unless field-id-node?
        (error "Not at a field_identifier node"))
      (unless field-decl-node
        (error "Cannot find field_declaration node in ancestors"))
      (unless class-specifier-node
        (error "Cannot find a class_specifier node in ancestors"))
      (unless class-name
        (error "Cannot find class name"))
      (let* ((field-name-pos-within-field-decl
              (- (treesit-node-start field-id-node)
                 (treesit-node-start field-decl-node)))
             (field-decl-with-class-name
              (concat (substring (treesit-node-text field-decl-node)
                                 0 field-name-pos-within-field-decl)
                      class-name
                      "::"
                      (substring (treesit-node-text field-decl-node)
                                 field-name-pos-within-field-decl)))
             (field-definition
              (concat (if (string-suffix-p ";" field-decl-with-class-name)
                          (substring field-decl-with-class-name 0 -1)
                        field-decl-with-class-name)
                      "\n{\n}")))
        (if (fboundp #'evil-yank-lines)
            (with-temp-buffer
              (insert field-definition)
              (evil-yank-lines (point-min) (point-max)))
          (kill-new field-definition)))))

  (osf-local-leader-define-key c++-mode-map
    "a d" #'osf-cpp-definition-for-declaration)
  )

(provide 'osf-c-cpp)
