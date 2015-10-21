;;; ensime-expand-region.el --- Ensime expansions for Expand Region  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add Ensime expansions to Expand Region.

;;; Code:

(require 'ensime-mode)
(require 'ensime-client)
(require 'ensime-editor)

;; Define Expand Region variable explicitly to avoid a hard `require' on Expand
;; Region.  Since we only enable this part of Ensime if Expand Region is
;; actually available, that's pretty safe nonetheless :)
(defvar er/try-expand-list)

(defun ensime-expand-region-mark-syntactic-context ()
  "Mark the next outer syntactic context."
  (when (ensime-connected-p)
    (ensime-expand-selection (mark) (point))))

(defun ensime-expand-region-add-expansions ()
  "Add expansions for Ensime."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(ensime-expand-region-mark-syntactic-context))))

(defun ensime-expand-region-enable ()
  "Enable Ensime expansions for all Ensime Mode buffers."
  (add-hook 'ensime-mode-hook #'ensime-expand-region-add-expansions)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when ensime-mode
          (ensime-expand-region-add-expansions))))))

(eval-after-load 'expand-region
  '(ensime-expand-region-enable))

(provide 'ensime-expand-region)
;;; ensime-expand-region.el ends here
