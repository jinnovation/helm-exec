;;; helm-exec --- simple execution framework for Helm -*- lexical-binding: t; -*-

;;; Copyright © 2018-2018 Jonathan Jin <jjin082693@gmail.com>

;; Author: Jonathan Jin <jjin082693@gmail.com>
;; URL: https://github.com/jinnovation/helm-exec
;; Keywords: helm, execution, framework
;; Version: 0.0.1

;; TODO: Package-Requires

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This libary provides a rudimentary execution framework for commonly-used
;; "applications" within Emacs. It's intended to be used for applications that
;; have alternate, one-off forms of execution, e.g. refreshing feeds for an RSS
;; application, or pulling recent emails for an email application.


;;; Code:

(require 'helm)

(defcustom helm-exec-prompt "> "
  "Completion prompt for execution input."
  :type 'string)

(defcustom helm-exec-applications nil
  "Mapping between symbolic identifiers and corresponding
'execution functions.'  Use this mapping to expose functions to
`helm-exec-execute' for easy access"
  :type '(alist
          :key-type symbol
          :value-type (list
                       (list (const executable) function)
                       (list (const alternate) (choice function (const nil))))))

(defun helm-exec-register-executable (exec &optional id alt)
  "Register EXEC as a function to be accessible via
`helm-exec-execute'. An explicit ID can be provided that the
function will be listed as; if not provided, the function is
listed under its own name. Similarly, an optional alternate
execution can be provided via ALT that defaults to nil.
  "
  (let* ((id (or id exec))
         (alt (or alt nil))
         (entry `(,id . ((executable . ,exec)
                         (alternate . ,alt)))))
    (add-to-list 'helm-exec-applications entry)))

(defun helm-exec--action/execute (selection)
  "Action that executes the executable corresponding to the given
  SELECTION."
  (let* ((spec (alist-get (intern selection) helm-exec-applications))
         (exec (alist-get 'executable spec)))
    (funcall exec)))

(defun helm-exec--action/alternate (selection)
  "Action that executes the alternate executable corresponding to
  the given SELECTION."
  (let* ((spec (alist-get (intern selection) helm-exec-applications))
         (exec (alist-get 'alternate spec)))
    (if (not exec)
        (message "No alternate execution for ID %s." selection)
      (funcall exec))))

(defun helm-exec--execution-candidates ()
  (mapcar (lambda (asoc) (car asoc)) helm-exec-applications))

(defconst helm-exec--actions
  (helm-make-actions
   "Execute" 'helm-exec--action/execute
   "Alternate" 'helm-exec--action/alternate))

(defconst helm-exec--exec-source
  (helm-build-sync-source
      "Available Applications"
    :candidates 'helm-exec--execution-candidates
    :action helm-exec--actions))

(defun helm-exec-execute ()
  (interactive)
  (helm :sources helm-exec--exec-source 
        :buffer "Execute"
        :prompt helm-exec-prompt))

(provide 'helm-exec)

;;; helm-exec ends here
