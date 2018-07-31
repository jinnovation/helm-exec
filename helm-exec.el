;;; helm-exec --- simple execution framework for Helm -*- lexical-binding: t; -*-

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
