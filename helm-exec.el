;;; helm-exec --- simple "application" execution framework for Helm  -*- lexical-binding: t; -*-

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

(defun helm-exec-execute ()
  (interactive)
  (helm :sources (helm-build-sync-source "Available Applications"
                   :candidates (mapcar (lambda (asoc) (car asoc))
                                       helm-exec-applications)
                   :action (helm-make-actions
                            "Execute" (lambda (selection)
                                        (funcall (alist-get 'executable (alist-get (intern selection) helm-exec-applications))
                                        ))))
        :buffer "Execute"
        :prompt helm-exec-prompt))

(provide 'helm-exec)

;;; helm-exec ends here
