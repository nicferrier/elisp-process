;;; run elisp processes in daemons

(defvar elisp-process-use-archives t
  "Started processes have the current Emacs' `package-archives'?

If this is `t' then any started process will include the
`package-archives' from the current Emacs in addition or despite
anything be specified in the call to
`elisp-process-run-server'.

Let bind this to turn it off.")

(defun elisp-process/rarchive-dir (archive-dir)
  "Make the archive dir setting for the daemon process."
  (format "(setq package-archives (quote %S))"
          (if archive-dir
              (acons
               "local" archive-dir
               (if elisp-process-use-archives package-archives nil))
              (if elisp-process-use-archives package-archives nil))))

(defun elisp-process/rafter-init-lisp (after-init-file)
  "Make the `after-init-hook' for the daemon process."
  (if after-init-file
      (format "(add-hook 'after-init (lambda nil (load \"%s\")))"
              after-init-file)
      ""))

(defun elisp-process/rlisp (lisp)
  "Make the LISP into a string."
  (if lisp
      (format "%S" lisp)
      ""))

(defun elisp-process/rinit
    (elpa-dir boot-file lisp archive-dir after-init-file)
  "Make the init file for an Emacs daemon process."
  (with-temp-file boot-file
    (insert
     (concat
      "(progn"
      (elisp-process/rarchive-dir archive-dir)
      (elisp-process/rafter-init-lisp after-init-file)
      (format "(setq package-user-dir %S)" elpa-dir)
      (elisp-process/rlisp lisp)
      "(package-initialize)"
      "(package-refresh-contents))"))))

(defun elisp-process/rstart (lisp archive-dir after-init-lisp)
  (let* ((unique (make-temp-name "elisp-daemon-process"))
         (emacs-dir (concat "/tmp/" unique ".emacsd/"))
         (elpa-dir (concat emacs-dir "elpa/"))
         (emacs-bin (concat invocation-directory invocation-name))
         (boot-file (concat "/tmp/" unique ".emacs-init.el"))
         (after-init-file
          (when after-init-lisp
            (let ((afl (concat "/tmp/" unique ".post-init.el")))
              (with-temp-file afl
                (mapconcat (lambda (a) (format "%S" a)) after-init-lisp "\n"))
              ;; Return the filename
              afl)))
         (args
          (list (concat "--daemon=" unique) "-Q" "-l" boot-file)))
    ;; Write this stuff to a bunch of files
    (elisp-process/rinit elpa-dir boot-file lisp archive-dir after-init-file)
    ;; Return interesting stuff
    (list unique emacs-bin args)))

(defun elisp-process-run-server
    (lisp &optional archive-dir after-init-lisp)
  "Start an Emacs daemon process and run LISP in it.

LISP is a quoted form.  It is run in the new daemon before any
packages are installed or initialized.

ARCHIVE-DIR, if specified, is a package repository.
AFTER-INIT-LISP, if specified, should be a quoted LISP form to
execute after package initialization.

The process that starts the daemon should die once the
initialization has finished.  The daemon remains of course.

This function returns a cons of the initialization process and the
emacs server identifier.  The emacs server identifier can be used
to contact the emacs daemon directly, thus:

  emacsclient -s /tmp/emacs$UID/$emacs-server-identifier

presuming that you're trying to start it from the same user."
  (destructuring-bind (unique emacs-bin args)
      (elisp-process/rstart lisp archive-dir after-init-lisp)
    (let ((proc (apply 'start-process
                       unique ; name
                       (format "*%s*" unique) ; buffer
                       emacs-bin args)))
      ;; Not sure we should do this bit either
      (with-current-buffer (process-buffer proc)
        (compilation-mode))
      ;; This is an interesting thing that elpakit does...  it adds
      ;; the unique id to a process list so it can be managed properly
      ;;
      ;; How should we do this? do we need a start daemon hook?
      ;;
      ;;  (elpakit/process-add
      ;;   unique :daemon proc ; then the extra stuff
      ;;   install :pre-lisp pre-lisp :extra-lisp extra-lisp)
      (cons proc unique))))

(defun elisp-process-eval (elisp-process-id expression)
  "Evaluate EXPRESSION in ELISP-PROCESS-ID."
  (let ((boot-process (car elisp-process-id))
        (daemon-id (cdr elisp-process-id)))
    (condition-case err
        (server-eval-at daemon-id expression)
      ;; We should handle stack exception here? to send a better signal?
      ((file-error error)
       (if (not
            (string-match-p
             "^\\(No such server\\|Make client process\\)"
             (cadr err)))
           ;; Rethrow if anything else
           (signal (car err) (cdr err))
           ;; Else it was this error, try again? note that We'll
           ;; eventually bust the stack and cause a different error
           (sit-for 0.001)
           (elisp-process-eval elisp-process-id expression))))))

(defun elisp-process-delete (elisp-process-id)
  "Delete the ELISP-PROCESS-ID."
  (destructuring-bind (boot-process . daemon-id) elisp-process-id
    (condition-case err
        (progn
          (server-eval-at daemon-id '(kill-emacs))
          (delete-process boot-process))
      ((file-error error)
       (if (not
            (string-match-p
             "^\\(No such server\\|Make client process\\)"
             (cadr err)))
           ;; Rethrow if anything else
           (signal (car err) (cdr err)))))))

(defalias 'kill-elisp-process 'elisp-process-delete)

(provide 'elisp-process-run)

;;; elisp-process-run.el ends here
