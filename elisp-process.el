;;; actors tests  -*- lexical-binding: t -*-

(require 'uuid)

(defconst elisp-process/list (make-hash-table :test 'equal)
  "The global list of processes in this Emacs.

The key is a process identifier string, currently a uuid.

The value is a list of:

  process-name package-spec wrapper-function queue
")

(defun elisp-process/make (name code &optional wrapper queue)
  (list name code (if wrapper wrapper (lambda nil)) (if queue queue nil)))

;; Sketch of maybe a better way to do the process creation
(defmacro* elisp-process/try-commit ((pid name code &optional wrapper queue)
                                     &rest body)
  (declare (indent 1))
  `(let ((proc (elisp-process/make-proc ,name ,code ,wrapper ,queue)))
     (unwind-protect
          (progn ,@body)
       (puthash pid proc elisp-process/list))))

;; example of using the sketch
(defun elisp-proces/try-commit-TEST ()
  (elisp-process/try-commit (10 [test-proc])
    (funcall wrapper)))

;; end macro sketch

(defun elisp-process/call-starter (starter)
  "Abstact the call to the process boot."
  (funcall starter))

(defun start-elisp-process (name package-spec server)
  "Start the process NAME with PACKAGE-SPEC on SERVER and get it's PID.

PACKAGE-SPEC is an array specifying what will run in the process.
It should be a function entry point and then the package name
containing the entry point and then a list of dependancies for
the package.  Note the list of package dependancies is optional,
a package can specify it's own dependancies and those are supported too.

An example PACKAGE-SPEC array looks like:

 [talkapp-mailer/send-email talkapp-mailer]

SERVER specifies how the communication between the current
process and the elisp-process will be done.  Currently only
Elnode communication is supported.  This is specified by passing
an array where the first element is `:elnode-static', the 2nd
element is the path to send requests to Elnode on and the third
element is the port number of the Elnode server.  See
`elisp-process-elnode-dispatcher' for details of Elnode
communication.

A PID or process-id is returned.  A PID is a cons with the car
the symbol `:elisp-process' and the cdr the process identifier.
The processes are stored in a process table keyed by process
identifier."
  (let* ((pid (uuid-string))
         (wrapper
          (lambda ()
            (larry-handle
             ;; needs to get the server from somewhere better
             (format "http://localhost:8100/elispproc/%s/" pid))))
         (process-details (list name package-spec wrapper nil)))
    (elisp-process/call-starter wrapper)
    ;; Record the PID in the process table
    (puthash pid process-details elisp-process/list)
    (cons :elisp-process pid)))

(defun elisp-process-kill-all ()
  "Kill all processes in the process table."
  (interactive)
  (let ((pids (kvalist->keys (kvhash->alist elisp-process/list))))
    (loop for pid in pids
       ;; We need this to actually send stop to the process
       do (remhash pid elisp-process/list))))

(defun elisp-process-send (pid &rest msg)
  "Send MSG to the process PID."
  (assert (eq (car pid) :elisp-process) t "%S is not an elisp-process." pid)
  (let* ((process (gethash (cdr pid) elisp-process/list)))
    (destructuring-bind (process-name code wrapper-fn input-queue) process
      (if input-queue
          (setcdr (last input-queue) (list msg))
          (setcar (last process) (list msg))))))

(defalias '@> 'elisp-process-send "The icecream operator sends MSG to PID")

(defun elisp-process/process-test (process input-queue)
  (let ((data (pop input-queue)))
    (setcar (last process) input-queue)
    data))

(defun elisp-process/work-test (pid)
  "Destructively tests whether there is anything to send to PID.

If there is something to give to the PID it is removed from the
queue and returned."
  (assert (eq (car pid) :elisp-process) t "%S is not an elisp-process." pid)
  (let* ((process (gethash (cdr pid) elisp-process/list)))
    (when process
      (destructuring-bind (process-name code wrapper-fn input-queue) process
        (when input-queue (elisp-process/process-test process input-queue))))))

(defun elisp-process/dispatch-handler (httpcon pid json-msg)
  (message "elisp-process handling request %s with %s" pid json-msg)
  (assert (eq (car pid) :elisp-process) t "%S is not an elisp-process." pid)
  (destructuring-bind (process-name package-spec wrapper queue)
      (gethash (cdr pid) elisp-process/list)
    (message
     "elisp-process handling pid details %s %s %s %s"
     (cdr pid) process-name package-spec queue)
    (let* ((actor-name (elt package-spec 0)) ; pull the actor name from the code
           ;; We need to put in the function name to the json
           (json-to-send (cons actor-name json-msg)))
      (message "elisp-process to %s sending %s" (cdr pid) json-to-send)
      (elnode-send-json httpcon json-to-send))))

;; FIXME - this should be an internal larry thing
;;
;; there are several ways that we could do the communication with
;; processes and this is just the larry way
(defun elisp-process-elnode-dispatcher (httpcon)
  "An elnode dispatcher to handle elisp-process requests.

The handler must be mapped to some path in an Elnode server.
This has to be done manually right now, by the user.

The Elnode communication works with defers.  When this handler
receives a request it defers processing waiting on an internal
queue which receives updates from `elisp-process-send', when an
update is received it is sent to the other process by processing
the defer."
  (let ((pid (elnode-http-mapping httpcon 1)))
    (message "elisp-process got request for %s" pid)
    (elnode-defer-until (elisp-process/work-test pid)
      (elisp-process/dispatch-handler httpcon pid elnode-defer-guard-it))))

(provide 'elisp-process)

;;; elisp-process.el ends here
