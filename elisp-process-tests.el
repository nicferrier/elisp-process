;;; elisp-process-tests.el -- actor tests -*- lexical-binding: t -*-

(require 'elisp-process)
(require 'ert)

(ert-deftest elisp-process/work-test ()
  "Test the we manipulae process comms queues correcctly."
  ;; Test that non-pids error properly
  (should-error (elisp-process/work-test 10))
  (let ((code [elisp-process/test-func])
        (elisp-process/list (make-hash-table :test 'equal)))
    ;; First nothing in the queue
    (puthash
     "10" (elisp-process/make "testproc" code)
     elisp-process/list)
    (should-not (elisp-process/work-test (cons :elisp-process "10")))
    ;; Now something in the queue
    (puthash
     "10" (elisp-process/make "testproc" code nil '((10) (20 30)))
     elisp-process/list)
    (should (equal '(10) (elisp-process/work-test (cons :elisp-process "10"))))
    ;; And check the queue still has the other stuff in it
    (should
     (equal '((20 30))
            (destructuring-bind (name code wrapper queue)
                (gethash "10" elisp-process/list)
              queue)))))

(ert-deftest elisp-process-send ()
  ;; It should fail if the pid isn't right
  (should-error (elisp-process-send 10 'this))
  (let ((code [elisp-process/test-func])
        (elisp-process/list (make-hash-table :test 'equal)))
    ;; First nothing in the queue
    (puthash
     "10" (elisp-process/make "testproc" code)
     elisp-process/list)
    (@> (cons :elisp-process "10") 1 2 3)
    (should
     (equal '((1 2 3))
            (destructuring-bind (name code wrapper queue)
                (gethash "10" elisp-process/list)
              queue)))
    ;; Cumulative queuing
    (@> (cons :elisp-process "10") 5 6)
    (should
     (equal '((1 2 3)
              (5 6))
            (destructuring-bind (name code wrapper queue)
                (gethash "10" elisp-process/list)
              queue)))))

(ert-deftest elisp-process/dispatch-handler ()
  "Elnode specific dispatch processor."
  (let* (pid code-proc-result
         (elisp-process/list (make-hash-table :test 'equal))
         (httpcon :httpcon)
         (code [code-proc]))
    (flet ((code-proc (a b c) (setq code-proc-result (list a b c)))
           ;; Fake the json send to set the result
           (elnode-send-json (httpcon data)
             ;; Mock larry-handle behaviour
             (larry/call
              (web/json-parse (json-encode data) :json-array-type 'list)))
           ;; Fake the call to starter to do nothing
           (elisp-process/call-starter (wrapper)))
      (setq pid (start-elisp-process
                 "testproc" code [:elnode-static "/path" 8000]))
      ;; Then we call the dispatcher
      (elisp-process/dispatch-handler httpcon pid '(1 2 3))
      (should (equal '(1 2 3) code-proc-result)))))


;;; elisp-process-tests.el ends here
