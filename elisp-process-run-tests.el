;;; tests for the run stuff

(require 'elisp-prcess-run)
(require 'ert)

(ert-deftest elisp-process/rarchive-dir ()
  "Test the archive-dir stuff."
  ;; Test with the default, using package-archives
  (should
   (equal
    '(setq package-archives
      (quote (("local" . "~/work")
              ("gnu" . "http://elpa.gnu.org/packages/"))))
    (let ((package-archives
           '(("gnu" . "http://elpa.gnu.org/packages/"))))
      (read (elisp-process/rarchive-dir "~/work")))))
  ;; Test turning off package-archives
  (should
   (equal
    '(setq package-archives
      (quote (("local" . "~/work"))))
    (let (elisp-process-use-archives
          (package-archives
           '(("gnu" . "http://elpa.gnu.org/packages/"))))
      (read (elisp-process/rarchive-dir "~/work")))))
  ;; Test turning off package-archives and specifying nil
  (should
   (equal
    '(setq package-archives '())
    (let (elisp-process-use-archives
          (package-archives
           '(("gnu" . "http://elpa.gnu.org/packages/"))))
      (read (elisp-process/rarchive-dir nil))))))

(ert-deftest elisp-process/rafter-init-lisp ()
  "Test the `after-init-hook' computation."
  (should
   (equal
    '(add-hook 'after-init (lambda nil (load "~/blah.el")))
    (read (elisp-process/rafter-init-lisp "~/blah.el")))))

(ert-deftest elisp-process/rlisp ()
  "Test making the LISP into a string."
  (should
   (equal
    '(let ((x '(some list)))
      (mapcar 'identity x))
    (read
     (elisp-process/rlisp
      '(let ((x '(some list)))(mapcar 'identity x)))))))

(ert-deftest elisp-process-run ()
  "Test actually starting and stopping servers."
  (let* (elisp-process-use-archives
         (server (elisp-process-run-server
                  '(let ((x 1)) x)
                  "/tmp/test-archive")))
    (unwind-protect
         (should
          (equal
           "hello world"
           (progn
             (elisp-process-eval server '(setq test-var-1 "hello world"))
             (elisp-process-eval server 'test-var-1))))
      (kill-elisp-process server))))

;;; end tests
