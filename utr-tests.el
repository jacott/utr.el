;;; utr-tests.el --- ERT tests for utr.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'compile)

(load-file "./utr.el") ;; don't rely on utr being in load-path
(require 'utr)

(defvar-local utr-my-args nil)

(defmacro utr-my-test-fixture (&rest body)
  "A fixture to set up a common environment for tests.  BODY is the test code."
  `(ert-with-temp-directory utr-temp-project
     (let ((user-emacs-directory utr-temp-project)
           (utr-history nil)
           (utr--current nil)
           (utr-history-size 5)
           (utr-default-history nil)
           (inhibit-message t)
           (utr-elisp-test-command (list "echo make SELECTOR=" :selector " test")))
       (unwind-protect
           (progn ,@body)

         (when (get-buffer "*test-run*")
           (let ((kill-buffer-query-functions nil))
             (kill-buffer "*test-run*")))
       ;; keep flycheck happy
         (or user-emacs-directory utr-history utr--current
             utr-history-size utr-default-history )))))

(defun log-msg (format-string &rest args)
  "Log a debug message.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((inhibit-message nil))
    (message "DEBUG: %s" (format format-string args))))

(defun utr-my-with-temp-file (prefix suffix body)
  "Create a temp file with PREFIX and SUFFIX to run BODY with."
  (let ((temp-file-path (make-temp-file prefix nil suffix)))
    (with-current-buffer (find-file-noselect temp-file-path)
      (unwind-protect
          (funcall body)
        ;; Clean u p: Delete the file when you're done
        (kill-buffer)
        (when (file-exists-p temp-file-path)
          (delete-file temp-file-path))))))

(defun utr-my-test-run (&rest args)
  "Dummy test-runner setting ARGS to `utr-my-args'."
  (setq-local utr-my-args args))

(defun utr-my-test-choose (test-current-method)
  "Dummy test-chooser.  Uses TEST-CURRENT-METHOD to set `:test-name'."
  (let ((plist (list :command '(sym "text" 2))))
    (if test-current-method
        (plist-put plist :test-name "my-test")
      plist)))

(ert-deftest utr-history ()
  "Test history manipulation."
  (utr-my-test-fixture
   (with-current-buffer (find-file-noselect "~/file1.foo")
     ;; need buffer because file probably does not exist
     (let ((buffer-read-only nil))
       (delete-region (point-min) (point-max))
       (insert "1234567890\n")
       (goto-char (point-min))
       (set-buffer-modified-p nil)))
   (find-file-noselect "~/file2.foo") ;; need buffer because file probably does not exist

   (utr--add-test '((my-test1 "~/file1.foo" "test1") :point 1 :command (2 3)))
   (utr--add-test '((my-test1 "~/file1.foo" "test2") :point 15))
   (utr--add-test '((my-test1 "~/file1.foo" "test1") :point 1))
   (utr--add-test '((my-test1 "~/file2.foo" "test1") :point 1))
   (utr--add-test '((my-test2 "~/file1.foo" "test1") :point 5))

   (should (equal 4 (length utr-history)))

   (utr-prev-test)
   (should (equal '(my-test1 "~/file2.foo" "test1") (caar (utr--history))))
   (should (equal buffer-file-truename "~/file2.foo"))

   (utr-next-test)
   (should (equal '(my-test2 "~/file1.foo" "test1") (caar (utr--history))))
   (should (equal buffer-file-truename "~/file1.foo"))
   (should (equal (point) 1))

   (utr-find-current)
   ;; point aligned because buffer already current
   (should (equal (point) 5))

   (should (equal buffer-file-truename "~/file1.foo"))
   (should (equal (utr--history) (utr--read-history)))
   (goto-char 8)

   (find-file "~/file2.foo")
   (utr-find-current)
   ;; point not aligned because buffer was not current
   (should (equal (point) 8))

   (utr--add-test '((rust "~/file1.bar") :point 1))
   (should (equal (cadaar (utr--history)) "~/file1.bar"))

   (utr--add-test '((rust "~/file2.bar") :point 1))
   (utr--add-test '((rust "~/file3.bar") :point 1))
   (utr--add-test '((rust "~/file4.bar") :point 1))

   (should (equal 5 (length utr-history)))
   (should (equal (caar (last utr-history)) '(my-test2 "~/file1.foo" "test1")))))

(ert-deftest utr--run ()
  (utr-my-test-fixture
   (utr-add-to-alist 'my-test-runner 'utr-my-test-run :choose-test 'utr-my-test-choose)
   (with-temp-buffer
     (setq-local utr-key 'my-test-runner)
     (utr-run-one)
     (with-current-buffer (find-file-noselect default-directory)
       (should (equal utr-my-args (list default-directory "my-test" '(:command (sym "text" 2) :point 1) nil)))
       (kill-buffer (current-buffer)))
     (utr-run-last 123)
     (with-current-buffer (find-file-noselect default-directory)
       (should (equal utr-my-args (list default-directory "my-test" '(:command (sym "text" 2) :point 1) 123)))
       (kill-buffer (current-buffer))))

   (let (buf1)
     (utr-my-with-temp-file
      "my-file" ".foo"
      (lambda ()
        (setq buf1 (current-buffer))
        (setq-local utr-key 'my-test-runner)
        (utr-run-file)
        (should (equal utr-my-args (list buffer-file-truename nil '(:command (sym "text" 2) :point 1) nil)))

        (utr-my-with-temp-file
         "my-file2" ".bar"
         (lambda ()
           (setq-local utr-key 'my-test-runner)
           (utr-run-file)
           (should (equal (car utr-my-args) buffer-file-truename))

           (utr-prev-test)
           (should (equal buf1 (current-buffer))))))))))

(ert-deftest utr-add-to-alist ()
  (utr-my-test-fixture
   (utr-add-to-alist 'my-test-runner 'utr-my-test-run :choose-test 'utr-my-test-choose)

   (should (equal (alist-get 'my-test-runner utr-alist)
                  '(:run-test utr-my-test-run :choose-test utr-my-test-choose)))))

(ert-deftest utr-choose-default ()
  (utr-my-test-fixture
   (let ((exp-command "echo Hello World")
         (utr-default-history '("prev")))
     (cl-letf (((symbol-function 'read-shell-command)
                (lambda (&rest args)
                  (pcase (car args)
                    ("Test command: "
                     (when (equal (cadr args) "prev")
                       (set (caddr args) (list exp-command))
                       exp-command))))))
         (should (equal (utr-choose-default t) (list :command exp-command)))
         (should (equal utr-default-history (list exp-command)))))))

(ert-deftest utr-run-default ()
  (utr-my-test-fixture
   (utr-run-default nil nil '(:command "echo my-test-command 123") nil)
   (with-current-buffer "*test-run*"
     (goto-char (point-min))
     (should (looking-at "-\\*- mode: utr-default-test;"))
     (forward-line 3)
     (should (looking-at "echo my-test-command 123")))))

(ert-deftest utr-choose-elisp-one ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" ".eol"
    (lambda ()
      (insert "\n\n(ert-deftest my-test () (my body))")
      (goto-char (point-min))
      (search-forward "body")
      (should (equal (utr-choose-elisp t) '(:test-name "my-test")))))))

(ert-deftest utr-choose-elisp-file ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" ".eol"
    (lambda ()
      (insert "\n\n(ert-deftest my-test () (my body))")
      (goto-char (point-min))
      (search-forward "body")
      (should (equal (utr-choose-elisp nil) '(:test-name "t")))))))

(ert-deftest utr-choose-elisp-one-not-found ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" ".eol"
    (lambda ()
      (insert "\n\n(ert-defteeest my-test () (my body))")
      (goto-char (point-min))
      (search-forward "body")
      (should-error (utr-choose-elisp t) :type 'error)))))

(ert-deftest utr-run-elisp ()
  (utr-my-test-fixture
   (utr-run-elisp nil "my-test-to-run" nil nil)
   (with-current-buffer "*test-run*"
     (goto-char (point-min))
     (should (looking-at "-\\*- mode: utr-default-test;"))
     (forward-line 3)
     (should (looking-at "echo make SELECTOR=my-test-to-run test")))))

(ert-deftest utr--filter-alist ()
  (should (equal (utr--filter-alist
                  '(:a 1 :b 2 :c 1 :d 3 :e 4)
                  (lambda (_k v) (> v 1)))
                 '(:b 2 :d 3 :e 4))))

;;; utr-tests.el ends here
