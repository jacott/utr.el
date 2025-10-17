;;; utr-tests.el --- ERT tests for utr.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'utr)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'compile)
(require 'project-find-test-helper)

(defvar-local utr-my-args nil)

(defconst utr-default-dir default-directory)

(defconst utr-test-file-name (abbreviate-file-name load-file-name))

(defmacro utr-my-test-fixture (&rest body)
  "A fixture to set up a common environment for tests.  BODY is the test code."
  `(ert-with-temp-directory
    utr-temp-project
    (let ((user-emacs-directory utr-temp-project)
          (utr-history nil)
          (utr--current nil)
          (utr-history-size 5)
          (utr-default-history nil)
          (inhibit-message t)
          (utr-elisp-test-command (list "echo make SELECTOR=" :selector " test")))

      (switch-to-buffer "*scratch*")
      (setq default-directory utr-default-dir)

      (unwind-protect
          (progn ,@body)

        (when (get-buffer "*test-run*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*test-run*")))
        ;; keep flycheck happy
        (or user-emacs-directory utr-history utr--current
            utr-history-size utr-default-history )))))

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

(ert-deftest utr-history-tests-per-file ()
  "Ensure utr-history-tests-per-file is obeyed."
  (utr-my-test-fixture
   (utr--add-test '((my-test1 "~/file1.foo" "test1") :point 1))
   (let ((utr-history-tests-per-file 2)
         (old-test (caar utr-history)))
     (utr--add-test '((my-test1 "~/file1.foo" "test2") :point 1))
     (should (assoc old-test utr-history #'eq))

     (utr--add-test '((my-test1 "~/file1.foo" "test3") :point 1))
     (should (not (assoc old-test utr-history)))

     )))

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

(ert-deftest utr-choose-elisp-from-prod-code ()
  (utr-my-test-fixture
   (find-file utr-test-file-name)
   (search-forward "(ert-deftest utr-choose-elisp-from-prod-code")
   (let ((pos (point)))
     (find-file (replace-regexp-in-string "-tests.el$" ".el" utr-test-file-name))
     (search-forward "(defun utr-choose-elisp (")
     (should (equal (utr-choose-elisp t) `(:test-name "utr-choose-elisp-from-prod-code"
                                                      :path ,utr-test-file-name
                                                      :point ,pos))))))

(ert-deftest utr-choose-elisp-one ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" "-tests.el"
    (lambda ()
      (insert "\n\n(ert-deftest my-test () (my body))")
      (goto-char (point-min))
      (search-forward "body")
      (should (equal (utr-choose-elisp t) '(:test-name "my-test")))))))

(ert-deftest utr-choose-elisp-file ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" "-tests.el"
    (lambda ()
      (insert "\n\n(ert-deftest my-test () (my body))")
      (goto-char (point-min))
      (search-forward "body")
      (should (equal (utr-choose-elisp nil) '(:test-name "t")))))))

(ert-deftest utr-choose-elisp-one-not-found ()
  (utr-my-test-fixture
   (utr-my-with-temp-file
    "my-file" "-tests.el"
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

(ert-deftest utr-filter-find-test ()
  (utr-my-test-fixture
   (pf-my-test-fixture
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test1") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t2.el") "test2") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test3") :point 1))
    (utr--add-test `((a-rust-test "~/src/another-test.rs" "ext") :point 1))

    (utr-find-test)
    (pf-my-add-keys "t1")
    (goto-char (point-min))
    (forward-line)
    (should (looking-at "^Tests: "))
    (should (not (search-forward "t2" nil t)))
    (should (not (search-forward "another" nil t))))))


(ert-deftest utr-pf-forget-selected-all-for-filename ()
  (utr-my-test-fixture
   (pf-my-test-fixture
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test1") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t2.el") "test2") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test3") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test4") :point 1))
    (utr--add-test `((a-rust-test "~/src/another-test.rs" "ext") :point 1))

    (utr-find-test)
    (should (pf--wait-for (lambda ()
                            (pf-goto-results)
                            (search-forward "~/src/another" nil t))))
    (utr-pf-forget-selected)
    (pf-goto-results)
    (should (search-forward "t1.el" nil t))
    (should (looking-at ": test3"))
    (utr-pf-forget-selected '(4))
    (pf-goto-results)
    (should (not (search-forward "t1.el" nil t)))
    )))

(ert-deftest utr-find-test ()
  (utr-my-test-fixture
   (pf-my-test-fixture
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test1") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t2.el") "test2") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test3") :point 1))
    (utr--add-test `((a-rust-test "~/src/another-test.rs" "ext") :point 1))

    (utr-find-test)
    (should (eq (current-local-map) utr-pf-local-map))
    (should (pf--wait-for (lambda ()
                            (goto-char (point-min))
                            (search-forward "1test/t1.el: test3" nil t))))
    (forward-line 0)
    (should (plist-get (text-properties-at (point)) 'invisible))
    (forward-char)
    (should (eq 'utr-filename-face (plist-get (text-properties-at (point)) 'face)))
    (search-forward ": t")
    (should (eq 'utr-testname-face (plist-get (text-properties-at (point)) 'face)))
    (search-forward "2~/src/another-test")
    (forward-line 0)
    (should (plist-get (text-properties-at (point)) 'invisible))
    (pf-forward-line 2)
    (pf-find-selected)
    (should (equal (buffer-name) "t2.el"))
    (should (equal (utr--testname) "test2")))))

(ert-deftest utr-find-test-limit-recent ()
  (utr-my-test-fixture
   (pf-my-test-fixture
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test1") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t2.el") "test2") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test5") :point 1))
    (utr--add-test `((a-rust-test "~/src/another-test.rs" "ext") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test6") :point 1))
    (utr--add-test `((my-test1 ,(concat default-directory "test/t1.el") "test3") :point 1))

    (utr-find-test '(4))
    (should (equal 5 pf-limit-to-recent))
    (utr-find-test 2)
    (should (equal 2 pf-limit-to-recent))

    (should (eq (current-local-map) utr-pf-local-map))
    (should (pf--wait-for (lambda ()
                            (goto-char (point-min))
                            (search-forward "test.rs" nil t))))
    (should
     (save-excursion
       (pf-goto-results)
       (not (search-forward "test2" nil t)))))))

;;; utr-tests.el ends here
