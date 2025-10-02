;;; utr.el --- Run unit tests for different frameworks from within Emacs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Geoff Jacobsen <geoffjacobsen@gmail.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Unit-test-runner is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; Unit-test-runner is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; Unit-test-runner.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; UTR, which stands for Unit Test Runner, is an Emacs package designed to
;; streamline the process of running unit tests.  It provides a uniform
;; interface for testing projects written in various programming languages using
;; differing frameworks, allowing users to run tests for a single function, a
;; specific file, a directory, or an entire project with a single keypress.  The
;; package automatically saves previous test configurations, enabling users to
;; quickly re-run their most recent tests without needing to re-configure them.
;;
;; This feature allows you to run your tests, make a small change, and then
;; immediately re-run the same test with a single command.  UTR also makes it
;; easy to switch between testing different granularities—from a single function
;; to an entire codebase—by remembering your last selection.  Its core focus is
;; to drastically reduce the friction associated with unit testing, making it a
;; fast and fluid part of your daily workflow.
;;
;; Built-in handling of elisp ert tests and a default runner are provided with
;; this package.
;;
;; Sample init.el setup:
;;
;; (global-set-key [C-f9] 'utr-run-one)
;; (global-set-key [C-S-f9] 'utr-run-file)
;; (global-set-key [f9] 'utr-run-last)
;; (global-set-key [S-f9] 'utr-find-current)
;; (global-set-key [M-f9] 'utr-prev-test)
;; (global-set-key [S-M-f9] 'utr-next-test)
;; (global-set-key [s-f9] 'utr-find-test)
;;
;;; Code:

;;; Options

(require 'compile)
(require 'cl-extra)
(require 'project-find)

(defgroup utr nil
  "Run unit tests across multiple languages."
  :group 'tools)

(defgroup utr-faces nil
  "Faces used with utr."
  :group 'utr)

(defcustom utr-history-size 30
  "The number of tests to remember."
  :type 'natnum
  :group 'utr)

(defcustom utr-elisp-test-command '("make SELECTOR=" :selector " test")
  "The command used to run an elisp test.
`:selector' is replaced by the test being run."
  :type 'sexp
  :group 'utr)

(defface utr-testname-face '((t :inherit font-lock-function-name-face))
  "Utr face use to highlight test names."
  :group 'utr-faces)

(defface utr-filename-face '((t :inherit font-lock-type-face))
  "Utr face use to highlight test file names."
  :group 'utr-faces)

(defvar utr-alist
  '((default :run-test utr-run-default :choose-test utr-choose-default)
    (emacs-lisp-mode :run-test utr-run-elisp :choose-test utr-choose-elisp))
  "Alist that specifies how to run a test for the current buffer.
Each elt has the form (KEY [KEYWORD VALUE]...).  KEY is the value of
either: local var `utr-key' or, if that is nil, `major-mode'.

See `utr-add-to-alist' for discription of KEYWORD and VALUE.")

(defvar-local utr-key nil
  "Override `major-mode' as unit-test key.
Set this to the key of the custom test runner you want to use.")

(defvar utr-default-history nil
  "History for choosing default test commands.")

(defvar utr--current nil
  "Utr history starting at the current test.")

(defvar utr-pf-filter-re ""
  "The Regexp used to filter `project-find' results for listing tests.")

(defconst utr--ert-deftest-re "^[[:space:]]*(ert-deftest \\([^[:space:]]+\\)"
  "Regexp to find ert tests.")

(defconst utr--key1 (propertize "1" 'invisible t)
  "Key for sorting history list.")

(defconst utr--key2 (propertize "2" 'invisible t)
  "Key for sorting history list.")

(defvar-keymap utr-pf-local-map
  :doc "Keymap for `project-find-mode'."
  :parent project-find-mode-map
  "M-<delete>" #'utr-pf-delete-selected)

(defun utr--data-directory ()
  "Return the persistent data directory for utr.
Creates the directory if it does not already exist."
  (let ((dir (expand-file-name "utr/" user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir))
    dir))

(defun utr--data-file ()
  "Name of history file."
  (expand-file-name "history" (utr--data-directory)))

(defun utr--read-history ()
  "Load history from a file."
  (let ((file (utr--data-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case _
            (read (current-buffer))
          (error nil))))))

(defvar utr-history (utr--read-history)
  "Test history.")


;;; functions

(defun utr--filter-alist (alist predicate)
  "Filter an ALIST, keeping only pairs where (PREDICATE key value) is non-nil."
  (let (result end)
    (while alist
      (let* ((key (car alist))
             (value (cadr alist)))
        (when (funcall predicate key value)
          (setq end (if result
                        (cdr (setcdr end (list (cadr end) key value)))
                      (setq result (list key value)))))
        (setq alist (cddr alist))))
    result))

(defun utr-add-to-alist (key run-test-func &rest rest)
  "Add a custom unit test runner.

KEY is the value of local var `utr-key' or `major-mode' in buffers that
should use this runner RUN-TEST-FUNC is the function name that runs the
test (it is stored in `utr-alist' under the keyword `:run-test').
RUN-TEST-FUNC will be passed the path, test-name and remaining plist
from `utr-add-test'.  See `utr-run-default' for an example.

REST is a plist with the following properties:

- `:choose-test' is the function which decides the test to run.  It is
passed one argument:

   TEST-CURRENT-METHOD is t when request test current method only or nil
             to test entire file.

   The function should return a plist used to run the test.  See ARGS in
   `utr-add-test'"

  (setq utr-alist (cons (cons key (cons :run-test (cons run-test-func rest)))
                        (assq-delete-all key utr-alist))))

(defun utr-add-test (key &optional args)
  "Add the test to test history.

KEY is the test type.

ARGS is a plist that configures how the test is run:

- `:path' the test's filename or directory.  Defaults to the
  current buffer's `buffer-file-truename' or `default-directory'.
- `:test-name' is used to run just one unit test.
- `:point' is the location the test was run from.  Defaults to the
  current buffer's `point'.
Any other property are specific to the test-runner.

See `utr-run-one' and `utr-run-file'."
  (let ((path (or (plist-get args :path) buffer-file-truename default-directory))
        (test-name (plist-get args :test-name))
        (point (or (plist-get args :point) (point)))
        (keywords '(:path :test-name)))
    (setq args (utr--filter-alist args (lambda (k _v) (not (memq k keywords)))))
    (setq args (plist-put args :point point))

    (utr--add-test (cons
                    (cons key
                          (cons (or path buffer-file-truename default-directory)
                                (if test-name (cons test-name nil) nil)))
                    args))))

(defun utr--add-test (list)
  "Add LIST to `utr-history' removing any duplicates.
Keep the list size to no larger that `utr-history-size'."
  (unless (equal list (car utr-history))
    (setq utr--current nil)
    (setq utr-history (cons list (ntake
                                  (- utr-history-size 1)
                                  (assoc-delete-all (car list) utr-history))))
    (utr--save-history)))

(defun utr--save-history ()
  "Write history to file."
  (let ((inhibit-message t))
    (write-region (prin1-to-string utr-history) nil (utr--data-file))))

(defun utr-prev-test ()
  "Find the previously run test."
  (interactive)
  (when (cdr (utr--history))
    (setq utr--current (cdr (utr--history))))
  (utr-find-current))

(defun utr-next-test ()
  "Find test in opposite direction to `utr-prev-test'."
  (interactive)
  (let ((pos utr-history))
    (when utr--current
      (while (if (eq utr--current (cdr pos))
                 (progn (setq utr--current pos) nil)
               (setq pos (cdr pos))))))
  (utr-find-current))

(defun utr-parent-in-history (entry)
  "Search history for item whom's cdr is ENTRY."
  (let ((pos utr-history))
    (while (and pos (not (eq (cdr pos) entry)))
      (setq pos (cdr pos)))
    pos))

(defun utr-cdr-in-history (key &optional alist pred)
  "Search history ALIST returning the cdr where PRED is true.
PRED is called with two arguments; the car of an element and KEY.
Uses `utr-history' if LIST is nil.  PRED defaults to `equal'."
  (let ((pos (or alist utr-history)))
    (unless pred
      (setq pred #'equal))
    (while (and pos (not (funcall pred (caar pos) key)))
      (setq pos (cdr pos)))
    pos))

(defun utr-find-current ()
  "Find the buffer the last test was run from.
If the buffer is already selected ensure the `point' is at the place it
was run from."
  (interactive)
  (when utr-history
    (when-let* ((buf (utr-current-buffer))
                (old-buf (current-buffer)))
      (switch-to-buffer buf)
      (message "testing %s" (utr-display-current))
      (when (eq buf old-buf)
        (goto-char (utr--get-prop :point))))))

(defun utr-display-current (&optional list)
  "Return the name of the current test as a string.
Use `utr-history' if LIST is nil."
  (if utr-history
      (let ((fn (or (utr--path list) ""))
            (tn (or (utr--testname list) "all")))
        (format "%s in: %s" (propertize tn 'face 'utr-testname-face) (propertize fn 'face 'utr-filename-face))
        )
    (error "Test not found")))

(defun utr-current-buffer (&optional list)
  "Find the buffer for the current test or open one if none.
Use `utr-history' if LIST is nil."
  (or (when utr-history
        (let ((fn (utr--path list)))
          (or (get-file-buffer fn)
              (and (file-exists-p fn) (find-file-noselect fn)))))
      (error "Test not found")))

(defun utr--history (&optional list)
  "Return LIST or the currently select test."
  (or list utr--current utr-history))

(defun utr--path (&optional list)
  "Return file path from LIST or, if nil, `utr--history'."
  (cadaar (utr--history list)))

(defun utr--testname (&optional list)
  "Return test name from LIST or, if nil, `utr--history'."
  (car (cddaar (utr--history list))))

(defun utr--run-args (&optional list)
  "Return test args from LIST or, if nil, `utr--history'."
  (cdar (utr--history list)))

(defun utr--get-prop (prop &optional list)
  "Return PROP value from LIST or, if nil, `utr--history'."
  (plist-get (cdar (utr--history list)) prop))

(defun utr-run-last (&optional arg)
  "Run the last recorded test.
ARG is a prefix in raw form passed to the test runner."
  (interactive "P")
  (if buffer-file-truename (save-buffer))
  (when utr-history
    (with-current-buffer (utr-current-buffer)
      (funcall (utr--runner) (utr--path)
               (utr--testname) (utr--run-args) arg))))

(defun utr--runner (&optional list)
  "Find the test runner from LIST or, if nil, `utr--history'."
  (when-let* ((key (caaar (utr--history list)))
              (entry (assoc key utr-alist)))
    (cl-getf (cdr entry) :run-test)))

(defun utr-run-one (&optional arg)
  "Run the test around point.
If ARG is set run the default test-runner."
  (interactive "P")
  (utr--run t arg))

(defun utr-run-file (&optional arg)
  "Run all the tests in the current buffer.
If ARG is not nil run the default test-runner."
  (interactive "P")
  (utr--run nil arg))

(defun utr--run (test-current-method &optional arg)
  "Choose and run a test.
TEST-CURRENT-METHOD is passed to the associated test chooser.
If ARG is not nil run the default test-runner."
  (if buffer-file-truename (save-buffer))
  (let* ((key (if arg
                  'default
                (or utr-key major-mode)))
         (entry (or (assoc key utr-alist)
                    (progn
                      (setq key 'default)
                      (assoc 'default utr-alist))))
         (choose (and entry (cl-getf (cdr entry) :choose-test))))

    (unless entry
      (error "The `utr-alist' is missing 'default key"))

    (utr-add-test key (if choose (funcall choose test-current-method)))
    (utr-run-last)))

(defun utr--default-test-buffer-name (_mode)
  "Return name of test buffer."
  "*test-run*")

(defun utr-run-default (_path _test-name args _raw-arg)
  "Run using the default test runner.
ARGS is a plist that should contain a `:command' property whose value is
the command to run."
  (save-some-buffers)
  (compilation-start (plist-get args :command)
                     'utr-default-test-mode 'utr--default-test-buffer-name))

(defun utr--colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (read-only-mode 1)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 0))


(define-compilation-mode utr-default-test-mode "Unit Test"
  "Compilation mode with enhanced coloring of buffer."
  (add-hook 'compilation-filter-hook 'utr--colorize-compilation-buffer))

(defun utr-choose-default (_test-current-method)
  "Choose command to run in the default test runner."
  (let* ((last-command (car utr-default-history))
         (command (read-shell-command
                   "Test command: "
                   last-command 'utr-default-history)))
    (list :command command)))


(defun utr--up-list ()
  "Move backward out of one level of parentheses.  Return t if successful."
  (condition-case _
      (progn
        (up-list -1 t)
        t)
    (scan-error
     (< (point-min) (forward-list -1)))))

(defun utr-choose-elisp (test-current-method)
  "Choose the elisp test to run based on where `point' is.
TEST-CURRENT-METHOD is t to run one test or nil run entire file."
  (if test-current-method
      (save-excursion
        (while (and (utr--up-list) (not (looking-at utr--ert-deftest-re))))
        (if-let* ((text (match-string-no-properties 1)))
            (list :test-name text)
          (error "Test not found")))
    (list :test-name "t")))

(defun utr--elisp-test-command (test-name)
  "Build elisp test command for TEST-NAME."
  (mapconcat (lambda (v)
               (prin1-to-string
                (if (eq v :selector)
                    test-name
                  v)
                t))
             utr-elisp-test-command))

(defun utr-run-elisp (_path test-name _args _raw-arg)
  "Run using the elisp test runner.
TEST-NAME contains the test to run or nil to run all tests."
  (save-some-buffers)
  (compilation-start (utr--elisp-test-command test-name)
                     'utr-default-test-mode 'utr--default-test-buffer-name))

(defun utr-pf-list-tests ()
  "List tests matching TEXT filter using `project-find'."
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (save-excursion
      (pf-goto-results)
      (delete-region (point) (point-max))
      (let ((inhibit-read-only t)
            (list utr-history)
            (l (length default-directory))
            path key)
        (while list
          (setq path (utr--path list))
          (setq key (if (string-prefix-p default-directory path)
                        (progn (setq path (substring path l)) utr--key1)
                      utr--key2))
          (when (string-match-p utr-pf-filter-re path)
            (pf-add-line (concat (propertize key 'utr-test list)
                                 (propertize path 'face 'utr-filename-face)
                                 ": "
                                 (propertize (utr--testname list) 'face 'utr-testname-face))))
          (setq list (cdr list)))
        (pf-goto-results)
        (pf-post-process-filter (point) 0)))))

(defun utr-pf-build-regex (text)
  "Build `utr-pf-filter-re' from TEXT."
  (setq utr-pf-filter-re
        (concat
         (mapconcat (lambda (char)
                      (regexp-quote (char-to-string char)))
                    text
                    ".*")
         ".*")))

(defun utr-pf-find-test (start _end)
  "Find test located in `project-find' buffer at START.
This function is used to override `pf-find-function' and assumes that it
is always called from the `project-find' buffer."
  (let ((entry (plist-get (text-properties-at start) 'utr-test)))
    (if (not entry)
        (error "Test not found")
      (setq utr--current entry)
      (pf-quit)
      (utr-find-current))))

(defun utr-pf-filter-changed (text _original)
  "Update results with new filter TEXT."
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (utr-pf-build-regex text)
    (utr-pf-list-tests)))

(defun utr-find-test ()
  "Find a test from `utr-history'.
Also allow managing the test history"
  (interactive)
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t)
        (dir default-directory))
    (pf-init)
    (use-local-map utr-pf-local-map)

    (setq default-directory dir
          utr-pf-filter-re ""
          pf-find-function #'utr-pf-find-test
          pf-filter-changed-function #'utr-pf-filter-changed)
    (pf-clear-output)
    (utr-pf-list-tests)))

(defun utr-toggle-test-buffer ()
  "Based on major-mode toggle between test buffer and production buffer."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((other (if (string-suffix-p "-tests.el" (or buffer-file-truename ""))
                     (concat (substring buffer-file-truename 0 -9) ".el")
                   (if (string-suffix-p ".el" (or buffer-file-truename ""))
                       (concat (substring buffer-file-truename 0 -3) "-tests.el")
                     ""))))
      (when (file-exists-p other)
        (find-file other))))))

(defun utr-pf-delete-selected (&optional arg)
  "Delete selected test.
With plain \\[universal-argument] for ARG, delete all tests for selected file."
  (interactive "P")
  (cond
   ((and (consp arg) (= (prefix-numeric-value arg) 4))
    (let ((entry (plist-get (text-properties-at (pf-selected-start)) 'utr-test))
          (curr (car utr--current)))
      (setq utr-history (assoc-delete-all
                         (utr--path entry) utr-history
                         (lambda (a b) (equal (cadr a) b))))
      (setq utr--current (utr-cdr-in-history (caar curr)))
      (utr-pf-list-tests)))
   (t
    (let* ((entry (plist-get (text-properties-at (pf-selected-start)) 'utr-test))
           (p (utr-parent-in-history entry)))
      (if p
          (setcdr p (cdr entry))
        (setq utr-history (cdr entry))))
    (utr-pf-list-tests))))

(provide 'utr)
;;; utr.el ends here
