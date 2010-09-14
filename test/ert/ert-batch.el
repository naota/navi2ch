;;; ert-batch.el --- Functions for running ERT tests in batch mode

;; Copyright (C) 2007, 2008, 2010 Christian M. Ohler

;; Author: Christian M. Ohler

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of ERT, the Emacs Lisp Regression Testing tool.
;; See ert.el or the texinfo manual for more details.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert-run)

(defvar ert-batch-backtrace-right-margin 70
  "*The maximum line length for printing backtraces in `ert-run-tests-batch'.")

;;;###autoload
(defun ert-run-tests-batch (&optional selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (ecase event-type
       (run-started
        (destructuring-bind (stats) event-args
          (message "Running %s tests (%s)"
                   (length (ert--stats-tests stats))
                   (ert--format-time-iso8601 (ert--stats-start-time stats)))))
       (run-ended
        (destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats)))
            (message "\n%sRan %s tests, %s results were as expected%s (%s)\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (ert-stats-completed-expected stats)
                     (if (zerop unexpected)
                         ""
                       (format ", %s unexpected" unexpected))
                     (ert--format-time-iso8601 (ert--stats-end-time stats)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (loop for test across (ert--stats-tests stats)
                    for result = (ert-test-most-recent-result test) do
                    (when (not (ert-test-result-expected-p test result))
                      (message "%9s  %S"
                               (ert-string-for-test-result result nil)
                               (ert-test-name test))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (destructuring-bind (stats test result) event-args
          (etypecase result
            (ert-test-passed)
            (ert-test-result-with-condition
             (message "Test %S backtrace:" (ert-test-name test))
             (with-temp-buffer
               (ert--print-backtrace (ert-test-result-with-condition-backtrace
                                      result))
               (goto-char (point-min))
               (while (not (eobp))
                 (let ((start (point))
                       (end (progn (end-of-line) (point))))
                   (setq end (min end
                                  (+ start ert-batch-backtrace-right-margin)))
                   (message "%s" (buffer-substring-no-properties
                                  start end)))
                 (forward-line 1)))
             (with-temp-buffer
               (insert "  ")
               (let ((print-escape-newlines t)
                     (print-level 5)
                     (print-length 10))
                 (let ((begin (point)))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result))))
               (goto-char (1- (point-max)))
               (assert (looking-at "\n"))
               (delete-char 1)
               (message "Test %S condition:" (ert-test-name test))
               (message "%s" (buffer-string))))
            (ert-test-aborted-with-non-local-exit))
          (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                 (format-string (concat "%9s  %"
                                        (prin1-to-string (length max))
                                        "s/" max "  %S")))
            (message format-string
                     (ert-string-for-test-result result
                                                 (ert-test-result-expected-p
                                                  test result))
                     (1+ (ert--stats-test-pos stats test))
                     (ert-test-name test)))))))))

;;;###autoload
(defun ert-run-tests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the framework detected an error
outside of the tests (e.g. invalid SELECTOR or bug in the code
that runs the tests)."
  (unwind-protect
      (let ((stats (ert-run-tests-batch selector)))
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 2))))


(provide 'ert-batch)

;;; ert-batch.el ends here
