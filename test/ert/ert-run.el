;;; ert-run.el --- ERT's internal infrastructure for running tests

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
(require 'ert)


(defvar ert-debug-on-error nil
  "Non-nil means enter debugger when a test fails or terminates with an error.")

;;; Running tests.

;; The data structures that represent the result of running a test.
(defstruct ert-test-result
  (messages nil)
  (should-forms nil)
  )
(defstruct (ert-test-passed (:include ert-test-result)))
(defstruct (ert-test-result-with-condition (:include ert-test-result))
  (condition (assert nil))
  (backtrace (assert nil)))
(defstruct (ert-test-error (:include ert-test-result-with-condition)))
(defstruct (ert-test-quit (:include ert-test-result-with-condition)))
(defstruct (ert-test-failed (:include ert-test-result-with-condition)))
(defstruct (ert-test-aborted-with-non-local-exit (:include ert-test-result)))


(defun ert--record-backtrace ()
  "Record the current backtrace (as a list) and return it."
  ;; Since the backtrace is stored in the result object, result
  ;; objects must only be printed with appropriate limits
  ;; (`print-level' and `print-length') in place.  For interactive
  ;; use, the cost of ensuring this possibly outweighs the advantage
  ;; of storing the backtrace for
  ;; `ert-results-pop-to-backtrace-for-test-at-point' given that we
  ;; already have `ert-results-rerun-test-debugging-errors-at-point'.
  ;; For batch use, however, printing the backtrace may be useful.
  (loop
   ;; 6 is the number of frames our own debugger adds (when
   ;; compiled; more when interpreted).  FIXME: Need to describe a
   ;; procedure for determining this constant.
   for i from 6
   for frame = (backtrace-frame i)
   while frame
   collect frame))

(defun ert--print-backtrace (backtrace)
  "Format the backtrace BACKTRACE to the current buffer."
  ;; This is essentially a reimplementation of Fbacktrace
  ;; (src/eval.c), but for a saved backtrace, not the current one.
  (let ((print-escape-newlines t)
        (print-level 8)
        (print-length 50))
    (dolist (frame backtrace)
      (ecase (first frame)
        ((nil)
         ;; Special operator.
         (destructuring-bind (special-operator &rest arg-forms)
             (cdr frame)
           (insert
            (format "  %S\n" (list* special-operator arg-forms)))))
        ((t)
         ;; Function call.
         (destructuring-bind (fn &rest args) (cdr frame)
           (insert (format "  %S(" fn))
           (loop for firstp = t then nil
                 for arg in args do
                 (unless firstp
                   (insert " "))
                 (insert (format "%S" arg)))
           (insert ")\n")))))))

;; A container for the state of the execution of a single test and
;; environment data needed during its execution.
(defstruct ert--test-execution-info
  (test (assert nil))
  (result (assert nil))
  ;; A thunk that may be called when RESULT has been set to its final
  ;; value and test execution should be terminated.  Should not
  ;; return.
  (exit-continuation (assert nil))
  ;; The binding of `debugger' outside of the execution of the test.
  next-debugger
  ;; The binding of `ert-debug-on-error' that is in effect for the
  ;; execution of the current test.  We store it to avoid being
  ;; affected by any new bindings the test itself may establish.  (I
  ;; don't remember whether this feature is important.)
  ert-debug-on-error)

(defun ert--run-test-debugger (info debugger-args)
  "During a test run, `debugger' is bound to a closure that calls this function.

This function records failures and errors and either terminates
the test silently or calls the interactive debugger, as
appropriate.

INFO is the ert--test-execution-info corresponding to this test
run.  DEBUGGER-ARGS are the arguments to `debugger'."
  (destructuring-bind (first-debugger-arg &rest more-debugger-args)
      debugger-args
    (ecase first-debugger-arg
      ((lambda debug t exit nil)
       (apply (ert--test-execution-info-next-debugger info) debugger-args))
      (error
       (let* ((condition (first more-debugger-args))
              (type (case (car condition)
                      ((quit) 'quit)
                      ((ert-test-failed) 'failed)
                      (otherwise 'error)))
              (backtrace (ert--record-backtrace)))
         (setf (ert--test-execution-info-result info)
               (ecase type
                 (quit
                  (make-ert-test-quit :condition condition
                                      :backtrace backtrace))
                 (failed
                  (make-ert-test-failed :condition condition
                                        :backtrace backtrace))
                 (error
                  (make-ert-test-error :condition condition
                                       :backtrace backtrace))))
         ;; Work around Emacs' heuristic (in eval.c) for detecting
         ;; errors in the debugger.
         (incf num-nonmacro-input-events)
         ;; FIXME: We should probably implement more fine-grained
         ;; control a la non-t `debug-on-error' here.
         (cond
          ((ert--test-execution-info-ert-debug-on-error info)
           (apply (ert--test-execution-info-next-debugger info) debugger-args))
          (t))
         (funcall (ert--test-execution-info-exit-continuation info)))))))

(defun ert--run-test-internal (ert-test-execution-info)
  "Low-level function to run a test according to ERT-TEST-EXECUTION-INFO.

This mainly sets up debugger-related bindings."
  (lexical-let ((info ert-test-execution-info))
    (setf (ert--test-execution-info-next-debugger info) debugger
          (ert--test-execution-info-ert-debug-on-error info) ert-debug-on-error)
    (catch 'ert--pass
      ;; For now, each test gets its own temp buffer and its own
      ;; window excursion, just to be safe.  If this turns out to be
      ;; too expensive, we can remove it.
      (with-temp-buffer
        (save-window-excursion
          (let ((debugger (lambda (&rest debugger-args)
                            (ert--run-test-debugger info debugger-args)))
                (debug-on-error t)
                (debug-on-quit t)
                ;; FIXME: Do we need to store the old binding of this
                ;; and consider it in `ert--run-test-debugger'?
                (debug-ignored-errors nil))
            (funcall (ert-test-body (ert--test-execution-info-test info))))))
      (ert-pass))
    (setf (ert--test-execution-info-result info) (make-ert-test-passed)))
  nil)

(defun ert--force-message-log-buffer-truncation ()
  "Immediately truncate *Messages* buffer according to `message-log-max'.

This can be useful after reducing the value of `message-log-max'."
  (with-current-buffer (get-buffer-create "*Messages*")
    ;; This is a reimplementation of this part of message_dolog() in xdisp.c:
    ;; if (NATNUMP (Vmessage_log_max))
    ;;   {
    ;;     scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
    ;;                   -XFASTINT (Vmessage_log_max) - 1, 0);
    ;;     del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
    ;;   }
    (when (and (integerp message-log-max) (>= message-log-max 0))
      (let ((begin (point-min))
            (end (save-excursion
                   (goto-char (point-max))
                   (forward-line (- message-log-max))
                   (point))))
        (delete-region begin end)))))

(defvar ert--running-tests nil
  "List of tests that are currently in execution.

This list is empty while no test is running, has one element
while a test is running, two elements while a test run from
inside a test is running, etc.  The list is in order of nesting,
innermost test first.

The elements are of type `ert-test'.")

(defun ert-run-test (ert-test)
  "Run ERT-TEST.

Returns the result and stores it in ERT-TEST's `most-recent-result' slot."
  (setf (ert-test-most-recent-result ert-test) nil)
  (block error
    (lexical-let ((begin-marker
                   (with-current-buffer (get-buffer-create "*Messages*")
                     (set-marker (make-marker) (point-max)))))
      (unwind-protect
          (lexical-let ((info (make-ert--test-execution-info
                               :test ert-test
                               :result
                               (make-ert-test-aborted-with-non-local-exit)
                               :exit-continuation (lambda ()
                                                    (return-from error nil))))
                        (should-form-accu (list)))
            (unwind-protect
                (let ((ert--should-execution-observer
                       (lambda (form-description)
                         (push form-description should-form-accu)))
                      (message-log-max t)
                      (ert--running-tests (cons ert-test ert--running-tests)))
                  (ert--run-test-internal info))
              (let ((result (ert--test-execution-info-result info)))
                (setf (ert-test-result-messages result)
                      (with-current-buffer (get-buffer-create "*Messages*")
                        (buffer-substring begin-marker (point-max))))
                (ert--force-message-log-buffer-truncation)
                (setq should-form-accu (nreverse should-form-accu))
                (setf (ert-test-result-should-forms result)
                      should-form-accu)
                (setf (ert-test-most-recent-result ert-test) result))))
        (set-marker begin-marker nil))))
  (ert-test-most-recent-result ert-test))

(defun ert-running-test ()
  "Return the top-level test currently executing."
  (car (last ert--running-tests)))


;;; Test selectors.

;; Autoload since ert.el refers to it in the docstring of
;; `ert-deftest'.
;;;###autoload
(defun ert-test-result-type-p (result result-type)
  "Return non-nil if RESULT matches type RESULT-TYPE.

Valid result types:

nil -- Never matches.
t -- Always matches.
:failed, :passed, :error -- Matches corresponding results.
\(and TYPES...\) -- Matches if all TYPES match.
\(or TYPES...\) -- Matches if some TYPES match.
\(not TYPE\) -- Matches if TYPE does not match.
\(satisfies PREDICATE\) -- Matches if PREDICATE returns true when called with
                           RESULT."
  ;; It would be easy to add `member' and `eql' types etc., but I
  ;; haven't bothered yet.
  (etypecase result-type
    ((member nil) nil)
    ((member t) t)
    ((member :failed) (ert-test-failed-p result))
    ((member :passed) (ert-test-passed-p result))
    ((member :error) (ert-test-error-p result))
    (cons
     (destructuring-bind (operator &rest operands) result-type
       (ecase operator
         (and
          (case (length operands)
            (0 t)
            (t
             (and (ert-test-result-type-p result (first operands))
                  (ert-test-result-type-p result `(and ,@(rest operands)))))))
         (or
          (case (length operands)
            (0 nil)
            (t
             (or (ert-test-result-type-p result (first operands))
                 (ert-test-result-type-p result `(or ,@(rest operands)))))))
         (not
          (assert (eql (length operands) 1))
          (not (ert-test-result-type-p result (first operands))))
         (satisfies
          (assert (eql (length operands) 1))
          (funcall (first operands) result)))))))

(defun ert-test-result-expected-p (test result)
  "Return non-nil if TEST's expected result type matches RESULT."
  (ert-test-result-type-p result (ert-test-expected-result-type test)))

;; Autoload since ert-ui.el refers to it in the docstring of
;; `ert-run-tests-interactively'.
;;;###autoload
(defun ert-select-tests (selector universe)
  "Return the tests that match SELECTOR.

UNIVERSE specifies the set of tests to select from; it should be
a list of tests, or t, which refers to all tests named by symbols
in `obarray'.

Returns the set of tests as a list.

Valid selectors:

nil -- Selects the empty set.
t -- Selects UNIVERSE.
:new -- Selects all tests that have not been run yet.
:failed, :passed, :error -- Select tests according to their most recent result.
:expected, :unexpected -- Select tests according to their most recent result.
a string -- Selects all tests that have a name that matches the string,
            a regexp.
a test -- Selects that test.
a symbol -- Selects the test that the symbol names, errors if none.
\(member TESTS...\) -- Selects TESTS, a list of tests or symbols naming tests.
\(eql TEST\) -- Selects TEST, a test or a symbol naming a test.
\(and SELECTORS...\) -- Selects the tests that match all SELECTORS.
\(or SELECTORS...\) -- Selects the tests that match any SELECTOR.
\(not SELECTOR\) -- Selects all tests that do not match SELECTOR.
\(tag TAG) -- Selects all tests that have TAG on their tags list.
\(satisfies PREDICATE\) -- Selects all tests that satisfy PREDICATE.

Only selectors that require a superset of tests, such
as (satisfies ...), strings, :new, etc. make use of UNIVERSE.
Selectors that do not, such as \(member ...\), just return the
set implied by them without checking whether it is really
contained in UNIVERSE."
  ;; This code needs to match the etypecase in
  ;; `ert-insert-human-readable-selector'.
  (etypecase selector
    ((member nil) nil)
    ((member t) (etypecase universe
                  (list universe)
                  ((member t) (ert-select-tests "" universe))))
    ((member :new) (ert-select-tests
                    `(satisfies ,(lambda (test)
                                   (null (ert-test-most-recent-result test))))
                    universe))
    ((member :failed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (ert-test-result-type-p
                                       (ert-test-most-recent-result test)
                                       ':failed)))
                       universe))
    ((member :passed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (ert-test-result-type-p
                                       (ert-test-most-recent-result test)
                                       ':passed)))
                       universe))
    ((member :error) (ert-select-tests
                      `(satisfies ,(lambda (test)
                                     (ert-test-result-type-p
                                      (ert-test-most-recent-result test)
                                      ':error)))
                      universe))
    ((member :expected) (ert-select-tests
                         `(satisfies
                           ,(lambda (test)
                              (ert-test-result-expected-p
                               test
                               (ert-test-most-recent-result test))))
                         universe))
    ((member :unexpected) (ert-select-tests `(not :expected) universe))
    (string
     (etypecase universe
       ((member t) (mapcar #'ert-get-test
                           (apropos-internal selector #'ert-test-boundp)))
       (list (ert--remove-if-not (lambda (test)
                                   (and (ert-test-name test)
                                        (string-match selector
                                                      (ert-test-name test))))
                                 universe))))
    (ert-test (list selector))
    (symbol
     (assert (ert-test-boundp selector))
     (list (ert-get-test selector)))
    (cons
     (destructuring-bind (operator &rest operands) selector
       (ecase operator
         (member
          (mapcar (lambda (purported-test)
                    (etypecase purported-test
                      (symbol (assert (ert-test-boundp purported-test))
                              (ert-get-test purported-test))
                      (ert-test purported-test)))
                  operands))
         (eql
          (assert (eql (length operands) 1))
          (ert-select-tests `(member ,@operands) universe))
         (and
          ;; Do these definitions of AND, NOT and OR satisfy de
          ;; Morgan's rules?  Should they?
          (case (length operands)
            (0 (ert-select-tests 't universe))
            (t (ert-select-tests `(and ,@(rest operands))
                                 (ert-select-tests (first operands)
                                                   universe)))))
         (not
          (assert (eql (length operands) 1))
          (let ((all-tests (ert-select-tests 't universe)))
            (ert--set-difference all-tests
                                 (ert-select-tests (first operands) all-tests))))
         (or
          (case (length operands)
            (0 (ert-select-tests 'nil universe))
            (t (ert--union (ert-select-tests (first operands) universe)
                           (ert-select-tests `(or ,@(rest operands))
                                             universe)))))
         (tag
          (assert (eql (length operands) 1))
          (let ((tag (first operands)))
            (ert-select-tests `(satisfies
                                ,(lambda (test)
                                   (member tag (ert-test-tags test))))
                              universe)))
         (satisfies
          (assert (eql (length operands) 1))
          (ert--remove-if-not (first operands)
                              (ert-select-tests 't universe))))))))

(defun ert--insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (labels ((rec (selector)
             ;; This code needs to match the etypecase in `ert-select-tests'.
             (etypecase selector
               ((or (member nil t
                            :new :failed :passed :error
                            :expected :unexpected)
                    string
                    symbol)
                selector)
               (ert-test
                (if (ert-test-name selector)
                    (make-symbol (format "<%S>" (ert-test-name selector)))
                  (make-symbol "<unnamed test>")))
               (cons
                (destructuring-bind (operator &rest operands) selector
                  (ecase operator
                    ((member eql and not or)
                     `(,operator ,@(mapcar #'rec operands)))
                    ((member tag satisfies)
                     selector)))))))
    (insert (format "%S" (rec selector)))))


;;; Facilities for running a whole set of tests.

;; The data structure that contains the set of tests being executed
;; during one particular test run, their results, the state of the
;; execution, and some statistics.
;;
;; The data about results and expected results of tests may seem
;; redundant here, since the test objects also carry such information.
;; However, the information in the test objects may be more recent, it
;; may correspond to a different test run.  We need the information
;; that corresponds to this run in order to be able to update the
;; statistics correctly when a test is re-run interactively and has a
;; different result than before.
(defstruct ert--stats
  (selector (assert nil))
  ;; The tests, in order.
  (tests (assert nil) :type vector)
  ;; A map of test names (or the test objects themselves for unnamed
  ;; tests) to indices into the `tests' vector.
  (test-map (assert nil) :type hash-table)
  ;; The results of the tests during this run, in order.
  (test-results (assert nil) :type vector)
  ;; The start times of the tests, in order, as reported by
  ;; `current-time'.
  (test-start-times (assert nil) :type vector)
  ;; The end times of the tests, in order, as reported by
  ;; `current-time'.
  (test-end-times (assert nil) :type vector)
  (passed-expected 0)
  (passed-unexpected 0)
  (failed-expected 0)
  (failed-unexpected 0)
  (error-expected 0)
  (error-unexpected 0)
  (start-time nil)
  (end-time nil)
  (aborted-p nil)
  (current-test nil)
  ;; The time at or after which the next redisplay should occur, as a
  ;; float.
  (next-redisplay 0.0))

(defun ert-stats-completed-expected (stats)
  "Return the number of tests in STATS that had expected results."
  (+ (ert--stats-passed-expected stats)
     (ert--stats-failed-expected stats)
     (ert--stats-error-expected stats)))

(defun ert-stats-completed-unexpected (stats)
  "Return the number of tests in STATS that had unexpected results."
  (+ (ert--stats-passed-unexpected stats)
     (ert--stats-failed-unexpected stats)
     (ert--stats-error-unexpected stats)))

(defun ert-stats-completed (stats)
  "Number of tests in STATS that have run so far."
  (+ (ert-stats-completed-expected stats)
     (ert-stats-completed-unexpected stats)))

(defun ert-stats-total (stats)
  "Number of tests in STATS, regardless of whether they have run yet."
  (length (ert--stats-tests stats)))

;; The stats object of the current run, dynamically bound.  This is
;; used for the mode line progress indicator.
(defvar ert--current-run-stats nil)

(defun ert--stats-test-key (test)
  "Return the key used for TEST in the test map of ert--stats objects.

Returns the name of TEST if it has one, or TEST itself otherwise."
  (or (ert-test-name test) test))

(defun ert--stats-set-test-and-result (stats pos test result)
  "Changes STATS by replacing the test at position POS with TEST and RESULT.

Also changes the counters in STATS to match."
  (let* ((tests (ert--stats-tests stats))
         (results (ert--stats-test-results stats))
         (old-test (aref tests pos))
         (map (ert--stats-test-map stats)))
    (flet ((update (d)
             (if (ert-test-result-expected-p (aref tests pos) (aref results pos))
                 (etypecase (aref results pos)
                   (ert-test-passed (incf (ert--stats-passed-expected stats) d))
                   (ert-test-failed (incf (ert--stats-failed-expected stats) d))
                   (ert-test-error (incf (ert--stats-error-expected stats) d))
                   (null)
                   (ert-test-aborted-with-non-local-exit))
               (etypecase (aref results pos)
                 (ert-test-passed (incf (ert--stats-passed-unexpected stats) d))
                 (ert-test-failed (incf (ert--stats-failed-unexpected stats) d))
                 (ert-test-error (incf (ert--stats-error-unexpected stats) d))
                 (null)
                 (ert-test-aborted-with-non-local-exit)))))
      ;; Adjust counters to remove the result that is currently in stats.
      (update -1)
      ;; Put new test and result into stats.
      (setf (aref tests pos) test
            (aref results pos) result)
      (remhash (ert--stats-test-key old-test) map)
      (setf (gethash (ert--stats-test-key test) map) pos)
      ;; Adjust counters to match new result.
      (update +1)
      nil)))

(defun ert--make-stats (tests selector)
  "Create a new `ert--stats' object for running TESTS.

SELECTOR is the selector that was used to select TESTS."
  (setq tests (ert--coerce-to-vector tests))
  (let ((map (make-hash-table :size (length tests))))
    (loop for i from 0
          for test across tests
          for key = (ert--stats-test-key test) do
          (assert (not (gethash key map)))
          (setf (gethash key map) i))
    (make-ert--stats :selector selector
                     :tests tests
                     :test-map map
                     :test-results (make-vector (length tests) nil)
                     :test-start-times (make-vector (length tests) nil)
                     :test-end-times (make-vector (length tests) nil))))

(defun ert-run-or-rerun-test (stats test listener)
  ;; checkdoc-order: nil
  "Run the single test TEST and record the result using STATS and LISTENER."
  (let ((ert--current-run-stats stats)
        (pos (ert--stats-test-pos stats test)))
    (ert--stats-set-test-and-result stats pos test nil)
    ;; Call listener after setting/before resetting
    ;; (ert--stats-current-test stats); the listener might refresh the
    ;; mode line display, and if the value is not set yet/any more
    ;; during this refresh, the mode line will flicker unnecessarily.
    (setf (ert--stats-current-test stats) test)
    (funcall listener 'test-started stats test)
    (setf (ert-test-most-recent-result test) nil)
    (setf (aref (ert--stats-test-start-times stats) pos) (current-time))
    (unwind-protect
        (ert-run-test test)
      (setf (aref (ert--stats-test-end-times stats) pos) (current-time))
      (let ((result (ert-test-most-recent-result test)))
        (ert--stats-set-test-and-result stats pos test result)
        (funcall listener 'test-ended stats test result))
      (setf (ert--stats-current-test stats) nil))))

(defun ert-run-tests (selector listener)
  "Run the tests specified by SELECTOR, sending progress updates to LISTENER."
  (let* ((tests (ert-select-tests selector t))
         (stats (ert--make-stats tests selector)))
    (setf (ert--stats-start-time stats) (current-time))
    (funcall listener 'run-started stats)
    (let ((abortedp t))
      (let ((ert--current-run-stats stats))
        (force-mode-line-update)
        (unwind-protect
            (progn
              (loop for test in tests do
                    (ert-run-or-rerun-test stats test listener))
              (setq abortedp nil))
          (setf (ert--stats-aborted-p stats) abortedp)
          (setf (ert--stats-end-time stats) (current-time))
          (funcall listener 'run-ended stats abortedp)))
      stats)))

(defun ert--stats-test-pos (stats test)
  ;; checkdoc-order: nil
  "Return the position (index) of TEST in the run represented by STATS."
  (gethash (ert--stats-test-key test) (ert--stats-test-map stats)))


;;; Formatting functions shared across UIs.

(defun ert--format-time-iso8601 (time)
  "Format TIME in the variant of ISO 8601 used for timestamps in ERT."
  (format-time-string "%Y-%m-%d %T%z" time))

(defun ert-char-for-test-result (result expectedp)
  "Return a character that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (etypecase result
             (ert-test-passed ".P")
             (ert-test-failed "fF")
             (ert-test-error "eE")
             (null "--")
             (ert-test-aborted-with-non-local-exit "aA"))))
    (elt s (if expectedp 0 1))))

(defun ert-string-for-test-result (result expectedp)
  "Return a string that represents the test result RESULT.

EXPECTEDP specifies whether the result was expected."
  (let ((s (etypecase result
             (ert-test-passed '("passed" "PASSED"))
             (ert-test-failed '("failed" "FAILED"))
             (ert-test-error '("error" "ERROR"))
             (null '("unknown" "UNKNOWN"))
             (ert-test-aborted-with-non-local-exit '("aborted" "ABORTED")))))
    (elt s (if expectedp 0 1))))

(defun ert--pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (let ((begin (point)))
    (pp object (current-buffer))
    (unless (bolp) (insert "\n"))
    (save-excursion
      (goto-char begin)
      (indent-sexp))))


(provide 'ert-run)

;;; ert-run.el ends here
