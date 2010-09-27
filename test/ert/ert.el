;;; ert.el --- Emacs Lisp Regression Testing

;; Copyright (C) 2007, 2008, 2010 Christian M. Ohler

;; Author: Christian M. Ohler
;; Version: 0.2
;; Keywords: lisp, tools

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

;; ERT is a tool for automated testing in Emacs Lisp.  Its main
;; features are facilities for defining and running test cases and
;; reporting the results as well as for debugging test failures
;; interactively.
;;
;; The main entry points are `ert-deftest', which is similar to
;; `defun' but defines a test, and `ert-run-tests-interactively',
;; which runs tests and offers an interactive interface for inspecting
;; results and debugging.  There is also
;; `ert-run-tests-batch-and-exit' for non-interactive use.  These
;; functions are autoloaded from separate files; ert.el contains only
;; the code necessary to define tests, to keep it small.  This way,
;; packages can include their test definitions without forcing all of
;; ERT's UI code to be loaded.
;;
;; The body of `ert-deftest' forms resembles a function body, but the
;; additional operators `should', `should-not' and `should-error' are
;; available.  `should' is similar to cl's `assert', but signals a
;; different error when its condition is violated that is caught and
;; processed by ERT.  In addition, it analyzes its argument form and
;; records information that helps debugging (`assert' tries to do
;; something similar when its second argument SHOW-ARGS is true, but
;; `should' is more sophisticated).  For information on `should-not'
;; and `should-error', see their docstrings.
;;
;; See ERT's info manual as well as the docstrings for more details.
;; To compile the manual, run `makeinfo ert.texinfo' in the ERT
;; directory, then C-u M-x info ert.info in Emacs to view it.
;;
;; To see some examples of tests written in ERT, see its self-tests in
;; ert-tests.el.  Some of these are tricky due to the bootstrapping
;; problem of writing tests for a testing tool, others test simple
;; functions and are straightforward.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'find-func)
(require 'debug)


;;; Copies/reimplementations of cl functions.

(defun ert--cl-do-remf (plist tag)
  "Copy of `cl-do-remf'.  Modify PLIST by removing TAG."
  (let ((p (cdr plist)))
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

(defun ert--remprop (sym tag)
  "Copy of `cl-remprop'.  Modify SYM's plist by removing TAG."
  (let ((plist (symbol-plist sym)))
    (if (and plist (eq tag (car plist)))
	(progn (setplist sym (cdr (cdr plist))) t)
      (ert--cl-do-remf plist tag))))

(defun ert--remove-if-not (ert-pred ert-list)
  "A reimplementation of `remove-if-not'.

ERT-PRED is a predicate, ERT-LIST is the input list."
  (loop for ert-x in ert-list
        if (funcall ert-pred ert-x)
        collect ert-x))

(defun ert--intersection (a b)
  "A reimplementation of `intersection'.  Intersect the sets A and B.

Elements are compared using `eql'."
  (loop for x in a
        if (memql x b)
        collect x))

(defun ert--set-difference (a b)
  "A reimplementation of `set-difference'.  Subtract the set B from the set A.

Elements are compared using `eql'."
  (loop for x in a
        unless (memql x b)
        collect x))

(defun ert--set-difference-eq (a b)
  "A reimplementation of `set-difference'.  Subtract the set B from the set A.

Elements are compared using `eq'."
  (loop for x in a
        unless (memq x b)
        collect x))

(defun ert--union (a b)
  "A reimplementation of `union'.  Compute the union of the sets A and B.

Elements are compared using `eql'."
  (append a (ert--set-difference b a)))

(eval-and-compile
  (defvar ert--gensym-counter 0))

(eval-and-compile
  (defun ert--gensym (&optional prefix)
    "Only allows string PREFIX, not compatible with CL."
    (unless prefix (setq prefix "G"))
    (make-symbol (format "%s%s"
                         prefix
                         (prog1 ert--gensym-counter
                           (incf ert--gensym-counter))))))

(defun ert--coerce-to-vector (x)
  "Coerce X to a vector."
  (when (char-table-p x) (error "Not supported"))
  (if (vectorp x)
      x
    (vconcat x)))

(defun* ert--remove* (x list &key key test)
  "Does not support all the keywords of remove*."
  (unless key (setq key #'identity))
  (unless test (setq test #'eql))
  (loop for y in list
        unless (funcall test x (funcall key y))
        collect y))

(defun ert--string-position (c s)
  "Return the position of the first occurrence of C in S, or nil if none."
  (loop for i from 0
        for x across s
        when (eql x c) return i))

(defun ert--mismatch (a b)
  "Return index of first element that differs between A and B.

Like `mismatch'.  Uses `equal' for comparison."
  (cond ((or (listp a) (listp b))
         (ert--mismatch (ert--coerce-to-vector a)
                        (ert--coerce-to-vector b)))
        ((> (length a) (length b))
         (ert--mismatch b a))
        (t
         (let ((la (length a))
               (lb (length b)))
           (assert (arrayp a) t)
           (assert (arrayp b) t)
           (assert (<= la lb) t)
           (loop for i below la
                 when (not (equal (aref a i) (aref b i))) return i
                 finally (return (if (/= la lb)
                                     la
                                   (assert (equal a b) t)
                                   nil)))))))

(defun ert--subseq (seq start &optional end)
  "Returns a subsequence of SEQ from START to END."
  (when (char-table-p seq) (error "Not supported"))
  (let ((vector (substring (ert--coerce-to-vector seq) start end)))
    (etypecase seq
      (vector vector)
      (string (concat vector))
      (list (append vector nil))
      (bool-vector (loop with result = (make-bool-vector (length vector) nil)
                         for i below (length vector) do
                         (setf (aref result i) (aref vector i))
                         finally (return result)))
      (char-table (assert nil)))))

(defun ert-equal-including-properties (a b)
  "Return t if A and B have similar structure and contents.

This is like `equal-including-properties' except that it compares
the property values of text properties structurally (by
recursing) rather than with `eq'.  Perhaps this is what
`equal-including-properties' should do in the first place; see
Emacs bug 6581 at URL `http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6581'."
  ;; This implementation is inefficient.  Rather than making it
  ;; efficient, let's hope bug 6581 gets fixed so that we can delete
  ;; it altogether.
  (not (ert--explain-not-equal-including-properties a b)))

;;; Defining and locating tests.

;; The data structure that represents a test case.
(defstruct ert-test
  (name nil)
  (documentation nil)
  (body (assert nil))
  (most-recent-result nil)
  (expected-result-type ':passed)
  (tags '()))

(defun ert-test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'ert--test) t))

(defun ert-get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (unless (ert-test-boundp symbol) (error "No test named `%S'" symbol))
  (get symbol 'ert--test))

(defun ert-set-test (symbol definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when (eq symbol 'nil)
    ;; We disallow nil since `ert-test-at-point' and related functions
    ;; want to return a test name, but also need an out-of-band value
    ;; on failure.  Nil is the most natural out-of-band value; using 0
    ;; or "" or signalling an error would be too awkward.
    ;;
    ;; Note that nil is still a valid value for the `name' slot in
    ;; ert-test objects.  It designates an anonymous test.
    (error "Attempt to define a test named nil"))
  (put symbol 'ert--test definition)
  definition)

(defun ert-make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (ert--remprop symbol 'ert--test)
  symbol)

(defun ert--parse-keys-and-body (keys-and-body)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
  (let ((extracted-key-accu '())
        (remaining keys-and-body))
    (while (and (consp remaining) (keywordp (first remaining)))
      (let ((keyword (pop remaining)))
        (unless (consp remaining)
          (error "Value expected after keyword %S in %S"
                 keyword keys-and-body))
        (when (assoc keyword extracted-key-accu)
          (warn "Keyword %S appears more than once in %S" keyword
                keys-and-body))
        (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    (list (loop for (key . value) in extracted-key-accu
                collect key
                collect value)
          remaining)))

;;;###autoload
(defmacro* ert-deftest (name () &body docstring-keys-and-body)
  "Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
\[:tags '(TAG...)] BODY...)"
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  (let ((documentation nil)
        (documentation-supplied-p nil))
    (when (stringp (first docstring-keys-and-body))
      (setq documentation (pop docstring-keys-and-body)
            documentation-supplied-p t))
    (destructuring-bind ((&key (expected-result nil expected-result-supplied-p)
                               (tags nil tags-supplied-p))
                         body)
        (ert--parse-keys-and-body docstring-keys-and-body)
      `(progn
         (ert-set-test ',name
                       (make-ert-test
                        :name ',name
                        ,@(when documentation-supplied-p
                            `(:documentation ,documentation))
                        ,@(when expected-result-supplied-p
                            `(:expected-result-type ,expected-result))
                        ,@(when tags-supplied-p
                            `(:tags ,tags))
                        :body (lambda () ,@body)))
         ;; This hack allows `symbol-file' to associate `ert-deftest'
         ;; forms with files, and therefore enables `find-function' to
         ;; work with tests.  However, it leads to warnings in
         ;; `unload-feature', which doesn't know how to undefine tests
         ;; and has no mechanism for extension.
         (push '(ert-deftest . ,name) current-load-list)
         ',name))))

;; We use these `put' forms in addition to the (declare (indent)) in
;; the defmacro form since the `declare' alone does not lead to
;; correct indentation before ert.el is loaded.  Autoloading these
;; `put' forms solves this.
;;;###autoload
(progn
  ;; TODO(ohler): figure out what these mean, and if both are needed.
  (put 'ert-deftest 'lisp-indent-function 2)
  (put 'ert-deftest 'lisp-indent-hook 2))

(defvar ert--find-test-regexp
  (concat "^\\s-*(ert-deftest"
          find-function-space-re
          "%s\\(\\s-\\|$\\)")
  "The regexp the `find-function' mechanisms use for finding test definitions.")


(put 'ert-test-failed 'error-conditions '(error ert-test-failed))
(put 'ert-test-failed 'error-message "Test failed")

(defun ert-pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'ert--pass nil))

(defun ert-fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'ert-test-failed (list data)))


;;; The `should' macros.

(defvar ert--should-execution-observer nil)

(defun ert--signal-should-execution (form-description)
  "Tell the current `should' form observer (if any) about FORM-DESCRIPTION."
  (when ert--should-execution-observer
    (funcall ert--should-execution-observer form-description)))

(defun ert--special-operator-p (thing)
  "Return non-nil if THING is a symbol naming a special operator."
  (and (symbolp thing)
       (let ((definition (indirect-function thing t)))
         (and (subrp definition)
              (eql (cdr (subr-arity definition)) 'unevalled)))))

(defun ert--expand-should-1 (whole form inner-expander)
  "Helper function for the `should' macro and its variants."
  (let ((form
         ;; If `cl-macroexpand' isn't bound, the code that we're
         ;; compiling doesn't depend on cl and thus doesn't need an
         ;; environment arg for `macroexpand'.
         (if (fboundp 'cl-macroexpand)
             ;; Suppress warning about run-time call to cl funtion: we
             ;; only call it if it's fboundp.
             (with-no-warnings
               (cl-macroexpand form (and (boundp 'cl-macro-environment)
                                         cl-macro-environment)))
           (macroexpand form))))
    ;; It's sort of a wart that `inner-expander' can't influence the
    ;; value the expansion returns.
    (cond
     ((atom form)
      (funcall inner-expander form `(list ',whole :form ',form :value ,form)))
     ((ert--special-operator-p (car form))
      (let ((value (ert--gensym "value-")))
        `(let ((,value (ert--gensym "ert-form-evaluation-aborted-")))
           ,(funcall inner-expander
                     `(setq ,value ,form)
                     `(list ',whole :form ',form :value ,value))
           ,value)))
     (t
      (let ((fn-name (car form))
            (arg-forms (cdr form)))
        (assert (or (symbolp fn-name)
                    (and (consp fn-name)
                         (eql (car fn-name) 'lambda)
                         (listp (cdr fn-name)))))
        (let ((fn (ert--gensym "fn-"))
              (args (ert--gensym "args-"))
              (value (ert--gensym "value-"))
              (default-value (ert--gensym "ert-form-evaluation-aborted-")))
          `(let ((,fn (function ,fn-name))
                 (,args (list ,@arg-forms)))
             (let ((,value ',default-value))
               ,(funcall inner-expander
                         `(setq ,value (apply ,fn ,args))
                         `(nconc (list ',whole)
                                 (list :form `(,,fn ,@,args))
                                 (unless (eql ,value ',default-value)
                                   (list :value ,value))
                                 (let ((-explainer-
                                        (and (symbolp ',fn-name)
                                             (get ',fn-name 'ert-explainer))))
                                   (when -explainer-
                                     (list :explanation
                                           (apply -explainer- ,args))))))
               ,value))))))))

(defun ert--expand-should (whole form inner-expander)
  "Helper function for the `should' macro and its variants.

Analyzes FORM and returns an expression that has the same
semantics under evaluation but records additional debugging
information.

INNER-EXPANDER should be a function and is called with two
arguments: INNER-FORM and FORM-DESCRIPTION-FORM, where INNER-FORM
is an expression equivalent to FORM, and FORM-DESCRIPTION-FORM is
an expression that returns a description of FORM.  INNER-EXPANDER
should return code that calls INNER-FORM and performs the checks
and error signalling specific to the particular variant of
`should'.  The code that INNER-EXPANDER returns must not call
FORM-DESCRIPTION-FORM before it has called INNER-FORM."
  (lexical-let ((inner-expander inner-expander))
    (ert--expand-should-1
     whole form
     (lambda (inner-form form-description-form)
       (let ((form-description (ert--gensym "form-description-")))
         `(let (,form-description)
            ,(funcall inner-expander
                      `(unwind-protect
                           ,inner-form
                         (setq ,form-description ,form-description-form)
                         (ert--signal-should-execution ,form-description))
                      `,form-description)))))))

(defmacro* should (form)
  "Evaluate FORM.  If it returns nil, abort the current test as failed.

Returns the value of FORM."
  (ert--expand-should `(should ,form) form
                      (lambda (inner-form form-description-form)
                        `(unless ,inner-form
                           (ert-fail ,form-description-form)))))

(defmacro* should-not (form)
  "Evaluate FORM.  If it returns non-nil, abort the current test as failed.

Returns nil."
  (ert--expand-should `(should-not ,form) form
                      (lambda (inner-form form-description-form)
                        `(unless (not ,inner-form)
                           (ert-fail ,form-description-form)))))

(defun ert--should-error-handle-error (form-description-fn
                                       condition type exclude-subtypes test)
  "Helper function for `should-error'.

Determines whether CONDITION matches TYPE, EXCLUDE-SUBTYPES and
TEST, and aborts the current test as failed if it doesn't."
  (let ((signalled-conditions (get (car condition) 'error-conditions))
        (handled-conditions (etypecase type
                              (list type)
                              (symbol (list type)))))
    (assert signalled-conditions)
    (unless (ert--intersection signalled-conditions handled-conditions)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason (concat "the error signalled did not"
                                       " have the expected type")))))
    (when exclude-subtypes
      (unless (member (car condition) handled-conditions)
        (ert-fail (append
                   (funcall form-description-fn)
                   (list
                    :condition condition
                    :fail-reason (concat "the error signalled was a subtype"
                                         " of the expected type"))))))
    (unless (funcall test condition)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason "the error signalled did not pass the test"))))))

;; FIXME: The expansion will evaluate the keyword args (if any) in
;; nonstandard order.
(defmacro* should-error (form &rest keys &key type exclude-subtypes test)
  "Evaluate FORM.  Unless it signals an error, abort the current test as failed.

The error signalled additionally needs to match TYPE and satisfy
TEST.  TYPE should be a condition name or a list of condition
names.  If EXCLUDE-SUBTYPES is nil, the error matches TYPE if one
of its condition names is an element of TYPE.  If
EXCLUDE-SUBTYPES is non-nil, the error matches TYPE if it is an
element of TYPE.  TEST should be a predicate."
  ;; Returns a gensym named `ert-form-evaluation-aborted-XXX', but
  ;; that's a wart, so let's not document it.
  (unless type (setq type ''error))
  (unless test (setq test '(lambda (condition) t)))
  (ert--expand-should
   `(should-error ,form ,@keys)
   form
   (lambda (inner-form form-description-form)
     (let ((errorp (ert--gensym "errorp"))
           (form-description-fn (ert--gensym "form-description-fn-")))
       `(let ((,errorp nil)
              (,form-description-fn (lambda () ,form-description-form)))
          (condition-case -condition-
              ,inner-form
            ;; We can't use ,type here because we want to evaluate it.
            (error
             (setq ,errorp t)
             (ert--should-error-handle-error ,form-description-fn
                                             -condition-
                                             ,type ,exclude-subtypes ,test)
             ;; It would make sense to have the `should-error' form
             ;; return the error in this case, but `ert--expand-should'
             ;; doesn't allow that at the moment.
             ))
          (unless ,errorp
            (ert-fail (append
                       (funcall ,form-description-fn)
                       (list
                        :fail-reason "did not signal an error")))))))))


;;; Explanation of `should' failures.

(defun ert--proper-list-p (x)
  "Return non-nil if X is a proper list, nil otherwise."
  (loop
   for firstp = t then nil
   for fast = x then (cddr fast)
   for slow = x then (cdr slow) do
   (when (null fast) (return t))
   (when (not (consp fast)) (return nil))
   (when (null (cdr fast)) (return t))
   (when (not (consp (cdr fast))) (return nil))
   (when (and (not firstp) (eq fast slow)) (return nil))))

(defun ert--explain-format-atom (x)
  "Format the atom X for `ert--explain-not-equal'."
  (typecase x
    (fixnum (list x (format "#x%x" x) (format "?%c" x)))
    (t x)))

(defun ert--explain-not-equal (a b)
  "Explainer function for `equal'.

Returns a programmer-readable explanation of why A and B are not
`equal', or nil if they are."
  (if (not (equal (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ert--proper-list-p a))
             (b-proper-p (ert--proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (equal (length a) (length b)))
                   `(proper-lists-of-different-length ,(length a) ,(length b)
                                                      ,a ,b
                                                      first-mismatch-at
                                                      ,(ert--mismatch a b))
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (ert--explain-not-equal ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))
                       finally (assert (equal a b) t)))
             (let ((car-x (ert--explain-not-equal (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (ert--explain-not-equal (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x)
                     (assert (equal a b) t)
                     nil))))))))
      (array (if (not (equal (length a) (length b)))
                 `(arrays-of-different-length ,(length a) ,(length b)
                                              ,a ,b
                                              ,@(unless (char-table-p a)
                                                  `(first-mismatch-at
                                                    ,(ert--mismatch a b))))
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (ert--explain-not-equal ai bi)
                     do (when xi (return `(array-elt ,i ,xi)))
                     finally (assert (equal a b) t))))
      (atom (if (not (equal a b))
                (if (and (symbolp a) (symbolp b) (string= a b))
                    `(different-symbols-with-the-same-name ,a ,b)
                  `(different-atoms ,(ert--explain-format-atom a)
                                    ,(ert--explain-format-atom b)))
              nil)))))
(put 'equal 'ert-explainer 'ert--explain-not-equal)

(defun ert--significant-plist-keys (plist)
  "Return the keys of PLIST that have non-null values, in order."
  (assert (zerop (mod (length plist) 2)) t)
  (loop for (key value . rest) on plist by #'cddr
        unless (or (null value) (memq key accu)) collect key into accu
        finally (return accu)))

(defun ert--plist-difference-explanation (a b)
  "Return a programmer-readable explanation of why A and B are different plists.

Returns nil if they are equivalent, i.e., have the same value for
each key, where absent values are treated as nil.  The order of
key/value pairs in each list does not matter."
  (assert (zerop (mod (length a) 2)) t)
  (assert (zerop (mod (length b) 2)) t)
  ;; Normalizing the plists would be another way to do this but it
  ;; requires a total ordering on all lisp objects (since any object
  ;; is valid as a text property key).  Perhaps defining such an
  ;; ordering is useful in other contexts, too, but it's a lot of
  ;; work, so let's punt on it for now.
  (let* ((keys-a (ert--significant-plist-keys a))
         (keys-b (ert--significant-plist-keys b))
         (keys-in-a-not-in-b (ert--set-difference-eq keys-a keys-b))
         (keys-in-b-not-in-a (ert--set-difference-eq keys-b keys-a)))
    (flet ((explain-with-key (key)
             (let ((value-a (plist-get a key))
                   (value-b (plist-get b key)))
               (assert (not (equal value-a value-b)) t)
               `(different-properties-for-key
                 ,key ,(ert--explain-not-equal-including-properties value-a
                                                                    value-b)))))
      (cond (keys-in-a-not-in-b
             (explain-with-key (first keys-in-a-not-in-b)))
            (keys-in-b-not-in-a
             (explain-with-key (first keys-in-b-not-in-a)))
            (t
             (loop for key in keys-a
                   when (not (equal (plist-get a key) (plist-get b key)))
                   return (explain-with-key key)))))))

(defun ert--abbreviate-string (s len suffixp)
  "Shorten string S to at most LEN chars.

If SUFFIXP is non-nil, returns a suffix of S, otherwise a prefix."
  (let ((n (length s)))
    (cond ((< n len)
           s)
          (suffixp
           (substring s (- n len)))
          (t
           (substring s 0 len)))))

(defun ert--explain-not-equal-including-properties (a b)
  "Explainer function for `ert-equal-including-properties'.

Returns a programmer-readable explanation of why A and B are not
`ert-equal-including-properties', or nil if they are."
  (if (not (equal a b))
      (ert--explain-not-equal a b)
    (assert (stringp a) t)
    (assert (stringp b) t)
    (assert (eql (length a) (length b)) t)
    (loop for i from 0 to (length a)
          for props-a = (text-properties-at i a)
          for props-b = (text-properties-at i b)
          for difference = (ert--plist-difference-explanation props-a props-b)
          do (when difference
               (return `(char ,i ,(substring-no-properties a i (1+ i))
                              ,difference
                              context-before
                              ,(ert--abbreviate-string
                                (substring-no-properties a 0 i)
                                10 t)
                              context-after
                              ,(ert--abbreviate-string
                                (substring-no-properties a (1+ i))
                                10 nil))))
          ;; TODO(ohler): Get `equal-including-properties' fixed in
          ;; Emacs, delete `ert-equal-including-properties', and
          ;; re-enable this assertion.
          ;;finally (assert (equal-including-properties a b) t)
          )))
(put 'ert-equal-including-properties
     'ert-explainer
     'ert--explain-not-equal-including-properties)


;;; Utility functions for load/unload actions.

(defun ert--activate-font-lock-keywords ()
  "Activate font-lock keywords for some of ERT's symbols."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(defun* ert--remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This can be used as an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (ert--remove* element
                      (symbol-value list-var)
                      :key key
                      :test test)))


;;; Actions on load/unload.

(add-to-list 'find-function-regexp-alist '(ert-deftest . ert--find-test-regexp))
(add-to-list 'minor-mode-alist '(ert--current-run-stats
                                 (:eval
                                  (ert--tests-running-mode-line-indicator))))
(add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)

(defun ert--unload-function ()
  "Unload function to undo the side-effects of loading ert.el."
  (ert--remove-from-list 'find-function-regexp-alist 'ert-deftest :key #'car)
  (ert--remove-from-list 'minor-mode-alist 'ert--current-run-stats :key #'car)
  (ert--remove-from-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
  nil)

(defvar ert-unload-hook '())
(add-hook 'ert-unload-hook 'ert--unload-function)


(provide 'ert)

;;; ert.el ends here
