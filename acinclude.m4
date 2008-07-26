# AC_EMACS_LISP(var, code)
# Set `var' with output of elisp `code'.
# To avoid incompatibilities of "single-quote in back-quote" etc,
# temporary files are used to store emacs-lisp and output of it.
AC_DEFUN([AC_EMACS_LISP],[dnl
{ ac_temp=./conftemp.$$
ac_output=confout.$$
rm -f $ac_temp $ac_output
cat >>$ac_temp <<\_ACEOF
(defun ac-temp-func (&optional argv)
$2
(princ "\n") ; make sure the output has trailing newline.
)
_ACEOF
$EMACS -batch -q -l $ac_temp -eval "(ac-temp-func $3)" | sed -e '/^ *$/d' > $ac_output
$1=`cat $ac_output`
rm -f $ac_temp $ac_output; }])
