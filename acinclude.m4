# grab and hack from lispdir.m4

## ------------------------
## Emacs LISP file handling
## From Ulrich Drepper
## Almost entirely rewritten by Alexandre Oliva
## ------------------------

# serial 3

AC_DEFUN([AM_PATH_LISPDIR],
 [AC_ARG_WITH(lispdir, 
  AC_HELP_STRING([--with-lispdir], [Override the default lisp directory]),
  [ lispdir="$withval" 
    AC_MSG_CHECKING([where .elc files should go])
    AC_MSG_RESULT([$lispdir])],
  [
  # If set to t, that means we are running in a shell under Emacs.
  # If you have an Emacs named "t", then use the full path.
  test x"$EMACS" = xt && EMACS=
  AC_CHECK_PROGS(EMACS, ${emacsen}, no)
  if test $EMACS = "no"; then
    AC_MSG_ERROR(cannot find emacs)
  fi
  if test x${lispdir+set} != xset; then
    AC_CACHE_CHECK([where .elc files should go], [am_cv_lispdir], [dnl
      am_cv_lispdir=`$EMACS -batch -q -eval '(princ (concat (car load-path) "\n"))' | sed -e 's,/$,,'`
      if test -z "$am_cv_lispdir"; then
	am_cv_lispdir='${datadir}/emacs/site-lisp'
      fi
      am_cv_lispdir="$am_cv_lispdir/navi2ch"
    ])
    lispdir="$am_cv_lispdir"
  fi
 ])
 AC_SUBST(lispdir)])
