# grab and hack from lispdir.m4

## ------------------------
## Emacs LISP file handling
## From Ulrich Drepper
## Almost entirely rewritten by Alexandre Oliva
## ------------------------

# serial 3

AC_DEFUN([AM_PATH_LISPDIR],
 [AC_ARG_WITH(lispdir, 
  [  --with-lispdir          Override the default lisp directory],
  [ lispdir="$withval" 
    AC_MSG_CHECKING([where .elc files should go])
    AC_MSG_RESULT([$lispdir])],
  [
  if test x${lispdir+set} != xset; then
    AC_CACHE_CHECK([where .elc files should go], [am_cv_lispdir], [dnl
      am_cv_lispdir=`$EMACS -batch -q -eval '(while load-path (princ (concat (car load-path) "\n")) (setq load-path (cdr load-path)))' | sed -n -e 's,/$,,' -e '/site-lisp$/ { p; q; }'`
      if test -z "$am_cv_lispdir"; then
	am_cv_lispdir='${datadir}/emacs/site-lisp'
      fi
      am_cv_lispdir="$am_cv_lispdir/navi2ch"
    ])
    lispdir="$am_cv_lispdir"
  fi
 ])
 AC_SUBST(lispdir)])
