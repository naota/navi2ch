;;; izonmoji-mode.el --- Visualize Windows and Macintosh izonmoji

;; Copyright (C) 2002 by Navi2ch Project

;; Author: SAITO Takuya <tabmore@users.sourceforge.net>
;; Keywords: 2ch, charset

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $BA4$F$N5!<o0MB8J8;z$rI=<($G$-$k$o$1$G$O$"$j$^$;$s$,!"(B
;; $B:GDc8BI,MW$J$b$N$OB7$($?$D$b$j$G$9!#(B

;; commands:
;;   izonmoji-mode	$B5!<o0MB8J8;zI=<($r%H%0%k(B
;;   izonmoji-mode-on	$B5!<o0MB8J8;z$rI=<((B
;;   izonmoji-mode-off	$B5!<o0MB8J8;zI=<($r$d$a$k(B

;; GNU Emacs 20 $B$G$O!"(BMule-UCS$B$,I,MW$G$9!#(B
;; $B$3$N%U%!%$%k$rFI$_9~$`A0$K(B (require 'jisx0213) $B$7$F$/$@$5$$!#(B

;; GNU Emacs 20,21$B$G$O!"(Bbuffer-display-table$B$K$h$C$FI=<($rCV$-BX$($?(B
;; Non-ASCII$B$JJ8;z$N(Bchar-width$B$,$*$+$7$/$J$j$^$9!#(B
;; $BJQ49A08e$G(Bchar-width$B$,JQ$o$i$J$$>l9g$O!"(B
;;  (defadvice char-width (around display-table-hack activate)
;;    (let ((buffer-display-table nil))
;;      ad-do-it))
;; $B$G$4$^$+$;$^$9!#(B string-width$B$bF1MM$G$9!#(B

;; XEmacs$B$G$O(Binit-file$B$K0J2<$N$h$&$K=q$$$F$/$@$5$$!#(B
;; (make-charset
;;  'japanese-jisx0213-1
;;  "JIS X 0213:2000 Plain 1"
;;  '(registry "jisx0213\\(\\.2000\\)-1"
;;             dimension 2 chars 94 final ?O graphic 0))
;; (make-charset
;;  'japanese-jisx0213-2
;;  "JIS X 0213:2000 Plain 2"
;;  '(registry "jisx0213\\(\\.2000\\)-2"
;;             dimension 2 chars 94 final ?P graphic 0))

;; $B@_DjNc(B

;; [$B6&DL(B] ~/.emacs $B$X(B
;;  (require 'izonmoji-mode)

;; [navi2ch] ~/.navi2ch/init.el $B$X(B
;;  (add-hook 'navi2ch-bm-mode-hook      'izonmoji-mode-on)
;;  (add-hook 'navi2ch-article-mode-hook 'izonmoji-mode-on)

;; [Mew] ~/.mew.el $B$X(B
;;  (add-hook 'mew-message-mode-hook 'izonmoji-mode-on)

;; [emacs-w3m] ~/.emacs-w3m.el $B$X(B
;;  (add-hook 'w3m-mode-hook 'izonmoji-mode-on)

;;; Bugs:

;;  1. display-table$B$r$$$8$k(B
;;  2. M-x izonmoji-mode-on
;;  3. 1$B$NJQ99$r85$KLa$9(B
;;  4. M-x izonmoji-mode-off
;;  $B$9$k$H85$K$b$I$i$J$$!#(B
;;  C-u M-x izonmoji-mode-off $B$7$F(Bdisplay-table$B$X$NA4$F$NJQ99$r<h$j>C$9(B
;;  $B$3$H$O$G$-$^$9!#(B

;; XEmacs
;;  izonmoji-win-face, izonmoji-mac-face $B$NE,MQJ}K!$rCN$i$J$$!#(B

;;; Code:

(defvar izonmoji-priority-list '(win mac)
  "*$BI=<($NM%@h=g0L!#(B
'(win mac) $B$J$i!"(BWindows$B$N5!<o0MB8J8;z$rM%@h$7$D$D!"(BMac$B$NJ8;z$bI=<(!#(B
'(win) $B$J$i!"(BWindows$B$N5!<o0MB8J8;z$N$_I=<(!#(B")

(defvar izonmoji-win-replace-list
  '("$(O-!(B" "$(O-"(B" "$(O-#(B" "$(O-$(B" "$(O-%(B" "$(O-&(B" "$(O-'(B" "$(O-((B" "$(O-)(B" "$(O-*(B"
    "$(O-+(B" "$(O-,(B" "$(O--(B" "$(O-.(B" "$(O-/(B" "$(O-0(B" "$(O-1(B" "$(O-2(B" "$(O-3(B" "$(O-4(B"
    "$(O-5(B" "$(O-6(B" "$(O-7(B" "$(O-8(B" "$(O-9(B" "$(O-:(B" "$(O-;(B" "$(O-<(B" "$(O-=(B" "$(O->(B"
    "$(O,5(B" "$(O,6(B" "$(O,7(B" "$(O,8(B" "$(O,9(B" "$(O,:(B" "$(O,;(B" "$(O,<(B" "$(O,=(B" "$(O,>(B"
    "$(O-@(B" "$(O-A(B" "$(O-B(B" "$(O-C(B" "$(O-D(B" "$(O-E(B" "$(O-F(B" "$(O-G(B" "$(O-H(B" "$(O-I(B" "$(O-J(B" "$(O-K(B"
    "$(O-L(B" "$(O-M(B" "$(O-N(B" "$(O-O(B" "$(O-P(B" "$(O-Q(B" "$(O-R(B" "$(O-S(B" "$(O-T(B" "$(O-U(B" "$(O-V(B"
    "$(O-_(B" "$(O-`(B" "$(O-a(B" "$(O-b(B" "$(O-c(B" "$(O-d(B" "$(O-e(B" "$(O-f(B" "$(O-g(B" "$(O-h(B" "$(O-i(B"
    "$(O-j(B" "$(O-k(B" "$(O-l(B" "$(O-m(B" "$(O-n(B" "$(O-o(B"
    "$B"b(B" "$B"a(B" "$B"i(B" "$(O-s(B" "$B&2(B" "$B"e(B" "$B"](B" "$B"\(B" "$(O-x(B" "$(O-y(B" "$B"h(B" "$B"A(B" "$B"@(B")
  "*Strings used to visualize Windows izonmoji.")

(defvar izonmoji-mac-replace-list
  '("$(O-!(B" "$(O-"(B" "$(O-#(B" "$(O-$(B" "$(O-%(B" "$(O-&(B" "$(O-'(B" "$(O-((B" "$(O-)(B" "$(O-*(B"
    "$(O-+(B" "$(O-,(B" "$(O--(B" "$(O-.(B" "$(O-/(B" "$(O-0(B" "$(O-1(B" "$(O-2(B" "$(O-3(B" "$(O-4(B"
    "(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)"
    "(11)" "(12)" "(13)" "(14)" "(15)" "(16)" "(17)" "(18)" "(19)" "(20)"
    "$(O,!(B" "$(O,"(B" "$(O,#(B" "$(O,$(B" "$(O,%(B" "$(O,&(B" "$(O,'(B" "$(O,((B" "$(O,)(B"
    "1." "2." "3." "4." "5." "6." "7." "8." "9." "0."
    "$(O-5(B" "$(O-6(B" "$(O-7(B" "$(O-8(B" "$(O-9(B" "$(O-:(B" "$(O-;(B" "$(O-<(B" "$(O-=(B" "$(O->(B"
    "$(O-?(B" "$(O-W(B" "X$(O-7(B" "X$(O-8(B" "XV"
    "$(O,5(B" "$(O,6(B" "$(O,7(B" "$(O,8(B" "$(O,9(B" "$(O,:(B" "$(O,;(B" "$(O,<(B" "$(O,=(B" "$(O,>(B"
    "$(O,?(B" "$(O,@(B" "x$(O,7(B" "x$(O,8(B" "xv"
    "(a)" "(b)" "(c)" "(d)" "(e)" "(f)" "(g)" "(h)" "(i)" "(j)" "(k)"
    "(l)" "(m)" "(n)" "(o)" "(p)" "(q)" "(r)" "(s)" "(t)" "(u)" "(v)"
    "(w)" "(x)" "(y)" "(z)"
    "$(O-P(B" "mm$(O),(B" "$(O-Q(B" "$(O-Q),(B" "$(O-Q)-(B" "m" "$(O-V(B" "m$(O)-(B" "$(O-R(B" "k$(O-V(B"
    "$(O-S(B" "g" "$(O-T(B" "$(O-U(B" "m$(O#_(B" "d$(O#_(B" "$(O#_(B" "k$(O#_(B"
    "ms" "$B&L(Bs" "ns" "ps" "$(O!k(BF" "mb" "HP" "Hz" "KB" "MB" "GB" "TB"
    "$(O-b(B" "$(O-c(B" "$(O-d(B" "FAX"
    "$(O&9(B" "$(O&?(B" "$(O&=(B" "$(O&;(B" "$(O&:(B" "$(O&@(B" "$(O&>(B" "$(O&<(B"
    "$(O&f(B" "$(O&g(B" "JIS"			;JIS$B%^!<%/(B
    "$B"*(B" "$B"+(B" "$B",(B" "$B"-(B"			;$BA4A30c$&(B
    "$(O#)(B" "$(O#)(B" "$B","-(B" "$B"-",(B"		;$B$A$g$C$H0c$&(B
    "$(O#*(B" "$(O#+(B" "$(O#,(B" "$(O#-(B" "$(O#*(B" "$(O#+(B" "$(O#,(B" "$(O#-(B" ;$B8eH>$OEI$jDY$7(B
    "($BF|(B)" "($B7n(B)" "($B2P(B)" "($B?e(B)" "($BLZ(B)" "($B6b(B)" "($BEZ(B)"
    "($B:W(B)" "($B=K(B)" "($B<+(B)" "($B;j(B)" "$(O-l(B" "($B8F(B)" "$(O-j(B" "($B;q(B)" "($BL>(B)" 
    "$(O-k(B" "($B3X(B)" "($B:b(B)" "($B<R(B)" "($BFC(B)" "($B4F(B)" "($B4k(B)" "($B6((B)" "($BO+(B)"
    "($BBg(B)" "($B>.(B)" "$(O-e(B" "$(O-f(B" "$(O-g(B" "$(O-h(B" "$(O-i(B"
    "($B0e(B)" "($B:b(B)" "($BM%(B)" "($BO+(B)" "($B0u(B)" "($B95(B)" "($BHk(B)" ;$BK\Ev$O4]IU$-(B
    "$(O-@(B" "$(O-B(B" "$(O-C(B" "$(O-A(B" "$(O-A-C(B" "(I2]A(B" "(IL(0D(B" "(IT0D^(B" "$(O-F(B" "$(O-G(B"
    "$(O-D(B" "$(O-A-D(B" "$(O-E(B" "$(O-H(B" "$(O-N(B" "(IMYB(B" "$(O-I(B" "$(O-J(B" "(IN0](B" "$(O-L(B" "$(O-K(B" "$(O-O(B" "$(O-M(B"
    "(I1J_0D(B" "(I:0N_(B" "(IJ2B(B" "(IK^Y(B" "(IO]<.](B"
    "$(O-m(B" "$(O-n(B" "$(O-o(B" "$(O-_(B"
    "$B3t<02q<R(B" "$BM-8B2q<R(B" "$B:bCDK!?M(B"	;"$(O-j(B" "$(O-k(B"
    "$(O-s(B" "$(O-x(B" "$(O-y(B"
    "$(O-`(B" "$(O-a(B"
    "$(O$t(B" "$(O'r(B" "$(O's(B" "$(O't(B" "$(O'u(B")
  "*Strings used to visualize Macintosh izonmoji.")

(defvar izonmoji-win-face 'izonmoji-win-face
  "*Symbol face used to visualize Windows izonmoji.")

(defvar izonmoji-mac-face 'izonmoji-mac-face
  "*Symbol face used to visualize Macintosh izonmoji.")

(defface izonmoji-win-face
  '((((class color) (type tty)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "Aquamarine4"))
    (((class color) (background dark))  (:foreground "Aquamarine3"))
    (t (:underline t)))
  "Face used to visualize Windows izonmoji.")

(defface izonmoji-mac-face
  '((((class color) (type tty)) (:foreground "magenta"))
    (((class color) (background light)) (:foreground "pink4"))
    (((class color) (background dark))  (:foreground "pink3"))
    (t (:underline t)))
  "Face used to visualize Macintosh izonmoji.")

(defun izonmoji-make-char-list (i js je)
  (let ((j js) list)
    (while (<= j je)
      (setq list (cons (make-char 'japanese-jisx0208 i j) list))
      (setq j (1+ j)))
    (nreverse list)))

;; Windows$B$N4]IU$-(B1$B$O!"(B
;; (split-char (decode-sjis-char (hexl-hex-string-to-integer "8740")))
(defvar izonmoji-win-chars-list
  (append
   (izonmoji-make-char-list  45  33  52) ;$B4]IU$-?t;z(B
   (izonmoji-make-char-list  45  53  62) ;$B%m!<%^?t;z(B($BBgJ8;z(B)
   (izonmoji-make-char-list 124 113 122) ;$B%m!<%^?t;z(B($B>.J8;z(B)
   (izonmoji-make-char-list  45  64  86) ;$BC10L(B
   (izonmoji-make-char-list  45  95 111) ;$B859f$J$I(B
   (izonmoji-make-char-list  45 112 124) ;$B?t3X5-9f(B
   )
  "*Windows izonmoji.")

(defvar izonmoji-mac-chars-list
  (append
   (izonmoji-make-char-list  41  33  52) ;$B4]IU$-?t;z(B
   (izonmoji-make-char-list  41  63  82) ;$B3g8LIU$-?t;z(B
   (izonmoji-make-char-list  41  93 101) ;$B9u4]IU$-?t;z(B
   (izonmoji-make-char-list  41 114 123) ;$BE@IU$-?t;z(B
   (izonmoji-make-char-list  42  33  47) ;$B%m!<%^?t;z(B($BBgJ8;z(B)
   (izonmoji-make-char-list  42  53  67) ;$B%m!<%^?t;z(B($B>.J8;z(B)
   (izonmoji-make-char-list  42  93 118) ;$B3g8LIU$-%"%k%U%!%Y%C%H(B
   (izonmoji-make-char-list  43  33  62) ;$BC10L(B
   (izonmoji-make-char-list  43 123 126) ;$BN,9f(B
   (izonmoji-make-char-list  44  33  40) ;$B%H%i%s%W(B
   (izonmoji-make-char-list  44  53  55) ;$BM9JX(B
   (izonmoji-make-char-list  44  73  88) ;$BLp0u(B
   (izonmoji-make-char-list  45  33  57) ;$BMKF|$J$I(B
   (izonmoji-make-char-list  45 113 126) ;$B4]IU$-J8;z(B
   (izonmoji-make-char-list  46  33  55) ;$B%+%?%+%JC10L(B
   (izonmoji-make-char-list  46  63  67) ;$B%"%Q!<%H(B
   (izonmoji-make-char-list  46 103 106) ;$B859f(B
   (izonmoji-make-char-list  46 124 126) ;$B3t<02q<R(B
   (izonmoji-make-char-list  47  33  35) ;$B?t3X5-9f(B
   (izonmoji-make-char-list  47  53  54) ;""
   (izonmoji-make-char-list  47  73  73) ;$B$&!+(B
   (izonmoji-make-char-list  47  75  78) ;$B%o!+(B
   )
  "*Macintosh izonmoji.")

(defvar izonmoji-mode-hook nil
  "*Hook run after izonmoji visualization.")

;; Internal variables

(defvar izonmoji-mode nil)
(make-variable-buffer-local 'izonmoji-mode)

(defvar izonmoji-backuped-display-table nil)
(make-variable-buffer-local 'izonmoji-backuped-display-table)

(defun izonmoji-mode (&optional arg)
  "Toggle izonmoji visualization.
If ARG is nil, toggle izonmoji visualization.
If ARG is a number and is greater than zero, turn on visualization; otherwise,
turn off visualization."
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(not izonmoji-mode))
      (izonmoji-mode-on)
    (izonmoji-mode-off)))

(defun izonmoji-mode-on (&optional reverse win-face mac-face)
  "Turn on visualization."
  (interactive "P")
  (let ((priority (reverse izonmoji-priority-list))
	from to table face-bits)
    (when reverse
      (setq priority (nreverse priority)))
    (unless izonmoji-mode
      (cond
       ((featurep 'xemacs)
	(let* ((ctable (specifier-instance current-display-table))
	       (len (- (1+ (apply 'max (append izonmoji-win-chars-list
					       izonmoji-mac-chars-list)))
		       (length ctable))))
	  (setq izonmoji-backuped-display-table (copy-sequence ctable))
	  (if (> len 0)
	      (setq table (vconcat ctable (make-vector len nil)))
	    (setq table ctable)))
	(while priority
	  (cond
	   ((eq (car priority) 'win)
	    (setq from izonmoji-win-chars-list
		  to   izonmoji-win-replace-list))
	   ((eq (car priority) 'mac)
	    (setq from izonmoji-mac-chars-list
		  to   izonmoji-mac-replace-list)))
	  (setq priority (cdr priority))
	  (while (and from to)
	    (aset table (car from) (car to))
	    (setq from (cdr from) to (cdr to))))
	(set-specifier current-display-table table (current-buffer)))
       (t				;GNU Emacs
	(setq izonmoji-backuped-display-table
	      (copy-sequence buffer-display-table)
	      table (or buffer-display-table (make-display-table)))
	(while priority
	  (cond
	   ((eq (car priority) 'win)
	    (setq from izonmoji-win-chars-list
		  to   izonmoji-win-replace-list
		  face-bits (ash (face-id (or win-face izonmoji-win-face))
				 19)))
	   ((eq (car priority) 'mac)
	    (setq from izonmoji-mac-chars-list
		  to   izonmoji-mac-replace-list
		  face-bits (ash (face-id (or mac-face izonmoji-mac-face))
				 19))))
	  (setq priority (cdr priority))
	  (while (and from to)
	    (aset table (car from)
		  (apply 'vector (mapcar
				  (lambda (ch) (logior ch face-bits))
				  (car to))))
	    (setq from (cdr from) to (cdr to))))
	(setq buffer-display-table table))))
    (setq izonmoji-mode t)
    (run-hooks 'izonmoji-mode-hook)))

(defun izonmoji-mode-off (&optional initialize)
  "Turn on visualization."
  (interactive "P")
  (when initialize
    (setq izonmoji-mode t
	  izonmoji-backuped-display-table (make-display-table)))
  (when izonmoji-mode
    (if (featurep 'xemacs)
	(set-specifier current-display-table
		       izonmoji-backuped-display-table (current-buffer))
      (setq buffer-display-table izonmoji-backuped-display-table))
    (setq izonmoji-mode nil)))

(add-to-list 'minor-mode-alist '(izonmoji-mode " Iz"))

(provide 'izonmoji-mode)

;;; izonmoji-mode.el ends here
