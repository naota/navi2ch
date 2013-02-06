;;; navi2ch-mona.el --- Mona Font Utils for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 by Navi2ch Project

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation,
;; Inc.

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; 431 $B$NL>L5$7$5$s(B
;; 874 $B$NL>L5$7$5$s(B
;; UEYAMA Rui <rui314159@users.sourceforge.net>
;; part5 $B%9%l$N(B 26, 45 $B$5$s(B

;; The part of find-face is originated form apel (poe.el).
;; You can get the original apel from <ftp://ftp.m17n.org/pub/mule/apel>.
;; poe.el's Authors:  MORIOKA Tomohiko <tomo@m17n.org>
;;      Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; apel is also licened under GPL.

;; Keywords: 2ch, network

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

;; custom $B$r;H$C$F(B M-x customize-group navi2ch-mona $B$+$i(B
;; $B@_Dj$9$k$H%i%/%A%s!#(B

;;; Code:
(provide 'navi2ch-mona)
(defconst navi2ch-mona-ident
  "$Id$")
(eval-when-compile (require 'cl))
(require 'base64)
(require 'navi2ch-util)

(eval-when-compile
  (navi2ch-defalias-maybe 'set-face-attribute 'ignore)
  (autoload 'x-decompose-font-name "fontset"))

(make-face 'navi2ch-mona-face)
(make-face 'navi2ch-mona12-face)
(make-face 'navi2ch-mona14-face)
(make-face 'navi2ch-mona16-face)

(eval-when-compile
  (navi2ch-defalias-maybe 'query-fontset 'ignore)
  (navi2ch-defalias-maybe 'new-fontset 'ignore))

;; $B%+%9%?%^%$%:JQ?t$N(B defcustom $B$KI,MW$J4X?t(B
(defun navi2ch-mona-create-fontset-from-family-name (family-name height)
  "navi2ch $B$,I,MW$H$9$k%U%)%s%H%;%C%H$r:n$j!"$=$NL>A0$rJV$9!#(B

FAMILY-NAME $B$O(B \"foundry-family\" $B$+$i$J$kJ8;zNs!#(BHEIGHT $B$O(B pixelsize$B!#(B

XEmacs $B$G$OL@<(E*$K%U%)%s%H%;%C%H$r:n$kI,MW$,$J$$$N$G!"(B
$B%U%)%s%H%;%C%HL>$H$7$F0UL#$N$"$kJ8;zNs(B
 \"-<FAMILY-NAME>-medium-r-*--<height>-*-*-*-p-*-*-*\"
$B$rJV$9$@$1!#(B"
  (let ((fontset-name (format "-%s-medium-r-*--%d-*-*-*-p-*-*-*"
                              family-name height)))
    (navi2ch-ifxemacs
	fontset-name
      (let* ((fields (x-decompose-font-name fontset-name))
	     (new-decompose-p (= (length fields) 12))
	     (slant (aref fields (if new-decompose-p 2 3)))
	     (swidth (or (aref fields (if new-decompose-p 3 4)) "normal"))
	     foundry family fontset-templ font-templ fontset)
	(if new-decompose-p
	    (let ((foundry-family (aref fields 0)))
	      (when (string-match "\\([^-]*\\)-\\([^-]*\\)" foundry-family)
		(setq foundry (match-string 1 foundry-family)
		      family (match-string 2 foundry-family))))
	  (setq foundry (aref fields 0)
		family  (aref fields 1)))
	(setq fontset-templ (format
			     "-%s-%s-%%s-%s-%s--%d-*-*-*-p-*-fontset-mona%d"
			     foundry family slant swidth height height)
	     font-templ (progn
			  (string-match "^\\(.*\\)\\(fontset-mona[^-]+\\)$"
					fontset-templ)
			  (concat (match-string 1 fontset-templ) "%s"))
	     fontset (format "-%s-%s-*-*-*--%d-*-*-*-*-*-%s"
			     foundry family height
			     (match-string 2 fontset-templ)))
	(setq fontset-name fontset)
	(dolist (weight '("medium" "bold"))
	  (let ((fontset (format fontset-templ weight))
		(font (format font-templ weight "%s")))
	    (unless (query-fontset fontset)
	      (new-fontset fontset
			   (list (cons 'ascii
				       (format font "iso8859-1"))
				 (cons 'latin-iso8859-1
				       (format font "iso8859-1"))
				 (cons 'katakana-jisx0201
				       (format font "jisx0201.1976-0"))
				 (cons 'latin-jisx0201
				       (format font "jisx0201.1976-0"))
				 (cons 'japanese-jisx0208
				       (format font "jisx0208.1990-0"))))))))
      fontset-name)))

(defun navi2ch-mona-create-face-from-family-name (family-name)
  "VALUE $B$G;XDj$5$l$k%U%)%s%H%;%C%H$K1~$8$F%U%'%$%9$r:n@.$9$k!#(B"
  (dolist (height '(12 14 16))
    (if navi2ch-mona-use-ipa-mona
	(let ((face (intern (format "navi2ch-mona%d-face" height))))
	  (set-face-font face (format "%s:pixelsize=%d"
				      family-name
				      height)))
      (ignore-errors
	(let ((fontset (navi2ch-mona-create-fontset-from-family-name
			family-name height))
	      (face (intern (format "navi2ch-mona%d-face" height))))
	  (set-face-font face fontset))))))

(defun navi2ch-mona-set-font-family-name (symbol value)
  (navi2ch-mona-create-face-from-family-name value)
  (set-default symbol value))

;; Customizable variables.
(defcustom navi2ch-mona-enable nil
  "*non-nil $B$J$i!"%b%J!<%U%)%s%H$r;H$C$F%9%l$rI=<($9$k!#(B"
  :set (lambda (symbol value)
	 (if value
	     (navi2ch-mona-setup)
	   (navi2ch-mona-undo-setup))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-enable-board-list nil
  "*$B%b%J!<%U%)%s%H$GI=<($9$kHD$N%j%9%H!#(B
nil $B$N$H$-$O(B `navi2ch-mona-disable-board-list' $B$G;XDj$7$?HD0J30$N(B
$B$9$Y$F$NHD$G%b%J!<%U%)%s%H$r;HMQ$9$k!#(B"
  :type '(repeat (string :tag "$BHD(B"))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-disable-board-list nil
  "*$B%b%J!<%U%)%s%H$r;H$o$J$$HD$N%j%9%H!#(B
`navi2ch-mona-enable-board-list' $B$h$j$bM%@h$5$l$k!#(B"
  :type '(repeat (string :tag "$BHD(B"))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-enable-article-list nil
  "*$B%b%J!<%U%)%s%H$GI=<($9$k%9%l$N%j%9%H!#(B
`navi2ch-mona-disable-board-list' $B$h$j$bM%@h$5$l$k!#(B"
  :type '(repeat (cons (string :tag "$BHD(B")
		       (string :tag "$B%9%l(B")))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-disable-article-list nil
  "*$B%b%J!<%U%)%s%H$r;H$o$J$$%9%l$N%j%9%H!#(B
`navi2ch-mona-enable-board-list', `navi2ch-mona-enable-article-list'
$B$h$j$bM%@h$5$l$k!#(B"
  :type '(repeat (cons (string :tag "$BHD(B")
		       (string :tag "$B%9%l(B")))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-pack-space-p nil
  "*non-nil $B$J$i!"(BWeb $B%V%i%&%6$N$h$&$K(B2$B$D0J>e$N6uGr$O(B1$B$D$K$^$H$a$FI=<($9$k!#(B"
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-font-family-name "mona-gothic"
  "*$B%b%J!<%U%)%s%H$H$7$F;H$&%U%)%s%H$N(B family $BL>!#(B
XLFD $B$G$$$&(B \`foundry-family\' $B$r;XDj$9$k!#MW$9$k$K(B X $B$G$N(B
$B%U%)%s%HL>$N:G=i$N(B2$B%U%#!<%k%I$r=q$1$P$$$$$C$F$3$C$?!#(B

XEmacs $B$G$O!";XDj$5$l$?(B family $B$KBP$7$F(B pixelsize: 12/14/16
$B$N(B 3$B$D$N%U%)%s%H%;%C%H$r:n$k!#(B

Emacs 21 $B$G$O!"$=$l$K2C$($F(B medium/bold $B$J%U%)%s%H$rJL!9$K:n$k!#(B
$B$?$H$($P0z?t(B \`moga-gothic\' $B$,$o$?$5$l$k$H!"(B

 -mona-gothic-medium-r-*--12-*-*-*-*-*-fontset-mona12
 -mona-gothic-medium-r-*--14-*-*-*-*-*-fontset-mona14
 -mona-gothic-medium-r-*--16-*-*-*-*-*-fontset-mona16
 -mona-gothic-bold-r-*--12-*-*-*-*-*-fontset-mona12
 -mona-gothic-bold-r-*--14-*-*-*-*-*-fontset-mona14
 -mona-gothic-bold-r-*--16-*-*-*-*-*-fontset-mona16

$B$H$$$&(B 6 $B$D$N%U%)%s%H%;%C%H$r:n$k$3$H$K$J$k!#(B

$BJ8;z$N$+$o$j$K%H!<%U$,I=<($5$l$A$c$&$N$O!"$?$V$s%U%)%s%H$,(B
$B8+$D$+$i$J$+$C$?$;$$$J$N$G!"(B\`xlsfonts\' $B$r<B9T$7$F(B

-<$B;XDj$7$?J8;zNs(B>-{medium,bold}-r-*--{12,14,16}-*-*\\
-*-*-*-{iso8859-1,jisx0201.1976-0,jisx0208.(1983|1990)-0}

$B$,$"$k$+$I$&$+3N$+$a$F$M!#(B"
  :type '(choice (const :tag "Mona Font"
			"mona-gothic")
		 (const :tag "MS P Gothic"
			"microsoft-pgothic")
		 (string :tag "family name"))
  :set 'navi2ch-mona-set-font-family-name
  :initialize 'custom-initialize-default
  :group 'navi2ch-mona)

(defconst navi2ch-mona-sample-string
  (concat "$B%5%s%W%k%F%-%9%H%2%C%H%)!*!*(B $B$R$i$,$J!"%+%?%+%J!"(BRoman Alphabet$B!#(B\n"
          (decode-coding-string
           (base64-decode-string
	    (concat
	     "gVCBUIFQgVCBUIHJgVCBUIFQgVCBUIFQgVCBUIFAgUAogUyBTAqBQIFAgUCBQCCB"
	     "yIHIgUCBQIFAgWqBQIFAgUCBQIFAgUAogUyB3CiBTAqBQIFAgbyBad+ERN+BvIHc"
	     "gU2CwoHfgd+B3yiBTIHcOzs7gd+B34HfCoFAgUCBQIFAgUCBQCCBUIFQgUAgKIFM"
	     "gdwogUyB3Ds7CoFAgUCBQIFAgUCBQL3eu9673rCwsLCwryK93rveCg=="))
	   'shift_jis)))

(defcustom navi2ch-mona-face-variable t
  "*$B%G%U%)%k%H$N(B Mona face $B$rA*$V!#(B"
  :type `(radio (const :tag "navi2ch-mona16-face"
                       :sample-face navi2ch-mona16-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona16-face)
                (const :tag "navi2ch-mona14-face"
                       :sample-face navi2ch-mona14-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona14-face)
                (const :tag "navi2ch-mona12-face"
                       :sample-face navi2ch-mona12-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona12-face)
                (const :tag "$B%G%U%)%k%H$N%U%)%s%H$HF1$8%5%$%:$N(B face $B$r<+F0A*Br$9$k(B"
                       t))
  :set (function (lambda (symbol value)
                   (set-default symbol value)
                   (navi2ch-mona-set-mona-face)))
  :initialize 'custom-initialize-default
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-on-message-mode nil
  "*non-nil $B$N>l9g!"%l%9$r=q$/;~$K$b%b%J!<%U%)%s%H$r;H$&!#(B"
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-use-ipa-mona nil
  "*non-nil $B$J$i!"(BIPA$B%b%J!<%U%)%s%H$r;H$C$F%9%l$rI=<($9$k!#(B"
  :set (lambda (symbol value)
	 (navi2ch-mona-setup)
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-ipa-mona-font-family-name "IPA $B%b%J!<(B P$B%4%7%C%/(B"
  "*$B%b%J!<%U%)%s%H$H$7$F;H$&%U%)%s%H$N(B family $BL>!#(B

$B%(%i!<$,$G$?$i!"(B (pp (font-family-list)) $B$rI>2A$7$F(B IPA$B%b%J!<$C$](B
$B$$$N$rC5$7$F$M!#(B"
  :type '(choice (const :tag "IPA Mona Font"
			"IPA $B%b%J!<(B P$B%4%7%C%/(B")
		 (string :tag "family name"))
  :set 'navi2ch-mona-set-font-family-name
  :initialize 'custom-initialize-default
  :group 'navi2ch-mona)

;; defun find-face for GNU Emacs
;; the code is originated from apel.
(defun navi2ch-find-face-subr (face-or-name)
  "Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned."
  (car (memq face-or-name (face-list))))

(eval-and-compile 
  (defalias 'navi2ch-find-face
    (if (fboundp 'find-face)
	#'find-face
      #'navi2ch-find-face-subr)))

(defun navi2ch-mona-char-height ()
  (navi2ch-ifxemacs
      (font-height (face-font 'default))
    (frame-char-height)))

(defun navi2ch-set-face-parent (face parent)
  (navi2ch-ifxemacs
      (set-face-parent face parent)
    (set-face-attribute face nil :inherit parent)))

;; functions
(defun navi2ch-mona-set-mona-face ()
  (let ((parent navi2ch-mona-face-variable))
    (when (eq t parent)
      (let* ((height (navi2ch-mona-char-height))
	     (face-name (if height
			    (format "navi2ch-mona%d-face" height)
			  "navi2ch-mona16-face")))
	(setq parent (intern face-name))))
    (when (navi2ch-find-face parent)
      (navi2ch-set-face-parent 'navi2ch-mona-face parent))))

(defun navi2ch-mona-put-face ()
  "face $B$,FC$K;XDj$5$l$F$$$J$$ItJ,$r(B mona-face $B$K$9$k!#(B
`navi2ch-article-face' $B$NItJ,$b(B mona-face $B$K$9$k!#(B"
  (save-excursion
    (goto-char (point-min))
    (let (p face)
      (while (not (eobp))
	(setq p (next-single-property-change (point)
					     'face nil (point-max)))
	(setq face (get-text-property (point) 'face))
	(if (or (null face)
		(eq face 'navi2ch-article-face))
	    (put-text-property (point) p 'face 'navi2ch-mona-face))
	(goto-char p)))))

(defun navi2ch-mona-pack-space ()
  "$BO"B3$9$k(B2$B$D0J>e$N6uGr$r(B1$B$D$K$^$H$a$k!#(B"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ +" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t)
      (replace-match " "))))

(defvar navi2ch-mona-enable-list nil
  "$B%b%J!<%U%)%s%H$rM-8z$K$9$k%j%9%H!#(B
nil $B$N>l9g$O%G%U%)%k%H$GM-8z$K$J$k!#(B
$B%(%l%a%s%H$,4X?t(B: $B8F$S=P$7$?7k2L$,(B non-nil $B$J$iM-8z(B
$B%(%l%a%s%H$,J8;zNs(B: $B$=$NHD$GM-8z(B
$B%(%l%a%s%H$,(B cons $B%Z%"(B: car $B$rHD!"(Bcdr $B$r%9%l$b$7$/$O%9%l$N%j%9%H$H$7!"(B
$B$=$NHD$N$=$N%9%l$GM-8z(B

$B3F%(%l%a%s%H$O=g$KI>2A$5$l!"M-8z$H$_$J$5$l$?;~E@$GI>2A$r=*N;$9$k!#(B")

(defvar navi2ch-mona-disable-list nil
  "$B%b%J!<%U%)%s%H$rL58z$K$9$k%j%9%H!#(B
`navi2ch-mona-enable-list' $B$h$j$bM%@h$5$l$k!#(B
$B%(%l%a%s%H$,4X?t(B: $B8F$S=P$7$?7k2L$,(B non-nil $B$J$iL58z(B
$B%(%l%a%s%H$,J8;zNs(B: $B$=$NHD$GL58z(B
$B%(%l%a%s%H$,(B cons $B%Z%"(B: car $B$rHD!"(Bcdr $B$r%9%l$b$7$/$O%9%l$N%j%9%H$H$7!"(B
$B$=$NHD$N$=$N%9%l$GL58z(B

$B3F%(%l%a%s%H$O=g$KI>2A$5$l!"M-8z$H$_$J$5$l$?;~E@$GI>2A$r=*N;$9$k!#(B")

(defun navi2ch-mona-match-p (list board-id article-id)
  (dolist (elt list)
    (when (cond ((functionp elt)
		 (funcall elt))
		((stringp elt)
		 (string= elt board-id))
		((and (consp elt)
		      (string= (car elt) board-id))
		 (if (stringp (cdr elt))
		     (string= (cdr elt) article-id)
		   (member article-id (cdr elt)))))
      (return t))))

(eval-when-compile
  (defvar navi2ch-popup-article-current-board)
  (defvar navi2ch-article-current-board)
  (defvar navi2ch-popup-article-current-article)
  (defvar navi2ch-article-current-article))

(defun navi2ch-mona-arrange-message ()
  "$B%b%J!<%U%)%s%H$r;H$&HD$J$i$=$N$?$a$N4X?t$r8F$V!#(B"
  (let ((id (cdr (assq 'id (if (eq major-mode 'navi2ch-popup-article-mode)
			       navi2ch-popup-article-current-board
			     navi2ch-article-current-board))))
	(artid (cdr (assq 'artid (if (eq major-mode 'navi2ch-popup-article-mode)
				     navi2ch-popup-article-current-article
				   navi2ch-article-current-article)))))
    (when (cond ((navi2ch-mona-match-p navi2ch-mona-disable-list
				       id artid)
		 nil)
		((navi2ch-mona-match-p navi2ch-mona-enable-list
				       id artid)
		 t)
		((member (cons id artid) navi2ch-mona-disable-article-list)
		 nil)
		((member (cons id artid) navi2ch-mona-enable-article-list)
		 t)
		((member id navi2ch-mona-disable-board-list)
		 nil)
		((member id navi2ch-mona-enable-board-list)
		 t)
		((or navi2ch-mona-enable-article-list
		     navi2ch-mona-enable-board-list
		     navi2ch-mona-enable-list)
		 nil)
		(t t))
      (navi2ch-mona-put-face))
    (when navi2ch-mona-pack-space-p
      (navi2ch-mona-pack-space))))

(defun navi2ch-mona-message-mode-hook ()
  (if navi2ch-mona-on-message-mode
      (navi2ch-ifxemacs
	  (let ((extent (make-extent (point-min) (point-max))))
	    (set-extent-properties extent
				   '(face navi2ch-mona-face
					  start-closed t end-closed t)))
	(let ((overlay (make-overlay (point-min) (point-max) nil nil t)))
	  (overlay-put overlay 'face 'navi2ch-mona-face)))))

(defun navi2ch-mona-setup ()
  "*$B%b%J!<%U%)%s%H$r;H$&$?$a$N%U%C%/$rDI2C$9$k!#(B"
  (when (and (or (eq window-system 'x) (eq window-system 'w32)
                 (eq window-system 'mac))
	     (or navi2ch-on-emacs21 navi2ch-on-xemacs))
    (navi2ch-mona-create-face-from-family-name 
     (if navi2ch-mona-use-ipa-mona
	 navi2ch-mona-ipa-mona-font-family-name
       navi2ch-mona-font-family-name))
    (navi2ch-mona-set-mona-face)	; $B2?2s8F$s$G$bBg>fIW(B
    (add-hook 'navi2ch-article-arrange-message-hook
	      'navi2ch-mona-arrange-message)
    (add-hook 'navi2ch-message-mode-hook
	      'navi2ch-mona-message-mode-hook)
    (run-hooks 'navi2ch-mona-setup-hook)))

(defun navi2ch-mona-undo-setup ()
  (run-hooks 'navi2ch-mona-undo-setup-hook)
  (remove-hook 'navi2ch-article-arrange-message-hook
	       'navi2ch-mona-arrange-message)
  (remove-hook 'navi2ch-message-mode-hook
	       'navi2ch-mona-message-mode-hook))

(run-hooks 'navi2ch-mona-load-hook)
;;; navi2ch-mona.el ends here
