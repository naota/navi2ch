;;; navi2ch-p2.el --- p2 frontend for navi2ch

;; Copyright (C) 2008, 2009 by Navi2ch Project

;; Authors: Naohiro Aota <naota@namazu.org>
;;          MIZUNUMA Yuto <mizmiz@users.sourceforge.net>
;; Keywords: network 2ch

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

;; $B35MW(B
;; navi2ch $B$r(B p2 $B$N%U%m%s%H%(%s%I$K$7$^$9!#(B
;; $B$H$$$C$F$b:#$O$^$@(B bookmark $B$NF14|$@$1!#(B
;; $B$^$@$^$@<B83CJ3,$J$N$GCm0U!#(B
;;
;; $B;H$$$+$?(B
;; $B%Q%9$rDL$7$F!"(B
;; (load "navi2ch-p2")
;; 
;; M-x navi2ch-p2-sync-global-bookmark-to-p2
;; 
;; $B;XDj$5$l$?(B global-bookmark $B$KEPO?$5$l$F$$$F!"(Bp2 $B$N$*5$$K$$$j$KEPO?$5(B
;; $B$l$F$$$J$$%9%l$r(Bp2 $B$N$*5$$K$$$j$KEPO?$7!"(B $B;XDj$5$l$?(B
;; global-bookmark $B$KEPO?$5$l$F$*$i$:!"(B p2 $B$N$*5$$K$$$j$KEPO?$5$l$F$$$k(B
;; $B%9%l$r?R$M$F$+$i(B p2 $B$N$*5$$K$$$j$+$i:o=|$7$^$9!#(B
;; 
;; M-x navi2ch-p2-sync-global-bookmark-from-p2
;; 
;; p2 $B$N$*5$$K$$$j$K$KEPO?$5$l$F$$$F!";XDj$5$l$?(B global-bookmark $B$KEPO?(B
;; $B$5$l$F$$$J$$%9%l$r;XDj$5$l$?(B global-bookmark $B$KEPO?$7!"(B p2 $B$N$*5$$K(B
;; $B$$$j$KEPO?$5$l$F$*$i$:!"(B $B;XDj$5$l$?(B global-bookmark $B$KEPO?$5$l$F$$$k(B
;; $B%9%l$r?R$M$F$+$i(B $B;XDj$5$l$?(B global-bookmark $B$+$i:o=|$7$^$9!#(B
;;
;; M-x navi2ch-p2-export
;; 
;; navi2ch $B$N$*5$$K$$$j!"MzNr!"$"$\!<$s%o!<%I!"(Bdat$B!"%9%l>pJs$r;XDj$5$l(B
;; $B$?%U%)%k%@$K(B rep2 $B7A<0$G(Bp export $B$7$^$9!#(B
;;
;; $B@_Dj(B
;; navi2ch-p2-mail-address $B$K%a!<%k%"%I%l%9(B
;; navi2ch-p2-password $B$K%Q%9%o!<%I$r@_Dj$7$F$*$/$HJXMx$G$7$g$&!#(B
;; 
;; $B%(%/%9%]!<%H$r;H$&?M$O!"(B navi2ch-p2-export-bookmark $B$K%(%/%9%]!<%H$7(B
;; $B$?$$(B global-bookmark $B$N(B id $B$r@_Dj$7$F$*$/$H$$$$$G$7$g$&!#(B
;; 
;; dat $B$N%(%/%9%]!<%H$K;H$o$l$k4X?t$,(B navi2ch-p2-dat-copy-function $B$K@_(B
;; $BDj$5$l$F$$$^$9!#(B $B%G%U%)%k%H$O(B add-name-to-file ($B%O!<%I%j%s%/(B)$B$K$J$C(B
;; $B$F$$$^$9!#(B $B%j%s%/$,;HMQIT2DG=$J$i$P(B copy-file $B!"%j%s%/$,;HMQ2DG=$@$,(B
;; $B%Q!<%F%#%7%g%s$,JL$G$"$k?M$O(B make-symbolic-link ($B%7%s%\%j%C%/%j%s%/(B)$B$K(B
;; $B$9$k$H$$$$$G$7$g$&!#(B

;; $B5!G=DI2C(B

;; 2ch$B8x<0(BP2(http://p2.2ch.net)$B$r7PM3$7$F=q$-9~$_$,=PMh$k$h$&$K$9$k5!G=DI2C$G$9!#(B
;; $B<g$JL\E*$O%W%m%P%$%@$N%"%/6X$K4,$-9~$^$l$?:]$N2sHr:v$G$9!#(B
;; P2$B$d$i%b%j%?%]$K4X$7$F$ON.F0E*$J$N$G!"$=$NETEY!"3F!9D4$Y$F$/$@$5$$!#(B
;; p2writer$B$d(Bp2proxy$B$"$?$j$H$d$C$F$k$3$H$OF1$8$J$N$G$=$NJU$N%9%l$G!#(B
;; 1000$B%b%j%?%]>CHq$G=q$-9~$_5v2D$rF@$k$N$O%V%i%&%67PM3$G(B

;; $B%9%lFI$_9~$_$O(Bp2$B7PM3$7$F$^$;$s!#=q$-9~$_$N$_$G$9!#(B

;; $B4JC1$JFbIt;EMM$N@bL@$H$7$F$O(B
;; http://p2.2ch.net/p2 $B$K%m%0%$%s$7$F(Bcookie(PS$B!"(Bcsid)$B<hF@(B
;; $B!!"-(B
;; $B$=$N(Bcookie$B$r;H$C$F(Bhttp://p2.2ch.net/p2/menu.php$B$r<hF@$9$k$H!"(B
;; $B%I%-%e%a%s%H$KKd$a9~$^$l$F$k(Bcsrfid$BJQ?t$NCM$r<hF@2D(B(XSS$BBP:v(B?)$B!#(B
;; $B!!"-(B
;; $B%l%9=q$-9~$_$N%9%/%j%W%H$G$"$k!"(Bhttp://p2.2ch.net/p2/post.php
;; $B$K(Bcookie$B$r(Bcookie$BEO$7!"(Bcsrfid$B$r(BPOST$B$N%Q%i%a!<%?!<EO$7$G%l%9$,2DG=$K$J$k!#(B

;; tepo=don$B$H$$$&%Q%i%a!<%?!<$G=q$-9~$_8"8B$r%"%/%F%#%Y!<%7%g%s(B(?)$B$7$F$d$k$N$O!"(B2ch$BK\BN$HF1$8$C$]$$!#(B
;; ($B$h$/$"$k!"3NG'2hLL$G(B2$BEY=q$-9~$`E[$G$9!K(B

;; $B$"$/$^$G8D?ME*$KD4$Y$?HO0O$J$N$G!"4V0c$C$F$?$j!"5v2D$5$l$F$J$$%m%8%C%/$r;H$C$F$k2DG=@-$b$"$j$^$9!#(B
;; cookie$B$d$i(Bcsrfid$B$N(B"$B@8B84|4V(B"$B$K4X$7$F$O2x$7$$ItJ,$b$"$k$N$G!"MW:F%m%0%$%s!":F<hF@$,$=$NETEYI,MW$+$b!#(B
;; $B$3$NJU$N%j%H%i%$%k!<%W$O$+$J$j2x$7$$(B

;; $B@_Dj$9$kJQ?t$O0J2<$N(B3$B$D$/$i$$(B
;; (setq navi2ch-p2-use-p2 t) ;;p2$B$r;H$C$F=q$-9~$_$9$k$+$I$&$+!#(B
;; (setq navi2ch-p2-mail-address "sample@hoge.hoge") ;; p2$B$N%a!<%k%"%I%l%9(B(ID)
;; (setq navi2ch-p2-password "password") ;; p2$B$N%Q%9%o!<%I(B

;;; Code:

(provide 'navi2ch-p2)

(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(defvar navi2ch-p2-func-alist
  '((bbs-p		. navi2ch-p2-p)
    (article-update 	. navi2ch-2ch-article-update)
    (send-message   	. navi2ch-p2-send-message)
    (send-success-p	. navi2ch-p2-send-message-success-p)
    (extract-post	. navi2ch-2ch-extract-post)))

(defvar navi2ch-p2-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(defconst navi2ch-p2-coding-system 'shift_jis)

(navi2ch-multibbs-regist 'p2
			 navi2ch-p2-func-alist
			 navi2ch-p2-variable-alist)

(defvar navi2ch-p2-madakana-url
    "http://qb7.2ch.net/_403/madakana.cgi")

;;-------------

(defvar navi2ch-p2-use-p2 nil	; $BJQ?tL>$OMW8!F$!#(B
  "*p2$B$r;H$C$F=q$-9~$_$9$k$+$I$&$+!#(B")
(defvar navi2ch-p2-mail-address ""
  "*p2$B$N%a!<%k%"%I%l%9(B(ID)$B!#(B")
(defvar navi2ch-p2-password ""
  "*p2$B$N%Q%9%o!<%I!#(B")

(defvar navi2ch-p2-domain "p2.2ch.net")

(defvar navi2ch-p2-login-url (concat "http://" navi2ch-p2-domain "/p2/")
  "*p2$B$N%m%0%$%s(BURL$B!#(B")

(defconst navi2ch-p2-cookie-names '("PS" "cid"))
(defconst navi2ch-p2-cookie-domain navi2ch-p2-domain)
(defconst navi2ch-p2-cookie-path '/p2)

(defvar navi2ch-p2-login-flag nil)
(defvar navi2ch-p2-csrfid nil)

(defvar navi2ch-p2-board nil)
(defvar navi2ch-p2-board-regexp nil)
(defvar navi2ch-p2-all-board nil)

(defun navi2ch-p2-p (uri)
  "p2$B7PM3$G=q$-9~$`$J$i(Bnon-nil$B$rJV$9!#(B"
  (when (and navi2ch-p2-use-p2
	     (string-match "http://\\([^/]+\\)/\\([^/]+\\)" uri))
    (let ((board (match-string 2 uri)))
      (or navi2ch-p2-all-board
	  (member board navi2ch-p2-board)
	  (and navi2ch-p2-board-regexp
	       (if (string-match "^live.*" board) t))))))

(defun navi2ch-p2-board-p (board)
;  (message "p2-board-p %s" board)
  (and navi2ch-p2-use-p2
       (or navi2ch-p2-all-board
           (member board navi2ch-p2-board)
           (and navi2ch-p2-board-regexp
                (if (string-match "^live.*" board) t)))))

(defun navi2ch-p2-login-p ()
  (let ((cookies (navi2ch-net-match-cookies navi2ch-p2-login-url)))
    (setq navi2ch-p2-login-flag
	  (null (memq nil
		      (mapcar (lambda (name) (assoc name cookies))
			      navi2ch-p2-cookie-names))))))

(defun navi2ch-p2-send-message-success-p (proc)
  (when proc
    (let ((str (navi2ch-net-get-content proc)))
      (setq str (decode-coding-string str navi2ch-p2-coding-system))
      (cond ((or (string-match "$B=q$-$3$_$^$7$?!#(B" str)
                 (string-match "$B=q$-$3$_$,=*$o$j$^$7$?!#(B" str))
             (message "P2$B$G=q$-9~$_$^$7$?(B")
             t)
	      ;;$B$*$=$i$/(Bcsrfid$B$N4|8B@Z$J$I(B
            ((or (string-match "Cookie$BG'>Z;~$K(BIP$B$NF10l@-$r%A%'%C%/$7$J$$(B" str)
                 (string-match "<b>$B=q$-$3$_!u%/%C%-!<3NG'(B</b>" str)
                 (string-match "p2 error: $B%Z!<%8A+0\$NBEEv@-$r3NG'$G$-$^$;$s$G$7$?!#(B" str)
                 )
	       ;;$B:F<hF@(B
             (message "reget-csrfid %s end" str)
	       (navi2ch-p2-get-csrfid)
	       'retry)
	      ((or  (string-match "p2 error: $B0z?t$N;XDj$,JQ$G$9(B" str))
	       (error str))
	      (t
	       (message "p2 error::%s" str)
	       nil)))))

(defun navi2ch-p2-make-deny-list ()
  "madakana.cgi$B$+$i%"%/%;%96X;_>uBV$r<hF@$9$k(B"
  (let (content str navi2ch-net-accept-gzip-org)
    (setq navi2ch-p2-all-board nil)
    (setq navi2ch-p2-board nil)
    (setq navi2ch-p2-board-regexp nil)
    (setq navi2ch-net-accept-gzip-org navi2ch-net-accept-gzip)
    (setq navi2ch-p2-all-board nil)
    (if (equal system-type 'windows-nt)
	(setq navi2ch-net-accept-gzip nil))
    (setq content (navi2ch-net-get-content (navi2ch-net-download-file navi2ch-p2-madakana-url)))
    (setq navi2ch-net-accept-gzip navi2ch-net-accept-gzip-org)
    (with-temp-buffer
      (if (not content)
	  (message "$B%G!<%?<hF@$K<:GT$7$^$7$?(B")
	(insert content)
	(goto-char (point-min))
	(while (re-search-forward "<font color=red><b>\\(.*[^>]\\)$" nil t)
	  (setq str (match-string 1))
	  (let (board host)
	    (cond
	     ((string-match "_BBS_\\(.*\\)_\\(.*\\)" str)
	      (progn
		(setq board (match-string 1 str))
		(setq host (match-string 2 str))
		(unless (member board navi2ch-p2-board)
		  (setq navi2ch-p2-board (cons board navi2ch-p2-board)))
		(message "deny board:%s host:%s" board host)))
	     ((string-match "_SRV_\\(.*\\)_\\(.*\\)" str)
	      (setq board (match-string 1 str))
	      (setq host (match-string 2 str))
	      (unless (member board navi2ch-p2-board-regexp)
		(setq navi2ch-p2-board-regexp (cons board navi2ch-p2-board-regexp)))
		     (message "deny regexp board:%s host:%s" board host))
	      (t
	       (setq navi2ch-p2-all-board t)
	       (message "all deny:%s" str)))
	    ))))))

(defun navi2ch-p2-send-message
  (from mail message subject bbs key time board article &optional post)
  (unless navi2ch-p2-csrfid
      (navi2ch-p2-get-csrfid))
  (when (navi2ch-message-samba24-check board)
    (let* ((url (concat navi2ch-p2-login-url "post.php?guid=ON"))
	   (referer (concat navi2ch-p2-login-url "menu.php"))
	   (param-alist (list
			 (cons "submit" "$B=q$-9~$`(B")
			 (cons "FROM"   (or from ""))
			 (cons "mail"   (or mail ""))
			 (cons "bbs"    bbs)
			 (cons "time"   time)
			 (cons "host"   (navi2ch-board-get-host board))
			 (cons "popup"   "1")
			 (cons "MESSAGE" message)
			 (cons "csrfid" navi2ch-p2-csrfid)
			 (cons "tepo" "don")
			 (cons "kuno" "ichi")
			 ))
	   (coding-system (navi2ch-board-get-coding-system board))
	   (cookies (navi2ch-net-match-cookies url)))
      (if (not subject)
	  (push (cons "key"    key) param-alist)
	(push (cons "newthread"   "1") param-alist)
	(push (cons "subject" subject) param-alist))
      
      (dolist (param post)
	(unless (assoc (car param) param-alist)
	  (push param param-alist)))
      (setq navi2ch-2ch-send-message-last-board board)
      (let ((proc
	     (navi2ch-net-send-request
	      url "POST"
	      (list (cons "Content-Type" "application/x-www-form-urlencoded")
		    (cons "Cookie"
			  (navi2ch-net-cookie-string cookies coding-system))
		    (cons "Referer" referer))
	      (navi2ch-net-get-param-string param-alist
					    coding-system))))
	(navi2ch-net-update-cookies url proc coding-system)
	(navi2ch-net-save-cookies)
	proc))))

(defun navi2ch-p2-get-csrfid ()
  (message "navi2ch-p2-get-csrfid")
  (dotimes (i 2)
    (unless navi2ch-p2-login-flag
      (navi2ch-p2-login))
    (let ((proc (navi2ch-net-send-request
		 (concat navi2ch-p2-login-url "menu.php")
		 "GET"
		 (list
		  (cons "Cookie"
			(navi2ch-net-cookie-string 
			 (navi2ch-net-match-cookies 
			  navi2ch-p2-login-url)
			 navi2ch-p2-coding-system)))
		 )))
    (navi2ch-net-update-cookies navi2ch-p2-login-url
				proc
				navi2ch-p2-coding-system)
    (navi2ch-net-save-cookies)
      (with-temp-buffer
	(insert (decode-coding-string
		 (navi2ch-net-get-content proc)
		 navi2ch-p2-coding-system))
	(goto-char (point-min))
	(if (re-search-forward "$B%f!<%6%m%0%$%s(B" nil t)
	    (if (and (zerop i)
		     (y-or-n-p "$BG'>Z<:GT!#%m%0%$%s$7$J$*$7$F$_$^$9$+(B? "))
		(setq navi2ch-p2-login-flag nil)
	      (error "$BG'>Z$K<:GT$7$^$7$?!#(B"))
	  (if (not (re-search-forward "csrfid=\\([a-f0-9]+\\)" nil t))
	      (error "csrfid$B$N<hF@$K<:GT$7$^$7$?(B")
	      (setq navi2ch-p2-csrfid (match-string 1))
	      (return proc)))))))

(defun navi2ch-p2-login (&optional mail password)
  (message "p2 login")
  (unless mail
    (setq mail (or navi2ch-p2-mail-address
		   (read-string "mail address: "))))
  (unless password
    (setq password (or navi2ch-p2-password
		       (read-passwd "password: "))))
  (navi2ch-p2-logout)
  (let ((proc (navi2ch-net-send-request
	       navi2ch-p2-login-url
	       "POST"
	       (list
		(cons "Referer" navi2ch-p2-login-url)
		(cons "User-Agent" navi2ch-net-user-agent)
		(cons "Content-Type" "application/x-www-form-urlencoded"))
	       (navi2ch-net-get-param-string 
		(list 
		 (cons "form_login_id" mail)
		 (cons "form_login_pass" password)
		 (cons "ctl_regist_cookie" "1")
		 (cons "regist_cookie" "1")
		 (cons "submit_member" "$B%f!<%6%m%0%$%s(B"))
		navi2ch-p2-coding-system))))
    (navi2ch-net-update-cookies navi2ch-p2-login-url
				proc
				navi2ch-p2-coding-system)
    (navi2ch-net-save-cookies)
    (navi2ch-p2-login-p)
    ))

(defun navi2ch-p2-logout ()
  (dolist (name navi2ch-p2-cookie-names)
    (navi2ch-net-store-cookie (list name "" 0 0)
			      navi2ch-p2-cookie-domain
			      navi2ch-p2-cookie-path))
  (navi2ch-net-save-cookies)
  (setq navi2ch-p2-login-flag nil))

(defmacro navi2ch-p2-with-updated-file (spec &rest body)
  `(dotimes (i 2)
     (unless navi2ch-p2-login-flag
       (navi2ch-p2-login))
     (navi2ch-net-update-file ,(car spec)
			      ,(cadr spec)
			      'file nil nil nil
			      (list
			       (cons "Cookie"
				     (navi2ch-net-cookie-string 
				      (navi2ch-net-match-cookies 
				       navi2ch-p2-bookmark-url)
				      navi2ch-p2-coding-system))))
     (if (and (file-exists-p ,(cadr spec))
	      (file-readable-p ,(cadr spec)))
	 (let ((coding-system-for-read navi2ch-p2-coding-system))
	   (with-temp-buffer
	     (insert-file-contents ,(cadr spec))
	     (goto-char (point-min))
	     (if (re-search-forward "$B%f!<%6%m%0%$%s(B" nil t)
		 (if (and (zerop i)
			  (y-or-n-p "$BG'>Z<:GT!#%m%0%$%s$7$J$*$7$F$_$^$9$+(B? "))
		     (setq navi2ch-p2-login-flag nil)
		   (error "$BG'>Z$K<:GT$7$^$7$?!#(B"))
	       (return (funcall (lambda () ,@body))))))
       (error "$B%U%!%$%k<hF@$K<:GT$7$^$7$?!#(B"))))

(put 'navi2ch-p2-with-updated-file 'lisp-indent-function 1)
(put 'navi2ch-p2-with-updated-file 'edebug-form-spec '((symbolp form) def-body))

(defun navi2ch-p2-send-request (url method &optional other-header content)
  (dotimes (i 2)
    (unless navi2ch-p2-login-flag
      (navi2ch-p2-login))
    (let ((proc (navi2ch-net-send-request
		 url
		 method
		 (append
		  (list
		   (cons "Cookie"
			 (navi2ch-net-cookie-string 
			  (navi2ch-net-match-cookies 
			   navi2ch-p2-bookmark-url)
			  navi2ch-p2-coding-system)))
		  other-header)
		 content)))
    (navi2ch-net-update-cookies navi2ch-p2-login-url
				proc
				navi2ch-p2-coding-system)
    (navi2ch-net-save-cookies)
      (with-temp-buffer
	(insert (decode-coding-string
		 (navi2ch-net-get-content proc)
		 navi2ch-p2-coding-system))
	(goto-char (point-min))
	(if (re-search-forward "$B%f!<%6%m%0%$%s(B" nil t)
	    (if (and (zerop i)
		     (y-or-n-p "$BG'>Z<:GT!#%m%0%$%s$7$J$*$7$F$_$^$9$+(B? "))
		(setq navi2ch-p2-login-flag nil)
	      (error "$BG'>Z$K<:GT$7$^$7$?!#(B"))
	  (return proc))))))


(defvar navi2ch-p2-bookmark-url "http://p2.2ch.net/p2/subject.php?spmode=fav&norefresh=true")
(defvar navi2ch-p2-bookmark-file-name "p2/bookmark.html")
(defun navi2ch-p2-get-bookmark ()
  (let ((file (navi2ch-expand-file-name navi2ch-p2-bookmark-file-name)))
      (navi2ch-p2-with-updated-file (navi2ch-p2-bookmark-url file)
	(let (result)
	  (goto-char (point-min))
	  (while (re-search-forward 
		  "href=\"read.php\\?host=\\([^&]*\\)&amp;bbs=\\([^&]*\\)&amp;key=\\([0-9]*\\)\\(?:&amp;rc=\\([0-9]+\\)\\)?[^\"]*\" class=\"thre_title\" onClick=\"[^\"]*\">\\([^<]*\\)</a>"
		  nil t)
	    (let ((host    (match-string 1))
		  (bbs     (match-string 2))
		  (key     (match-string 3))
		  (rc      (match-string 4))
		  (subject (match-string 5)))
	      (setq result
		    (cons (list
			   (concat "http://" host "/" bbs "/" key)
			   (list 'board
				 (cons 'name
				       (navi2ch-message-samba24-board-conversion
					'id
					bbs
					'name))
				 (cons 'uri
				       (concat "http://" host "/" bbs "/"))
				 (cons 'id bbs))
			   (list 'article
				 (cons 'subject subject)
				 (cons 'artid key)))
			  result))))
	  result))))

(defun navi2ch-p2-add-bookmark (url name &optional delete)
  (when (or (not delete)
	    (yes-or-no-p (format "%s$B$r:o=|$7$^$9$+(B? " 
				 name)))
    (string-match "http://\\([^/]*\\)/\\([^/]*\\)/\\([0-9]*\\)" url)
    (let ((host (match-string 1 url))
	  (bbs  (match-string 2 url))
	  (key  (match-string 3 url))
	  (add-or-del (if delete "$B:o=|(B" "$BDI2C(B")))
      (message "%s$B$r(B%s$BCf(B..." name add-or-del)
      (let ((si:message (symbol-function 'message))
	    (si:current-message (symbol-function 'current-message)))
	(unwind-protect
	    (progn
	      (lexical-let ((name name)
			    (add-or-del add-or-del))
		(fset 'message
		      (byte-compile
		       `(lambda (fmt &rest args)
			  (funcall ,si:message
				   "%s$B$r(B%s$BCf(B...%s"
				   name add-or-del
				   (apply 'format fmt args)))))
		(fset 'current-message
		      (byte-compile
		       `(lambda ()
			  (substring
			   (funcall ,si:current-message)
			   (length (format "%s$B$r(B%s$BCf(B..."
					   name add-or-del)))))))
	      (navi2ch-p2-send-request
	       (format
		(concat
		 "http://p2.2ch.net/p2/info.php?host=%s&"
		 "bbs=%s&key=%s&ttitle_en=%s&setfav=%d&")
		host bbs key (navi2ch-p2-encode-string name)
		(if delete 0 1))
	       "GET"))
	  (fset 'message si:message)
	  (fset 'current-message si:current-message)))
      (message "%s$B$r(B%s$BCf(B...done" name add-or-del))))

(defun navi2ch-p2-sync-global-bookmark-to-p2 (bookmark-id)
  (interactive 
   (list (completing-read "Bookmark ID: " 
			  navi2ch-bookmark-list 
			  nil t navi2ch-bookmark-current-bookmark-id)))
  (let ((bookmark (cddr (assoc bookmark-id navi2ch-bookmark-list))))
    (if bookmark
	(let ((p2-bookmark (navi2ch-p2-get-bookmark)))
	  (dolist (url (mapcar #'car bookmark))
	    (when (and (string-match 
			"^http://[^.]+\\.\\(?:2ch\\.net\\|machi\\.to\\|bbspink\\.com\\)/"
			url)
		       (not (assoc url p2-bookmark)))
	      (navi2ch-p2-add-bookmark url
				       (cdr (assq 'subject
						  (assq 'article
							(assoc url
							       bookmark)))))))
	  (dolist (url (mapcar #'car p2-bookmark))
	    (unless (assoc url bookmark)
	      (navi2ch-p2-add-bookmark url
				       (cdr (assq 'subject
						  (assq 'article
							(assoc url
							       p2-bookmark))))
				       t))))
      (error "No such bookmark"))))

(defun navi2ch-p2-sync-global-bookmark-from-p2 (bookmark-id)
  (interactive 
   (list (navi2ch-bookmark-read-id "Bookmark ID: ")))
  (unless (assoc bookmark-id navi2ch-bookmark-list)
    (navi2ch-bookmark-create-bookmark bookmark-id))
  (let ((p2-bookmark (navi2ch-p2-get-bookmark))
	(bookmark (cddr (assoc bookmark-id navi2ch-bookmark-list)))
	item article name)
    (dolist (url (mapcar #'car p2-bookmark))
      (unless (assoc url bookmark)
	(setq item (assoc url p2-bookmark)
	      article (cdr (assq 'article item))
	      name (cdr (assq 'subject article)))
	(message "%s$B$rDI2CCf(B..." name)
	(navi2ch-bookmark-add-subr 
	 bookmark-id
	 (cdr (assq 'board item))
	 article)
	(message "%s$B$rDI2CCf(B...done" name)))
    (dolist (url (mapcar #'car bookmark))
      (when (and (not (assoc url p2-bookmark))
		 (yes-or-no-p
		  (format "%s$B$r:o=|$7$^$9$+(B? "
			  (cdr (assq 'subject article)))))
	(navi2ch-bookmark-delete-key bookmark-id
				     url)))))

(defvar navi2ch-p2-dat-copy-function 'add-name-to-file)

(defvar navi2ch-p2-recent-file-name    "p2_recent.idx")
(defvar navi2ch-p2-res-hist-file-name  "p2_res_hist.idx")
(defvar navi2ch-p2-favlist-file-name   "p2_favlist.idx")
(defconst navi2ch-p2-data-dir-2ch      "2channel")
(defconst navi2ch-p2-data-dir-machibbs "machibbs.com")
(defconst navi2ch-p2-aborn-name-file-name "p2_aborn_name.txt")
(defconst navi2ch-p2-aborn-mail-file-name "p2_aborn_mail.txt")
(defconst navi2ch-p2-aborn-message-file-name "p2_aborn_msg.txt")
(defconst navi2ch-p2-aborn-id-file-name "p2_aborn_id.txt")
(defconst navi2ch-p2-ng-name-file-name "p2_ng_name.txt")
(defconst navi2ch-p2-ng-mail-file-name "p2_ng_mail.txt")
(defconst navi2ch-p2-ng-message-file-name "p2_ng_msg.txt")
(defconst navi2ch-p2-ng-id-file-name "p2_ng_id.txt")

(defun navi2ch-p2-get-dat-dir (board dir)
  (let ((uri (cdr (assq 'uri board))))
    (cond
     ((string-match "^http://[^.]+.\\(?:2ch\\.net\\|bbspink\\.com\\)/" uri)
      (expand-file-name (cdr (assq 'id board))
			(expand-file-name navi2ch-p2-data-dir-2ch
					  dir)))
     ((string-match "^http://[^.]+\\.\\(machi\\.to\\|machibbs\\.com\\)/" uri)
      (expand-file-name (cdr (assq 'id board))
			(expand-file-name navi2ch-p2-data-dir-machibbs
					  dir)))
     (t (let ((host (navi2ch-url-to-host uri)))
	  (while (string-match "\\(?:\\.\\|:/\\)/" host)
	    (setq host (replace-match "" nil nil host)))
	  (expand-file-name host
			    dir))))))

(defvar navi2ch-p2-export-bookmark nil)

(defun navi2ch-p2-make-idx-data (board file)
  (let* ((artid (navi2ch-article-file-name-to-artid file))
	 (article (navi2ch-article-load-info
		   board
		   (list (cons 'artid artid))))
	 (subject (if (and (file-exists-p file)
			   (file-readable-p file))
		      (cdr (assq 'subject
				 (navi2ch-article-get-first-message-from-file
				  file board)))
		    "$BITL@(B"))
	 (response (or (cdr (assq 'response article))
		       "0")))
    (format "%s<>%s<>%d<>%s<>%s<>%d<>%d<>%s<>%s<>%d<>%s<>%s<>0\n"
	    subject
	    artid
	    (if (file-exists-p file)
		(nth 7 (file-attributes file))
	      0)
	    response
	    (or (cdr (assq 'time article)) "")
	    (or (cdr (assq 'number article))
		0)
	    (if (and navi2ch-p2-export-bookmark
		     (assoc (navi2ch-bookmark-get-key board article)
			    (cddr (assoc navi2ch-p2-export-bookmark
					 navi2ch-bookmark-list))))
		1
	      0)
	    (or (cdr (assq 'name article)) "")
	    (or (cdr (assq 'mail article)) "")
	    (1+ (string-to-number
		 response))
	    (navi2ch-url-to-host
	     (cdr (assq 'uri board)))
	    (cdr (assq 'id board)))))

(defun navi2ch-p2-export (dir)
  (interactive "G$B=PNO@h(B: ")
  (if (file-exists-p dir)
      (unless (file-directory-p dir)
	(error "%s is not a directory." dir))
    (make-directory dir))
  (let* ((navi2ch-p2-export-bookmark
	  (or navi2ch-p2-export-bookmark
	      (completing-read "Bookmark ID: " 
			       navi2ch-bookmark-list 
			       nil t navi2ch-bookmark-current-bookmark-id))))
    (message "$B$*5$$K$$$j$r%(%/%9%]!<%HCf(B...")
    (with-temp-file (expand-file-name navi2ch-p2-favlist-file-name
				      dir)
      (apply 'insert
	     (mapcar
	      (lambda (item)
		(let ((board (cdr (assq 'board item))))
		  (navi2ch-p2-make-idx-data
		   board
		   (navi2ch-article-get-file-name 
		    board
		    (cdr (assq 'article item))))))
	      (cddr (assoc navi2ch-p2-export-bookmark
			   navi2ch-bookmark-list)))))
    (message "$B$*5$$K$$$j$r%(%/%9%]!<%HCf(B...done")
    (message "$BMzNr$r%(%/%9%]!<%HCf(B...")
    (with-temp-file (expand-file-name navi2ch-p2-recent-file-name
				      dir)
      (apply 'insert
	     (mapcar
	      (lambda (item)
		(let ((board (nth 1 item)))
		  (navi2ch-p2-make-idx-data
		   board
		   (navi2ch-article-get-file-name 
		    board
		    (nth 2 item)))))
	      navi2ch-history-alist)))
    (message "$BMzNr$r%(%/%9%]!<%HCf(B...done")
    ;; (message "$B=q$-$3$_MzNr$r%(%/%9%]!<%HCf(B...")
    ;; (message "$B=q$-$3$_MzNr$r%(%/%9%]!<%HCf(B...done")
    (message "$B$"$\!<$s%o!<%I$r%(%/%9%]!<%HCf(B...")
    (dolist (part '("name" "mail" "message" "id"))
      (with-temp-file (expand-file-name 
		       (symbol-value
			(intern (concat "navi2ch-p2-ng-"
					part
					"-file-name")))
		       dir)
	(apply 
	 'insert
	 (mapcar
	  (lambda (item)
	    (if (eq (cdr item) 'hide)
		(let* ((rule (car item))
		       (maybe-matchstr (if (consp rule)
					   (car rule)
					 rule))
		       (char (and (consp rule)
				  (stringp maybe-matchstr)
				  (string-to-char 
				   (symbol-name (cadr rule)))))
		       (case-fold (and char
				       (eq char
					   (setq char (downcase char)))))
		       (invert (and char
				    (plist-get rule :invert)))
		       (bbs (plist-get rule :board-id))
		       (artid (plist-get rule :artid))
		       (file (and artid
				  (navi2ch-article-get-file-name
				   (dolist (x navi2ch-list-board-name-list)
				     (when (string= artid (cdr (assq 'id x)))
				       (return x)))
				   (list (cons 'artid artid)))))
		       (subject
			(and artid file
			     (file-exists-p file)
			     (file-readable-p file)
			     (cdr (assq 
				   'subject
				   (navi2ch-article-get-first-message-from-file
				    file)))))
		       (regexp
			(cond
			 ((eq char ?r)
			  maybe-matchstr)
			 ((eq char ?s)
			  (regexp-quote maybe-matchstr))
			 ((eq char ?e)
			  (concat "^" 
				  (regexp-quote maybe-matchstr) 
				  "$"))
			 ((eq char ?f)
			  (navi2ch-fuzzy-regexp maybe-matchstr
						case-fold
						"[$B!!(B \f\t\n\r\v]*"))
			 (t nil))))
		  (concat
		   (if regexp
		       (if invert
			   "<regex:i>"
			 "<regex>")
		     (if invert 
			 "<invert>"
		       ""))
		   (if bbs (concat "<bbs>" bbs "</bbs>") "")
		   (if subject 
		       (concat "<title>"
			       (regexp-quote subject)
			       "</title>")
		     "")
		   (or regexp maybe-matchstr)
		   "\t\t0\n"))
	      ""))
	  (symbol-value
	   (intern (concat "navi2ch-article-message-filter-by-"
			   part
			   "-alist")))))))
    (message "$B$"$\!<$s%o!<%I$r%(%/%9%]!<%HCf(B...done")
    (message "dat, idx $B$r%(%/%9%]!<%HCf(B...")
    (lexical-let ((dir dir))
      (navi2ch-search-for-each-article
       (lambda (board file)
	 (let* ((artid (navi2ch-article-file-name-to-artid file))
		(article (navi2ch-article-load-info
			  board
			  (list (cons 'artid artid))))
		(subject (assq 'subject
			       (navi2ch-article-get-first-message-from-file
				file board)))
		(response (or (cdr (assq 'response article))
			      "0"))
		(dat-dir (navi2ch-p2-get-dat-dir board dir)))
	   (when (and (file-exists-p file)
		      (file-readable-p file))
	     (make-directory dat-dir t)
	     (funcall navi2ch-p2-dat-copy-function
		      file
		      (expand-file-name (file-name-nondirectory file)
					dat-dir)
		      t))
	   (with-temp-file (expand-file-name (concat artid ".idx")
					     dat-dir)
	     (insert (navi2ch-p2-make-idx-data board file)))))
       (navi2ch-search-all-board-list)))
    (message "dat, idx $B$r%(%/%9%]!<%HCf(B...done")))

(defun navi2ch-p2-encode-string (text)
  (when (stringp text)
    (setq text 
	  (base64-encode-string
	   (encode-coding-string text navi2ch-p2-coding-system)
	   t))
    (while (string-match "[/+]" text)
      (setq text (replace-match
		  (if (string= (match-string 0 text) "/")
		      "%2F"
		    "%2B")
		  nil nil text)))
    text))

(defun navi2ch-p2-decode-string (text)
  (when (stringp text)
    (while (string-match "%2\\(B\\|F\\)" text)
      (setq text (replace-match
		  (if (string= (match-string 1 text) "F")
		      "/"
		    "+")
		  nil nil text)))
    (decode-coding-string
     (base64-decode-string text)
     navi2ch-p2-coding-system)))

