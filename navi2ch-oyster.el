;;; navi2ch-oyster.el --- oyster module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006 by Navi2ch Project

;; Author: MIZUNUMA Yuto <mizmiz@users.sourceforge.net>
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

;; $B#2$A$c$s$M$k%S%e!<%"(B($BDL>N!|(B)$B$r;H$C$F=q$-9~$_$7$?$j2a5n%m%0$r<hF@$G$-$k$h$&$K$J$j$^$9!#(B
;; oyster$B$H$$$&C18l$O!|$N=i4|$N3+H/%3!<%I%M!<%`$_$?$$$J$b$N$G$7$?!#(B
;; $B!|$N@5<0$J>&IJL>$O9uF&$H$$$&$i$7$$$G$9(B($B%3%s%S%K7h:QNN<}=q$h$j(B)$B!#(B
;; $B;EMM(B http://kage.monazilla.org/system_DOLIB100.html
;;
;; $B%$%s%9%H!<%k!'(B
;; $B!|(BSESSION-ID$B<hF@$N$?$a$K!"(BSSL$B%"%/%;%9$G$-$k%i%$%V%i%j(B(gnu-tls$B$J$I(B)$B$,I,MW(B
;; Windows: cygwin$B$N(Bgnutls$B$r%$%s%9%H!<%k!#(Bgnutls-cli.exe$B$,%Q%9$KF~$C$F$$$k$3$H!#(B
;; Linux: gnutls$B$r(Bapt$B$J$I$G%$%s%9%H!<%k(B
;;
;; $B;HMQNc!'(B
;; M-x navi2ch-oyster-login $B$G%m%0%$%s!#(B
;; $B=q$-9~$_;~$K%"%/%;%96X;_$N%(%i!<$r<u$1$F!"(B'y'$B$G%m%0%$%s$b2D!#(B
;; $B!|%m%0%$%s>uBV$K$"$l$P!"2a5n%m%0<hF@$9$k(B(y-or-n$B$GJ9$+$l$k(B)
;; M-x navi2ch-oyster-logout $B$G%m%0%"%&%H(B
;;
;; $B!|$N(BSESSION-ID$B$N@8B84|4V$O1=$G$O(B24$B;~4V$H8@$o$l$F$$$^$9$,!"(B
;; $B;EMM$KL@5-$5$l$F$$$J$$$N$G(Bnavi2ch$BB&$G(Bexpire$B$O$;$:!"%(%i!<$G%j%H%i%$<hF@$7$^$9!#(B

;;; Code:

(provide 'navi2ch-oyster)

(defconst navi2ch-oyster-ident
  "$Id$")

(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(autoload 'open-tls-stream "tls")

(defvar navi2ch-oyster-func-alist
  '((bbs-p		. navi2ch-oyster-p)
    (article-update 	. navi2ch-oyster-article-update)
    (send-message   	. navi2ch-oyster-send-message)
    (send-success-p	. navi2ch-oyster-send-message-success-p)
;    (extract-post	. navi2ch-2ch-extract-post)
    ))
;; navi2ch-net-user-agent $B$b(B multibbs $B2=$9$kI,MW$"$j(B?

(defvar navi2ch-oyster-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'oyster
			 navi2ch-oyster-func-alist
			 navi2ch-oyster-variable-alist)

;;-------------

(defvar navi2ch-oyster-use-oyster nil	; $BJQ?tL>$OMW8!F$!#(B
  "*$B!|$r;H$&$+$I$&$+!#(B")
(defvar navi2ch-oyster-id nil
  "*$B!|$N(BID($B$*$=$i$/%a!<%k%"%I%l%9(B)")
(defvar navi2ch-oyster-password nil
  "*$B!|$N%Q%9%o!<%I!#(B")
(defvar navi2ch-oyster-server "2chv.tora3.net"
  "*$B!|(BID $B<hF@%5!<%P!#(B")
(defvar navi2ch-oyster-cgi "/futen.cgi"
  "*$B!|(BID $B<hF@(B CGI$B!#(B")

(defvar navi2ch-oyster-session-id nil
  "$B!|%5!<%P$+$i<hF@$7$?%;%C%7%g%s(B ID$B!#(B")

(defun navi2ch-oyster-p (uri)
  "$B!|$KBP1~$9$k(B URI $B$J$i(B non-nil$B$rJV$9!#(B"
  (and navi2ch-oyster-use-oyster
       (or (string-match "http://.*\\.2ch\\.net/" uri)
	   (string-match "http://.*\\.bbspink\\.com/" uri))))

(defun navi2ch-oyster-article-update (board article start)
  "BOARD, ARTICLE $B$KBP1~$9$k%U%!%$%k$r99?7$9$k!#(B
START $B$,(B non-nil $B$J$i$P%l%9HV9f(B START $B$+$i$N:9J,$r<hF@$9$k!#(B
START $B$+$i$8$c$J$$$+$b$7$l$J$$$1$I!&!&!&!#(B
$BJV$jCM$O(B HEADER$B!#(B"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	(url (navi2ch-article-get-url board article))
	header)
    (setq header (if start
		     (navi2ch-net-update-file-diff url file time)
		   (navi2ch-net-update-file url file time)))
    ;; $B%(%i!<$@$C$?$i2a5n%m%0$r<hF@(B
    (when (navi2ch-net-get-state 'error header)
      (setq url (navi2ch-article-get-kako-url board article))
      (setq header (navi2ch-net-update-file url file))

      ;; $B$d$C$Q$j%@%a$@$C$?$i(B ID $B$r;H$C$F2a5n%m%0$r<hF@(B
      (if (not (navi2ch-net-get-state 'error header))
	  (setq header (navi2ch-net-add-state 'kako header))
        (when (y-or-n-p "$B!|$r;H$C$F2a5n%m%0$r<hF@$7$^$9$+!)(B")
          (unless navi2ch-oyster-session-id
            (navi2ch-oyster-login))
          (setq url (navi2ch-oyster-get-offlaw-url
                     board article navi2ch-oyster-session-id file))
;         (message "offlaw url %s" url)
          (setq header
                (if start
                    (progn
                      (message "article %s" article)
                      (navi2ch-oyster-update-file-with-offlaw url file time t))
                  (prog1
                      (navi2ch-oyster-update-file-with-offlaw url file time nil)
;                    (message "Getting from 0 offlaw.cgi")
                    )))
          (unless (navi2ch-net-get-state 'error header)
            (setq header (navi2ch-net-add-state 'kako header))))))
    header))

(defun navi2ch-oyster-send-message
  (from mail message subject bbs key time board article &optional post)
  (let ((post (navi2ch-put-alist "sid"
				 ;;$B%;%C%7%g%s(BID$B<hF@:Q$_$G$"$l$P!|$G=q$-9~$_(B
				 (or navi2ch-oyster-session-id "")
				 post)))
    (navi2ch-2ch-send-message from mail message subject bbs key
			      time board article post)))

(defun navi2ch-oyster-send-message-success-p (proc)
  (when proc
    (let ((str (navi2ch-net-get-content proc)))
      (setq str (decode-coding-string str navi2ch-p2-coding-system))
      (cond ((or (string-match "$B=q$-$3$_$^$7$?!#(B" str)
                 (string-match "$B=q$-$3$_$,=*$o$j$^$7$?!#(B" str))
             t)
	    ((or (string-match "<b>$B%/%C%-!<$,$J$$$+4|8B@Z$l$G$9!*(B</b>" str)
		 (string-match "<b>$B=q$-$3$_!u%/%C%-!<3NG'(B</b>" str))
	     'retry)
            ((string-match "$B#E#R#R#O#R!'%"%/%;%95,@)Cf$G$9!*!*(B" str)
             (if (not (y-or-n-p "$B%"%/%;%95,@)Cf$G$9$,!|%m%0%$%s$7$^$9$+!)(B"))
                 nil
               (message "$B!|(Blogin..")
               (navi2ch-oyster-login)
               (if navi2ch-oyster-session-id
                   'retry
                 nil)))
            (t
             (message "$B!|(Berror::%s" str)
             nil)))))

(defun navi2ch-oyster-get-offlaw-url (board article session-id file)
  "BOARD, ARTICLE, SESSION-ID, FILE $B$+$i(B offlaw url $B$KJQ49!#(B"
  (let ((uri (navi2ch-board-get-uri board))
	(artid (cdr (assq 'artid article)))
	(size 0)
	encoded-s)
    (setq encoded-s (navi2ch-net-url-hexify-string session-id))
    (when (file-exists-p file)
      (setq size (max 0 (navi2ch-file-size file))))
    (string-match "\\(.*\\)\\/\\([^/]*\\)\\/" uri)
    (format "%s/test/offlaw.cgi/%s/%s/?raw=.%s&sid=%s"
	    (match-string 1 uri) (match-string 2 uri) artid size encoded-s)))

(defun navi2ch-oyster-update-file-with-offlaw (url file &optional time diff)
  "FILE $B$r(B URL $B$+$i(B offlaw.cgi $B$r;H$C$F99?7$9$k!#(B
TIME $B$,(B non-nil $B$J$i$P(B TIME $B$h$j?7$7$$;~$@$199?7$9$k!#(B
DIFF $B$,(B non-nil $B$J$i$P:9J,$r<hF@$9$k!#(B
$B99?7$G$-$l$P(B HEADER $B$rJV$9!#(B"
  (let ((dir (file-name-directory file))
	proc header status)
    (unless (file-exists-p dir)
      (make-directory dir t))
    (setq proc (navi2ch-net-download-file url time))
    (setq header (and proc
		      (navi2ch-net-get-header proc)))
    (setq status (and proc
		      (navi2ch-net-get-status proc)))
    (cond ((or (not proc)
	       (not header)
	       (not status))
	   (setq header (navi2ch-net-add-state 'error header)))
	  ((string= status "304")
	   (setq header (navi2ch-net-add-state 'not-updated header)))
	  ((string= status "200")
	   (let ((coding-system-for-write 'binary)
		 (coding-system-for-read 'binary)
		 cont)
	     (message "%s: getting file with offlaw.cgi..." (current-message))
	     (setq cont (navi2ch-net-get-content proc))
	     (if (or (string= cont "")
		     (not cont))
		 (progn (message "%sfailed" (current-message))
			(signal 'navi2ch-update-failed nil))
	       (message "%sdone" (current-message))
	       (let (state data cont-size)
		 (when (string-match "^\\([^ ]+\\) \\(.+\\)\n" cont)
		   (setq state (match-string 1 cont))
		   (setq data (match-string 2 cont))
		   (setq cont (replace-match "" t nil cont)))
		 (when (and (string-match "\\(OK\\|INCR\\)" state)
			    (string-match "\\(.+\\)/\\(.+\\)K" data))
		   (setq cont-size (string-to-number (match-string 1 data))))
		 (cond
		  ((string= "+OK" state)
		   (with-temp-file file
		     (navi2ch-set-buffer-multibyte nil)
		     (when (and (file-exists-p file) diff)
		       (insert-file-contents file)
		       (goto-char (point-max)))
		     (insert (substring cont 0 cont-size))))
		  ((string= "-INCR" state) ;; $B$"$\!<$s(B
		   (with-temp-file file
		     (navi2ch-set-buffer-multibyte nil)
		     (insert (substring cont 0 cont-size)))
		   (setq header (navi2ch-net-add-state 'aborn header)))
		  (t
		   (when (string= "-ERR" state)
		     (let ((err-msg (decode-coding-string
				     data navi2ch-coding-system)))
		       (message "Error! %s" err-msg)
                       (when (string-match "$B;XDj;~4V$,2a$.$^$7$?!#(B" err-msg)
                         (if (not (y-or-n-p "$B!|(BSESSION-ID$B$NM-8z4|8B$,@Z$l$^$7$?%m%0%$%s$7$^$9$+!)(B"))
                             (setq header (navi2ch-net-add-state 'error header))
                           (message "$B!|(Blogin..")
                           (navi2ch-oyster-login)
                             )))                   )))))))
	  (t
	   (setq header (navi2ch-net-add-state 'error header))))
    header))

(defun navi2ch-oyster-get-status-from-proc (proc)
  "PROC$B@\B3$N(BHTTP$B%9%F!<%?%9It$rJV$9!#(B"
  (with-current-buffer (process-buffer proc)
	 (while (and (memq (process-status proc) '(open run))
		     (goto-char (point-min))
		     (not (looking-at "HTTP/1\\.[01] \\([0-9]+\\)")))
	   (accept-process-output proc))
         (sleep-for 1)
	 (goto-char (point-min))
         (let ((i 3))
           (catch 'loop
             (while (>= (setq i (1- i)) 0)
;               (sleep-for 1)	  ; $B2?$@$+$&$^$/F0$+$J$$$N$G(Bwait$BF~$l$?(B
               (accept-process-output proc 1)
               (goto-char (point-min))
               ;; $B:G8e$^$G8+$D$+$i$J$$$^$^$@$H%(%i!<(B
               (when (search-forward "HTTP/1\." nil (> i 0))
                 (throw 'loop
                        (if (looking-at "[01] \\([0-9]+\\).+\n")
                            (match-string 1)))))))))

(defun navi2ch-oyster-get-session-id-from-proc (proc)
  "proc$B$+$i!|$N(BSESSIOIN-ID$B$r<hF@(B"
   (or (with-current-buffer (process-buffer proc)
         (while (and (eq (process-status proc) 'open)
                     (goto-char (point-min))
                     (not (search-forward "HTTP/1\\.[01] \\([0-9]+\\)")))
           (accept-process-output proc)
           (message "Retrying")
           (sleep-for 2))
         (let ((i 10))
           (catch 'loop
             (while (>= (setq i (1- i)) 0)
;               (sleep-for 1)	  ; $B2?$@$+$&$^$/F0$+$J$$$N$G(Bwait$BF~$l$?(B
               (accept-process-output proc 1)
               (goto-char (point-min))
               ;; $B:G8e$^$G8+$D$+$i$J$$$^$^$@$H%(%i!<(B
               (when (search-forward "SESSION-ID=" nil (> i 0))
                 (throw 'loop
                        (if (looking-at "\\(.*\\)\n")
                            (match-string 1))))))))))

(defun navi2ch-oyster-login ()
  "$B!|$N%5!<%P$K%m%0%$%s$7$F(B session-id $B$r<hF@$9$k!#(B"
  (interactive)
  (let (buf proc strus)
    (message "$B!|$N%5!<%P$K%m%0%$%s$7$^$9(B")
    (setq buf (get-buffer-create (concat " *" "navi2ch oyster-ssl")))
    (with-current-buffer buf
      (erase-buffer)
      (setq proc (open-tls-stream "ssl" buf navi2ch-oyster-server 443))
      (let ((contents (concat "ID=" navi2ch-oyster-id
                              "&PW=" navi2ch-oyster-password)))
        (process-send-string proc
                             (concat
                              (concat "POST " navi2ch-oyster-cgi " HTTP/1.1\n")
                              (concat "Host: " navi2ch-oyster-server "\n")
                              "Accept: */*\n"
                              (concat "Referer: https://" navi2ch-oyster-server "/\n")
                              "User-Agent: DOLIB/1.00\n"
                              "X-2ch-UA: "
                              (format "Navigator for 2ch %s" navi2ch-version) "\n"
                              "Content-Length: "
                              (number-to-string (length contents)) "\n"
                              "Connection: close\n"
                              "\n"
                              contents "\n")))
      (setq status (navi2ch-oyster-get-status-from-proc proc))
      (cond
       ((string= status "200")
        (setq navi2ch-oyster-session-id (navi2ch-oyster-get-session-id-from-proc proc))
        (when (or (not navi2ch-oyster-session-id)
                  (string-match "ERROR:.+" navi2ch-oyster-session-id))
            (error "$B!|(BID$B<hF@(BERROR $B$*$=$i$/4|8B@Z$l(B")
            (setq navi2ch-oyster-session-id nil))
        (message "$B!|(BID$B<hF@(B ID=%s" navi2ch-oyster-session-id))
       ((string= status "400")
        (message "$B!|(BID$B<hF@(BERROR $B%5!<%PITD4(B %s" status)))
      (kill-buffer buf))))

(defun navi2ch-oyster-logout ()
  "$B!|$N%m%0%"%&%H(B"
  (interactive)
  (setq navi2ch-oyster-session-id nil)
  (message "$B!|$N%5!<%P$+$i%m%0%"%&%H$7$^$7$?(B"))
  
;;; navi2ch-oyster.el ends here
