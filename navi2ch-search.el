;;; navi2ch-search.el --- Search Module for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2009 by Navi2ch
;; Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
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

;; namazu($BBg@h@8$N8!:w(B) $B$r;H$&$K$O(B
;; (navi2ch-net-send-request
;; (format "http://64.124.197.202/cgi-bin/search/namazu.cgi?%s"
;;         (navi2ch-net-get-param-string
;;          '(("query" . "navi2ch")
;;            ("submit" . "$B8!:w(B")
;;            ("whence" . "0")
;;            ("idxname" . "2chpc")
;;            ("max" . "10")
;;            ("result" . "normal")
;;            ("sort" . "score"))))
;; "GET")
;; $B$J46$8$G!#(B




;;; Code:
(provide 'navi2ch-search)
(defconst navi2ch-search-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-search-mode-map nil)
(unless navi2ch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    (define-key map "Q" 'navi2ch-search-return-previous-board-maybe)
    (define-key map "s" 'navi2ch-search-sync)
    (setq navi2ch-search-mode-map map)))

(defvar navi2ch-search-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Search"
   nil))

(defvar navi2ch-search-searched-subject-list nil)
(defvar navi2ch-search-board
  '((name . "$B8!:w0lMw(B")
    (type . search)
    (id . "#search")))

(defvar navi2ch-search-history nil)
(defvar navi2ch-search-mode-line-info nil)

;;; navi2ch-bm callbacks
(defun navi2ch-search-set-property (begin end item)
  (put-text-property begin end 'item item))

(defun navi2ch-search-get-property (point)
  (get-text-property (save-excursion (goto-char point)
				     (beginning-of-line)
				     (point))
		     'item))

(defun navi2ch-search-get-board (item)
  (car item))

(defun navi2ch-search-get-article (item)
  (cdr item))

(defun navi2ch-search-exit ()
  (run-hooks 'navi2ch-search-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'search 'navi2ch-search
			 navi2ch-search-board)

;;; navi2ch-search functions
(defun navi2ch-search-insert-subjects ()
  (let ((i 1))
    (dolist (x navi2ch-search-searched-subject-list)
      (let ((article (navi2ch-search-get-article x))
            (board (navi2ch-search-get-board x)))
        (navi2ch-bm-insert-subject
         x i
         (cdr (assq 'subject article))
	 ;; $B%l%9?t>pJs$,$"$C$?$iI=<((B
	 (if (assq 'response x)
	     (format "(%s) [%s]" (cdr (assq 'response x)) (cdr (assq 'name board)))
	   (format "[%s]" (cdr (assq 'name board)))))
        (setq i (1+ i))))))

(defun navi2ch-search-for-each-board (board-func board-list)
  (let (alist)
    (dolist (board board-list)
      (message "Searching in %s..." (cdr (assq 'name board)))
      (setq alist (nconc (funcall board-func board)
			 alist)))
    (message "Searching...%s" (if alist "done" "not found"))
    (nreverse alist)))

(defun navi2ch-search-for-each-article (article-func board-list)
  (navi2ch-search-for-each-board
   (lambda (board)
     (let ((default-directory (navi2ch-board-get-file-name board "")))
       (delq nil
	     (mapcar (lambda (file)
		       (funcall article-func board file))
		     (and default-directory
			  (file-directory-p default-directory)
			  (sort (directory-files default-directory
						 nil navi2ch-article-local-dat-regexp)
				#'navi2ch-right-aligned-string<))))))
   board-list))

(defun navi2ch-search-board-subject-regexp (board-list regexp)
  (navi2ch-search-for-each-board
   (lambda (board)
     (let* ((file (navi2ch-board-get-file-name board))
	    (subject-list (and file
			       (navi2ch-board-get-subject-list file)))
	    alist)
       (dolist (article subject-list)
	 (let ((subject (cdr (assq 'subject article))))
	   (when (string-match regexp subject)
	     (push (cons board article) alist))))
       alist))
   board-list))

(defun navi2ch-search-article-regexp (board-list regexp)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (with-temp-buffer
       (navi2ch-board-insert-file-contents board file)
       (navi2ch-apply-filters board navi2ch-article-filter-list)
       (goto-char (point-min))
       (when (re-search-forward regexp nil t)
	 (let ((subject
		(cdr (assq 'subject
			   (navi2ch-article-get-first-message)))))
	   (list board
		 (cons 'subject subject)
		 (cons 'artid
		       (navi2ch-article-file-name-to-artid file)))))))
   board-list))

(defun navi2ch-search-cache (board-list)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (let ((subject (assq 'subject
			  (navi2ch-article-get-first-message-from-file
			   file board))))
       (list board subject
	     (cons 'artid (navi2ch-article-file-name-to-artid file)))))
   board-list))

(defun navi2ch-search-orphan (board-list)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (let ((article (list (cons 'artid (navi2ch-article-file-name-to-artid file)))))
       (if (navi2ch-article-orphan-p board article)
	   (let ((subject (assq 'subject
				(navi2ch-article-get-first-message-from-file
				 file board))))
	     (nconc (list board subject)
		    article)))))
   board-list))

(easy-menu-define navi2ch-search-mode-menu
  navi2ch-search-mode-map
  "Menu used in navi2ch-search"
  navi2ch-search-mode-menu-spec)

(defun navi2ch-search-setup-menu ()
  (easy-menu-add navi2ch-search-mode-menu))

(defun navi2ch-search-mode ()
  "\\{navi2ch-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-search-mode)
  (setq mode-name "Navi2ch Search")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-search-mode-map)
  (setq navi2ch-mode-line-identification 
	'navi2ch-search-mode-line-info)
  (navi2ch-set-mode-line-identification)
  (navi2ch-search-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-search-mode-hook))

(defun navi2ch-search (&rest args)
  (navi2ch-search-mode)
  (navi2ch-bm-setup 'navi2ch-search)
  (navi2ch-search-sync))

(defun navi2ch-search-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-search-insert-subjects))))

(defun navi2ch-search-return-previous-board-maybe ()
  (interactive)
  (if navi2ch-board-current-board
      (navi2ch-board-select-board	; $B1x$M$'!&!&!&(B
       (prog1
	   navi2ch-board-current-board
	 (setq navi2ch-board-current-board nil)))
    (navi2ch-bm-exit)))

(defun navi2ch-search-subject-subr (board-list-or-function)
  (let ((regexp (navi2ch-read-string "Subject regexp: " nil
				     'navi2ch-search-history))
	(board-list (if (functionp board-list-or-function)
			(funcall board-list-or-function)
		      board-list-or-function)))
    (setq navi2ch-search-searched-subject-list
	  (navi2ch-search-board-subject-regexp board-list regexp)
	  navi2ch-search-mode-line-info
	  (format "Search subject %s" regexp)))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-for-each-directory-recursive (function directory)
  (let ((default-directory directory))
    (dolist (file (directory-files "."))
      (if (and (file-directory-p file)
	       (not (member file '("." ".."))))
	  (navi2ch-search-for-each-directory-recursive
	   function (expand-file-name file)))))
  (funcall function directory))

(defun navi2ch-search-directory-to-board (directory directory-to-board-alist)
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (or (cdr (assoc dir directory-to-board-alist))
	(navi2ch-board-url-to-board
	 (concat "http://"
		 (file-relative-name dir navi2ch-directory))))))

(defun navi2ch-search-all-board-list ()
  (let ((directory-to-board-alist
	 (mapcar (lambda (board)
		   (cons (navi2ch-board-get-file-name board "")
			 board))
		 (navi2ch-list-get-board-name-list
		  (navi2ch-list-get-normal-category-list
		   navi2ch-list-category-list))))
	l)
    (navi2ch-search-for-each-directory-recursive
     (lambda (directory)
       (if (file-exists-p (expand-file-name navi2ch-article-summary-file-name
					    directory))
	   (push (navi2ch-search-directory-to-board directory
						    directory-to-board-alist)
		 l)))
     navi2ch-directory)
    (nreverse l)))

(defun navi2ch-search-all-subject ()
  (interactive)
  (navi2ch-search-subject-subr #'navi2ch-search-all-board-list))

(defun navi2ch-search-article-subr (board-list-or-function)
  (let ((regexp (navi2ch-read-string "Search regexp: " nil
				     'navi2ch-search-history))
	(board-list (if (functionp board-list-or-function)
			(funcall board-list-or-function)
		      board-list-or-function)))
    (setq navi2ch-search-searched-subject-list
	  (navi2ch-search-article-regexp board-list regexp)
	  navi2ch-search-mode-line-info
	  (format "Search article %s" regexp)))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-article ()
  (interactive)
  (navi2ch-search-article-subr #'navi2ch-search-all-board-list))

(defun navi2ch-search-cache-subr (board-list)
  (setq navi2ch-search-searched-subject-list
	(navi2ch-search-cache board-list)
	navi2ch-search-mode-line-info "Search cache")
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-cache ()
  (interactive)
  (navi2ch-search-cache-subr (navi2ch-search-all-board-list)))

(defun navi2ch-search-orphan-subr (board-list)
  (setq navi2ch-search-searched-subject-list
	(navi2ch-search-orphan board-list)
	navi2ch-search-mode-line-info "Search orphan")
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-orphan ()
  (interactive)
  (navi2ch-search-orphan-subr (navi2ch-search-all-board-list)))


(defun navi2ch-search-set-mode-line (&optional str)
  
  (setq navi2ch-mode-line-identification str)
  (navi2ch-set-mode-line-identification))

;;; navi2ch web subject search
(defvar navi2ch-search-web-last-search-word nil
  "$B:G8e$K8!:w$7$?J8;zNs(B")
(defvar navi2ch-search-web-current-start 0
  "$B8=:_!"I=<($7$F$$$k%9%l$N%*%U%;%C%H(B")
(defvar navi2ch-search-web-current-end 0)
(defvar navi2ch-search-web-total-hit 0
  "$B8!:wAm%R%C%H?t(B")

;; find.2ch.net is dead
;(defvar navi2ch-search-web-search-method 'navi2ch-search-find-2ch-method)

;; hula search is case sensitive
;(defvar navi2ch-search-web-search-method 'navi2ch-search-hula-method)

;; dig will be a standard?
(defvar navi2ch-search-web-search-method 'navi2ch-search-dig-method)

(defsubst navi2ch-search-web-method ()
  (if (symbolp navi2ch-search-web-search-method)
      (symbol-value navi2ch-search-web-search-method)
    navi2ch-search-web-search-method))
(defsubst navi2ch-search-web-subject-list (keyword arg)
  (funcall (nth 0 (navi2ch-search-web-method)) keyword arg))
(defun navi2ch-search-web-next (&optional args)
  (interactive "P")
  (let ((item (nth 1 (navi2ch-search-web-method))))
    (navi2ch-search-web
     (if (functionp item) 
	 (apply item args)
       item))))
(defun navi2ch-search-web-previous (&optional args)
  (interactive "P")
  (let ((item (nth 2 (navi2ch-search-web-method))))
    (navi2ch-search-web
     (if (functionp item) 
	 (apply item args)
       item))))

(defun navi2ch-search-web (&optional arg)
  "web $B$N8!:w%7%9%F%`$r;H$C$F(B 2ch $B$r%9%l%?%$$G8!:w$9$k!#(B
`arg' $B$O(B backend $B$K$h$j2r<a$,0[$J$k$,!"(B `arg' $B$,(B `nil' $B$N>l9g$OI,(B
$B$:?7$7$$8!:w$r3+;O$9$k!#(B"
  (interactive "P")
  (let (keyword)
    (if (and navi2ch-search-web-last-search-word arg)
	(setq keyword navi2ch-search-web-last-search-word)
      (setq keyword (navi2ch-read-string "Subject search from web: " 
					nil
					'navi2ch-search-history)))
    (setq navi2ch-search-web-last-search-word keyword
	  navi2ch-search-web-current-start 0
	  navi2ch-search-web-current-end 0
	  navi2ch-search-web-total-hit 0)
    ;; board mode $B$KEO$9%9%l%?%$$N%j%9%H:n@.(B
    (setq navi2ch-search-searched-subject-list
	  (navi2ch-search-web-subject-list keyword arg))
    (navi2ch-bm-select-board navi2ch-search-board)
    (setq navi2ch-search-mode-line-info
	  (format "Search: %s [%d-%d/%d]"
		  navi2ch-search-web-last-search-word
		  navi2ch-search-web-current-start
		  navi2ch-search-web-current-end
		  navi2ch-search-web-total-hit))))

(defun navi2ch-search-web-make-list (url title num)
  "((board) (subject)) $B$N$h$&$J(B navi2ch $BFbIt$N%9%l>pJs$r5<;wE*$K:n@.!#(B"
  (when (string-match 
	 "\\(http://[-a-zA-Z0-9_.!~*';/?:@&=+$,%#]+/\\)test/read.cgi/\\(.+\\)/\\([0-9]+\\)/.*" 
	 url)
    (let ((subject (cons 'subject title))
	  (response (cons 'response num))
	  (artid (cons 'artid (match-string 3 url))))
      (list (navi2ch-board-url-to-board url) subject artid response))))

;;; navi2ch find.2ch.net
(defvar navi2ch-search-find-2ch-method
    '(navi2ch-search-find-2ch-subject-list
      navi2ch-search-find-2ch-next
      navi2ch-search-find-2ch-previous))
(defvar navi2ch-search-find-2ch-last-search-num nil)
(defvar navi2ch-search-find-2ch-search-num 30
  "$B0lEY$KI=<($9$k8!:w7k2L(B
find.2ch.net $B$N;EMM>e!":GBg$O(B50$B7o(B")
(defvar navi2ch-search-find-2ch-coding 'euc-japan-dos
  "find.2ch.net $B$G;H$o$l$k%3!<%G%#%s%0(B")
(defconst navi2ch-search-find-2ch-thread-regexp
  "<dt><a href=\"\\(http://[-a-zA-Z0-9_.!~*';/?:@&=+$,%#]+\\)\">\\(.*\\)</a> (\\([0-9]+\\)) - <font size=[-0-9]+><a href=.+/>\\(.+\\)$BHD(B</a>.+</font></dt><dd>"
  "find.2ch.net $B$G8!:w$9$k(B regexp")

(defun navi2ch-search-find-2ch-subject-list (query offset)
  "find.2ch.net $B$KJ8;zNs(B `query' $B$G%j%/%(%9%H!#(B
`offset' $B$O!V<!$N(B10$B7o!W$H$+I=<($5$;$?$$$H$-$K;H$&!#(B"
  (setq offset
        (if offset
            (+ (or navi2ch-search-find-2ch-last-search-num 0) offset)
          0))
  (setq navi2ch-search-find-2ch-last-search-num offset)
  (let* ((query (navi2ch-url-encode-string query 
                                           navi2ch-search-find-2ch-coding t))
         ;; $B0UL#$bJ,$+$i$:;H$C$F$k%Q%i%a!<%?B?$7!#FbIt;EMM$,J,$+$j<!Bh2~A1M=Dj(B
         (url (format
               "http://find.2ch.net/?STR=%s&SCEND=A&SORT=MODIFIED&COUNT=%s&TYPE=TITLE&BBS=ALL&OFFSET=%s" 
               query navi2ch-search-find-2ch-search-num offset))
         (proc (navi2ch-net-download-file url))
         (cont (decode-coding-string (navi2ch-net-get-content proc)
                                     navi2ch-search-find-2ch-coding))
         subject-list)
    (with-temp-buffer
      (insert cont)
      (goto-char (point-min))
      ;; $B$^$:Am%R%C%H7o?t$rC5$9(B
      (if (re-search-forward "<font color=white size=-1>\\([0-9]+\\)$B%9%lCf(B.*$BIC(B</font>" nil t)
	  (progn
	    (setq navi2ch-search-web-total-hit (string-to-number (match-string 1)))
	    (while (re-search-forward 
		    navi2ch-search-find-2ch-thread-regexp
		    nil t)
	      (let ((url (match-string 1))
		    (title (navi2ch-replace-html-tag (match-string 2)))
		    (num  (match-string 3)))
		(push (navi2ch-search-web-make-list url title num)
		      subject-list))))
	(setq navi2ch-search-web-total-hit 0)
	(message "No match")))
    (setq navi2ch-search-web-current-start
	  (min (1+ navi2ch-search-find-2ch-last-search-num)
	       navi2ch-search-web-total-hit)
	  navi2ch-search-web-current-end
	  (min (+ navi2ch-search-find-2ch-last-search-num 
		  (min navi2ch-search-find-2ch-search-num 50))
	       navi2ch-search-web-total-hit))
    (nreverse subject-list)))

;; $B<!$N%Z!<%8(B
(defun navi2ch-search-find-2ch-next ()
   (min navi2ch-search-find-2ch-search-num 50))

;; $BA0$N%Z!<%8(B
(defun navi2ch-search-find-2ch-previous ()
  (- (min navi2ch-search-find-2ch-search-num 50)))

;;; navi2ch dig.2ch.net
(defvar navi2ch-search-dig-method
    '(navi2ch-search-dig-subject-list 1 -1))
(defvar navi2ch-search-dig-current-page nil)
(defvar navi2ch-search-dig-coding 'utf-8)
(defconst navi2ch-search-dig-thread-regexp
  "<br><a href=\"\\(http://.+.2ch.net/test/read.cgi/.+/[0-9]+/\\)l5\">\\(.+\\) (\\([0-9]+\\))</a>")

(defun navi2ch-search-dig-subject-list (query arg)
  "dig.2ch.net $B$KJ8;zNs(B `query' $B$G%j%/%(%9%H!#(B"
;;; sample url http://dig.2ch.net/?keywords=navi2ch&AndOr=0&maxResult=50&atLeast=1&Sort=5&Link=1&Bbs=all&924=1
  (setq navi2ch-search-dig-current-page
        (if arg
            (+ navi2ch-search-dig-current-page
               (if (< arg 0) -1 1))
          1))
  (let* ((query (navi2ch-url-encode-string query
                                           navi2ch-search-dig-coding t))
         (url (format
               "http://dig.2ch.net/?P=%d&keywords=%s&maxResult=50&atLeast=1&Sort=5&Link=1&Bbs=all&924=1"
               navi2ch-search-dig-current-page query))
         (proc (navi2ch-net-download-file url))
         (cont (decode-coding-string (navi2ch-net-get-content proc)
                                     navi2ch-search-dig-coding))
         subject-list)
    (with-temp-buffer
      (insert cont)
      (goto-char (point-min))
      (if (re-search-forward "\\([0-9,]+\\)$B7o8+$D$+$j$^$7$?(B"
			     nil t)
	  (progn
	    (setq navi2ch-search-web-total-hit
		  (string-to-number (navi2ch-replace-string "," ""
							    (match-string 1))))
	    (while (re-search-forward
		    navi2ch-search-dig-thread-regexp
		    nil t)
	      (let ((url (match-string 1))
		    (title (navi2ch-replace-html-tag (match-string 2)))
		    (num  (match-string 3)))
		(push (navi2ch-search-web-make-list url title num)
		      subject-list))))
	(setq navi2ch-search-web-total-hit 0)
	(message "No match")))
    (setq navi2ch-search-web-current-start
	  (min (1+ (* (1- navi2ch-search-dig-current-page) 50))
	       navi2ch-search-web-total-hit)
	  navi2ch-search-web-current-end
	  (min (* navi2ch-search-dig-current-page 50)
	       navi2ch-search-web-total-hit))
    (nreverse subject-list)))

;;; navi2ch h.ula.cc
(defvar navi2ch-search-hula-method
    '(navi2ch-search-hula-subject-list 1 -1))
(defvar navi2ch-search-hula-current-page nil)
(defvar navi2ch-search-hula-coding 'shift_jis)
(defconst navi2ch-search-hula-thread-regexp 
  "<nobr>[^<]+<a href=\"http://same.ula.cc/test/r.so/\\([^/]+\\)/\\([^/]+\\)/\\([0-9]+\\)/\\?guid=ON\" target=\"_blank\">\\([^<]*\\) (\\([0-9]+\\))</a></nobr>")

(defun navi2ch-search-hula-subject-list (query arg)
  "h.ula.cc $B$KJ8;zNs(B `query' $B$G%j%/%(%9%H!#(B"
  (setq navi2ch-search-hula-current-page
	(if arg
	    (+ navi2ch-search-hula-current-page
	       (if (< arg 0) -1 1))
	  1))
  (let* ((query (navi2ch-url-encode-string query 
                                           navi2ch-search-hula-coding t))
         (url (format
               "http://h.ula.cc/dance/?P=%d&kenken=%s"
               navi2ch-search-hula-current-page query))
         (proc (navi2ch-net-download-file url))
         (cont (decode-coding-string (navi2ch-net-get-content proc) 
                                     navi2ch-search-hula-coding))
         subject-list)
    (with-temp-buffer
      (insert cont)
      (goto-char (point-min))
      (if (re-search-forward "<font color=red face=\"Arial\"><b>\\([0-9,]+\\)</b></font>"
                             nil t)
	  (progn
	    (setq navi2ch-search-web-total-hit
		  (string-to-number (navi2ch-replace-string "," "" 
							    (match-string 1))))
	    (while (re-search-forward
		    navi2ch-search-hula-thread-regexp
                    nil t)
	      (let ((url (format "http://%s/test/read.cgi/%s/%s/"
				 (match-string 1)
				 (match-string 2)
				 (match-string 3)))
		    (title (navi2ch-replace-html-tag (match-string 4)))
		    (num  (match-string 5)))
		(push (navi2ch-search-web-make-list url title num) 
		      subject-list))))
	(setq navi2ch-search-web-total-hit 0)
	(message "No match")))
    (setq navi2ch-search-web-current-start
	  (min (1+ (* (1- navi2ch-search-hula-current-page) 50))
	       navi2ch-search-web-total-hit)
	  navi2ch-search-web-current-end
	  (min (* navi2ch-search-hula-current-page 50)
	       navi2ch-search-web-total-hit))
    (nreverse subject-list)))

;; navi2ch union
(defvar navi2ch-search-union-method
    '(navi2ch-search-union-subject-list 1 -1))
(defvar navi2ch-search-union-method-list
  '(navi2ch-search-find-2ch-method
    navi2ch-search-hula-method))
(defvar navi2ch-search-union-last-search-num 0)
(defvar navi2ch-search-union-prev-num nil)

(defun navi2ch-search-union-subject-list (keyword arg)
  (unless arg 
    (setq navi2ch-search-union-last-search-num 0
	  navi2ch-search-union-prev-num nil))
  (let ((all 0) (current 0) method-list url-list result)
    (dolist (m navi2ch-search-union-method-list)
      (let ((navi2ch-search-web-search-method m))
	(setq method-list (navi2ch-search-web-subject-list 
			   keyword
			   (cond
			    ((eq arg 1)
			     (let ((item (nth 1 (navi2ch-search-web-method))))
			       (if (functionp item) 
				   (apply item arg)
				 item)))
			    ((eq arg -1)
			     (let ((item (nth 2 (navi2ch-search-web-method))))
			       (if (functionp item) 
				   (apply item arg)
				 item)))
			    (t arg)))))
      (setq all (+ all navi2ch-search-web-total-hit)
	    current (+ current 
		       (- navi2ch-search-web-current-end
			  navi2ch-search-web-current-start)
		       1))
      (dolist (l method-list)
	(let ((url (navi2ch-article-to-url (car l) (cdr l))))
	  (unless (member url url-list)
	    (push url url-list)
	    (push l result)))))
    (setq navi2ch-search-web-total-hit all)
    (setq navi2ch-search-web-current-start
	  (if (eq arg -1)
	      (or (cadr navi2ch-search-union-prev-num)
		  1)
	    (1+ navi2ch-search-union-last-search-num)))
    (setq navi2ch-search-web-current-end
	  (+ current navi2ch-search-web-current-start -1))
    (setq navi2ch-search-union-last-search-num
	  navi2ch-search-web-current-end)
    (setq navi2ch-search-union-prev-num
	  (if (eq arg -1)
	      (or (cdr navi2ch-search-union-prev-num) '(1))
	    (cons navi2ch-search-web-current-start
		  navi2ch-search-union-prev-num)))
    (nreverse result)))

(run-hooks 'navi2ch-search-load-hook)
;;; navi2ch-search.el ends here
