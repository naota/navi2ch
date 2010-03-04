;;; navi2ch-machibbs.el --- View machiBBS module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2009 by Navi2ch Project

;; Author:
;; Part5 スレの 509 の名無しさん
;; <http://pc.2ch.net/test/read.cgi/unix/1013457056/509>

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

;;

;;; Code:
(provide 'navi2ch-machibbs)
(defconst navi2ch-machibbs-ident
  "$Id$")

(eval-when-compile (require 'cl))
(require 'navi2ch-multibbs)

(defvar navi2ch-machibbs-func-alist
  '((bbs-p		. navi2ch-machibbs-p)
    (subject-callback	. navi2ch-machibbs-subject-callback)
    (article-update 	. navi2ch-machibbs-article-update)
    (article-to-url 	. navi2ch-machibbs-article-to-url)
    (url-to-board   	. navi2ch-machibbs-url-to-board)
    (url-to-article 	. navi2ch-machibbs-url-to-article)
    (send-message   	. navi2ch-machibbs-send-message)
    (send-success-p 	. navi2ch-machibbs-send-message-success-p)
    (board-update	. navi2ch-machibbs-board-update)))

(defvar navi2ch-machibbs-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'machibbs
			 navi2ch-machibbs-func-alist
			 navi2ch-machibbs-variable-alist)

;; (defvar navi2ch-machibbs-subject-max-bytes 5000
;;   "スレの一覧をどれだけ表示するか。
;; 0の場合は全て表示する。")

;;-------------

(defun navi2ch-machibbs-p (uri)
  "URI が machibbs なら non-nilを返す。"
  (or (string-match "http://[^\\.]+\\.machibbs\\.com/" uri)
      (string-match "http://[^\\.]+\\.machi\\.to/" uri)))

;; (defun navi2ch-machibbs-subject-callback (string)
;;   "subject.txt を取得するとき navi2ch-net-update-file
;; で使われるコールバック関数"
;;   (let ((sub-string (if (> navi2ch-machibbs-subject-max-bytes 0)
;; 			(substring string 0 navi2ch-machibbs-subject-max-bytes)
;; 		      string)))
;;     (navi2ch-replace-string
;;      "\\([0-9]+\\.\\)cgi\\([^\n]+\n\\)" "\\1dat\\2" sub-string t)))

(navi2ch-multibbs-defcallback navi2ch-machibbs-subject-callback (machibbs)
  "subject.txt を取得するとき navi2ch-net-update-file
で使われるコールバック関数"
  (while (re-search-forward "\\([0-9]+\\.\\)cgi\\([^\n]+\n\\)" nil t)
    (replace-match "\\1dat\\2")))

(defun navi2ch-machibbs-article-update (board article start)
  "BOARD ARTICLE の記事を更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	(url  (navi2ch-machibbs-article-to-url board article start nil start))
	(func (if start 'navi2ch-machibbs-article-callback-diff
		'navi2ch-machibbs-article-callback)))
    (navi2ch-net-update-file url file time func nil start)))

(defun navi2ch-machibbs-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する" ; 効かなかったら教えてください。
  (let ((uri   (cdr (assq 'uri board)))
	(artid (cdr (assq 'artid article))))
    (string-match "\\(.*\\)\\/\\([^/]*\\)\\/" uri) ; \\/ --> / ?
    (concat
     (format "%s/bbs/read.cgi?BBS=%s&KEY=%s"
	     (match-string 1 uri) (match-string 2 uri) artid)
     (if (and (stringp start)
	      (string-match "l\\([0-9]+\\)" start))
	 (format "&LAST=%s" (match-string 1 start))
       (concat
	(and start (format "&START=%d" start))
	(and end (format "&END=%d" end))))
     (and nofirst
	  (not (eq start 1))
	  "&NOFIRST=TRUE"))))

(defun navi2ch-machibbs-url-to-board (url)
  "url から BOARD に変換。"
  (cond
   ;; http://www.machi.to/bbs/read.pl?BBS=tawara&KEY=1059722839
   ;; http://tohoku.machi.to/bbs/read.pl?BBS=touhoku&KEY=1062265542
   ((string-match
     "http://\\(.+\\)/bbs/read\\..*BBS=\\([^&]+\\)"
     url)
    (list (cons 'uri (format "http://%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url)))
	  (cons 'id  (match-string 2 url))))
   ;; http://hokkaido.machi.to/bbs/read.cgi/hokkaidou/
   ((string-match
     "http://\\([^/]+\\)/bbs/read.cgi/\\([^/]+\\)/"
     url)
    (list (cons 'uri (format "http://%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url)))
	  (cons 'id  (match-string 2 url))))
   ;; http://www.machi.to/tawara/
   ;; http://tohoku.machi.to/touhoku/
   ((string-match
     "http://\\([^/]+\\)/\\([^/]+\\)"
     url)
    (list (cons 'uri (format "http://%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url)))
	  (cons 'id  (match-string 2 url))))))

(defun navi2ch-machibbs-url-to-article (url)
  (cond ((string-match
	  "http://.+/bbs/read\\..*KEY=\\([0-9]+\\)"
	  url)
	 (list (cons 'artid (match-string 1 url))))
	((string-match
	  "http://[^/]+/bbs/read.cgi/[^/]+/\\([0-9]+\\)"
	  url)
	 (list (cons 'artid (match-string 1 url))))))

(defun navi2ch-machibbs-send-message
  (from mail message subject bbs key time board article &optional post)
  (let ((url          (navi2ch-machibbs-get-writecgi-url board))
	(referer      (navi2ch-board-get-uri board))
	(param-alist  (list
		       (cons "submit" "書き込む")
		       (cons "NAME" (or from ""))
		       (cons "MAIL" (or mail ""))
		       (cons "MESSAGE" message)
		       (cons "BBS" bbs)
		       (cons "KEY" key)
		       (cons "TIME" time))))
    (navi2ch-net-send-request
     url "POST"
     (list (cons "Content-Type" "application/x-www-form-urlencoded")
	   (cons "Referer" referer))
     (navi2ch-net-get-param-string param-alist))))

(defun navi2ch-machibbs-get-writecgi-url (board)
  (let ((uri (navi2ch-board-get-uri board)))
    (string-match "\\(.+\\)/[^/]+/$" uri)
    (format "%s/bbs/write.cgi" (match-string 1 uri))))

(defun navi2ch-machibbs-send-message-success-p (proc)
  (string-match "302 Found" (navi2ch-net-get-content proc)))

;; -- parse html --
(defvar navi2ch-machibbs-parse-regexp "\
<dt>\\([0-9]+\\) ?名前：\\(<a href=\"mailto:\\([^\"]*\\)\"><b> ?\\|<font[^>]*>\
<b> ?\\)\\(.*\\) ?</b>.+ ?投稿日： ?\\(.*\\)\\(\n\\( ?]</font>\\)\\)?<br>\
<dd> ?\\(.*\\) ?<br><br>$")
(defvar navi2ch-machibbs-parse-subject-regexp "<title>\\(.*\\)</title>")

(defun navi2ch-machibbs-parse-subject ()
  (let ((case-fold-search t))
    (and (re-search-forward navi2ch-machibbs-parse-subject-regexp nil t)
	 (match-string 1))))

(defun navi2ch-machibbs-parse ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-machibbs-parse-regexp nil t)))

(defun navi2ch-machibbs-make-article (&optional subject)
  (let ((mail (match-string 3))
	(name (match-string 4))
	(date (match-string 5))
	(date-tail (match-string 7))
	(contents (match-string 8)))
    (format "%s<>%s<>%s<>%s<>%s\n"
	    name (or mail "") (concat date (or date-tail ""))
	    contents (or subject ""))))

(navi2ch-multibbs-defcallback navi2ch-machibbs-article-callback
    (machibbs &optional diff)
  (let ((beg (point))
	(max-num 0)
	subject alist num min-num)
    (unless diff
      (setq subject (navi2ch-machibbs-parse-subject)))
    (while (navi2ch-machibbs-parse)
      (setq num (string-to-number (match-string 1))
	    min-num (or min-num num)
	    max-num (max max-num num)
	    alist (cons (cons (string-to-number (match-string 1))
			      (navi2ch-machibbs-make-article subject))
			alist)
	    subject nil))
    (delete-region beg (point-max))
    (when (and min-num max-num)
      (let ((i min-num))
	(while (<= i max-num)
	  (insert (or (cdr (assoc i alist))
		      "あぼーん<>あぼーん<>あぼーん<>あぼーん<>\n"))
	  (setq i (1+ i)))))))

(defun navi2ch-machibbs-article-callback-diff ()
  (navi2ch-machibbs-article-callback t))

(defun navi2ch-machibbs-board-update (board)
  (let ((url (navi2ch-board-get-url board))
	(file (navi2ch-board-get-file-name board))
	(time (cdr (assq 'time board)))
	(func (navi2ch-multibbs-subject-callback board)))
    (navi2ch-net-update-file url file time func)))

;;; navi2ch-machibbs.el ends here
