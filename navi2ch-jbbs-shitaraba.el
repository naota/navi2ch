;;; navi2ch-jbbs-shitaraba.el --- View jbbs-shitaraba module for Navi2ch.

;; Copyright (C) 2002 by Navi2ch Project

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
(provide 'navi2ch-jbbs-shitaraba)
(defvar navi2ch-jbbs-shitaraba-ident
  "$Id$")

(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(defvar navi2ch-js-func-alist
  '((bbs-p		. navi2ch-js-p)
    (subject-callback	. navi2ch-js-subject-callback)
    (article-update 	. navi2ch-js-article-update)
    (url-to-board   	. navi2ch-js-url-to-board)
    (url-to-article 	. navi2ch-js-url-to-article)
    (send-message   	. navi2ch-js-send-message)
    (send-success-p 	. navi2ch-js-send-message-success-p)
    (error-string   	. navi2ch-js-send-message-error-string)
    (article-to-url 	. navi2ch-js-article-to-url)))

(defvar navi2ch-js-variable-alist
  '((coding-system	. euc-japan)))

(navi2ch-multibbs-regist 'jbbs-shitaraba
			 navi2ch-js-func-alist
			 navi2ch-js-variable-alist)

;;-------------

(defun navi2ch-js-p (uri)
  "URI がJBBS＠したらばなら non-nilを返す。"
  (or (string-match "http://jbbs.shitaraba.com/" uri)
      (string-match "http://jbbs.shitaraba.net/" uri)))

(navi2ch-multibbs-defcallback navi2ch-js-subject-callback (jbbs-shitaraba)
  "subject.txt を取得するとき navi2ch-net-update-file
で使われるコールバック関数"
   (while (re-search-forward "\\([0-9]+\\.\\)cgi\\([^\n]+\n\\)" nil t)
     (replace-match "\\1dat\\2"))
   (re-search-backward "\\(\n.*\n\\)")
   (replace-match "\n"))

(defun navi2ch-js-article-update (board article)
  "BOARD ARTICLEの記事を更新する。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	(url  (navi2ch-js-article-to-url board article))
	(func 'navi2ch-js-article-callback)
	ret)
    (setq ret (navi2ch-net-update-file url file time func))
    (list ret nil)))

(defun navi2ch-js-url-to-board (url)
  (let (uri id)
    (cond ((string-match
	    "\\(http://[^/]+/[^/]+/\\([0-9]+\\)/\\)" url)
	   (setq uri (match-string 1 url)
		 id  (match-string 2 url)))
	  ((string-match
	    "\\(http://[^/]+/[^/]+\\)/bbs/read\\.cgi.*BBS=\\([0-9]+\\)" url)
	   (setq uri (format "%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url))
		 id  (match-string 2 url))))
    (if id (list (cons 'uri uri) (cons 'id id)))))

(defun navi2ch-js-url-to-article (url)
  "URL から article に変換。"
  (let (list)
    (if (string-match
	 "http://[^/]+/[^/]+/bbs/read\\.cgi.*KEY=\\([0-9]+\\)" url)
	(progn
	  (setq list (list (cons 'artid (match-string 1 url))))
	  (when (string-match "&START=\\([0-9]+\\)" url)
	    (setq list (cons (cons 'number
				   (string-to-number (match-string 1 url)))
			     list)))))
    list))

(defun navi2ch-js-send-message
  (from mail message subject bbs key time board article)
  (let ((url         (navi2ch-js-get-writecgi-url board))
	(referer     (navi2ch-board-get-uri board))
	(param-alist (list
		      (cons "submit" (if subject
					 "新規書き込み"
				       "書き込む"))
		      (cons "NAME" (or from ""))
		      (cons "MAIL" (or mail ""))
		      (cons "MESSAGE" message)
		      (cons "BBS" bbs)
		      (if subject
			  (cons "SUBJECT" subject)
			(cons "KEY" key))
		      (cons "TIME" time))))
    (navi2ch-net-send-request
     url "POST"
     (list (cons "Content-Type" "application/x-www-form-urlencoded")
	   (cons "Referer" referer))
     (let ((navi2ch-coding-system 'euc-japan))
       (navi2ch-net-get-param-string param-alist)))))

(defun navi2ch-js-send-message-success-p (proc)
  (string= "" (navi2ch-net-get-content proc)))

(defun navi2ch-js-send-message-error-string (proc)
  (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				   'euc-japan)))
    (cond ((string-match "ＥＲＲＯＲ：\\([^<]+\\)" str)
	   (match-string 1 str))
	  ((string-match "<b>\\([^<]+\\)" str)
	   (match-string 1 str)))))

(defun navi2ch-js-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (let ((uri   (cdr (assq 'uri board)))
	(artid (cdr (assq 'artid article))))
    (string-match "\\(.*\\)\\/\\([^/]*\\)\\/" uri)
    (concat
     (format "%s/bbs/read.cgi?BBS=%s&KEY=%s"
	     (match-string 1 uri) (match-string 2 uri) artid)
     (and start (format "&START=%d" start))
     (and end (format "&END=%d" end))
     (and nofirst
	  (not (eq start 1))
	  "&NOFIRST=TRUE"))))

;;------------------

(defvar navi2ch-js-parse-regexp "\
<dt>\\([0-9]+\\) 名前：\\(<a href=\"mailto:\\([^\"]*\\)\">\\|<[^>]+>\\)\
<b> \\(.*\\) </b><[^>]+> 投稿日： \\(.*\\)<br><dd>\\(.*\\)<br><br>\n")
(defvar navi2ch-js-parse-subject-regexp "<title>\\([^\\n]*\\)</title>")

(defun navi2ch-js-parse-subject ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-js-parse-subject-regexp nil t)
    (match-string 1)))

(defun navi2ch-js-parse ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-js-parse-regexp nil t)))

(defun navi2ch-js-make-article (&optional subject)
  (let ((mail (match-string 3))
	(name (match-string 4))
	(date (match-string 5))
	(contents (match-string 6)))
    (format "%s<>%s<>%s<>%s<>%s\n"
	    name (or mail "") date contents (or subject ""))))

(navi2ch-multibbs-defcallback navi2ch-js-article-callback (jbbs-shitaraba)
  (let ((beg (point))
	subject)
    (setq subject (navi2ch-js-parse-subject))
    (while (navi2ch-js-parse)
      (insert (prog1 (navi2ch-js-make-article subject)
		(delete-region beg (point))))
      (setq subject nil)
      (setq beg (point)))
    (delete-region beg (point-max))))

(defun navi2ch-js-get-writecgi-url (board)
  "write.cgi の url を返す"
  (let ((uri (navi2ch-board-get-uri board)))
    (string-match "\\(.+\\)/[^/]+/$" uri)
    (format "%s/bbs/write.cgi" (match-string 1 uri))))

;;; navi2ch-jbbs-shitaraba.el ends here
