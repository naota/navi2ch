;;; navi2ch-jbbs-shitaraba.el --- View jbbs-shitaraba module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2006 by Navi2ch Project

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

;; ＪＢＢＳ＠したらばの仕様は下記参照。
;; http://jbbs.shitaraba.com/bbs/read.cgi/computer/351/1040452916/126-140n

;;; Code:
(provide 'navi2ch-jbbs-shitaraba)
(defconst navi2ch-jbbs-shitaraba-ident
  "$Id$")

(eval-when-compile 
  (require 'cl))

(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(eval-when-compile
  (navi2ch-defalias-maybe 'coding-system-list 'ignore))

(defvar navi2ch-js-func-alist
  '((bbs-p		. navi2ch-js-p)
    (subject-callback	. navi2ch-js-subject-callback)
    (article-update 	. navi2ch-js-article-update)
    (article-to-url 	. navi2ch-js-article-to-url)
    (url-to-board   	. navi2ch-js-url-to-board)
    (url-to-article 	. navi2ch-js-url-to-article)
    (send-message   	. navi2ch-js-send-message)
    (send-success-p 	. navi2ch-js-send-message-success-p)
    (error-string   	. navi2ch-js-send-message-error-string)
    (board-update	. navi2ch-js-board-update)))

(defvar navi2ch-js-coding-system
  (or (car (memq 'eucjp-ms (coding-system-list)))
      'euc-japan))

(defvar navi2ch-js-variable-alist
  (list (cons 'coding-system navi2ch-js-coding-system)))

(navi2ch-multibbs-regist 'jbbs-shitaraba
			 navi2ch-js-func-alist
			 navi2ch-js-variable-alist)

(defvar navi2ch-js-host-list '("jbbs.shitaraba.com"
			       "jbbs.shitaraba.net"
			       "jbbs.livedoor.com"
			       "jbbs.livedoor.jp"))

;;-------------

(defun navi2ch-js-p (uri)
  "URI がJBBS＠したらばなら non-nil を返す。"
  (let ((list navi2ch-js-host-list)
	host result)
    (while (and list (not result))
      (setq host (car list))
      (setq list (cdr list))
      (setq result (string-match (format "^http://%s" (regexp-quote host))
				 uri)))
    result))

(navi2ch-multibbs-defcallback navi2ch-js-subject-callback (jbbs-shitaraba)
  "subject.txt を取得するとき navi2ch-net-update-file
で使われるコールバック関数"
  (while (re-search-forward "\\([0-9]+\\.\\)cgi\\([^\n]+\n\\)" nil t)
    (replace-match "\\1dat\\2"))
  (re-search-backward "\\(\n.*\n\\)")
  (replace-match "\n"))

(defun navi2ch-js-article-update (board article start)
  "BOARD ARTICLE の記事を更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	(url  (navi2ch-js-article-to-rawmode-url board article start nil start))
	(func (if start
		  (lexical-let ((start start))
		    (lambda () (navi2ch-js-article-callback start)))
		'navi2ch-js-article-callback)))
    (navi2ch-net-update-file url file time func nil start)))

(defun navi2ch-js-url-to-board (url)
  (let (prefix category id)
    (when (or
	   ;; http://jbbs.shitaraba.com/computer/bbs/read.cgi?BBS=351&KEY=1040452814&START=1&END=5
	   (string-match
	    "http://\\(.+\\)/\\([^/]+\\)/bbs/read\\.cgi.*BBS=\\([0-9]+\\)" url)
	   ;; http://jbbs.shitaraba.com/bbs/read.cgi/computer/351/1040452814/1-5
	   (string-match
	    "http://\\(.+\\)/bbs/[^/]+\\.cgi/\\([^/]+\\)/\\([0-9]+\\)" url)
	   ;; http://jbbs.shitaraba.com/computer/351/
	   (string-match
	    "http://\\(.+\\)/\\([^/]+\\)/\\([0-9]+\\)/" url))
      (setq prefix (match-string 1 url)
	    category (match-string 2 url)
	    id (match-string 3 url)))
    (if id (list (cons 'uri (format "http://%s/%s/%s/" prefix category id))
		 (cons 'id id)))))

(defun navi2ch-js-url-to-article (url)
  "URL から article に変換。"
  (let (artid number kako)
    (cond
     ;; http://jbbs.shitaraba.com/computer/bbs/read.cgi?BBS=351&KEY=1040452814&START=1&END=5
     ((string-match
       "http://.+/bbs/read\\.cgi.*KEY=\\([0-9]+\\)" url)
      (setq artid (match-string 1 url))
      (when (string-match "&START=\\([0-9]+\\)" url)
	(setq number (string-to-number (match-string 1 url)))))
     ;; http://jbbs.shitaraba.com/computer/351/storage/1014729216.html
     ((string-match
       "http://.+/storage/\\([0-9]+\\)\\.html" url)
      (setq artid (match-string 1 url)
	    kako t))
     ;; http://jbbs.shitaraba.com/bbs/read.cgi/computer/351/1040452814/1-5
     ((string-match
       "http://.+/bbs/[^/]+\\.cgi/[^/]+/[^/]+/\\([^/]+\\)" url)
      (setq artid (match-string 1 url))
      (when (string-match
	     (format
	      "http://.+/bbs/[^/]+\\.cgi/[^/]+/[^/]+/%s/[ni.]?\\([0-9]+\\)[^/]*$"
	      artid)
	     url)
	(setq number (string-to-number (match-string 1 url))))))
    (let (list)
      (when artid
	(setq list (cons (cons 'artid artid) list))
	(when number
	  (setq list (cons (cons 'number number) list)))
	(when kako
	  (setq list (cons (cons 'kako kako) list)))
	list))))

(defun navi2ch-js-send-message
  (from mail message subject bbs key time board article &optional post)
  (let ((url         (navi2ch-js-get-cgi-url "write" board))
	(referer     (navi2ch-board-get-uri board))
	(param-alist (list
		      (cons "submit" (if subject
					 "新規書き込み"
				       "書き込む"))
		      (cons "NAME" (or from ""))
		      (cons "MAIL" (or mail ""))
		      (cons "MESSAGE" message)
		      (cons "BBS" bbs)
		      (cons "DIR" (navi2ch-js-get-dir board))
		      (if subject
			  (cons "SUBJECT" subject)
			(cons "KEY" key))
		      (cons "TIME" time))))
    (navi2ch-net-send-request
     url "POST"
     (list (cons "Content-Type" "application/x-www-form-urlencoded")
	   (cons "Referer" referer))
     (let ((navi2ch-coding-system navi2ch-js-coding-system))
       (navi2ch-net-get-param-string param-alist)))))

(defun navi2ch-js-send-message-success-p (proc)
  (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				   navi2ch-js-coding-system)))
    (or (string-match "<title>書きこみました。</title>" str)
	(string= "" str))))

(defun navi2ch-js-send-message-error-string (proc)
  (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				   navi2ch-js-coding-system)))
    (cond ((string-match "ＥＲＲＯＲ：\\([^<]+\\)" str)
	   (match-string 1 str))
	  ((string-match "<b>\\([^<]+\\)" str)
	   (match-string 1 str)))))

(defun navi2ch-js-article-to-url-subr
  (string board article &optional start end nofirst)
  "BOARD, ARTICLE から  STRING.cgi の url に変換。
START, END, NOFIRST で範囲を指定する。"
  (let ((url (concat (navi2ch-js-get-cgi-url string board)
		     (cdr (assq 'artid article))
		     "/")))
    (if (numberp start)
	(setq start (number-to-string start)))
    (if (numberp end)
	(setq end (number-to-string end)))
    (if (equal start end)
	(concat url start)
      (concat url
	      start (and (or start end) "-") end
	      (and nofirst "n")))))

(defun navi2ch-js-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から read.cgi の url に変換。
START, END, NOFIRST で範囲を指定する"
  (navi2ch-js-article-to-url-subr "read"
				  board article start end nofirst))

(defun navi2ch-js-article-to-rawmode-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から rawmode.cgi の url に変換。
START, END, NOFIRST で範囲を指定する"
  (navi2ch-js-article-to-url-subr "rawmode"
				  board article start end nofirst))

;;------------------

(defvar navi2ch-js-parse-regexp
  ;; レス番        名前      メール  投稿日時    本文  スレタイトル  ID/リモホ
  "\\([0-9]+\\)<>\\(.*\\)<>\\(.*\\)<>\\(.*\\)<>\\(.*\\)<>\\(.*\\)<>\\(.*\\)\n")

(defun navi2ch-js-parse ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-js-parse-regexp nil t)))

(navi2ch-multibbs-defcallback navi2ch-js-article-callback
    (jbbs-shitaraba &optional start)
  (let ((i (or start 1))
	(beg (point))
	num name mail date contents subject id)
    (while (navi2ch-js-parse)
      (setq num		(match-string 1)
	    name	(match-string 2)
	    mail	(match-string 3)
	    date	(match-string 4)
	    contents	(match-string 5)
	    subject	(match-string 6)
	    id		(match-string 7))
      (delete-region beg (match-end 0))
      (while (< i (string-to-number num))
	(insert "あぼーん<>あぼーん<>あぼーん<>あぼーん<>\n")
	(setq i (1+ i)))
      (insert (format "%s<>%s<>%s%s<>%s<>%s\n"
		      name
		      (or mail "")
		      date
		      (if (= 0 (length id)) "" (concat " ID:" id))
		      contents
		      (or subject "")))
      (setq i (1+ i))
      (setq beg (point)))))

(defconst navi2ch-js-url-regexp
  ;;    prefix   カテゴリ     BBS番号
  "\\`\\(.+\\)/\\([^/]+\\)/\\([^/]+\\)/\\'")

(defun navi2ch-js-get-cgi-url (string board)
  "STRING.cgi の url を返す。"
  (let ((uri (navi2ch-board-get-uri board)))
    (and (string-match navi2ch-js-url-regexp uri)
	 (format "%s/bbs/%s.cgi/%s/%s/"
		 (match-string 1 uri)
		 string
		 (match-string 2 uri)
		 (match-string 3 uri)))))

(defun navi2ch-js-get-dir (board)
  "write.cgi に渡す DIR パラメータを返す。"
  (let ((uri (navi2ch-board-get-uri board)))
    (and (string-match navi2ch-js-url-regexp uri)
	 (match-string 2 uri))))

(defun navi2ch-js-board-update (board)
  (let ((url (navi2ch-board-get-url board))
	(file (navi2ch-board-get-file-name board))
	(time (cdr (assq 'time board)))
	(func (navi2ch-multibbs-subject-callback board)))
    (navi2ch-net-update-file url file time func)))

;;; navi2ch-jbbs-shitaraba.el ends here
