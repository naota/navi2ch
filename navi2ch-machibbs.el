;;; navi2ch-machibbs.el --- View machiBBS module for Navi2ch.

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
(provide 'navi2ch-machibbs)
(defvar navi2ch-machibbs-ident
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
    (error-string   	. navi2ch-machibbs-navi2ch-net-get-content)))

(defvar navi2ch-machibbs-variable-alist
  '((coding-system	. shift_jis)))

(navi2ch-multibbs-regist 'machibbs
			 navi2ch-machibbs-func-alist
			 navi2ch-machibbs-variable-alist)

;; (defvar navi2ch-machibbs-subject-max-bytes 5000
;;   "スレの一覧をどれだけ表示するか。
;; 0の場合は全て表示する。")

;;-------------

(defun navi2ch-machibbs-p (uri)
  "URI が machibbs なら non-nilを返す。"
  (string-match "http://[^\\.]+\\.machibbs\\.com/" uri))

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

(defun navi2ch-machibbs-article-update (board article)
  "BOARD ARTICLEの記事を更新する。"
  (let ((file (navi2ch-article-get-file-name board article))
        (time (cdr (assq 'time article)))
        (url  (navi2ch-machibbs-article-to-url board article))
        (func 'navi2ch-machibbs-article-callback)
        ret)
    (setq ret (navi2ch-net-update-file url file time func))
    (list ret nil)))

(defun navi2ch-machibbs-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"	; 効かなかったら教えてください。
  (let ((uri   (cdr (assq 'uri board)))
	(artid (cdr (assq 'artid article))))
    (string-match "\\(.*\\)\\/\\([^/]*\\)\\/" uri) ; \\/ --> / ?
    (concat
     (format "%s/bbs/read.pl?BBS=%s&KEY=%s"
	     (match-string 1 uri) (match-string 2 uri) artid)
     (and start (format "&START=%d" start))
     (and end (format "&END=%d" end))
     (and nofirst
	  (not (eq start 1))
	  "&NOFIRST=TRUE"))))

(defun navi2ch-machibbs-url-to-board (url)
  "urlから BOARDに変換。"
  (cond ((string-match
	  "http://www\\.machibbs\\.com/[^/]+/bbs/read\\.cgi.*BBS=\\([^&]+\\)"
	  url)
	 (list (cons 'uri (format "http://%s.machibbs.com/%s/"
				  (match-string 1 url)
				  (match-string 1 url)))
	       (cons 'id  (match-string 1 url))))
	((string-match
	  "\\(http://[^\\.]+\\.machibbs\\.com\\)/bbs/read\\.\\(pl\\|cgi\\).*BBS=\\([^&]+\\)"
	  url)
	 (list (cons 'uri (format "%s/%s/" (match-string 1 url)
				  (match-string 3 url)))
	       (cons 'id  (match-string 3 url))))
	((string-match
	  "\\(http://[^\\.]+\\.machibbs\\.com/\\([^/]+\\)/\\)" url)
	 (list (cons 'uri (match-string 1 url))
	       (cons 'id  (match-string 2 url))))))

(defun navi2ch-machibbs-url-to-article (url)
  (cond ((string-match
	  "http://www.machibbs.com/[^/]+/bbs/read\\.cgi.*KEY=\\([0-9]+\\)" url)
	 (list (cons 'artid (match-string 1 url))))
	((string-match
	  "http://[^\\.]+\\.machibbs\\.com/bbs/read\\.\\(pl\\|cgi\\).*KEY=\\([0-9]+\\)"
	  url)
	 (list (cons 'artid (match-string 2 url))))))

(defun navi2ch-machibbs-send-message
  (from mail message subject bbs key time board article)
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
    (re-search-forward navi2ch-machibbs-parse-subject-regexp nil t)
    (match-string 1)))

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

(navi2ch-multibbs-defcallback navi2ch-machibbs-article-callback (machibbs)
  (let ((beg (point))
	(max-num 0)
	subject alist num)
    (setq subject (navi2ch-machibbs-parse-subject))
    (while (navi2ch-machibbs-parse)
      (setq num (string-to-number (match-string 1))
	    max-num (max max-num num)
	    alist (cons (cons (string-to-number (match-string 1))
			      (navi2ch-machibbs-make-article subject))
			alist)
	    subject nil))
    (delete-region beg (point-max))
    (dotimes (i max-num)
      (insert (or (cdr (assoc (1+ i) alist))
		  "あぼーん<>あぼーん<>あぼーん<>あぼーん<>\n")))))
;;; navi2ch-machibbs.el ends here
