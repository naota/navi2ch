;;; navi2ch-multibbs.el --- View 2ch like BBS module for Navi2ch.

;; Copyright (C) 2002, 2003, 2004 by Navi2ch Project

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
(provide 'navi2ch-multibbs)
(defconst navi2ch-multibbs-ident
  "$Id$")

(require 'navi2ch-http-date)
(require 'navi2ch)

(defvar navi2ch-multibbs-func-alist nil
  "BBS の種類と関数群の alist。
各要素は
\(BBSTYPE . FUNC-ALIST)
BBSTYPE: BBS の種類を表すシンボル。
FUNC-ALIST: その BBS での動作を指定する関数群。

FUNC-ALIST は以下の通り
\((bbs-p			. BBS-P-FUNC)
 (subject-callback	. SUBJECT-CALLBACK-FUNC)
 (article-update	. ARTICLE-UPDATE-FUNC)
 (article-to-url	. ARTICLE-TO-URL-FUNC)
 (url-to-board		. URL-TO-BOARD-FUNC)
 (url-to-article	. URL-TO-ARTICLE-FUNC)
 (send-message		. SEND-MESSAGE-FUNC)
 (send-success-p	. SEND-MESSAGE-SUCCESS-P-FUNC)
 (error-string		. ERROR-STRING-FUNC)
 (board-update		. BOARD-UPDATE-FUNC)
 (board-get-file-name	. BOARD-GET-FILE-NAME-FUNC))

BBS-P-FUNC(URI):
    URI がその BBS のものならば non-nil を返す。

SUBJECT-CALLBACK-FUNC():
    subject.txt を取得するときに navi2ch-net-update-file で使われるコー
    ルバック関数

ARTICLE-UPDATE-FUNC(BOARD ARTICLE START):
    BOARD ARTICLE で表されるファイルを更新する。
    START が non-nil ならばレス番号 START からの差分を取得する。

ARTICLE-TO-URL-FUNC(BOARD ARTICLE
		    &OPTIONAL START END NOFIRST):
    BOARD, ARTICLE から url に変換する。

URL-TO-BOARD-FUNC(URL):
URL から board に変換する。

URL-TO-ARTICLE-FUNC(URL):
URL から article に変換する。

SEND-MESSAGE-FUNC(FROM MAIL MESSAGE
		  SUBJECT BBS KEY TIME BOARD ARTICLE):
    MESSAGE を送信する。

SEND-MESSAGE-SUCCESS-P-FUNC(PROC):
    PROC の送信セッションが成功していれば non-nil を、
    失敗したら nil を、再試行可能な失敗なら 'retry を返す。

ERROR-STRING-FUNC(PROC):
    PROC の送信セッションが失敗したときのエラーメッセージを返す。

BOARD-UPDATE-FUNC(BOARD):
    BOARD で表されるファイルを更新する。

BOARD-GET-FILE-NAME-FUNC(BOARD &optional FILE-NAME)
    BOARD の情報を保存するディレクトリを基準として、FILE-NAME の
    絶対パスを返す。
")

(defvar navi2ch-multibbs-variable-alist nil
  "BBS の種類と変数群の alist。
各要素は
\(BBSTYPE . FUNC-ALIST)
BBSTYPE: BBS の種類を表すシンボル。
VARIABLE-ALIST: その BBS の設定を指定する変数群。

VARIABLE-ALIST は以下の通り
\((coding-system		. CODING-SYSTEM-VAR))

CODING-SYSTEM-VAR:
    その BBS のファイルの文字コード
")


(defun navi2ch-multibbs-get-bbstype-subr (uri list)
  (if list
      (let ((bbstype    (caar list))
	    (func       (cdr (assq 'bbs-p (cdar list)))))
	(if (and func (funcall func uri))
	    bbstype
	  (navi2ch-multibbs-get-bbstype-subr uri (cdr list))))))

(defun navi2ch-multibbs-set-bbstype (board type)
  (when (consp board)
      (setcdr board
	      (cons (cons 'bbstype type) (cdr board)))))

(defun navi2ch-multibbs-get-bbstype (board)
  (let ((type (cdr (assq 'bbstype board))))
    (unless type
      (setq type (navi2ch-multibbs-url-to-bbstype
		  (cdr (assq 'uri board))))
      (navi2ch-multibbs-set-bbstype board type))
    type))

(defun navi2ch-multibbs-subject-callback (board)
  (navi2ch-multibbs-get-func
   (navi2ch-multibbs-get-bbstype board)
   'subject-callback 'navi2ch-2ch-subject-callback))

(defmacro navi2ch-multibbs-defcallback (name spec &rest body)
  "navi2ch-net-update-file に渡す callback を定義する。
SPEC は (BBSTYPE [ARG]...)。
実際には、callback を定義するのに必要な BBSTYPE な板の coding-system
による decode, encode 処理を BODY を評価する前後に行なう、NAME という
引数 [ARG]... を持つ関数が定義される。"
  (let ((bbstype (gensym "--bbstype--"))
	(decoding (gensym "--decoding--"))
	docstring)
    (when (stringp (car body))
	  (setq docstring (car body))
	  (setq body (cdr body)))
    `(defun ,name (,@(cdr spec))
       ,docstring
       (let* ((coding-system-for-read 'binary)
	      (coding-system-for-write 'binary)
	      (,bbstype ',(car spec))
	      (,decoding (navi2ch-multibbs-get-variable
			  ,bbstype 'coding-system
			  navi2ch-coding-system)))
	 (decode-coding-region (point-min) (point-max)
			       ,decoding)
	 (navi2ch-set-buffer-multibyte t)
	 ,@body
	 (encode-coding-region (point-min) (point-max)
			       navi2ch-coding-system)
	 (navi2ch-set-buffer-multibyte nil)))))
(put 'navi2ch-multibbs-defcallback 'lisp-indent-function 2)

(defun navi2ch-multibbs-article-update (board article start)
  (let* ((bbstype (navi2ch-multibbs-get-bbstype board))
	 (func    (navi2ch-multibbs-get-func
		   bbstype 'article-update 'navi2ch-2ch-article-update)))
    (funcall func board article start)))

(defun navi2ch-multibbs-regist (bbstype func-alist variable-alist)
  (setq navi2ch-multibbs-func-alist
	(cons (cons bbstype func-alist)
	      navi2ch-multibbs-func-alist))
  (setq navi2ch-multibbs-variable-alist
	(cons (cons bbstype variable-alist)
	      navi2ch-multibbs-variable-alist)))

(defsubst navi2ch-multibbs-get-func-from-board
  (board func &optional default-func)
  (navi2ch-multibbs-get-func
   (navi2ch-multibbs-get-bbstype board)
   func default-func))

(defun navi2ch-multibbs-get-func (bbstype func &optional default-func)
  (or (cdr (assq func
		 (cdr (assq bbstype
			    navi2ch-multibbs-func-alist))))
      default-func))

(defun navi2ch-multibbs-get-variable
  (bbstype variable &optional default-value)
  (or (cdr (assq variable
		 (cdr (assq bbstype
			    navi2ch-multibbs-variable-alist))))
      default-value))

(defun navi2ch-multibbs-url-to-bbstype (url)
  (or
   (and url
	(navi2ch-multibbs-get-bbstype-subr url navi2ch-multibbs-func-alist))
   'unknown))

(defun navi2ch-multibbs-url-to-article (url)
  (let* ((bbstype (navi2ch-multibbs-url-to-bbstype url))
	 (func    (navi2ch-multibbs-get-func
		   bbstype 'url-to-article 'navi2ch-2ch-url-to-article)))
    (funcall func url)))

(defun navi2ch-multibbs-url-to-board (url)
  (let* ((bbstype (navi2ch-multibbs-url-to-bbstype url))
	 (func    (navi2ch-multibbs-get-func
		   bbstype 'url-to-board 'navi2ch-2ch-url-to-board)))
    (funcall func url)))

(defun navi2ch-multibbs-article-to-url
  (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (let ((func (navi2ch-multibbs-get-func-from-board
	       board 'article-to-url 'navi2ch-2ch-article-to-url)))
    (funcall func board article start end nofirst)))

(defun navi2ch-multibbs-get-message-time-field ()
  (if (stringp navi2ch-net-last-date)
      (navi2ch-http-date-decode navi2ch-net-last-date)
    (let* ((now (current-time))
	   (lag 300)			; ずらす秒数
	   (h (nth 0 now))
	   (l (- (nth 1 now) lag)))
      (when (< l 0)
	(setq l (+ l 65536)
	      h (- h 0)))
      (cons h l))))

(defun navi2ch-multibbs-send-message
  (from mail message subject board article)
  (let* ((bbstype      (navi2ch-multibbs-get-bbstype board))
	 (send         (navi2ch-multibbs-get-func
			bbstype 'send-message 'navi2ch-2ch-send-message))
	 (success-p    (navi2ch-multibbs-get-func
			bbstype 'send-success-p
			'navi2ch-2ch-send-message-success-p))
	 (error-string (navi2ch-multibbs-get-func
			bbstype 'error-string
			'navi2ch-2ch-send-message-error-string))
	 (bbs          (let ((uri (navi2ch-board-get-uri board)))
			 (string-match "\\([^/]+\\)/$" uri)
			 (match-string 1 uri)))
	 (key          (cdr (assq 'artid article)))
	 (time         (format-time-string
			"%s" (navi2ch-multibbs-get-message-time-field)))
	 (navi2ch-net-http-proxy (and navi2ch-net-send-message-use-http-proxy
				      (or navi2ch-net-http-proxy-for-send-message
					  navi2ch-net-http-proxy)))
	 (navi2ch-net-http-proxy-userid (if navi2ch-net-http-proxy-for-send-message
					    navi2ch-net-http-proxy-userid-for-send-message
					  navi2ch-net-http-proxy-userid))
	 (navi2ch-net-http-proxy-password (if navi2ch-net-http-proxy-for-send-message
					      navi2ch-net-http-proxy-password-for-send-message
					    navi2ch-net-http-proxy-password))
	 (tries 2)	; 送信試行の最大回数
	 (message-str "send message...")
	 (result 'retry))
    (dotimes (i tries)
      (let ((proc (funcall send from mail message subject bbs key time
			   board article)))
	(message message-str)
	(setq result (funcall success-p proc))
	(cond ((eq result 'retry)
	       (save-window-excursion
		 (with-temp-buffer
		   (insert (decode-coding-string
			    (navi2ch-net-get-content proc)
			    navi2ch-coding-system))
		   (navi2ch-replace-html-tag-with-buffer)
		   (goto-char (point-min))
		   (while (re-search-forward "[ \t]*\n\\([ \t]*\n\\)*" nil t)
		     (replace-match "\n"))
		   (delete-other-windows)
		   (switch-to-buffer (current-buffer))
		   (unless (yes-or-no-p "Retry? ")
		     (navi2ch-board-save-spid board nil)
		     (return nil))))
	       (sit-for navi2ch-message-retry-wait-time)
	       (setq message-str "re-send message..."))
	      (result
	       (message (concat message-str "succeed"))
	       (return result))
	      (t
	       (let ((err (funcall error-string proc)))
		 (if (stringp err)
		     (message (concat message-str "failed: %s") err)
		   (message (concat message-str "failed"))))
	       (return nil)))))))

(defun navi2ch-multibbs-board-update (board)
  (let ((func (navi2ch-multibbs-get-func-from-board
	       board 'board-update 'navi2ch-2ch-board-update)))
    (funcall func board)))

(defun navi2ch-multibbs-board-get-file-name (board &optional file-name)
  (let ((func (navi2ch-multibbs-get-func-from-board
	       board 'board-get-file-name 'navi2ch-2ch-board-get-file-name)))
    (funcall func board file-name)))

;;;-----------------------------------------------

(defun navi2ch-2ch-subject-callback ()
  (when navi2ch-board-use-subback-html
    (navi2ch-board-make-subject-txt)))

(defun navi2ch-2ch-article-update (board article start)
  "BOARD, ARTICLE に対応するファイルを更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	url header kako-p)
    (if (and (navi2ch-enable-readcgi-p
	      (navi2ch-board-get-host board)))
	(progn
	  (setq url (navi2ch-article-get-readcgi-raw-url board article start))
	  (setq header (navi2ch-net-update-file-with-readcgi url file
							     time start))
	  ;; read.cgi を使っててエラーになるのはホントにエラーのとき
	  (setq kako-p (navi2ch-net-get-state 'kako header)))
      (setq url (navi2ch-article-get-url board article))
      (setq header (if start
		       (navi2ch-net-update-file-diff url file time)
		     (navi2ch-net-update-file url file time)))
      (setq kako-p (navi2ch-net-get-state 'error header)))
    (when kako-p
      (setq url (navi2ch-article-get-kako-url board article))
      (setq header (navi2ch-net-update-file url file))
      (unless (navi2ch-net-get-state 'error header)
	(setq header (navi2ch-net-add-state 'kako header))))
    header))

(defun navi2ch-2ch-url-to-board (url)
  (let (host id)
    (cond ((or (string-match
		"http://\\(.+\\)/test/\\(read\\.cgi\\|r\\.i\\).*bbs=\\([^&]+\\)" url)
	       (string-match
		"http://\\(.+\\)/test/\\(read\\.cgi\\|r\\.i\\)/\\([^/]+\\)/" url))
	   (setq host (match-string 1 url)
		 id (match-string 3 url)))
	  ((or (string-match
		"http://\\(.+\\)/\\([^/]+\\)/kako/[0-9]+/" url)
	       (string-match
		"http://\\(.+\\)/\\([^/]+\\)/i/" url)
	       (string-match
		"http://\\(.+\\)/\\([^/]+\\)" url))
	   (setq host (match-string 1 url)
		 id (match-string 2 url))))
    (when id
      (list (cons 'uri (format "http://%s/%s/" host id))
	    (cons 'id id)))))

(defun navi2ch-2ch-url-to-article (url)
  "URL から article に変換。"
  (let (artid number kako)
    (cond ((string-match
	    "http://.+/test/read\\.cgi.*&key=\\([0-9]+\\)" url)
	   (setq artid (match-string 1 url))
	   (when (string-match "&st=\\([0-9]+\\)" url)
	     (setq number (string-to-number (match-string 1 url)))))
	  ;; http://pc.2ch.net/test/read.cgi/unix/1065246418/ とか。
	  ((string-match
	    "http://.+/test/\\(read\\.cgi\\|r\\.i\\)/[^/]+/\\([^/]+\\)" url)
	   (setq artid (match-string 2 url))
	   (when (string-match
		  "http://.+/test/\\(read\\.cgi\\|r\\.i\\)/[^/]+/[^/]+/[ni.]?\\([0-9]+\\)[^/]*$" url)
	     (setq number (string-to-number (match-string 2 url)))))
	  ;; "http://pc.2ch.net/unix/kako/999/999166513.html" とか。
	  ;; "http://pc.2ch.net/unix/kako/1009/10093/1009340234.html" とか。
	  ((or (string-match
		"http://.+/kako/[0-9]+/\\([0-9]+\\)\\.\\(dat\\|html\\)" url)
	       (string-match
		"http://.+/kako/[0-9]+/[0-9]+/\\([0-9]+\\)\\.\\(dat\\|html\\)" url))
	   (setq artid (match-string 1 url))
	   (setq kako t))
	  ((string-match
	    "http://.+/\\([0-9]+\\)\\.\\(dat\\|html\\)" url)
	   (setq artid (match-string 1 url))))
    (let (list)
      (when artid
	(setq list (cons (cons 'artid artid) list))
	(when number
	  (setq list (cons (cons 'number number) list)))
	(when kako
	  (setq list (cons (cons 'kako kako) list)))
	list))))

(defun navi2ch-2ch-send-message
  (from mail message subject bbs key time board article)
  (let ((url         (navi2ch-board-get-bbscgi-url board))
	(referer     (navi2ch-board-get-uri board))
	(spid        (navi2ch-board-load-spid board))
	(param-alist (list
		      (cons "submit" "書き込む")
		      (cons "FROM"   (or from ""))
		      (cons "mail"   (or mail ""))
		      (cons "bbs"    bbs)
		      (cons "time"   time)
		      (cons "MESSAGE" message)
		      (if subject
			  (cons "subject" subject)
			(cons "key"    key)))))
    (setq spid
	  (when (and (consp spid)
		     (navi2ch-compare-times (cdr spid) (current-time)))
	    (car spid)))
    (let ((proc
	   (navi2ch-net-send-request
	    url "POST"
	    (list (cons "Content-Type" "application/x-www-form-urlencoded")
		  (cons "Cookie" (concat "NAME=" from "; MAIL=" mail
					 (if spid (concat "; SPID=" spid
							  "; PON=" spid))))
		  (cons "Referer" referer))
	    (navi2ch-net-get-param-string param-alist))))
      (setq spid (navi2ch-net-send-message-get-spid proc))
      (if spid (navi2ch-board-save-spid board spid))
      proc)))

(defun navi2ch-2ch-article-to-url
  (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (let ((url (navi2ch-board-get-readcgi-url board)))
    (setq url (concat url (cdr (assq 'artid article)) "/"))
    (if (numberp start)
	(setq start (number-to-string start)))
    (if (numberp end)
	(setq end (number-to-string end)))
    (if (equal start end)
	(concat url start)
      (concat url
	      start (and (or start end) "-") end
	      (and nofirst "n")))))

(defalias 'navi2ch-2ch-send-message-success-p
  'navi2ch-net-send-message-success-p)
(defalias 'navi2ch-2ch-send-message-error-string
  'navi2ch-net-send-message-error-string)

(defun navi2ch-2ch-board-update (board)
  (let ((file (navi2ch-board-get-file-name board))
	(time (cdr (assq 'time board))))
    (if navi2ch-board-enable-readcgi
	(navi2ch-net-update-file-with-readcgi
	 (navi2ch-board-get-readcgi-raw-url board) file time)
      (let ((url (navi2ch-board-get-url
		  board (if navi2ch-board-use-subback-html
			    navi2ch-board-subback-file-name)))
	    (func (navi2ch-multibbs-subject-callback board)))
	(navi2ch-net-update-file url file time func)))))

(defun navi2ch-2ch-board-get-file-name (board &optional file-name)
  (let ((uri (navi2ch-board-get-uri board)))
    (when uri
      (cond ((string-match "http://\\(.+\\)" uri)
	     (navi2ch-expand-file-name
	      (concat (match-string 1 uri)
		      (or file-name navi2ch-board-subject-file-name))))
	    ((string-match "file://\\(.+\\)" uri)
	     (expand-file-name (or file-name
				   navi2ch-board-subject-file-name)
			       (match-string 1 uri)))))))

;;; navi2ch-multibbs.el ends here
