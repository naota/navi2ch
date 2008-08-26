;;; navi2ch-multibbs.el --- View 2ch like BBS module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2008 by Navi2ch Project

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

(eval-when-compile (require 'cl))
(require 'navi2ch-http-date)
(require 'navi2ch)
(require 'navi2ch-be2ch)

(defvar navi2ch-multibbs-func-table nil
  "BBS の種類と関数群の hash。
BBSTYPE を key に FUNC-TABLE が格納される。
BBSTYPE: BBS の種類を表すシンボル。
FUNC-TABLE: その BBS での動作を指定する関数群。

FUNC-TABLE は以下の左側のシンボルを key に
関数が格納される。
bbs-p			BBS-P-FUNC
subject-callback	SUBJECT-CALLBACK-FUNC
article-update	ARTICLE-UPDATE-FUNC
article-to-url	ARTICLE-TO-URL-FUNC
url-to-board		URL-TO-BOARD-FUNC
url-to-article	URL-TO-ARTICLE-FUNC
send-message		SEND-MESSAGE-FUNC
extract-post          EXTRACT-POST-FUNC
send-success-p	SEND-MESSAGE-SUCCESS-P-FUNC
error-string		ERROR-STRING-FUNC
board-update		BOARD-UPDATE-FUNC
board-get-file-name	BOARD-GET-FILE-NAME-FUNC

BBS-P-FUNC(URI):
    URI がその BBS のものならば non-nil を返す。

SUBJECT-CALLBACK-FUNC():
    subject.txt を取得するときに `navi2ch-net-update-file' で使われるコー
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
		  SUBJECT BBS KEY TIME BOARD ARTICLE POST):
    MESSAGE を送信する。

EXTRACT-POST-FUNC(OLD-POST BUFFER):
    MESSAGE の再送信に使う情報を取り出す。取り出したデータを
    返り値として返すと、再送のためにSEND-MESSAGE-FUNCを呼び出す
    ときに、その値がPOST引数に束縛されます。
     
    BUFFERには、SEND-MESSAGE-FUNCが返したPROCから取り出したコン
    テンツをデコードしたものが挿入されています。再送を繰り返す
    場合、最後にEXTRACT-POST-FUNCが返した値がOLD-POSTに束縛され
    ています。

SEND-MESSAGE-SUCCESS-P-FUNC(PROC):
    PROC の送信セッションが成功していれば non-nil を、
    失敗したら nil を、再試行可能な失敗なら 'retry を返す。

ERROR-STRING-FUNC(PROC):
    PROC の送信セッションが失敗したときのエラーメッセージを返す。

BOARD-UPDATE-FUNC(BOARD):
    BOARD で表されるファイルを更新する。

BOARD-GET-FILE-NAME-FUNC(BOARD &optional FILE-NAME)
    BOARD の情報を保存するディレクトリを基準として、FILE-NAME の
    絶対パスを返す。")

(defvar navi2ch-multibbs-variable-alist nil
  "BBS の種類と変数群の alist。
各要素は
\(BBSTYPE . FUNC-ALIST)
BBSTYPE: BBS の種類を表すシンボル。
VARIABLE-ALIST: その BBS の設定を指定する変数群。

VARIABLE-ALIST は以下の通り
\((coding-system		. CODING-SYSTEM-VAR))

CODING-SYSTEM-VAR:
    その BBS のファイルの文字コード")

(defvar navi2ch-2ch-board-file-name-cache nil)
(defvar navi2ch-2ch-board-file-name-cache-limit 1000)

(defun navi2ch-multibbs-get-bbstype-subr (uri table)
  (when (hash-table-p table)
    (catch 'loop
      (maphash (lambda (type func-table)
		 (let ((func (gethash 'bbs-p func-table)))
		   (when (and func (funcall func uri))
		     (throw 'loop type))))
	       table))))

(defun navi2ch-multibbs-set-bbstype (board type)
  (when (consp board)
    (setcdr board
	    (cons (cons 'bbstype type) (cdr board)))))

(defsubst navi2ch-multibbs-get-bbstype (board)
  (let ((type (cdr (assq 'bbstype board))))
    (unless type
      (setq type (navi2ch-multibbs-url-to-bbstype
		  (cdr (assq 'uri board))))
      (navi2ch-multibbs-set-bbstype board type))
    type))

(defsubst navi2ch-multibbs-get-func (bbstype func &optional default-func)
  (let ((func-table (gethash bbstype navi2ch-multibbs-func-table)))
    (or (and func-table
	     (gethash func func-table))
	default-func)))

(defun navi2ch-multibbs-subject-callback (board)
  (navi2ch-multibbs-get-func
   (navi2ch-multibbs-get-bbstype board)
   'subject-callback 'navi2ch-2ch-subject-callback))

(defmacro navi2ch-multibbs-defcallback (name spec &rest body)
  "`navi2ch-net-update-file' に渡す callback を定義する。
SPEC は (BBSTYPE [ARG]...)。
実際には、callback を定義するのに必要な BBSTYPE な板の coding-system
による decode, encode 処理を BODY を評価する前後に行なう、NAME という
引数 [ARG]... を持つ関数が定義される。"
  (let ((bbstype (make-symbol "--bbstype--"))
	(decoding (make-symbol "--decoding--"))
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
	 (navi2ch-ifxemacs
	     (navi2ch-decode-coding-region-linewise (point-min) (point-max) ,decoding)
	   (decode-coding-region (point-min) (point-max) ,decoding))
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
  (unless navi2ch-multibbs-func-table
    (setq navi2ch-multibbs-func-table
	  (make-hash-table :size 6))) ;FIXME: 6 でたりると思うけどどうすかね。
  (puthash bbstype
	   (navi2ch-alist-to-hash func-alist)
	   navi2ch-multibbs-func-table)
  (setq navi2ch-multibbs-variable-alist
	(cons (cons bbstype variable-alist)
	      navi2ch-multibbs-variable-alist)))

(defsubst navi2ch-multibbs-get-func-from-board
  (board func &optional default-func)
  (navi2ch-multibbs-get-func
   (navi2ch-multibbs-get-bbstype board)
   func default-func))

(defun navi2ch-multibbs-get-variable
  (bbstype variable &optional default-value)
  (or (cdr (assq variable
		 (cdr (assq bbstype
			    navi2ch-multibbs-variable-alist))))
      default-value))

(defun navi2ch-multibbs-url-to-bbstype (url)
  (or
   (and url
	(navi2ch-multibbs-get-bbstype-subr url navi2ch-multibbs-func-table))
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

(defun navi2ch-multibbs-send-message-error-string (board proc)
  (let* ((func (navi2ch-multibbs-get-func
		(navi2ch-multibbs-get-bbstype board)
		'error-string
		'navi2ch-2ch-send-message-error-string))
	 (err (funcall func proc)))
    (or err
	(let ((status (and proc (navi2ch-net-get-status proc))))
	  (when status
	    (concat "HTTP status: " status))))))

(defun navi2ch-multibbs-send-message
  (from mail message subject board article)
  (let* ((bbstype      (navi2ch-multibbs-get-bbstype board))
	 (send         (navi2ch-multibbs-get-func
			bbstype 'send-message 'navi2ch-2ch-send-message))
	 (extract-post (navi2ch-multibbs-get-func
			bbstype 'extract-post 'navi2ch-2ch-extract-post))
	 (success-p    (navi2ch-multibbs-get-func
			bbstype 'send-success-p
			'navi2ch-2ch-send-message-success-p))
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
	 (tries 2)			; 送信試行の最大回数
	 (message-str "send message...")
	 (result 'retry)
	 (post-data nil))
    (dotimes (i tries)
      (let ((proc (funcall send from mail message subject bbs key time
			   board article post-data)))
	(message message-str)
	(setq result (funcall success-p proc))
	(cond ((eq result 'retry)
	       (save-window-excursion
		 (with-temp-buffer
		   (insert (decode-coding-string
			    (navi2ch-net-get-content proc)
			    (navi2ch-board-get-coding-system board)))
		   (setq post-data (funcall extract-post post-data (current-buffer)))
		   (navi2ch-replace-html-tag-with-buffer)
		   (goto-char (point-min))
		   (while (re-search-forward "[ \t]*\n\\([ \t]*\n\\)*" nil t)
		     (replace-match "\n"))
		   (delete-other-windows)
		   (switch-to-buffer (current-buffer))
		   (unless (y-or-n-p "Retry? ")
		     (return nil))))
	       (sit-for navi2ch-message-retry-wait-time)
	       (setq message-str "re-send message..."))
	      (result
	       (message (concat message-str "succeed"))
	       (return result))
	      (t
	       (let ((err (navi2ch-multibbs-send-message-error-string board proc)))
		 (if (stringp err)
		     (message (concat message-str "failed: %s") err)
		   (message (concat message-str "failed"))))
	       (return nil)))))))

(defsubst navi2ch-multibbs-board-update (board)
  (let ((func (navi2ch-multibbs-get-func-from-board
	       board 'board-update 'navi2ch-2ch-board-update)))
    (funcall func board)))

(defsubst navi2ch-multibbs-board-get-file-name (board &optional file-name)
  (let ((func (navi2ch-multibbs-get-func-from-board
	       board 'board-get-file-name 'navi2ch-2ch-board-get-file-name)))
    (funcall func board file-name)))

;;;-----------------------------------------------

(defsubst navi2ch-2ch-subject-callback ()
  (when navi2ch-board-use-subback-html
    (navi2ch-board-make-subject-txt)))

(defun navi2ch-2ch-article-update (board article start)
  "BOARD, ARTICLE に対応するファイルを更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	url header kako-p)
    (setq url (navi2ch-article-get-url board article))
    (setq header (if start
		     (navi2ch-net-update-file-diff url file time)
		   (navi2ch-net-update-file url file time)))
    (setq kako-p (navi2ch-net-get-state 'error header))
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

(defvar navi2ch-2ch-send-message-last-board nil)

(defun navi2ch-2ch-send-message
  (from mail message subject bbs key time board article &optional post)
  (when (navi2ch-message-samba24-check board)
    (let* ((url         (navi2ch-board-get-bbscgi-url board))
	   (referer     (navi2ch-board-get-uri board))
	   (param-alist (list
			 (cons "submit" "書き込む")
			 (cons "FROM"   (or from ""))
			 (cons "mail"   (or mail ""))
			 (cons "bbs"    bbs)
			 (cons "time"   time)
			 (cons "MESSAGE" message)
			 (if subject
			     (cons "subject" subject)
			   (cons "key"    key))))
	   (coding-system (navi2ch-board-get-coding-system board))
	   (cookies (navi2ch-net-match-cookies url)))
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

(defun navi2ch-2ch-article-to-url
  (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (let ((uri (navi2ch-board-get-uri board))
	(start (if (numberp start)
		   (number-to-string start)
		 start))
	(end (if (numberp end)
		 (number-to-string end)
	       end)))
    (if (string-match "\\(.+\\)/\\([^/]+\\)/$" uri)
	(format "%s/test/read.cgi/%s/%s/%s"
		(match-string 1 uri) (match-string 2 uri)
		(cdr (assq 'artid article))
		(if (equal start end)
		    (or start "")
		  (concat start (and (or start end) "-") end
			  (and nofirst "n")))))))

(defun navi2ch-2ch-send-message-success-p (proc)
  (navi2ch-net-send-message-success-p
   proc
   (navi2ch-board-get-coding-system
    navi2ch-2ch-send-message-last-board)))

(defun navi2ch-2ch-send-message-error-string (proc)
  (navi2ch-net-send-message-error-string
   proc
   (navi2ch-board-get-coding-system
    navi2ch-2ch-send-message-last-board)))

(defun navi2ch-2ch-board-update (board)
  (let ((file (navi2ch-board-get-file-name board))
	(time (cdr (assq 'time board))))
    (let ((url (navi2ch-board-get-url
		board (if navi2ch-board-use-subback-html
			  navi2ch-board-subback-file-name)))
	  (func (navi2ch-multibbs-subject-callback board)))
      (navi2ch-net-update-file url file time func))))

(defun navi2ch-2ch-board-get-file-name (board &optional file-name)
  (let ((uri (navi2ch-board-get-uri board))
	(file-name (or file-name
		       navi2ch-board-subject-file-name)))
    (when uri
      (or navi2ch-2ch-board-file-name-cache
	  (setq navi2ch-2ch-board-file-name-cache
		(navi2ch-make-cache navi2ch-2ch-board-file-name-cache-limit
				    'equal)))
      (navi2ch-cache-get
       (cons uri file-name)
       (cond ((string-match "http://\\(.+\\)" uri)
	      (navi2ch-expand-file-name
	       (concat (match-string 1 uri)
		       file-name)))
	     ((string-match "file://\\(.+\\)" uri)
	      (expand-file-name file-name
				(match-string 1 uri))))
       navi2ch-2ch-board-file-name-cache))))

(defun navi2ch-2ch-extract-post (old-post buffer)
  ;; Get hana and mogera from following string.
  ;; <input type=hidden name="hana" value="mogera">  
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
	(goto-char (point-min))
	(let ((case-fold-search t)
	      (re "\\<%s=\\(\"\\([^\"]*\\)\"\\|[^\"> \r\n\t]*\\)")
	      r)
	  (while (re-search-forward "<input\\>[^>]+>" nil t)
	    (let ((str (match-string 0)) name value)
	      (and (string-match (format re "name") str)
		   (setq name (or (match-string 2 str)
				  (match-string 1 str)))
		   (string-match (format re "value") str)
		   (setq value (or (match-string 2 str)
				   (match-string 1 str)))
		   (setq name (navi2ch-replace-html-tag name)
			 value (navi2ch-replace-html-tag value))
		   (push (cons name value) r))))
	  (nreverse r))))))

;;; navi2ch-multibbs.el ends here
