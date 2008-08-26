;;; navi2ch-megabbs.el --- View megabbs.net module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2004, 2006 by Navi2ch Project

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


;;; ----------例----------
;;; 普通に閲覧したとき
;;; http://www.megabbs.com/egame/index.html
;;;
;;; スレ一覧の全て (過去ログメニュー)
;;; http://www.megabbs.com/cgi-bin/readtitle.cgi?bo=egame&br=off
;;; html で落ちてくる．`br' は <br> の有無[on|off]．
;;;
;;; http://www.megabbs.com/egame/egame_newb.txt
;;; text で落ちてくる．1 行目と最後の行は不要．
;;;
;;; スレ (レスを全部見る)
;;; http://www.megabbs.com/cgi-bin/readres.cgi?bo=egame&vi=1080276946
;;; html で落ちてくる．
;;;
;;; スレ (最新 100 件)
;;; http://www.megabbs.com/cgi-bin/readres.cgi?bo=egame&vi=1080276946&rm=100
;;; html で落ちてくる．
;;;
;;; readres.cgi にて，>>1を除去
;;; fi=no
;;;
;;; readres.cgi にて，>>30のみ
;;; res=30

;;; Code:
(provide 'navi2ch-megabbs)
(defconst navi2ch-megabbs-ident
  "$Id$")

(eval-when-compile (require 'cl))
(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(defvar navi2ch-megabbs-func-alist
  '((bbs-p               . navi2ch-megabbs-p)
    (subject-callback    . navi2ch-megabbs-subject-callback)
    (article-update      . navi2ch-megabbs-article-update)
    (article-to-url      . navi2ch-megabbs-article-to-url)
    (url-to-board        . navi2ch-megabbs-url-to-board)
    (url-to-article      . navi2ch-megabbs-url-to-article)
    (send-message        . navi2ch-megabbs-send-message)
    (send-success-p      . navi2ch-megabbs-send-message-success-p)
;    (error-string   	 . navi2ch-megabbs-send-message-error-string)
    (board-update        . navi2ch-megabbs-board-update)
    (board-get-file-name . navi2ch-megabbs-board-get-file-name)))

(defvar navi2ch-megabbs-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'megabbs
                         navi2ch-megabbs-func-alist
                         navi2ch-megabbs-variable-alist)

(defgroup navi2ch-megabbs nil
  "*Navi2ch, megabbs."
  :prefix "navi2ch-megabbs-"
  :group 'navi2ch)

(defcustom navi2ch-megabbs-max-articles 300
  "スレ一覧の最大値．"
  :type '(choice (const :tag "無制限" 0)
                 (integer :tag "制限値"))
  :group 'navi2ch-megabbs)

;;-------------

(defun navi2ch-megabbs-p (uri)
  "URI が megabbs.net なら non-nilを返す。"
  (string-match "^http://www.megabbs.com/" uri))

(navi2ch-multibbs-defcallback navi2ch-megabbs-subject-callback (megabbs)
  "subject.txt を取得するとき navi2ch-net-update-file
で使われるコールバック関数"
  (progn (re-search-forward "[^\n]*\n" nil t) (replace-match ""))
  (if (= navi2ch-megabbs-max-articles 0)
      (while (navi2ch-megabbs-subject-callback-sub))
    (let ((n navi2ch-megabbs-max-articles))
      (while (and (not (zerop n))
                  (navi2ch-megabbs-subject-callback-sub))
        (setq n (1- n)))))
  (delete-region (point) (point-max)))

(defun navi2ch-megabbs-subject-callback-sub ()
  (when (re-search-forward "\\([0-9]+\\)<>\\(.*\\)<>\\(.*\\)\n" nil t)
    (replace-match "\\1.dat,\\2(\\3)\n" t)
    t))

(defun navi2ch-megabbs-article-update (board article start)
  "BOARD ARTICLE の記事を更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
        (time (cdr (assq 'time article)))
        (url  (navi2ch-megabbs-article-to-url board article start nil start))
        (func (if start 'navi2ch-megabbs-article-callback-diff
		'navi2ch-megabbs-article-callback)))
    (navi2ch-net-update-file url file time func nil start)))

(defmacro navi2ch-megabbs-with-board (uri id board &rest body)
  (let ((alist (make-symbol "alist")))
    `(let* ((,alist (navi2ch-megabbs-url-to-board
                     (cdr (assq 'uri ,board))))
            (,uri (cdr (assq 'uri ,alist)))
	    ,@(if id
		  `((,id (cdr (assq 'id ,alist))))))
       ,@body)))

(defun navi2ch-megabbs-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (navi2ch-megabbs-with-board
   uri id board
   (let ((artid (cdr (assq 'artid article))))
     (concat
      (progn (string-match "\\(http://[^/]+\\)/" uri)
             (format "%s/cgi-bin/readres.cgi?bo=%s&vi=%s"
                     (match-string 1 uri) id artid))
      (cond ((and (stringp start))
             (string-match "l\\([0-9]+\\)" start)
             (format "&rm=%s" (match-string 1 start)))
            ((and start end (= (- end start) 1))
             (format "&res=%d" start))
            (t (concat
                (and start (format "&rs=%d" start))
                (and end (format "&re=%d" end)))))
      (and nofirst
           (not (eq start 1))
           "&fi=no")))))

(defun navi2ch-megabbs-url-to-board (url)
  "url から BOARD に変換。"
  (cond
   ;; http://www.megabbs.com/cgi-bin/readtitle.cgi?bo=hoge&br=off
   ((string-match
     "http://\\([^/]+\\)/cgi-bin/[^?]*\\?.*bo=\\([^&]*\\)"
     url)
    (list (cons 'uri (format "http://%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url)))
	  (cons 'id  (match-string 2 url))))
   ;; http://www.megabbs.com/egame/index.html
   ((string-match
     "http://\\([^/]+\\)/\\([^/]+\\)"
     url)
    (list (cons 'uri (format "http://%s/%s/"
			     (match-string 1 url)
			     (match-string 2 url)))
	  (cons 'id  (match-string 2 url))))))

(defun navi2ch-megabbs-url-to-article (url)
  (cond ((string-match
	  "http://.+/cgi-bin/readres\\.cgi.*vi=\\([0-9]+\\)"
	  url)
	 (list (cons 'artid (match-string 1 url))))))


;------------------------------

(defconst navi2ch-megabbs-url-regexp
  ;;    prefix   カテゴリ     BBS番号
  "\\`\\(.+\\)/\\([^/]+\\)/\\([^/]+\\)/\\'")

(defun navi2ch-megabbs-get-writecgi-url (board)
  "write.cgi の url を返す。"
  (let* ((alist (navi2ch-megabbs-url-to-board (cdr (assq 'uri board))))
         (uri (cdr (assq 'uri alist))))
    (string-match "\\(http://[^/]*/\\)" uri)
    (format "%s/cgi-bin/megabbs.cgi"
            (match-string 1 uri))))

;;; (defun navi2ch-megabbs-get-writecgi-url (board)
;;;   "write.cgi の url を返す。"
;;;   (let ((uri (navi2ch-board-get-uri board)))
;;;     (and (string-match navi2ch-megabbs-url-regexp uri)
;;;          (format "%s/%s/bbs/write.cgi"
;;;                  (match-string 1 uri)
;;;                  (match-string 2 uri)))))

(defun navi2ch-megabbs-send-message
  (from mail message subject bbs key time board article)
  (let ((url         (navi2ch-megabbs-get-writecgi-url board))
	(referer     (navi2ch-board-get-uri board))
	(param-alist (list
		      (cons "submit" "書き込む")
		      (cons "mode" "res")
		      (cons "pre" "")
		      (cons "touhaba" "")
		      (cons "name" (or from ""))
		      (cons "email" (or mail ""))
		      (cons "com" message)
		      (cons "cook" "on")
		      (cons "board" bbs)
		      (cons "res" key))))
    (navi2ch-net-send-request
     url "POST"
     (list (cons "Content-Type" "application/x-www-form-urlencoded")
	   (cons "Cookie" (concat "NAME=" from "; MAIL=" mail))
	   (cons "Referer" referer))
     (navi2ch-net-get-param-string param-alist))))

(defun navi2ch-megabbs-send-message-success-p (proc)
  (string-match "302 Found" (navi2ch-net-get-content proc)))

;;-------------

(defvar navi2ch-megabbs-parse-regexp
  ;; 1. num
  ;; 2. (mail? + name?)
  ;; 3. date
  ;; 4. (id + contents)
  "<dt><a href[^>]*>\\([0-9]+\\)</a>[^<]*<b>\\(.*\\)</b>[ 　]*\
\\([^<]*\\).*\n<dd>\\(.*\\)<hr[^>]*>$"
)

(defvar navi2ch-megabbs-parse-subject-regexp "<title>\\(.*\\)</title>")

(defun navi2ch-megabbs-parse-subject ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-megabbs-parse-subject-regexp nil t)
    (match-string 1)))

(defun navi2ch-megabbs-parse ()
  (let ((case-fold-search t))
    (re-search-forward navi2ch-megabbs-parse-regexp nil t)))

(defun navi2ch-megabbs-make-article (&optional subject)
  (let* ((mail+name (match-string 2))
         (date (match-string 3))
         (contents-with-id (match-string 4))
         mail name id)
    (progn
      (setq mail+name (navi2ch-replace-string
                       "<font[^>]*>\\|</font>\\|</a>\\|<b>\\|</b>"
                       "" mail+name t))
      (string-match "\\(<a href=\"mailto:\\([^\"]*\\)[ ]*\">\\(.*\\)\\|\\(.*\\)\\)"
                    mail+name)
      (setq mail (match-string 2 mail+name))
      (setq name (match-string (if mail 3 4) mail+name)))
    (let ((m (string-match "^<!-- para=\\([^>]*\\)-->" contents-with-id)))
      (setq id (and m (match-string 1 contents-with-id))))
    (format "%s<>%s<>%s<>%s<>%s\n"
            name (or mail "")
            (concat date (and id " ID:") (or id ""))
            contents-with-id (or subject ""))))

(navi2ch-multibbs-defcallback navi2ch-megabbs-article-callback
    (megabbs &optional diff)
  (let ((beg (point))
	(max-num 0)
	subject alist num min-num)
    (unless diff
      (setq subject (navi2ch-megabbs-parse-subject)))
    (while (navi2ch-megabbs-parse)
      (setq num (string-to-number (match-string 1))
	    min-num (or min-num num)
	    max-num (max max-num num)
	    alist (cons (cons (string-to-number (match-string 1))
			      (navi2ch-megabbs-make-article subject))
			alist)
	    subject nil))
    (delete-region beg (point-max))
    (when (and min-num max-num)
      (let ((i min-num))
	(while (<= i max-num)
	  (insert (or (cdr (assoc i alist))
		      "あぼーん<>あぼーん<>あぼーん<>あぼーん<>\n"))
	  (setq i (1+ i)))))))

(defun navi2ch-megabbs-article-callback-diff ()
  (navi2ch-megabbs-article-callback t))

;------------------------------

(defun navi2ch-megabbs-board-update (board)
  (let ((url (navi2ch-megabbs-util-article-list-url board))
	(file (navi2ch-megabbs-board-get-file-name board))
	(time (cdr (assq 'time board)))
	(func (navi2ch-multibbs-subject-callback board)))
    (navi2ch-net-update-file url file time func)))

(defun navi2ch-megabbs-util-article-list-url (board)
  (navi2ch-megabbs-with-board
   url id board
   (concat url id "_newb.txt")))

(defun navi2ch-megabbs-board-get-file-name (board &optional file-name)
  (navi2ch-megabbs-with-board
   uri nil board
   (string-match "http://\\(.+\\)" uri)
   (navi2ch-expand-file-name
    (concat (match-string 1 uri)
            (or file-name navi2ch-board-subject-file-name)))))

;;; navi2ch-megabbs.el ends here
