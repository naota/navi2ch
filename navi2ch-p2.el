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

;; 概要
;; navi2ch を p2 のフロントエンドにします。
;; といっても今はまだ bookmark の同期だけ。
;; まだまだ実験段階なので注意。
;;
;; 使いかた
;; パスを通して、
;; (load "navi2ch-p2")
;; 
;; M-x navi2ch-p2-sync-global-bookmark-to-p2
;; 
;; 指定された global-bookmark に登録されていて、p2 のお気にいりに登録さ
;; れていないスレをp2 のお気にいりに登録し、 指定された
;; global-bookmark に登録されておらず、 p2 のお気にいりに登録されている
;; スレを尋ねてから p2 のお気にいりから削除します。
;; 
;; M-x navi2ch-p2-sync-global-bookmark-from-p2
;; 
;; p2 のお気にいりにに登録されていて、指定された global-bookmark に登録
;; されていないスレを指定された global-bookmark に登録し、 p2 のお気に
;; いりに登録されておらず、 指定された global-bookmark に登録されている
;; スレを尋ねてから 指定された global-bookmark から削除します。
;;
;; M-x navi2ch-p2-export
;; 
;; navi2ch のお気にいり、履歴、あぼーんワード、dat、スレ情報を指定され
;; たフォルダに rep2 形式でp export します。
;;
;; 設定
;; navi2ch-p2-mail-address にメールアドレス
;; navi2ch-p2-password にパスワードを設定しておくと便利でしょう。
;; 
;; エクスポートを使う人は、 navi2ch-p2-export-bookmark にエクスポートし
;; たい global-bookmark の id を設定しておくといいでしょう。
;; 
;; dat のエクスポートに使われる関数が navi2ch-p2-dat-copy-function に設
;; 定されています。 デフォルトは add-name-to-file (ハードリンク)になっ
;; ています。 リンクが使用不可能ならば copy-file 、リンクが使用可能だが
;; パーティションが別である人は make-symbolic-link (シンボリックリンク)に
;; するといいでしょう。

;; 機能追加

;; 2ch公式P2(http://p2.2ch.net)を経由して書き込みが出来るようにする機能追加です。
;; 主な目的はプロバイダのアク禁に巻き込まれた際の回避策です。
;; P2やらモリタポに関しては流動的なので、その都度、各々調べてください。
;; p2writerやp2proxyあたりとやってることは同じなのでその辺のスレで。
;; 1000モリタポ消費で書き込み許可を得るのはブラウザ経由で

;; スレ読み込みはp2経由してません。書き込みのみです。

;; 簡単な内部仕様の説明としては
;; http://p2.2ch.net/p2 にログインしてcookie(PS、csid)取得
;; 　↓
;; そのcookieを使ってhttp://p2.2ch.net/p2/menu.phpを取得すると、
;; ドキュメントに埋め込まれてるcsrfid変数の値を取得可(XSS対策?)。
;; 　↓
;; レス書き込みのスクリプトである、http://p2.2ch.net/p2/post.php
;; にcookieをcookie渡し、csrfidをPOSTのパラメーター渡しでレスが可能になる。

;; tepo=donというパラメーターで書き込み権限をアクティベーション(?)してやるのは、2ch本体と同じっぽい。
;; (よくある、確認画面で2度書き込む奴です）

;; あくまで個人的に調べた範囲なので、間違ってたり、許可されてないロジックを使ってる可能性もあります。
;; cookieやらcsrfidの"生存期間"に関しては怪しい部分もあるので、要再ログイン、再取得がその都度必要かも。
;; この辺のリトライループはかなり怪しい

;; 設定する変数は以下の3つくらい
;; (setq navi2ch-p2-use-p2 t) ;;p2を使って書き込みするかどうか。
;; (setq navi2ch-p2-mail-address "sample@hoge.hoge") ;; p2のメールアドレス(ID)
;; (setq navi2ch-p2-password "password") ;; p2のパスワード

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

(defvar navi2ch-p2-use-p2 nil	; 変数名は要検討。
  "*p2を使って書き込みするかどうか。")
(defvar navi2ch-p2-mail-address ""
  "*p2のメールアドレス(ID)。")
(defvar navi2ch-p2-password ""
  "*p2のパスワード。")

(defvar navi2ch-p2-domain "p2.2ch.net")

(defvar navi2ch-p2-login-url (concat "http://" navi2ch-p2-domain "/p2/")
  "*p2のログインURL。")

(defconst navi2ch-p2-cookie-names '("PS" "cid"))
(defconst navi2ch-p2-cookie-domain navi2ch-p2-domain)
(defconst navi2ch-p2-cookie-path '/p2)

(defvar navi2ch-p2-login-flag nil)
(defvar navi2ch-p2-csrfid nil)

(defvar navi2ch-p2-board nil)
(defvar navi2ch-p2-board-regexp nil)
(defvar navi2ch-p2-all-board nil)

(defun navi2ch-p2-p (uri)
  "p2経由で書き込むならnon-nilを返す。"
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
      (cond ((or (string-match "書きこみました。" str)
                 (string-match "書きこみが終わりました。" str))
             (message "P2で書き込みました")
             t)
	      ;;おそらくcsrfidの期限切など
            ((or (string-match "Cookie認証時にIPの同一性をチェックしない" str)
                 (string-match "<b>書きこみ＆クッキー確認</b>" str)
                 (string-match "p2 error: ページ遷移の妥当性を確認できませんでした。" str)
                 )
	       ;;再取得
             (message "reget-csrfid %s end" str)
	       (navi2ch-p2-get-csrfid)
	       'retry)
	      ((or  (string-match "p2 error: 引数の指定が変です" str))
	       (error str))
	      (t
	       (message "p2 error::%s" str)
	       nil)))))

(defun navi2ch-p2-make-deny-list ()
  "madakana.cgiからアクセス禁止状態を取得する"
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
	  (message "データ取得に失敗しました")
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
			 (cons "submit" "書き込む")
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
	(if (re-search-forward "ユーザログイン" nil t)
	    (if (and (zerop i)
		     (y-or-n-p "認証失敗。ログインしなおしてみますか? "))
		(setq navi2ch-p2-login-flag nil)
	      (error "認証に失敗しました。"))
	  (if (not (re-search-forward "csrfid=\\([a-f0-9]+\\)" nil t))
	      (error "csrfidの取得に失敗しました")
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
		 (cons "submit_member" "ユーザログイン"))
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
	     (if (re-search-forward "ユーザログイン" nil t)
		 (if (and (zerop i)
			  (y-or-n-p "認証失敗。ログインしなおしてみますか? "))
		     (setq navi2ch-p2-login-flag nil)
		   (error "認証に失敗しました。"))
	       (return (funcall (lambda () ,@body))))))
       (error "ファイル取得に失敗しました。"))))

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
	(if (re-search-forward "ユーザログイン" nil t)
	    (if (and (zerop i)
		     (y-or-n-p "認証失敗。ログインしなおしてみますか? "))
		(setq navi2ch-p2-login-flag nil)
	      (error "認証に失敗しました。"))
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
	    (yes-or-no-p (format "%sを削除しますか? " 
				 name)))
    (string-match "http://\\([^/]*\\)/\\([^/]*\\)/\\([0-9]*\\)" url)
    (let ((host (match-string 1 url))
	  (bbs  (match-string 2 url))
	  (key  (match-string 3 url))
	  (add-or-del (if delete "削除" "追加")))
      (message "%sを%s中..." name add-or-del)
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
				   "%sを%s中...%s"
				   name add-or-del
				   (apply 'format fmt args)))))
		(fset 'current-message
		      (byte-compile
		       `(lambda ()
			  (substring
			   (funcall ,si:current-message)
			   (length (format "%sを%s中..."
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
      (message "%sを%s中...done" name add-or-del))))

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
	(message "%sを追加中..." name)
	(navi2ch-bookmark-add-subr 
	 bookmark-id
	 (cdr (assq 'board item))
	 article)
	(message "%sを追加中...done" name)))
    (dolist (url (mapcar #'car bookmark))
      (when (and (not (assoc url p2-bookmark))
		 (yes-or-no-p
		  (format "%sを削除しますか? "
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
		    "不明"))
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
  (interactive "G出力先: ")
  (if (file-exists-p dir)
      (unless (file-directory-p dir)
	(error "%s is not a directory." dir))
    (make-directory dir))
  (let* ((navi2ch-p2-export-bookmark
	  (or navi2ch-p2-export-bookmark
	      (completing-read "Bookmark ID: " 
			       navi2ch-bookmark-list 
			       nil t navi2ch-bookmark-current-bookmark-id))))
    (message "お気にいりをエクスポート中...")
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
    (message "お気にいりをエクスポート中...done")
    (message "履歴をエクスポート中...")
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
    (message "履歴をエクスポート中...done")
    ;; (message "書きこみ履歴をエクスポート中...")
    ;; (message "書きこみ履歴をエクスポート中...done")
    (message "あぼーんワードをエクスポート中...")
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
						"[　 \f\t\n\r\v]*"))
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
    (message "あぼーんワードをエクスポート中...done")
    (message "dat, idx をエクスポート中...")
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
    (message "dat, idx をエクスポート中...done")))

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
;;; navi2ch-p2.el ends here
