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

;; see http://kage.monazilla.org/system_DOLIB100.html

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
    (extract-post	. navi2ch-2ch-extract-post)))
;; navi2ch-net-user-agent も multibbs 化する必要あり?

(defvar navi2ch-oyster-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'oyster
			 navi2ch-oyster-func-alist
			 navi2ch-oyster-variable-alist)

;;-------------

(defvar navi2ch-oyster-use-oyster nil	; 変数名は要検討。
  "*オイスターするかどうか。")
(defvar navi2ch-oyster-id "odebu@tora3.net"
  "*オイスターの ID。")
(defvar navi2ch-oyster-password "odebuchan"
  "*オイスターのパスワード。")
(defvar navi2ch-oyster-server "2chv.tora3.net"
  "*オイスターの ID 取得サーバ。")
(defvar navi2ch-oyster-cgi "/futen.cgi"
  "*オイスターの ID 取得 CGI。")

(defvar navi2ch-oyster-session-id nil
  "オイスターサーバから取得したセッション ID。")

(defun navi2ch-oyster-p (uri)
  "オイスター作戦に対応する URI なら non-nilを返す。"
  (and navi2ch-oyster-use-oyster
       (or (string-match "http://.*\\.2ch\\.net/" uri)
	   (string-match "http://.*\\.bbspink\\.com/" uri))))

(defun navi2ch-oyster-article-update (board article start)
  "BOARD, ARTICLE に対応するファイルを更新する。
START が non-nil ならばレス番号 START からの差分を取得する。
START からじゃないかもしれないけど・・・。
返り値は HEADER。"
  (let ((file (navi2ch-article-get-file-name board article))
	(time (cdr (assq 'time article)))
	(url (navi2ch-article-get-url board article))
	header)
    (setq header (if start
		     (navi2ch-net-update-file-diff url file time)
		   (navi2ch-net-update-file url file time)))
    ;; エラーだったら過去ログを取得
    (when (navi2ch-net-get-state 'error header)
      (setq url (navi2ch-article-get-kako-url board article))
      (setq header (navi2ch-net-update-file url file))

      ;; やっぱりダメだったら ID を使って過去ログを取得
      (if (not (navi2ch-net-get-state 'error header))
	  (setq header (navi2ch-net-add-state 'kako header))
	(unless navi2ch-oyster-session-id
	  (navi2ch-oyster-login))
	(setq url (navi2ch-oyster-get-offlaw-url
		   board article navi2ch-oyster-session-id file))
	(message "offlaw url %s" url)
	(setq header
	      (if start
		  (progn
		    (message "article %s" article)
		    (navi2ch-oyster-update-file-with-offlaw url file time t))
		(prog1
		    (navi2ch-oyster-update-file-with-offlaw url file time nil)
		  (message "Getting from 0 offlaw.cgi"))))
	(unless (navi2ch-net-get-state 'error header)
	  (setq header (navi2ch-net-add-state 'kako header)))))
    header))

(defun navi2ch-oyster-send-message
  (from mail message subject bbs key time board article &optional post)
  (let ((post (navi2ch-put-alist "sid"
				 ;;セッションID取得済みであれば●カキコ
				 ;;y-n で聞きたいなぁ
				 (or navi2ch-oyster-session-id "")
				 post)))
    (navi2ch-2ch-send-message from mail message subject bbs key
			      time board article post)))

(defun navi2ch-oyster-get-offlaw-url (board article session-id file)
  "BOARD, ARTICLE, SESSION-ID, FILE から offlaw url に変換。"
  (let ((uri (navi2ch-board-get-uri board))
	(artid (cdr (assq 'artid article)))
	(size 0)
	encoded-s)
    ;; (setq encoded-s (w3m-url-encode-string session-id))
    (setq encoded-s (navi2ch-net-url-hexify-string session-id))
    (when (file-exists-p file)
      (setq size (max 0 (navi2ch-file-size file))))
    (string-match "\\(.*\\)\\/\\([^/]*\\)\\/" uri)
    (format "%s/test/offlaw.cgi/%s/%s/?raw=.%s&sid=%s"
	    (match-string 1 uri) (match-string 2 uri) artid size encoded-s)))

(defun navi2ch-oyster-update-file-with-offlaw (url file &optional time diff)
  "FILE を URL から offlaw.cgi を使って更新する。
TIME が non-nil ならば TIME より新しい時だけ更新する。
DIFF が non-nil ならば差分を取得する。
更新できれば HEADER を返す。"
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
		  ((string= "-INCR" state) ;; あぼーん
		   (with-temp-file file
		     (navi2ch-set-buffer-multibyte nil)
		     (insert (substring cont 0 cont-size)))
		   (setq header (navi2ch-net-add-state 'aborn header)))
		  (t
		   (when (string= "-ERR" state)
		     (let ((err-msg (decode-coding-string
				     data navi2ch-coding-system)))
		       (message "Error! %s" err-msg)))
		   (setq header (navi2ch-net-add-state 'error header))))))))
	  (t
	   (setq header (navi2ch-net-add-state 'error header))))
    header))

(defun navi2ch-oyster-get-status (proc)
  "オイスターサーバの接続のステータス部を返す。"
  (navi2ch-net-ignore-errors
   (or (save-excursion
         (set-buffer (process-buffer proc))
         (while (and (eq (process-status proc) 'open)
                     (goto-char (point-min))
                     (not (search-forward "HTTP/1\\.[01] \\([0-9]+\\)")))
           (accept-process-output proc)
           (message "Retrying")
           (sleep-for 3))
         (let ((i 10))
           (catch 'loop
             (while (>= (setq i (1- i)) 0)
               (sleep-for 1)	  ; 何だかうまく動かないのでwait入れた
               (goto-char (point-min))
               ;; 最後まで見つからないままだとエラー
               (when (search-forward "SESSION-ID=" nil (> i 0))
                 (throw 'loop
                        (if (looking-at "\\(.*\\)\n")
                            (match-string 1)))))))))))

;; (defun navi2ch-oyster-get-status (proc)
;;   "オイスターサーバの接続のステータス部を返す。"
;;   (navi2ch-net-ignore-errors
;;    (or (save-excursion
;; 	 (set-buffer (process-buffer proc))
;; 	 (while (and (eq (process-status proc) 'open)
;; 		     (goto-char (point-min))
;; 		     (not (search-forward "HTTP/1\\.[01] \\([0-9]+\\)")))
;; 	   (accept-process-output proc)
;; 	   (message "Retrying")
;; 	   (sit-for 3))
;; 	 (sit-for 5)		  ; 何だかうまく動かないのでwait入れた
;; 	 (goto-char (point-min))
;; 	 (search-forward "SESSION-ID=")
;; 	 (if (looking-at "\\(.*\\)\n")
;; 	     (match-string 1))))))

(defun navi2ch-oyster-login ()
  "オイスターのサーバにログインして session-id を取得する。"
  (interactive)
  (let (buf proc)
    (message "オイスターのサーバにログインします")
    (setq buf (get-buffer-create (concat " *" "navi2ch oyster-ssl")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer))
    (setq proc (open-tls-stream "ssl" buf navi2ch-oyster-server 443))
    (let ((contents (concat "ID=" navi2ch-oyster-id
			    "&PW=" navi2ch-oyster-password)))
      (process-send-string proc
			   (concat
			    (concat "POST " navi2ch-oyster-cgi " HTTP/1.0\n")
			    "User-Agent: DOLIB/1.00\n"
			    "X-2ch-UA: "
			    (format "Navigator for 2ch %s" navi2ch-version) "\n"
			    "Content-Length: "
			    (number-to-string (length contents)) "\n"
			    "\n"
			    contents "\n")))
    (setq navi2ch-oyster-session-id (navi2ch-oyster-get-status proc))
    (message "IDを取得しますた ID= %s" navi2ch-oyster-session-id)
    (and (string-match "ERROR(.*)" navi2ch-oyster-session-id)
	 (message "ID取得に失敗しますた ID= %s" navi2ch-oyster-session-id)
	 (setq navi2ch-oyster-session-id nil))))

;;; navi2ch-oyster.el ends here
