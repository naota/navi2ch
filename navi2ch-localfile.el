;;; navi2ch-localfile.el --- View localfile for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008 by Navi2ch Project

;; Author: Nanashi San <nanashi@users.sourceforge.net>
;; Part6 スレの 427 の名無しさん
;; <http://pc.2ch.net/test/read.cgi/unix/1023884490/427>

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

;; まず、BBS を用意したいディレクトリを作る。
;;  % mkdir /tmp/localfile
;; 次に、ディレクトリのパーミッションを適切に設定する。
;;  % chgrp navi2ch /tmp/localfile
;;  % chmod g+w /tmp/localfile
;;  % chmod g+s /tmp/localfile (OS によっては必要)
;; 最後に、読み書きしたい奴らの etc.txt に
;; ====
;; ローカルファイルテスト
;; x-localbbs:///tmp/localfile
;; localfile
;; ====
;; のように設定してやると、ディレクトリ /tmp/localfile に書き込みます。

;;; Code:
(provide 'navi2ch-localfile)

(defconst navi2ch-localfile-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch-http-date)
(require 'navi2ch-multibbs)

(defcustom navi2ch-localfile-cache-name "localfile"
  "*ローカル BBS の情報を保存するディレクトリの名前。
`navi2ch-directory' からの相対パスを指定する。"
  :type 'string
  :group 'navi2ch-localfile)

(defcustom navi2ch-localfile-default-file-modes (+ (* 64 7) (* 8 7) 5)
  "*ローカル BBS にファイルを書き込む際に使用する `default-file-modes'。
意味があるのは8進数なので生で操作する時は注意。"
  :type '(choice (const :tag "特定グループの奴らのみが書きこめる" (+ (* 64 7) (* 8 7) 5))
		 (const :tag "自分のみが書きこめる" (+ (* 64 7) (* 8 5) 5))
		 (const :tag "特定グループの奴らのみが読み書きできる" (+ (* 64 7) (* 8 5)))
		 (const :tag "自分のみが読み書きできる" (* 64 7)))
  :group 'navi2ch-localfile)

(defcustom navi2ch-localfile-default-user-name "名無しさん"
  "*ローカル BBS に書き込む際の名無しの名前。"
  :type 'string
  :group 'navi2ch-localfile)

(defvar navi2ch-localfile-regexp "\\`x-localbbs://")
(defvar navi2ch-localfile-use-lock t)
(defvar navi2ch-localfile-lock-name "lockdir_localfile")

(defvar navi2ch-localfile-func-alist
  '((bbs-p		. navi2ch-localfile-p)
    (article-update 	. navi2ch-localfile-article-update)
    (article-to-url	. navi2ch-localfile-article-to-url)
    (url-to-board	. navi2ch-localfile-url-to-board)
    (url-to-article	. navi2ch-localfile-url-to-article)
    (send-message   	. navi2ch-localfile-send-message)
    (send-success-p 	. navi2ch-localfile-send-message-success-p)
    (error-string   	. navi2ch-localfile-error-string)
    (board-update	. navi2ch-localfile-board-update)
    (board-get-file-name . navi2ch-localfile-board-get-file-name)))

(defvar navi2ch-localfile-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'localfile
			 navi2ch-localfile-func-alist
			 navi2ch-localfile-variable-alist)

;;-------------

;; internal functions like bbs.cgi
(defconst navi2ch-localfile-coding-system
  (intern (format "%s-unix" navi2ch-coding-system)))

(defvar navi2ch-localfile-encode-html-tag-alist
  '((">" . "&gt;")
    ("<" . "&lt;")
    ("\n" . "<br>")))

(defvar navi2ch-localfile-subject-file-name "subject.txt")

(defun navi2ch-localfile-lock (dir)
  "`navi2ch-directory' をロックする。"
  (when navi2ch-localfile-use-lock
    (let ((redo t)
	  error-message)
      (while redo
	(setq redo nil)
	(unless (navi2ch-lock-directory dir navi2ch-localfile-lock-name)
	  (setq error-message "ディレクトリのロックに失敗しました。")
	  (cond ((y-or-n-p (format "%sもう一度試しますか? "
				   error-message))
		 (setq redo t))
		((yes-or-no-p (format "%s危険を承知で続けますか? "
				      error-message))
		 nil)
		(t
		 (error "Lock failed"))))))))

(defun navi2ch-localfile-unlock (dir)
  "DIR のロックを解除する。"
  (when navi2ch-localfile-use-lock
    (navi2ch-unlock-directory dir navi2ch-localfile-lock-name)))

(defmacro navi2ch-localfile-with-lock (directory &rest body)
  "DIRECTORY をロックし、BODY を実行する。
BODY の実行後は DIRECTORY のロックを解除する。"
  `(unwind-protect
       (progn
	 (navi2ch-localfile-lock ,directory)
	 ,@body)
     (navi2ch-localfile-unlock ,directory)))

(put 'navi2ch-localfile-with-lock 'lisp-indent-function 1)

(defun navi2ch-localfile-encode-string (string)
  (let* ((alist navi2ch-localfile-encode-html-tag-alist)
	 (regexp (regexp-opt (mapcar 'car alist))))
    (navi2ch-replace-string regexp (lambda (key)
				     (cdr (assoc key alist)))
			    string t)))

(defun navi2ch-localfile-encode-message (from mail time message
					      &optional subject)
  (format "%s<>%s<>%s<>%s<>%s\n"
	  (navi2ch-localfile-encode-string from)
	  (navi2ch-localfile-encode-string mail)
	  (format-time-string "%y/%m/%d %R" time)
	  (navi2ch-localfile-encode-string message)
	  (navi2ch-localfile-encode-string (or subject ""))))

(defun navi2ch-localfile-update-subject-file (directory
					      &optional article-id sage-flag)
  "DIRECTORY 以下の `navi2ch-localfile-subject-file-name' を更新する。
ARTICLE-ID が指定されていればそのアーティクルのみを更新する。
`navi2ch-localfile-subject-file-name' に指定されたアーティクルが無い場
合は SUBJECT を使用する。DIRECTORY は呼び出し元でロックしておくこと。"
  (if (not article-id)
      (dolist (file (directory-files (expand-file-name "dat" directory)
				     nil "\\`[0-9]+\\.dat\\'"))
	(navi2ch-localfile-update-subject-file
	 directory (file-name-sans-extension file)))
    (let* ((coding-system-for-read navi2ch-localfile-coding-system)
	   (coding-system-for-write navi2ch-localfile-coding-system)
	   (dat-directory (expand-file-name "dat" directory))
	   (article-file (expand-file-name (concat article-id ".dat")
					   dat-directory))
	   (subject-file (expand-file-name navi2ch-localfile-subject-file-name
					   directory))
	   (temp-file (navi2ch-make-temp-file subject-file))
	   subject lines new-line)
      (unwind-protect
	  (progn
	    (with-temp-buffer
	      (insert-file-contents article-file)
	      (setq lines (count-lines (point-min) (point-max)))
	      (goto-char (point-min))
	      (let ((list (split-string (buffer-substring (point-min)
							  (progn
							    (end-of-line)
							    (point)))
					"<>")))
		(setq subject (or (nth 4 list) ""))))
	    (setq new-line
		  (format "%s.dat<>%s (%d)\n" article-id subject lines))
	    (with-temp-file temp-file
	      (if (file-exists-p subject-file)
		  (insert-file-contents subject-file))
	      (goto-char (point-min))
	      (if (re-search-forward (format "^%s\\.dat<>[^\n]+\n"
					     article-id) nil t)
		  (replace-match "")
		(goto-char (point-max))
		(if (and (char-before)
			 (not (= (char-before) ?\n))) ; 念のため
		    (insert "\n")))
	      (unless sage-flag
		(goto-char (point-min)))
	      (insert new-line))
	    (rename-file temp-file subject-file t))
	(if (file-exists-p temp-file)
	    (delete-file temp-file))))))

;; ↓とりあえずスタブ。将来的には SETTING.TXT を読むようにしたい。
(defun navi2ch-localfile-default-user-name (directory)
  "DIRECTORY でのデフォルトの名無しさんを返す。"
  navi2ch-localfile-default-user-name)

(defun navi2ch-localfile-create-thread (directory from mail message subject)
  "DIRECTORY 以下にスレを作る。"
  (if (string= from "")
      (setq from (navi2ch-localfile-default-user-name directory)))
  (navi2ch-with-default-file-modes navi2ch-localfile-default-file-modes
    (navi2ch-localfile-with-lock directory
      (let ((coding-system-for-read navi2ch-localfile-coding-system)
	    (coding-system-for-write navi2ch-localfile-coding-system)
	    (dat-directory (expand-file-name "dat" directory))
	    now article-id file)
	(unless (file-exists-p dat-directory)
	  (make-directory dat-directory t))
	(while (progn
		 (setq now (current-time)
		       article-id (format-time-string "%s" now)
		       file (expand-file-name (concat article-id ".dat")
					      dat-directory))
		 ;; ここでファイルをアトミックに作りたいとこだけど、
		 ;; write-region に mustbenew 引数の無い XEmacs でどう
		 ;; やればいいんだろう。。。
		 (file-exists-p file))
	  (sleep-for 1))		; ちょっと待ってみる。
	(with-temp-file file
	  (insert (navi2ch-localfile-encode-message
		   from mail now message subject)))
	(navi2ch-localfile-update-subject-file directory article-id
					       (string-match "sage" mail))))))

(defun navi2ch-localfile-append-message (directory article-id
						   from mail message)
  "DIRECTORY の ARTICLE-ID スレにレスを付ける。"
  (if (string= from "")
      (setq from (navi2ch-localfile-default-user-name directory)))
  (navi2ch-with-default-file-modes navi2ch-localfile-default-file-modes
    (navi2ch-localfile-with-lock directory
      (let* ((coding-system-for-read navi2ch-localfile-coding-system)
	     (coding-system-for-write navi2ch-localfile-coding-system)
	     (dat-directory (expand-file-name "dat" directory))
	     (file (expand-file-name (concat article-id ".dat")
				     dat-directory))
	     (temp-file (navi2ch-make-temp-file file)))
	(unwind-protect
	    (when (file-readable-p file)
	      (with-temp-file temp-file
		(insert-file-contents file)
		(goto-char (point-max))
		(if (not (= (char-before) ?\n)) ; 念のため
		    (insert "\n"))
		(insert (navi2ch-localfile-encode-message
			 from mail (current-time) message)))
	      (rename-file temp-file file t))
	  (if (file-exists-p temp-file)
	      (delete-file temp-file)))
	(navi2ch-localfile-update-subject-file directory article-id
					       (string-match "sage" mail))))))

;; interface functions for multibbs
(defun navi2ch-localfile-p (uri)
  "URI が localfile なら non-nilを返す。"
  (string-match navi2ch-localfile-regexp uri))

(defun navi2ch-localfile-article-update (board article start)
  "BOARD ARTICLE の記事を更新する。"
  (let* ((url (navi2ch-article-get-url board article))
	 (file (navi2ch-article-get-file-name board article))
	 (time (or (cdr (assq 'time article))
		   (and (file-exists-p file)
			(navi2ch-http-date-encode (navi2ch-file-mtime file))))))
    (navi2ch-localfile-update-file url file time)))

(defun navi2ch-localfile-article-to-url
  (board article &optional start end nofirst)
  (let* ((uri (cdr (assq 'uri board)))
	 (artid (cdr (assq 'artid article)))
	 url)
    (unless (string= (substring uri -1) "/")
      (setq uri (concat uri "/")))
    (if (null artid)
	uri
      (setq url (concat uri "dat/" artid ".dat/"))
      (when (numberp start)
	(setq start (number-to-string start)))
      (when (numberp end)
	(setq end (number-to-string end)))
      (if (equal start end)
	  (concat url start)
	(concat url
		start (and (or start end) "-") end
		(and nofirst "n"))))))

(defun navi2ch-localfile-url-to-board (url)
  (let (list uri id)
    (cond
     ((string-match
       "\\`\\(x-localbbs://.*/\\([^/]+\\)\\)/dat/[0-9]+\\.dat" url)
      (setq uri (match-string 1 url)
	    id  (match-string 2 url)))
     ((string-match
       "\\`\\(x-localbbs://.*/\\([^/]+\\)\\)/?$" url)
      (setq uri (match-string 1 url)
	    id  (match-string 2 url))))
    (when uri
      (setq uri (concat uri "/"))
      (setq list (cons (cons 'uri uri) list)))
    (when id
      (setq list (cons (cons 'id id) list)))
    list))

(defun navi2ch-localfile-url-to-article (url)
  (let (list)
    (when (string-match
	   "\\`x-localbbs://.*/\\([0-9]+\\)\\.dat/?\\([0-9]+\\)?" url)
      (setq list (cons (cons 'artid (match-string 1 url))
		       list))
      (when (match-string 2 url)
	(setq list (cons (cons 'number
			       (string-to-number (match-string 2 url)))
			 list))))
    list))

(defvar navi2ch-localfile-last-error nil)

(defun navi2ch-localfile-send-message
  (from mail message subject bbs key time board article &optional post)
  (setq navi2ch-localfile-last-error
	(catch 'error
	  (when (= (length message) 0)
	    (throw 'error "本文が書かれていません。"))
	  (when (and subject
		     (= (length subject) 0))
	    (throw 'error "Subject が書かれていません。"))
	  (save-match-data
	    (let* ((url (navi2ch-board-get-url board))
		   directory)
	      (if (string-match (concat navi2ch-localfile-regexp "\\(.+\\)")
				url)
		  (setq directory (file-name-directory (match-string 1 url)))
		(throw 'error "何か変です。"))
	      (if subject
		  ;; スレ立て
		  (navi2ch-localfile-create-thread directory
						   from mail message subject)
		;; レス書き
		(navi2ch-localfile-append-message directory key
						  from mail message))))
	  nil)))

(defun navi2ch-localfile-send-message-success-p (proc)
  (null navi2ch-localfile-last-error))

(defun navi2ch-localfile-error-string (proc)
  navi2ch-localfile-last-error)

(defun navi2ch-localfile-board-update (board)
  (let* ((url (navi2ch-board-get-url board))
	 (file (navi2ch-board-get-file-name board))
	 (time (or (cdr (assq 'time board))
		   (and (file-exists-p file)
			(navi2ch-http-date-encode (navi2ch-file-mtime file))))))
    (navi2ch-localfile-update-file url file time)))

(defun navi2ch-localfile-board-get-file-name (board &optional file-name)
  (let ((uri (navi2ch-board-get-uri board))
	(cache-dir (navi2ch-expand-file-name navi2ch-localfile-cache-name)))
    (when (and uri
	       (string-match
		(concat navi2ch-localfile-regexp "/*\\(.:/\\)?\\(.+\\)") uri))
      (expand-file-name (or file-name
			    navi2ch-board-subject-file-name)
			(expand-file-name (match-string 2 uri) cache-dir)))))

(defun navi2ch-localfile-update-file (url file &optional time &rest args)
  (let ((directory (file-name-directory file)))
    (unless (file-exists-p directory)
      (make-directory directory t)))
  (let (source-file)
    (save-match-data
      (when (string-match (concat navi2ch-localfile-regexp "\\(.+\\)") url)
	(setq source-file (match-string 1 url))))
    (when (and source-file (file-readable-p source-file))
      (message "Checking file...")
      (let* ((mtime (navi2ch-file-mtime source-file))
	     (mtime-string (navi2ch-http-date-encode mtime))
	     header)
	(when time (setq time (navi2ch-http-date-decode time)))
	(setq header (list (cons 'date mtime-string)
			   (cons 'server "localfile")))
	(if (or navi2ch-net-force-update
		(navi2ch-compare-times mtime time)
		(not (file-exists-p file)))
	    (progn
	      (copy-file source-file file t)
	      (setq header (cons (cons 'last-modified mtime-string) header))
	      (message "%supdated" (current-message)))
	  (setq header (navi2ch-net-add-state 'not-updated header))
	  (message "%snot updated" (current-message)))
	header))))

;;; navi2ch-localfile.el ends here
