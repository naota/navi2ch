;;; navi2ch-localfile.el --- View localfile for Navi2ch.

;; Copyright (C) 2002 by Navi2ch Project

;; Author:
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

;; etc.txt に
;; ====
;; ローカルファイルテスト
;; x-localbbs:///tmp/localfile
;; localfile
;; ====
;; のように設定してやると、ディレクトリ /tmp/localfile に書き込みます。

;;; Code:
(provide 'navi2ch-localfile)

(defvar navi2ch-localfile-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch-multibbs)

(defvar navi2ch-localfile-regexp "^x-localbbs://")
(defvar navi2ch-localfile-use-lock t)
(defvar navi2ch-localfile-lock-name "lockdir_localfile")

(defvar navi2ch-localfile-func-alist
  '((bbs-p		. navi2ch-localfile-p)
    (article-update 	. navi2ch-localfile-article-update)
    (send-message   	. navi2ch-localfile-send-message)
    (send-success-p 	. navi2ch-localfile-send-message-success-p)
    (error-string   	. navi2ch-localfile-navi2ch-net-get-content)
    (board-get-file-name . navi2ch-localfile-board-get-file-name)))

(defvar navi2ch-localfile-variable-alist
  '((coding-system	. shift_jis)))

(navi2ch-multibbs-regist 'localfile
			 navi2ch-localfile-func-alist
			 navi2ch-localfile-variable-alist)

;;-------------

(defconst navi2ch-localfile-coding-system 'shift_jis-unix)

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
		 (error "lock failed"))))))))

(defun navi2ch-localfile-unlock (dir)
  "`dir' のロックを解除する。"
  (when navi2ch-localfile-use-lock
    (navi2ch-unlock-directory dir navi2ch-localfile-lock-name)))

(defun navi2ch-localfile-p (uri)
  "URI が localfile なら non-nilを返す。"
  (string-match navi2ch-localfile-regexp uri))

(defun navi2ch-localfile-article-update (board article)
  "BOARD ARTICLEの記事を更新する。"
  (list article '(dummy)))

(defvar navi2ch-localfile-encode-html-tag-alist
  '(("<" . "&gt;")
    (">" . "&lt;")
    ("\n" . "<br>")))

(defun navi2ch-localfile-encode-string (string)
  (let ((regexp (regexp-opt
		 (mapcar 'car navi2ch-localfile-encode-html-tag-alist)))
	(start 0)
	result rest mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
	    me (match-end 0))
      (setq result (cons (cdr (assoc (match-string 0 string)
				     navi2ch-localfile-encode-html-tag-alist))
			 (cons (substring string start mb)
			       result)))
      (if (= start me)
	  (setq start (1+ start)
		result (cons (substring string me start) result))
	(setq start me)))
    (apply 'concat (nreverse (cons (substring string start) result)))))

(defun navi2ch-localfile-encode-message (from mail time message
					      &optional subject)
  (format "%s<>%s<>%s<>%s<>%s\n"
	  (navi2ch-localfile-encode-string from)
	  (navi2ch-localfile-encode-string mail)
	  (format-time-string "%y/%m/%d %R" time)
	  (navi2ch-localfile-encode-string message)
	  (navi2ch-localfile-encode-string (or subject ""))))

(defvar navi2ch-localfile-subject-file-name "subject.txt")

(defun navi2ch-localfile-update-subject-file
  (directory &optional article-id sage-flag)
  "DIRECTORY 以下の `navi2ch-localfile-subject-file-name' を更新する。
ARTICLE-ID が指定されていればそのアーティクルのみを更新する。
`navi2ch-localfile-subject-file-name' に指定されたアーティクルが無い場
合は SUBJECT を使用する。DIRECTORY は呼び出し元でロックしておくこと。"
  (if (not article-id)
      (dolist (file (directory-files directory nil "[0-9]+\\.dat\\'"))
	(navi2ch-localfile-update-subject-file
	 directory (file-name-sans-extension file)))
    (let* ((coding-system-for-read navi2ch-localfile-coding-system)
	   (coding-system-for-write navi2ch-localfile-coding-system)
	   (article-file (expand-file-name (concat article-id ".dat")
					   directory))
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
	      (if (re-search-forward (format "^%s.dat<>[^\n]+$\n"
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

(defun navi2ch-localfile-create-thread (directory from mail message subject)
  "DIRECTORY 以下にスレを作る。"
  (navi2ch-localfile-lock directory)
  (unwind-protect
      (let ((coding-system-for-read navi2ch-localfile-coding-system)
	    (coding-system-for-write navi2ch-localfile-coding-system)
	    (redo t)
	    now article-id file)
	(while (progn
		 (setq now (current-time)
		       article-id (format-time-string "%s" now)
		       file (expand-file-name (concat article-id ".dat")
					      directory))
		 ;; ここでファイルをアトミックに作りたいとこだけど、
		 ;; write-region に mustbenew 引数の無い XEmacs でどう
		 ;; やればいいんだろう。。。
		 (file-exists-p file))
	  (sleep-for 1))		; ちょっと待ってみる。
	(with-temp-file file
	  (insert (navi2ch-localfile-encode-message
		   from mail now message subject)))
	(navi2ch-localfile-update-subject-file directory article-id
					       (string-match "sage" mail)))
    (navi2ch-localfile-unlock directory)))

(defun navi2ch-localfile-append-message (directory article-id
						   from mail message)
  "DIRECTORY の ARTICLE-ID スレにレスを付ける。"
  (navi2ch-localfile-lock directory)
  (unwind-protect
      (let* ((coding-system-for-read navi2ch-localfile-coding-system)
	     (coding-system-for-write navi2ch-localfile-coding-system)
	     (file (expand-file-name (concat article-id ".dat")
				     directory))
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
					       (string-match "sage" mail)))
    (navi2ch-localfile-unlock directory)))

(defun navi2ch-localfile-send-message
  (from mail message subject bbs key time board article)

  (when (= (length message) 0)
    (error "本文が書かれていません。"))
  (when (and subject
	     (= (length subject) 0))
    (error "Subject が書かれていません。"))

  (let* ((dat-file-name (navi2ch-article-get-file-name board article))
	 (directory (file-name-directory dat-file-name)))
    (if subject
	;; スレ立て
	(navi2ch-localfile-create-thread directory from mail message subject)
      ;; レス書き
      (navi2ch-localfile-append-message directory key
					from mail message))))

(defun navi2ch-localfile-send-message-success-p (proc)
;  (string-match "302 Found" (navi2ch-net-get-content proc)))
  t)

(defun navi2ch-localfile-board-get-file-name (board &optional file-name)
  (let ((uri (navi2ch-board-get-uri board)))
    (when (and uri
	       (string-match (concat navi2ch-localfile-regexp "\\(.+\\)")
			     uri))
      (expand-file-name (or file-name
			    navi2ch-board-subject-file-name)
			(match-string 1 uri)))))

(defun navi2ch-localfile-update-file (&rest args)
  '(("Date" . "dummy")
    ("Server" . "localfile")
    ("Last-Modified" . "dummy")))

;;; navi2ch-localfile.el ends here
