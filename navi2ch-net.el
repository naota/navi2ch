;;; navi2ch-net.el --- Network module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008 by
;; Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
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

;;

;;; Code:
(provide 'navi2ch-net)
(defconst navi2ch-net-ident
  "$Id$")

(eval-when-compile 
  (require 'cl))
(require 'timezone)
(require 'base64)

(require 'navi2ch)

(defvar navi2ch-net-connection-name "navi2ch connection")
(defvar navi2ch-net-user-agent "Monazilla/1.00 Navi2ch")
(defvar navi2ch-net-setting-file-name "SETTING.TXT")
(defvar navi2ch-net-last-date nil)
(defvar navi2ch-net-last-url nil)
(defvar navi2ch-net-process nil)
(defvar navi2ch-net-last-host nil)
(defvar navi2ch-net-last-port nil)
(defvar navi2ch-net-status nil)
(defvar navi2ch-net-header nil)
(defvar navi2ch-net-content nil)
(defvar navi2ch-net-state-header-table
  (navi2ch-alist-to-hash
   '((aborn . "X-Navi2ch-Aborn")             ; あぼーんされてる
     (kako . "X-Navi2ch-Kako")               ; 過去ログになってる
     (not-updated . "X-Navi2ch-Not-Updated") ; 更新されていない
     (error . "X-Navi2ch-Error")))          ; エラー(ファイルが取得できないとか)
  "STATE のシンボルと実際にヘッダに書かれる文字列の hash。")

(add-hook 'navi2ch-exit-hook 'navi2ch-net-cleanup)

;; shut up XEmacs warnings
(eval-when-compile
  (defvar inherit-process-coding-system))

(defmacro navi2ch-net-ignore-errors (&rest body)
  "BODY を評価し、その値を返す。
BODY の評価中にエラーが起こると nil を返す。"
  (let ((done (make-symbol "--done-temp--"))
	(err (make-symbol "--err-temp--")))
    `(let ((,done nil))
       (unwind-protect
	   (condition-case ,err
	       (prog1
		   ,(cons 'progn body)
		 (setq ,done t))
	     (error
	      (ding)
	      (if ,err
		  (message "Error: %s" (error-message-string ,err))
		(message "Error"))
	      (sleep-for 1)
	      nil))
	 (unless ,done
	   (ignore-errors
	     (navi2ch-net-cleanup-process)))))))

(defun navi2ch-net-cleanup ()
  (let (buf)
    (if (processp navi2ch-net-process)
	(setq buf (process-buffer navi2ch-net-process)))
    (unwind-protect
	(navi2ch-net-cleanup-process)
      (if buf
	  (kill-buffer buf)))))

(defun navi2ch-net-cleanup-process ()
  (unwind-protect
      (if (processp navi2ch-net-process)
	  (delete-process navi2ch-net-process))
    (setq navi2ch-net-process nil)
    (navi2ch-net-cleanup-vars)))

(defun navi2ch-net-cleanup-vars ()
  (setq navi2ch-net-status nil
	navi2ch-net-header nil
	navi2ch-net-content nil))

(defun navi2ch-open-network-stream-with-retry (name buffer host service)
  (let ((retry t) proc)
    (while retry
      (condition-case err
	  (setq proc (open-network-stream name buffer host service)
		retry nil)
	(file-error
	 (save-match-data
	   (if (string-match "in progress" ; EINPROGRESS or EALREADY
			     (nth 2 err))
	       (progn
		 (setq retry t)
		 (sleep-for 1))
	     (signal (car err) (cdr err)))))))
    proc))

(defun navi2ch-open-network-stream-via-command (name buffer host service)
  (let ((command (cond ((stringp navi2ch-open-network-stream-command)
			(format navi2ch-open-network-stream-command
				host service))
		       ((functionp navi2ch-open-network-stream-command)
			(funcall navi2ch-open-network-stream-command
				 host service)))))
    (apply #'start-process name buffer
	   (if (stringp command)
	       (list shell-file-name shell-command-switch command)
	     command))))

;; (let ((sum 0))
;;   (dotimes (i 400 sum)
;;     (setq sum (+ sum (1- (floor (expt 1.00925 i)))))))
;; => 3602
(defvar navi2ch-net-connect-wait-power 1.00925)
(defvar navi2ch-net-connect-time-list '())

(defun navi2ch-net-connect-wait (host)
  (let* ((host (intern host))
	 (now (navi2ch-float-time))
	 (limit (- now 3600.0))
	 (list (delq nil (mapcar (lambda (x) (if (> (cdr x) limit) x))
				 navi2ch-net-connect-time-list)))
	 (len (length (delq nil (mapcar (lambda (x)
					  (if (eq host (car x)) x))
					list))))
	 (wait (floor (- (+ (expt navi2ch-net-connect-wait-power len)
			    (or (cdr (assq host list)) now))
			 1
			 now))))
    (when (> wait 0)
      (message "waiting for %dsec..." wait)
      (sleep-for wait)
      (message "waiting for %dsec...done" wait))
    (setq navi2ch-net-connect-time-list
	  (cons (cons host (navi2ch-float-time)) list))))

(defvar navi2ch-net-down-host-alist nil)

(defvar navi2ch-net-retry-down-host 300
  "以前落ちていたホストに再接続するまでの秒数。
nil なら常に再接続する。")

(defun navi2ch-net-add-down-host (host)
  (setq host (intern (downcase (format "%s" host))))
  (setq navi2ch-net-down-host-alist
	(navi2ch-put-alist host (navi2ch-float-time)
			   navi2ch-net-down-host-alist)))

(defun navi2ch-net-down-p (host)
  (setq host (intern (downcase (format "%s" host))))
  (let ((elt (assq host navi2ch-net-down-host-alist)))
    (and elt
	 (numberp navi2ch-net-retry-down-host)
	 (> navi2ch-net-retry-down-host 0)
	 (< (navi2ch-float-time)
	    (+ (cdr elt) (float navi2ch-net-retry-down-host))))))

(defun navi2ch-net-send-request (url method &optional other-header content)
  (setq navi2ch-net-last-url url)
  (unless navi2ch-net-enable-http11
    (navi2ch-net-cleanup-process))
  (let ((buf (get-buffer-create (concat " *" navi2ch-net-connection-name)))
        (process-connection-type nil)
	(inherit-process-coding-system
	 navi2ch-net-inherit-process-coding-system)
        host file port host2ch credentials)
    (let ((list (navi2ch-net-split-url url navi2ch-net-http-proxy)))
      (setq host (cdr (assq 'host list))
            file (cdr (assq 'file list))
            port (cdr (assq 'port list))
            host2ch (cdr (assq 'host2ch list))))
    (navi2ch-net-connect-wait host)
    (when navi2ch-net-http-proxy
      (setq credentials (navi2ch-net-http-proxy-basic-credentials
			 navi2ch-net-http-proxy-userid
			 navi2ch-net-http-proxy-password)))
    (let ((proc navi2ch-net-process))
      (condition-case nil
	  (if (and navi2ch-net-enable-http11
		   (equal host navi2ch-net-last-host)
		   (equal port navi2ch-net-last-port)
		   (processp proc)
		   (memq (process-status proc) '(open run)))
	      (progn
		(message "Reusing connection...")
		(navi2ch-net-get-content proc))	; 前回のゴミを読み飛ばしておく
	    (if (processp proc)
		(delete-process proc))
	    (setq proc nil))
	(error (setq proc nil)))
      (when (or (not proc)
		(not (processp proc))
		(not (memq (process-status proc) '(open run))))
	(message "Now connecting...")
	(setq proc nil)
	(unless (navi2ch-net-down-p host)
	  (condition-case nil
	      (setq proc (funcall navi2ch-open-network-stream-function
				  navi2ch-net-connection-name buf host port))
	    (error (navi2ch-net-add-down-host host)))))
      (when proc
	(save-excursion
	  (set-buffer buf)
	  (navi2ch-set-buffer-multibyte nil)
	  (erase-buffer))
	(setq navi2ch-net-last-host host)
	(setq navi2ch-net-last-port port)
	(message "%ssending request..." (current-message))
	(set-process-coding-system proc 'binary 'binary)
	(set-process-sentinel proc 'ignore) ; exited abnormary を出さなくする
	(process-send-string
	 proc
	 (format (concat
		  "%s %s %s\r\n"
		  "MIME-Version: 1.0\r\n"
		  "Host: %s\r\n"
		  "%s"			;connection
		  "%s"			;other-header
		  "%s"			;content
		  "\r\n")
		 method file
		 (if navi2ch-net-enable-http11
		     "HTTP/1.1"
		   "HTTP/1.0")
		 host2ch
		 (if navi2ch-net-enable-http11
		     ""
		   "Connection: close\r\n")
		 (or (navi2ch-net-make-request-header
		      (append (list (cons "Proxy-Authorization" credentials)
				    (cons "User-Agent" navi2ch-net-user-agent))
			      other-header))
		     "")
		 (if content
		     (format "Content-length: %d\r\n\r\n%s"
			     (length content) content)
		   "")))
	(message "%sdone" (current-message)))
      (navi2ch-net-cleanup-vars)
      (setq navi2ch-net-process proc))))

(defun navi2ch-net-split-url (url &optional proxy)
  (let (host2ch)
    (string-match "http://\\([^/]+\\)" url)
    (setq host2ch (match-string 1 url))
    (if proxy
        (progn
          (string-match "^\\(http://\\)?\\(.*\\):\\([0-9]+\\)" proxy)
          (list
           (cons 'host (match-string 2 proxy))
           (cons 'file url)
           (cons 'port (string-to-number (match-string 3 proxy)))
           (cons 'host2ch host2ch)))
      (string-match "http://\\([^/:]+\\):?\\([0-9]+\\)?\\(.*\\)" url)
      (list
       (cons 'host (match-string 1 url))
       (cons 'port (string-to-number (or (match-string 2 url)
					 "80")))
       (cons 'file (match-string 3 url))
       (cons 'host2ch host2ch)))))

(defun navi2ch-net-http-proxy-basic-credentials (user pass)
  "USER と PASS から Proxy 認証の証明書 (?) 部分を返す。"
  (when (and user pass)
    (concat "Basic "
	    (base64-encode-string
	     (concat user ":" pass)))))

(defun navi2ch-net-make-request-header (header-alist)
  "'((NAME . VALUE)...) な HEADER-ALIST からリクエストヘッダを作る。"
  (let (header)
    (dolist (pair header-alist)
      (when (and pair (cdr pair))
	(setq header (concat header
			     (car pair) ": " (cdr pair) "\r\n"))))
    header))

(defun navi2ch-net-get-status (proc)
  "PROC の接続のステータス部を返す。"
  (navi2ch-net-ignore-errors
   (or navi2ch-net-status
       (save-excursion
	 (set-buffer (process-buffer proc))
	 (while (and (memq (process-status proc) '(open run))
		     (goto-char (point-min))
		     (not (looking-at "HTTP/1\\.[01] \\([0-9]+\\)")))
	   (accept-process-output proc))
	 (goto-char (point-min))
	 (if (looking-at "HTTP/1\\.[01] \\([0-9]+\\)")
	     (setq navi2ch-net-status (match-string 1)))))))

(defun navi2ch-net-get-protocol (proc)
  (when (navi2ch-net-get-status proc)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (if (looking-at "\\(HTTP/1\\.[01]\\) [0-9]+")
	  (match-string 1)))))

(defun navi2ch-net-get-header (proc)
  "PROC の接続のヘッダ部を返す。"
  (when (navi2ch-net-get-status proc)
    (navi2ch-net-ignore-errors
     (or navi2ch-net-header
	 (save-excursion
	   (set-buffer (process-buffer proc))
	   (while (and (memq (process-status proc) '(open run))
		       (goto-char (point-min))
		       (not (re-search-forward "\r\n\r?\n" nil t)))
	     (accept-process-output proc))
	   (goto-char (point-min))
	   (re-search-forward "\r\n\r?\n")
	   (let ((end (match-end 0))
		 list)
	     (goto-char (point-min))
	     (while (re-search-forward "^\\([^\r\n:]+\\): \\(.+\\)\r\n" end t)
	       (setq list (cons (cons (intern (downcase (match-string 1)))
				      (match-string 2))
				list)))
	     (let ((date (assq 'date list)))
	       (when (and date (stringp (cdr date)))
		 (setq navi2ch-net-last-date (cdr date))))
	     (setq navi2ch-net-header (nreverse list))))))))

(defun navi2ch-net-get-content-subr-with-temp-file (gzip-p start end)
  (if gzip-p
      (let* ((tempfn (make-temp-name (navi2ch-temp-directory)))
	     (tempfngz (concat tempfn ".gz")))
	(let ((coding-system-for-write 'binary)
	      ;; auto-compress-modeをdisableにする
	      (inhibit-file-name-operation 'write-region)
	      (inhibit-file-name-handlers (cons 'jka-compr-handler
						inhibit-file-name-handlers)))
	  (navi2ch-write-region start end tempfngz))
	(let ((status (call-process shell-file-name nil nil nil
				    shell-command-switch
				    (concat "gzip -d " tempfngz))))
	  (unless (and (numberp status) (zerop status))
	    (error "Failed to execute gzip")))
	(delete-region start end)
	(goto-char start)
	(goto-char (+ start
		      (nth 1 (insert-file-contents-literally tempfn))))
	(delete-file tempfn))))

(defun navi2ch-net-get-content-subr-region (gzip-p start end)
  (if gzip-p
      (let ((status (apply 'call-process-region
			   start end
			   navi2ch-net-gunzip-program t t nil
			   navi2ch-net-gunzip-args)))
	(unless (and (numberp status) (zerop status))
	  (error "Failed to execute gzip")))))

(eval-and-compile
  (defalias 'navi2ch-net-get-content-subr
    (navi2ch-ifemacsce
	'navi2ch-net-get-content-subr-with-temp-file
      'navi2ch-net-get-content-subr-region)))

(defun navi2ch-net-get-chunk (proc)
  "カレントバッファの PROC の point 以降を chunk とみなして chunk を得る。
chunk のサイズを返す。point は chunk の直後に移動。"
  (catch 'ret
    (let ((p (point))
	  size end)
      (while (and (not (looking-at "\\([0-9a-fA-F]+\\)[^\r\n]*\r\n"))
		  (memq (process-status proc) '(open run)))
	(accept-process-output proc)
	(goto-char p))
      (when (not (match-string 1))
	(message "No chunk-size line")
	(throw 'ret 0))
      (goto-char (match-end 0))
      (setq size (string-to-number (match-string 1) 16)
	    end (+ p size 2))		; chunk-data CRLF
      (delete-region p (point))		; chunk size 行を消す
      (if (= size 0)
	  (throw 'ret 0))
      (while (and (memq (process-status proc) '(open run))
		  (goto-char end)
		  (not (= (point) end)))
	(accept-process-output proc))
      (goto-char end)
      (when (not (= (point) end))
	(message "Unable goto chunk end (size: %d, end: %d, point: %d)"
		 size end (point))
	(throw 'ret 0))
      (when (not (string= (buffer-substring (- (point) 2) (point))
			  "\r\n"))
	(message "Invalid chunk body")
	(throw 'ret 0))		   ; chunk-data の末尾が CRLF じゃない
      (delete-region (- (point) 2) (point))
      size)))

(defun navi2ch-net-get-content (proc)
  "PROC の接続の本文を返す。"
  (when (and (navi2ch-net-get-status proc) (navi2ch-net-get-header proc))
    (navi2ch-net-ignore-errors
     (or navi2ch-net-content
	 (let* ((header (navi2ch-net-get-header proc))
		(gzip (and navi2ch-net-accept-gzip
			   (string-match "gzip"
					 (or (cdr (assq 'content-encoding
							header))
					     ""))))
		p)
	   (save-excursion
	     (set-buffer (process-buffer proc))
	     (goto-char (point-min))
	     (re-search-forward "\r\n\r?\n") ; header の後なので取れてるはず
	     (setq p (point))
	     (cond ((equal (cdr (assq 'transfer-encoding header))
			   "chunked")
		    (while (> (navi2ch-net-get-chunk proc) 0)
		      nil))
		   ((assq 'content-length header)
		    (let ((size (string-to-number (cdr (assq 'content-length
							     header)))))
		      (while (and (memq (process-status proc) '(open run))
				  (goto-char (+ p size))
				  (not (= (point) (+ p size))))
			(accept-process-output proc))
		      (goto-char (+ p size))))
		   ((or (string= (navi2ch-net-get-protocol proc)
				 "HTTP/1.0")
			(not navi2ch-net-enable-http11)
			(and (stringp (cdr (assq 'connection header)))
			     (string= (cdr (assq 'connection header))
				      "close")))
		    (while (memq (process-status proc) '(open run))
		      (accept-process-output proc))
		    (goto-char (point-max))))
	     (when (and (stringp (cdr (assq 'connection header)))
			(string= (cdr (assq 'connection header))
				 "close"))
	       (delete-process proc))
	     (navi2ch-net-get-content-subr gzip p (point))
	     (setq navi2ch-net-content
		   (buffer-substring-no-properties p (point)))))))))

(defun navi2ch-net-download-file (url
				  &optional time accept-status other-header)
  "URL からダウンロードを開始する。
TIME が `non-nil' ならば TIME より新しい時だけダウンロードする。
リスト ACCEPT-STATUS が `non-nil' ならばステータスが ACCEPT-STATUS に含まれ
ている時だけダウンロードする。
OTHER-HEADER が `non-nil' ならばリクエストにこのヘッダを追加する。
ダウンロードできればその接続を返す。"
  (catch 'ret
    (navi2ch-net-ignore-errors
     (let ((time (if (or (null time) (stringp time)) time
		   (navi2ch-http-date-encode time)))
	   proc status)
       (while (not status)
	 (setq proc
	       (navi2ch-net-send-request
		url "GET"
		(append
		 (list (if navi2ch-net-force-update
			   (cons "Pragma" "no-cache")
			 (and time (cons "If-Modified-Since" time)))
		       (and navi2ch-net-accept-gzip
			    ;; regexp は変数にした方がいいのかな。いい変数名が思いつかない。
			    (not (string-match "\\.gz$" url))
			    (not (assoc "Range" other-header))
			    '("Accept-Encoding" . "gzip")))
		 other-header)))
	 (unless proc
	   (throw 'ret nil))
	 (message "Checking file...")
	 (setq status (navi2ch-net-get-status proc))
	 (when (and (string= status "416")
		    (assoc "Range" other-header))
	   (let ((elt (assoc "Range" other-header)))
	     (setq other-header (delq elt other-header)
		   status nil)))
	 (unless status
	   (message "Retrying...")
	   (sit-for 3)))		; リトライする前にちょっと待つ
       (cond ((not (stringp status))
	      (message "%serror" (current-message))
	      (setq proc nil))
	     ((string= status "404")
	      (message "%snot found" (current-message))
	      (setq proc nil))
	     ((string= status "304")
	      (message "%snot updated" (current-message)))
	     ((string= status "302")
	      (message "%smoved" (current-message)))
	     ((string-match "\\`2[0-9][0-9]\\'" status)
	      (message "%supdated" (current-message)))
	     (t
	      (message "%serror" (current-message))
	      (setq proc nil)))
       (if (or (not accept-status)
	       (member status accept-status))
	   proc)))))

(defun navi2ch-net-download-file-range (url range &optional time other-header)
  "Range ヘッダを使ってファイルをダウンロードする。"
  (navi2ch-net-download-file url time '("206" "200" "304") ;; 200 もあってもいいのかな？
			     (append
			      (list (cons "Range" (concat "bytes=" range)))
			      other-header)))

(defsubst navi2ch-net-add-state (state header)
  "HEADER に STATE を追加する。"
  (navi2ch-put-alist (gethash state navi2ch-net-state-header-table)
		     "yes"
		     header))

(defun navi2ch-net-update-file (url file 
				&optional time func location diff other-header)
  "FILE を更新する。
TIME が non-nil ならば TIME より新しい時だけ更新する。
TIME が 'file ならばファイルの更新日時を TIME とする。
FUNC が non-nil ならば更新後 FUNC を使ってファイルを変換する。
FUNC は current-buffer を操作する関数である事。
LOCATION が non-nil ならば Location ヘッダがあったらそこに移動するよう
にする。
DIFF が non-nil ならば 差分として FILE を上書きせずに追加する。
OTHER-HEADER は `navi2ch-net-download-file' に渡される。
更新できれば header を返す"
  (when (eq time 'file)
    (setq time (and (file-exists-p file)
		    (navi2ch-file-mtime  file))))
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (let ((coding-system-for-write 'binary)
	(coding-system-for-read 'binary)
	(redo t)
	proc status header cont)
    (while redo
      (setq redo nil
	    proc (navi2ch-net-download-file url time
					    (list "200" "304"
						  (and location "302"))
					    other-header)
	    status (and proc
			(navi2ch-net-get-status proc))
	    header (and proc
			(navi2ch-net-get-header proc)))
      (cond ((or (not proc)
		 (not status)
		 (not header))
	     ;; 念のため
	     (setq header (navi2ch-net-add-state 'error header)))
	    ((string= status "200")
	     (message (if diff
			  "%s: Getting file diff..."
			"%s: Getting new file...")
		      (current-message))
	     (setq cont (navi2ch-net-get-content proc))
	     (when (and cont func)
	       (message "%stranslating..." (current-message))
	       (setq cont (with-temp-buffer
			    (navi2ch-set-buffer-multibyte nil)
			    (insert cont)
			    (goto-char (point-min))
			    (funcall func)
			    (buffer-string))))
	     (if (and cont (not (string= cont "")))
		 (with-temp-file file
		   (navi2ch-set-buffer-multibyte nil)
		   (when diff
		     (insert-file-contents file)
		     (goto-char (point-max)))
		   (insert cont)
		   (message "%sdone" (current-message)))
	       (setq header (navi2ch-net-add-state 'not-updated header))
	       (message "%snot updated" (current-message))))
	    ((and location
		  (string= status "302")
		  (assq 'location header))
	     (setq url (cdr (assq 'location header))
		   redo t)
	     (message "%s: Redirecting..." (current-message)))
	    ((string= status "304")
	     (setq header (navi2ch-net-add-state 'not-updated header)))
	    (t
	     ;; ここに来るはずないけど一応
	     (setq header (navi2ch-net-add-state 'error header)))))
    header))

(defun navi2ch-net-get-length-from-header (header)
  "header から contents 全体の長さを得る。
header に長さが含まれていない場合は nil を返す。"
  (let ((range (cdr (assq 'content-range header)))
	(length (cdr (assq 'content-length header))))
    (cond ((and range
		(string-match "/\\(.+\\)" range))
	   (string-to-number (match-string 1 range)))
	  (length
	   (string-to-number length)))))

(defun navi2ch-net-check-aborn (size header)
  "あぼーんされてなければ t を返す。"
  (let ((len (navi2ch-net-get-length-from-header header)))
    (if len
	(>= len (or size 0))
      t)))				; ホントにこれでいいかな?

(defconst navi2ch-net-update-file-diff-size 16
  "差分取得する際にオーバーラップさせる幅。")

(defun navi2ch-net-update-file-diff (url file &optional time)
  "FILE を差分で更新する。
TIME が `non-nil' ならば TIME より新しい時だけ更新する。
更新できれば HEADER を返す。"
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (let* ((coding-system-for-write 'binary)
	 (coding-system-for-read 'binary)
	 ;; ファイルサイズと等しい値を range にするとファイルを全部送っ
	 ;; てくるので引いておく。
	 (size (max 0 (- (navi2ch-file-size file)
			 navi2ch-net-update-file-diff-size)))
	 (last (and (> size 0)
		    (with-temp-buffer
		      (navi2ch-set-buffer-multibyte nil)
		      (insert-file-contents file nil size)
		      (buffer-string))))
	 proc header status aborn-p)
    (setq proc (navi2ch-net-download-file-range url (format "%d-" size) time))
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
	  ((string= status "206")
	   (if (not (navi2ch-net-check-aborn size header))
	       (setq aborn-p t)
	     (message "%s: Getting file diff..." (current-message))
	     (let ((cont (navi2ch-net-get-content proc)))
	       (cond ((and (> size 0) last
			   (or (> (length last) (length cont))
			       (not (string= (substring cont 0 (length last))
					     last))))
		      (setq aborn-p t))	; 前回と一致しない場合はあぼーん
		     ((string= cont last)
		      (message "%snot updated" (current-message))
		      (setq header
			    (navi2ch-net-add-state 'not-updated header)))
		     (t
		      (with-temp-file file
			(navi2ch-set-buffer-multibyte nil)
			(insert-file-contents file nil nil size)
			(goto-char (point-max))
			(insert cont))
		      (message "%sdone" (current-message)))))))
	  ((string= status "200")
	   (if (not (navi2ch-net-check-aborn size header))
	       (setq aborn-p t)
	     (message "%s: Getting whole file..." (current-message))
	     (let ((cont (navi2ch-net-get-content proc)))
	       (with-temp-file file
		 (navi2ch-set-buffer-multibyte nil)
		 (insert cont)))
	     (message "%sdone" (current-message))))
	  ((string= status "304")
	   (setq header (navi2ch-net-add-state 'not-updated header)))
	  (t
	   (setq header (navi2ch-net-add-state 'error header))))
    (if (not aborn-p)
	header
      (message "あぼーん!!!")
      (navi2ch-net-save-aborn-file file)
      (navi2ch-net-add-state
       'aborn
       (navi2ch-net-update-file url file)))))

(defun navi2ch-net-save-aborn-file (file)
  (when (and navi2ch-net-save-old-file-when-aborn
	     (or (not (eq navi2ch-net-save-old-file-when-aborn
			  'ask))
		 (y-or-n-p "あぼーん!!! Backup old file? ")))
    (copy-file file (read-file-name "file name: "))))

;; <http://www.ietf.org/rfc/rfc2396.txt>
;; 2.3. Unreserved Characters
;; unreserved  = alphanum | mark
;; mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

;; from Emacs/W3
(defconst navi2ch-net-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserve in the URL spec.
This is taken from RFC 2396.")

;; from Emacs/W3
(defun navi2ch-net-url-hexify-string (str &optional coding-system)
  "Escape characters in a string."
  (mapconcat (lambda (char)
	       (if (not (memq char navi2ch-net-url-unreserved-chars))
		   (format "%%%02X" char)
		 (char-to-string char)))
	     (encode-coding-string str (or coding-system navi2ch-coding-system)) ""))

(defun navi2ch-net-get-param-string (param-alist &optional coding-system)
  (mapconcat (lambda (x)
	       (concat (navi2ch-net-url-hexify-string (car x) coding-system) "="
		       (navi2ch-net-url-hexify-string (cdr x) coding-system)))
	     param-alist "&"))

(defun navi2ch-net-send-message-success-p (proc coding-system)
  (when proc
    (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				     coding-system)))
      (cond ((or (string-match "書きこみました。" str)
		 (string-match "書きこみが終わりました。" str))
	     t)
	    ((or (string-match "<b>クッキーがないか期限切れです！</b>" str)
		 (string-match "<b>書きこみ＆クッキー確認</b>" str))
	     'retry)
	    (t
	     nil)))))

(defun navi2ch-net-send-message-error-string (proc coding-system)
  (when proc
    (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				     coding-system)))
      (cond ((string-match "ＥＲＲＯＲ：\\([^<]+\\)" str)
	     (match-string 1 str))
	    ;; Samba24 http://age.s22.xrea.com/talk2ch/new.txt
	    ((string-match "ＥＲＲＯＲ - \\([^<\n]+\\)" str)
	     (match-string 1 str))
	    ((string-match "\\(ログインエラー[^<]*\\)<br>" str)
	     (match-string 1 str))
	    ((string-match "<b>\\([^<]+\\)" str)
	     (match-string 1 str))
	    ((string-match "\\([^<>\n]+\\)<br>\\([^<>]+\\)<hr>"  str)
	     (concat (match-string 1 str) (match-string 2 str)))))))

;; Cookie はこんな感じの alist に入れておく。
;; ((domain1 (/path1 ("name1" "value1" ...)
;;		     ("name2" "value2" ...) ...)
;;	     (/path2 ...) ...)
;;  (domain2 ...) ...)

(defvar navi2ch-net-cookies nil)

(defun navi2ch-net-cookie-domains (host)
  (let* ((host (downcase host))
	 (domain-list (list (intern host)
			    (intern (concat "." host)))))
    (while (string-match "\\.\\(.*\\..*\\)\\'" host)
      (push (intern (match-string 0 host)) domain-list)
      (let ((h (match-string 1 host)))
	(push (intern h) domain-list)
	(setq host h)))
    domain-list))

(defun navi2ch-net-cookie-paths (file)
  (let (path-list)
    (while (string-match "\\`\\(.*\\)/" file)
      (push (intern (match-string 0 file)) path-list)
      (let ((f (match-string 1 file)))
	(unless (string= f "")
	  (push (intern f) path-list))
	(setq file f)))
    path-list))

(defun navi2ch-net-store-cookie (cookie domain path)
  (let ((domain (if (stringp domain) (intern (downcase domain)) domain))
	(path (if (stringp path) (intern path) path)))
    (let ((path-alist (assq domain navi2ch-net-cookies)))
      (unless path-alist
	(setq path-alist (list domain))
	(push path-alist navi2ch-net-cookies))
      (let ((cookie-list (assq path (cdr path-alist))))
	(if cookie-list
	    (let ((elt (assoc (car cookie) (cdr cookie-list))))
	      (if elt
		  (setcdr elt (cdr cookie))
		(setcdr cookie-list (cons cookie (cdr cookie-list)))))
	  (setq cookie-list (list path cookie))
	  (setcdr path-alist (cons cookie-list (cdr path-alist))))))))

(defun navi2ch-net-match-cookies (url)
  (let* ((alist (navi2ch-net-split-url url))
	 (domain-list (navi2ch-net-cookie-domains (cdr (assq 'host alist))))
	 (path-list (navi2ch-net-cookie-paths (cdr (assq 'file alist)))))
    (flet ((mcn (function list) (apply #'nconc (mapcar function list))))
      (mcn (lambda (domain)
	     (mcn (lambda (path)
		    (navi2ch-net-expire-cookies
		     (cdr (assq path
				(cdr (assq domain
					   navi2ch-net-cookies))))))
		  path-list))
	   domain-list))))

(defvar navi2ch-net-cookie-file "cookie.info")

(defun navi2ch-net-save-cookies ()
  (let ((now (current-time)))
    (flet ((strip (f l) (let ((tmp (delq nil (mapcar f (cdr l)))))
			  (and tmp (cons (car l) tmp)))))
      (navi2ch-save-info
       navi2ch-net-cookie-file
       (delq nil
	     (mapcar (lambda (path-alist)
		       (strip (lambda (cookie-list)
				(strip (lambda (cookie)
					 (and (cddr cookie)
					      (navi2ch-compare-times
					       (cddr cookie) now)
					      cookie))
				       cookie-list))
			      path-alist))
		     navi2ch-net-cookies))))))

(defun navi2ch-net-load-cookies ()
  (setq navi2ch-net-cookies
	(navi2ch-load-info navi2ch-net-cookie-file)))

(add-hook 'navi2ch-save-status-hook 'navi2ch-net-save-cookies)
(add-hook 'navi2ch-load-status-hook 'navi2ch-net-load-cookies)

(defun navi2ch-net-update-cookies (url proc coding-system)
  (let* ((case-fold-search t)
	 (alist (navi2ch-net-split-url url))
	 (host (cdr (assq 'host alist)))
	 (file (cdr (assq 'file alist)))
	 (domain-list (navi2ch-net-cookie-domains host))
	 (path-list (navi2ch-net-cookie-paths file)))
    (dolist (pair (navi2ch-net-get-header proc) navi2ch-net-cookies)
      (when (eq (car pair) 'set-cookie)
	(let* ((str (cdr pair))
	       (date (when (string-match "expires=\\([^;]+\\)" str)
		       (navi2ch-http-date-decode (match-string 1 str))))
	       (domain (intern (downcase (if (string-match "domain=\\([^;]+\\)"
							   str)
					     (match-string 1 str)
					   host))))
	       (path (intern (if (string-match "path=\\([^;]+\\)" str)
				 (match-string 1 str)
			       (if (and (string-match "\\(.*\\)/" file)
					(> (length (match-string 1 file)) 0))
				   (match-string 1 file)
				 "/")))))
	  (when (and (memq domain domain-list)
		     (memq path path-list)
		     (string-match "^\\([^=]+\\)=\\([^;]*\\)" str))
	    (let ((name (match-string 1 str))
		  (value (match-string 2 str)))
	      (setq value
		    (decode-coding-string
		     (navi2ch-replace-string "%[0-9A-Za-z][0-9A-Za-z]"
					     (lambda (s)
					       (string (string-to-number
							(substring s 1) 16)))
					     value t t t)
		     coding-system))
	      (navi2ch-net-store-cookie (cons name
					      (cons value date))
					domain path))))))))

(defun navi2ch-net-expire-cookies (cookie-list)
  "COOKIE-LIST から期限切れのクッキーを除いたリストを返す。"
  (let ((now (current-time)))
    (delq nil
	  (mapcar (lambda (cookie)
		    (when (or (null (cddr cookie))
			      (navi2ch-compare-times (cddr cookie) now))
		      cookie))
		  cookie-list))))

(defun navi2ch-net-cookie-string (cookies coding-system)
  "HTTP の Cookie ヘッダとして渡す文字列を返す。"
  (mapconcat (lambda (elt)
	       (concat (navi2ch-net-url-hexify-string (car elt)
						      coding-system)
		       "="
		       (navi2ch-net-url-hexify-string (cadr elt)
						      coding-system)))
	     cookies "; "))

(defun navi2ch-net-download-logo (board)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(setting-file (navi2ch-board-get-file-name
		       board navi2ch-net-setting-file-name))
	(setting-url (navi2ch-board-get-url
		      board navi2ch-net-setting-file-name))
	content src)
    (when (and (navi2ch-net-update-file setting-url setting-file 'file)
	       (file-exists-p setting-file))
      (setq content (with-temp-buffer
		      (insert-file-contents setting-file)
		      (buffer-string))))
    (when (and content
	       (string-match
		"BBS_\\(TITLE_PICTURE\\|FIGUREHEAD\\)=\\(.+\\)" content))
      (setq src (match-string 2 content))
      (let (url file)
	(setq url (if (string-match "http://" src)
		      src
		    (navi2ch-board-get-url board src)))
	(string-match "/\\([^/]+\\)$" url)
	(setq file (match-string 1 url))
	(when file
	  (setq file (navi2ch-board-get-file-name board file))
	  (when (navi2ch-net-update-file url file 'file nil t)
	    file))))))

(defsubst navi2ch-net-get-state (state header)
  "HEADER から STATE を取得する。"
  (cdr (assq (gethash state navi2ch-net-state-header-table)
	     header)))

(run-hooks 'navi2ch-net-load-hook)
;;; navi2ch-net.el ends here
