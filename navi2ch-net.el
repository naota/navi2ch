;;; navi2ch-net.el --- Network module for navi2ch

;; Copyright (C) 2000-2002 by Navi2ch Project

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

;;; Code:
(provide 'navi2ch-net)
(defvar navi2ch-net-ident
  "$Id$")

(eval-when-compile (require 'cl))
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
(defvar navi2ch-net-state-header-alist
  '((aborn . "X-Navi2ch-Aborn")		; あぼーんされてる
    (kako . "X-Navi2ch-Kako")		; 過去ログになってる
    (not-updated . "X-Navi2ch-Not-Updated") ; 更新されていない
    (error . "X-Navi2ch-Error"))	; エラー(ファイルが取得できないとか)

  "STATE のシンボルと 実際にヘッダに書かれる文字列の alist")

(add-hook 'navi2ch-exit-hook 'navi2ch-net-cleanup)

;; shut up XEmacs warnings
(eval-when-compile
  (defvar inherit-process-coding-system))

(defmacro navi2ch-net-ignore-errors (&rest body)
  "BODY を評価し、その値を返す。
BODY の評価中にエラーが起こると nil を返す。"
  `(condition-case err
       ,(cons 'progn body)
     (error
      (condition-case nil
	  (navi2ch-net-cleanup-process)
	(error nil))
      (ding)
      (if err
	  (message "Error: %s" (error-message-string err))
	(message "Error"))
      (sleep-for 1)
      nil)
     (quit
      (condition-case nil
	  (navi2ch-net-cleanup-process)
	(error nil))
      (signal (car err) (cdr err)))))

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
		   (eq (process-status proc) 'open))
	      (progn
		(message "reusing connection...")
		(process-send-string proc "") ; ping
		(navi2ch-net-get-content proc))	; 前回のゴミを読み飛ばしておく
	    (if (processp proc)
		(delete-process proc))
	    (setq proc nil))
	(error (setq proc nil)))
      (when (or (not proc)
		(not (processp proc))
		(not (eq (process-status proc) 'open)))
	(message "now connecting...")
	(setq proc (open-network-stream navi2ch-net-connection-name
					buf host port)))
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
                "%s"                    ;other-header
                "%s"                    ;content
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
		    (cons (cons "Proxy-Authorization" credentials)
			  other-header))
		   "")
	       (if content
                   (format "Content-length: %d\r\n\r\n%s"
                           (length content) content)
                 "")))
      (message "%sdone" (current-message))
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
  "USER と PASS から Proxy 認証の証明書(？)部分を返す。"
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
  "PROC の接続のステータス部を返す"
  (navi2ch-net-ignore-errors
   (or navi2ch-net-status
       (save-excursion
	 (set-buffer (process-buffer proc))
	 (while (and (eq (process-status proc) 'open)
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
  "PROC の接続のヘッダ部を返す"
  (when (navi2ch-net-get-status proc)
    (navi2ch-net-ignore-errors
     (or navi2ch-net-header
	 (save-excursion
	   (set-buffer (process-buffer proc))
	   (while (and (eq (process-status proc) 'open)
		       (goto-char (point-min))
		       (not (re-search-forward "\r\n\r?\n" nil t)))
	     (accept-process-output proc))
	   (goto-char (point-min))
	   (re-search-forward "\r\n\r?\n")
	   (let ((end (match-end 0))
		 list)
	     (goto-char (point-min))
	     (while (re-search-forward "^\\([^\r\n:]+\\): \\(.+\\)\r\n" end t)
	       (setq list (cons (cons (match-string 1) (match-string 2))
				list)))
	     (let ((date (assoc-ignore-case "Date" list)))
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
	(call-process shell-file-name nil nil nil
		      shell-command-switch (concat "gzip -d " tempfngz))
	(delete-region start end)
	(goto-char start)
	(goto-char (+ start
		      (nth 1 (insert-file-contents-literally tempfn))))
	(delete-file tempfn))))

(defun navi2ch-net-get-content-subr-region (gzip-p start end)
  (if gzip-p
      (apply 'call-process-region
	     start end
	     navi2ch-net-gunzip-program t t nil
	     navi2ch-net-gunzip-args)))

(defalias 'navi2ch-net-get-content-subr
  (if (string-match "windowsce" system-configuration)
      'navi2ch-net-get-content-subr-with-temp-file
    'navi2ch-net-get-content-subr-region))

(defun navi2ch-net-get-chunk (proc)
  "カレントバッファの PROC の point 以降を chunk とみなして chunk を得る。
chunk のサイズを返す。point は chunk の直後に移動。"
  (catch 'ret
    (let ((p (point))
	  size end)
      (while (and (eq (process-status proc) 'open)
		  (not (looking-at "\\([0-9a-fA-F]+\\)[^\r\n]*\r\n")))
	(accept-process-output proc)
	(goto-char p))
      (when (not (match-string 1))
	(message "no chunk-size line")
	(throw 'ret 0))
      (goto-char (match-end 0))
      (setq size (string-to-number (match-string 1) 16)
	    end (+ p size 2))		; chunk-data CRLF
      (delete-region p (point))		; chunk size 行を消す
      (if (= size 0)
	  (throw 'ret 0))
      (while (and (eq (process-status proc) 'open)
		  (goto-char end)
		  (not (= (point) end)))
	(accept-process-output proc))
      (goto-char end)
      (when (not (= (point) end))
	(message "unable goto chunk end (size: %d, end: %d, point: %d)"
		 size end (point))
	(throw 'ret 0))
      (when (not (string= (buffer-substring (- (point) 2) (point))
			  "\r\n"))
	(message "invalid chunk body")
	(throw 'ret 0))		   ; chunk-data の末尾が CRLF じゃない
      (delete-region (- (point) 2) (point))
      size)))

(defun navi2ch-net-get-content (proc)
  "PROC の接続の本文を返す"
  (when (and (navi2ch-net-get-status proc) (navi2ch-net-get-header proc))
    (navi2ch-net-ignore-errors
     (or navi2ch-net-content
	 (let* ((header (navi2ch-net-get-header proc))
		(gzip (and navi2ch-net-accept-gzip
			   (string-match "gzip"
					 (or (cdr (assoc "Content-Encoding"
							 header))
					     ""))))
		p)
	   (save-excursion
	     (set-buffer (process-buffer proc))
	     (goto-char (point-min))
	     (re-search-forward "\r\n\r?\n") ; header の後なので取れてるはず
	     (setq p (point))
	     (cond ((equal (cdr (assoc "Transfer-Encoding" header))
			   "chunked")
		    (while (> (navi2ch-net-get-chunk proc) 0)
		      nil))
		   ((assoc "Content-Length" header)
		    (let ((size (string-to-number (cdr (assoc "Content-Length"
							      header)))))
		      (while (and (eq (process-status proc) 'open)
				  (goto-char (+ p size))
				  (not (= (point) (+ p size))))
			(accept-process-output proc))
		      (goto-char (+ p size))))
		   ((or (string= (navi2ch-net-get-protocol proc)
				 "HTTP/1.0")
			(not navi2ch-net-enable-http11)
			(and (stringp (cdr (assoc "Connection" header)))
			     (string= (cdr (assoc "Connection" header))
				      "close")))
		    (while (eq (process-status proc) 'open)
		      (accept-process-output proc))
		    (goto-char (point-max))))
	     (navi2ch-net-get-content-subr gzip p (point))
	     (setq navi2ch-net-content
		   (navi2ch-string-as-multibyte
		    (buffer-substring-no-properties p (point))))))))))

(defun navi2ch-net-download-file (url
				  &optional time accept-status other-header)
  "URL からダウンロードを開始する。
TIME が `non-nil' ならば TIME より新しい時だけダウンロードする。
リスト `accept-status' が `non-nil' ならばステータスが `accept-status' に含まれ
ている時だけダウンロードする。
OTHER-HEADER が `non-nil' ならばリクエストにこのヘッダを追加する。
ダウンロードできればその接続を返す。"
  (navi2ch-net-ignore-errors
   (let (proc status)
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
			  '("Accept-Encoding" . "gzip"))
		     (and navi2ch-net-user-agent
			  (cons "User-Agent" navi2ch-net-user-agent)))
	       other-header)))
       (message "checking file...")
       (setq status (navi2ch-net-get-status proc))
       (when (and (string= status "416")
		  (assoc "Range" other-header))
	 (let ((elt (assoc "Range" other-header)))
	   (setq other-header (delq elt other-header)
		 status nil)))
       (unless status
	 (message "retrying...")
	 (sit-for 3)))			; リトライする前にちょっと待つ
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
	 proc))))

(defun navi2ch-net-download-file-range (url range &optional time other-header)
  "Range ヘッダを使ってファイルをダウンロードする。"
  (navi2ch-net-download-file url time '("206" "200" "304") ;; 200 もあってもいいのかな？
			     (append
			      (list (cons "Range" (concat "bytes=" range)))
			      other-header)))


(defun navi2ch-net-update-file (url file &optional time func location diff)
  "FILE を更新する。
TIME が non-nil ならば TIME より新しい時だけ更新する。
FUNC が non-nil ならば更新後 FUNC を使ってファイルを変換する。
FUNC は current-buffer を操作する関数である事。
LOCATION が non-nil ならば Location ヘッダがあったらそこに移動するよう
にする。
DIFF が non-nil ならば 差分として FILE を上書きせずに追加する。
更新できれば header を返す"
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
						  (and location "302")))
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
			  "%s: getting file diff..."
			"%s: getting new file...")
		      (current-message))
	     (setq cont (navi2ch-net-get-content proc))
	     (when (and cont func)
	       (message "%stranslating..." (current-message))
	       (setq cont (with-temp-buffer
			    (insert cont)
			    (goto-char (point-min))
			    (funcall func)
			    (buffer-string))))
	     (if (and cont (not (string= cont "")))
		 (with-temp-file file
		   (when diff
		     (insert-file-contents file)
		     (goto-char (point-max)))
		   (insert cont)
		   (message "%sdone" (current-message)))
	       (setq header (navi2ch-net-add-state 'not-updated header))
	       (message "%snot updated" (current-message))))
	    ((and location
		  (string= status "302")
		  (assoc "Location" header))
	     (setq url (cdr (assoc "Location" header))
		   redo t)
	     (message "%s: redirecting..." (current-message)))
	    ((string= status "304")
	     (setq header (navi2ch-net-add-state 'not-updated header)))
	    (t
	     ;; ここに来るはずないけど一応
	     (setq header (navi2ch-net-add-state 'error header)))))
    header))

(defun navi2ch-net-get-length-from-header (header)
  "header から contents 全体の長さを得る。
header に長さが含まれていない場合は nil を返す。"
  (let ((range (cdr (assoc "Content-Range" header)))
	(length (cdr (assoc "Content-Length" header))))
    (cond ((and range
		(string-match "/\\(.+\\)" range))
	   (string-to-number (match-string 1 range)))
	  (length
	   (string-to-number length)))))

(defun navi2ch-net-check-aborn (size header)
  "あぼーんされてなければ t"
  (let ((len (navi2ch-net-get-length-from-header header)))
    (if len
	(>= len (or size 0))
      t)))				; ホントにこれでいいかな?

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
	 ;; てくるので 1- する。
	 (size (max 0 (1- (nth 7 (file-attributes file)))))
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
	     (message "%s: getting file diff..." (current-message))
	     (let ((cont (navi2ch-net-get-content proc)))
	       (cond ((and (> size 0)
			   (not (= (aref cont 0) ?\n)))
		      (setq aborn-p t)) ; \n で始まってない場合はあぼーん
;;; 		       ((string= cont "\n")
;;; 			(message "%snot updated" (current-message))
;;; 			(setq header (cons '("Not-Updated" . "yes")
;;; 					   header))
;;; 			(setq ret (list header nil)))
		       (t
			(with-temp-file file
			  (insert-file-contents file nil nil size)
			  (goto-char (point-max))
			  (insert cont))
			(message "%sdone" (current-message)))))))
	  ((string= status "200")
	   (if (not (navi2ch-net-check-aborn size header))
	       (setq aborn-p t)
	     (message "%s: getting whole file..." (current-message))
	     (with-temp-file file
	       (insert (navi2ch-net-get-content proc)))
	     (message "%sdone" (current-message))))
	  ((string= status "304")
	   (setq header (navi2ch-net-add-state 'not-updated header)))
	  (t
	   (setq header (navi2ch-net-add-state 'error header))))
    (if (not aborn-p)
	header
      (message "あぼーん!!!")
      (when (and navi2ch-net-save-old-file-when-aborn
		 (or (not (eq navi2ch-net-save-old-file-when-aborn
			      'ask))
		     (y-or-n-p "あぼーん!!! backup old file? ")))
	(copy-file file (read-file-name "file name: ")))
      (navi2ch-net-add-state
       'aborn
       (navi2ch-net-update-file url file nil nil)))))

(defun navi2ch-net-update-file-with-readcgi (url file &optional time diff)
  "FILE を URL から read.cgi を使って更新する。
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
	     (message (if diff
			  "%s: getting file diff with read.cgi..."
			"%s: getting new file with read.cgi...")
		      (current-message))
	     (setq cont (navi2ch-net-get-content proc))
	     (if (or (not cont)
		     (string= cont ""))
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
		 (setq cont (navi2ch-string-as-unibyte cont))
		 (cond
		  ((string= "+OK" state)
		   (with-temp-file file
		     (navi2ch-set-buffer-multibyte nil)
		     (when (and (file-exists-p file) diff)
		       (insert-file-contents file)
		       (goto-char (point-max)))
		     (insert (substring cont 0 cont-size))))
		  ((string= "-INCR" state);; あぼーん
		   (with-temp-file file
		     (navi2ch-set-buffer-multibyte nil)
		     (insert (substring cont 0 cont-size)))
		   (setq header (navi2ch-net-add-state 'aborn header)))
		  ((string= "-ERR" state)
		   (let ((err-msg (decode-coding-string
				   data navi2ch-coding-system)))
		     (message "error! %s" err-msg)
		     (cond
		      ((string-match "過去ログ倉庫で発見" err-msg)
		       (setq header (navi2ch-net-add-state 'kako header)))
;;; 		      ((and (string-match "html化待ち" err-msg)
;;; 			    (string-match "/read\\.cgi/" url))
;;; 		       (setq url (replace-match "/offlaw.cgi/" t nil url))
;;; 		       (navi2ch-net-update-file-with-readcgi
;;; 			url file time diff))
		      (t
		       (setq header
			     (navi2ch-net-add-state 'error header)))))))))))
	  (t
	   ;; ここに来るはずないけど一応
	   (setq header (navi2ch-net-add-state 'error header))))
    header))

;; from Emacs/W3
(defconst navi2ch-net-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?$ ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\) ?,)
  "A list of characters that are _NOT_ reserve in the URL spec.
This is taken from draft-fielding-url-syntax-02.txt - check your local
internet drafts directory for a copy.")

;; from Emacs/W3
(defun navi2ch-net-url-hexify-string (str)
  "Escape characters in a string"
  (mapconcat
   (function
    (lambda (char)
      (if (not (memq char navi2ch-net-url-unreserved-chars))
	  (if (< char 16)
	      (upcase (format "%%0%x" char))
	    (upcase (format "%%%x" char)))
	(char-to-string char))))
   (encode-coding-string str navi2ch-coding-system) ""))

(defun navi2ch-net-get-param-string (param-alist)
  (mapconcat
   (function
    (lambda (x)
      (concat (car x) "=" (navi2ch-net-url-hexify-string (cdr x)))))
   param-alist
   "&"))

(defun navi2ch-net-send-message-success-p (proc)
  (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				   navi2ch-coding-system)))
    (cond ((or (string-match "書きこみました。" str)
	       (string-match "書きこみが終わりました。" str))
	   t)
	  ((or (string-match "<b>クッキーがないか期限切れです！</b>" str)
	       (string-match "<b>書きこみ＆クッキー確認</b>" str))
	   'retry)
	  (t
	   nil))))

(defun navi2ch-net-send-message-error-string (proc)
  (let ((str (decode-coding-string (navi2ch-net-get-content proc)
				   navi2ch-coding-system)))
    (cond ((string-match "ＥＲＲＯＲ：\\([^<]+\\)" str)
	   (match-string 1 str))
	  ((string-match "<b>\\([^<]+\\)" str)
	   (match-string 1 str))
	  ((string-match "\\([^<>\n]+\\)<br>\\([^<>]+\\)<hr>"  str)
	   (concat (match-string 1 str) (match-string 2 str))))))

;; Set-Cookie: SPID=6w9HFhEM; expires=Tuesday, 23-Apr-2002 00:00:00 GMT; path=/
(defun navi2ch-net-send-message-get-spid (proc)
  (dolist (pair (navi2ch-net-get-header proc))
    (if (string-equal "Set-Cookie" (car pair))
	(let ((str (cdr pair)))
	  (cond ((string-match "^SPID=\\([^;]+\\);" str)
		 (return (match-string 1 str)))
		((string-match "^PON=\\([^;]+\\);" str)
		 (return (match-string 1 str))))))))

(defun navi2ch-net-download-logo (board)
  (let* ((coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (content (navi2ch-net-get-content
		   (navi2ch-net-download-file
		    (navi2ch-board-get-url board
					   navi2ch-net-setting-file-name))))
	 src)
    (when (string-match
	   "BBS_\\(TITLE_PICTURE\\|FIGUREHEAD\\)=\\(.+\\)" content)
      (setq src (match-string 2 content)))
    (let (url file)
      (setq url (if (string-match "http://" src)
		    src
		  (navi2ch-board-get-url board src)))
      (string-match "/\\([^/]+\\)$" url)
      (setq file (match-string 1 url))
      (when file
	(setq file (navi2ch-board-get-file-name board file))
	(when (navi2ch-net-update-file url file nil nil t)
	  file)))))

(defun navi2ch-net-add-state (state header)
  "HEADER に STATE を追加する。"
  (navi2ch-put-alist (cdr (assq state navi2ch-net-state-header-alist))
		     "yes"
		     header))

(defun navi2ch-net-get-state (state header)
  "HEADER から STATE を取得する。"
  (cdr (assoc (cdr (assq state navi2ch-net-state-header-alist))
	      header)))

(run-hooks 'navi2ch-net-load-hook)
;;; navi2ch-net.el ends here
