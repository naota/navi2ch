;;; navi2ch-board.el --- subject list module for navi2ch

;; Copyright (C) 2000 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network, 2ch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(provide 'navi2ch-board)

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-board-mode-map nil)
(unless navi2ch-board-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    ;; (define-key map "q" 'navi2ch-board-exit)
    (define-key map "s" 'navi2ch-board-sync)
    (define-key map "r" 'navi2ch-board-select-view-range)
    (define-key map "a" 'navi2ch-board-add-bookmark)
    (define-key map "d" 'navi2ch-board-hide-article)
    (define-key map "h" 'navi2ch-board-toggle-hide)
    (define-key map "+" 'navi2ch-board-toggle-updated)
    (define-key map "b" 'navi2ch-board-toggle-bookmark)
    (define-key map "w" 'navi2ch-board-write-message)
    (define-key map "\M-e" 'navi2ch-board-expire)
    (define-key map "md" 'navi2ch-board-hide-mark-article)
    (setq navi2ch-board-mode-map map)))

(defvar navi2ch-board-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Board"
   '(["Sync" navi2ch-board-sync]
     ["Add bookmark" navi2ch-board-add-bookmark]
     ["Add global bookmark" navi2ch-board-add-global-bookmark]
     ["Hide line" navi2ch-board-hide-article])))

(defvar navi2ch-board-regexp-list
  '("^\\([0-9]+\\)\\.dat,\\(.*\\)(\\([0-9]+\\))\n"
    "^\\([0-9]+\\)\\.dat<>\\(.*\\)[(<]\\([0-9]+\\)[>)]\n"
    "^\\([0-9]+\\)\\.dat,\\(.*\\)（\\([0-9]+\\)）\n"))

(defvar navi2ch-board-current-board nil)
(defvar navi2ch-board-subject-list nil)
(defvar navi2ch-board-old-subject-list nil)
(defvar navi2ch-board-subject-alist nil)
(defvar navi2ch-board-old-subject-alist nil)
(defvar navi2ch-board-subject-file-name "subject.txt")
(defvar navi2ch-board-old-subject-file-name "old-subject.txt")
(defvar navi2ch-board-enable-readcgi nil)

(defvar navi2ch-board-subback-file-name "subback.html")
(defvar navi2ch-board-use-subback-html nil)

(defvar navi2ch-board-minor-mode-list
  '(navi2ch-board-bookmark-mode
    navi2ch-board-hide-mode
    navi2ch-board-updated-mode))

;; bookmark mode
(defvar navi2ch-board-bookmark-mode nil)
(defvar navi2ch-board-bookmark-mode-map nil)
(unless navi2ch-board-bookmark-mode-map
  (setq navi2ch-board-bookmark-mode-map (make-sparse-keymap))
  (define-key navi2ch-board-bookmark-mode-map
    "d" 'navi2ch-board-delete-bookmark)
  (define-key navi2ch-board-bookmark-mode-map
    "md" 'navi2ch-board-delete-bookmark-mark-article)
  (define-key navi2ch-board-bookmark-mode-map
    "a" 'undefined))

;; hide mode
(defvar navi2ch-board-hide-mode nil)
(defvar navi2ch-board-hide-mode-map nil)
(unless navi2ch-board-hide-mode-map
  (setq navi2ch-board-hide-mode-map (make-sparse-keymap))
  (define-key navi2ch-board-hide-mode-map
    "d" 'navi2ch-board-cancel-hide)
  (define-key navi2ch-board-hide-mode-map
    "md" 'navi2ch-board-cancel-hide-mark-article)
  (define-key navi2ch-board-hide-mode-map
    "a" 'undefined))

;; updated mode
(defvar navi2ch-board-updated-mode nil)
(defvar navi2ch-board-updated-mode-map nil)
(unless navi2ch-board-updated-mode-map
  (setq navi2ch-board-updated-mode-map (make-sparse-keymap)))

;;; navi2ch-bm callbacks
(defun navi2ch-board-set-property (begin end item)
  (put-text-property begin end 'article item))

(defun navi2ch-board-get-property (point)
  (get-text-property (save-excursion (goto-char point)
				     (beginning-of-line)
				     (point))
		     'article))

(defun navi2ch-board-get-article (item)
  item)

(defun navi2ch-board-get-board (item)
  navi2ch-board-current-board)

(defun navi2ch-board-exit ()
  (run-hooks 'navi2ch-board-exit-hook)
  (navi2ch-board-save-info))
 
;; regist board
(navi2ch-bm-regist-board 'board 'navi2ch-board-select-board)

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-board-save-info)

;;; navi2ch-board functions
(defun navi2ch-board-get-uri (board)
  "後ろの / が付いた uri を返す。"
  (let ((uri (cdr (assq 'uri board))))
    (when uri
      (when (string-match "[^/]$" uri)
	(setq uri (concat uri "/")))
      uri)))

(defun navi2ch-board-get-host (board)
  (navi2ch-url-to-host (cdr (assq 'uri board))))

(defun navi2ch-board-get-url (board &optional file-name)
  (if (and file-name (string-match "^/" file-name))
      (concat "http://" (navi2ch-board-get-host board) file-name)
    (concat (navi2ch-board-get-uri board)
	    (or file-name navi2ch-board-subject-file-name))))

(defun navi2ch-board-get-readcgi-url (board)
  "read.cgi の板名までの url を返す。"
  (let ((uri (navi2ch-board-get-uri board))
	(id (cdr (assq 'id board))))
    (setq uri (navi2ch-replace-string (concat id "/") "" uri))
    (format "%stest/read.cgi/%s/" uri id)))

(defun navi2ch-board-get-bbscgi-url (board)
  "bbs.cgi の url を返す"
  (let ((uri (navi2ch-board-get-uri board))
	(id (cdr (assq 'id board))))
    (setq uri (navi2ch-replace-string (concat id "/") "" uri))
    (format "%stest/bbs.cgi" uri)))
	  
(defun navi2ch-board-equal (board1 board2)
  (string= (cdr (assq 'uri board1))
	   (cdr (assq 'uri board2))))
  
(defun navi2ch-board-get-file-name (board &optional file-name)
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

(defsubst navi2ch-board-from-file-p (board)
  (string= (cdr (assq 'name board)) navi2ch-board-name-from-file))
	     

(defsubst navi2ch-board-get-matched-article ()
  "match した結果から article を得る"
  (let ((id (match-string 1))
	(str (match-string 2))
	(num (match-string 3))
	article)
    (setq str (navi2ch-replace-string "^ +" "" str)
	  str (navi2ch-replace-string " +$" "" str)
	  str (navi2ch-replace-html-tag str))
    (list (cons 'subject str)
	  (cons 'response num)
	  (cons 'artid id))))

(defun navi2ch-board-url-to-board (url)
  "URL から board を得る"
  (let (uri id board kako)
    (cond ((string-match
            "http://\\(.+\\)/test/read\\.cgi.*bbs=\\([^&]+\\)" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)))
	  ((string-match
            "http://\\(.+\\)/test/read\\.cgi/\\([^/]+\\)/" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)))
          ((string-match
            "http://\\(.+\\)/\\([^/]+\\)/kako/[0-9]+/" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)))
          ((string-match "http://\\(.+\\)/\\([^/]+\\)" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id))))
    (when id
      (let (name)
        (dolist (x (navi2ch-list-get-board-name-list
                    navi2ch-list-category-list))
          (when (string= (cdr (assq 'uri x)) uri)
            (setq board x)))
        (unless board
          (setq board (list (cons 'uri uri)
                            (cons 'id id)
                            (cons 'name "No Name"))))
	board))))

(defun navi2ch-board-to-url (board)
  "BOARD から url に変換"
  (navi2ch-board-get-uri board))
							   
(defun navi2ch-board-get-subject-list (file)
  "file からスレの list を作る。"
  (when (file-exists-p file)
    (with-temp-buffer
      (navi2ch-insert-file-contents file)
      (goto-char (point-min))
      (let ((regexp (navi2ch-board-regexp-test))
	    ;; (file-list (directory-files
	    ;;		   (navi2ch-board-get-file-name
	    ;;		    navi2ch-board-current-board "") nil "\\.dat$"))
	    list)
	(if (null regexp) nil
	  (while (re-search-forward regexp nil t)
	    (setq list (cons (navi2ch-board-get-matched-article) list)))
	  (nreverse list))))))

(defsubst navi2ch-board-updated-article-p (article seen)
  (let ((artid (cdr (assq 'artid article))))
    (when (assoc artid navi2ch-board-subject-alist)
      (let ((res (string-to-number
		  (or (cdr (assoc artid
				  navi2ch-board-subject-alist)) "0")))
	    (old-res (string-to-number
		      (or (cdr (assoc artid
				      navi2ch-board-old-subject-alist)) "0"))))
	(> res (or seen old-res))))))

(defun navi2ch-board-regexp-test ()
  (save-excursion
    (beginning-of-line)
    (catch 'loop
      (dolist (regexp navi2ch-board-regexp-list)
	(when (looking-at regexp)
	  (throw 'loop regexp))))))


(defun navi2ch-board-insert-subjects (list)
  (let ((prev (point))
	(bookmark (cdr (assq 'bookmark navi2ch-board-current-board)))
	(hide (cdr (assq 'hide navi2ch-board-current-board)))
	(summary (navi2ch-article-load-article-summary
		  navi2ch-board-current-board))
	(i 1))
    (dolist (article list)
      (let* ((artid (cdr (assq 'artid article)))
	     (seen (navi2ch-article-summary-element-seen
		    (cdr (assoc artid summary)))))
	(when (cond (navi2ch-board-bookmark-mode
		     (member artid bookmark))
		    (navi2ch-board-hide-mode
		     (member artid hide))
		    (navi2ch-board-updated-mode
		     (if navi2ch-board-hide-updated-article
			 (and (navi2ch-board-updated-article-p article seen)
			      (not (member artid hide)))
		       (navi2ch-board-updated-article-p article seen)))
		    (t
		     (not (member artid hide))))
	  (navi2ch-bm-insert-subject
	   article i
	   (cdr (assq 'subject article))
	   (format "(%4s)" (cdr (assq 'response article)))
	   (cond ((and navi2ch-board-check-updated-article-p
		       (navi2ch-board-updated-article-p article seen))
		  'updated)
		 (seen 'seen)))
	  (setq i (1+ i)))))))

(defun navi2ch-board-select-board (board &optional force)
  (let ((old-mode major-mode))
    (navi2ch-board-mode)
    (navi2ch-bm-setup 'navi2ch-board)
    (setq navi2ch-board-bookmark-mode nil)
    (save-excursion
      (if (and (eq navi2ch-board-current-board board)
	       (eq old-mode major-mode))
	  (navi2ch-board-sync force)
	(run-hooks 'navi2ch-board-select-board-hook)
	(setq navi2ch-board-current-board (navi2ch-board-load-info board))
	(navi2ch-board-sync force 'first)))))

(defun navi2ch-board-setup-menu ()
  (easy-menu-define navi2ch-board-mode-menu
		    navi2ch-board-mode-map
		    "Menu used in navi2ch-board"
		    navi2ch-board-mode-menu-spec)
  (easy-menu-add navi2ch-board-mode-menu))
  
(defun navi2ch-board-mode ()
  "\\{navi2ch-board-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-board-mode)
  (setq mode-name "Navi2ch Board")
  (setq buffer-read-only t)
  (use-local-map navi2ch-board-mode-map)
  (navi2ch-board-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-board-mode-hook)
  (force-mode-line-update))

(defun navi2ch-board-save-old-subject-file (board)
  (let ((file (navi2ch-board-get-file-name board)))
    (when (file-exists-p file)
      (let ((coding-system-for-write navi2ch-coding-system))
	(with-temp-file (navi2ch-board-get-file-name
			 board
			 navi2ch-board-old-subject-file-name)
	  (navi2ch-insert-file-contents file))))))
  
(defun navi2ch-board-update-seen-articles ()
  (let ((summary (navi2ch-article-load-article-summary
		  navi2ch-board-current-board)))
    (dolist (x summary)
      (let ((element (cdr x))
	    (artid (car x)))
	(when (navi2ch-board-updated-article-p
	       (list (cons 'artid artid))
	       (navi2ch-article-summary-element-seen element))
	  (navi2ch-article-summary-element-set-seen element nil))
	(navi2ch-put-alist artid element summary)))
    (navi2ch-article-save-article-summary
     navi2ch-board-current-board summary)))

(defun navi2ch-board-get-readcgi-raw-url (board)
  (format "%s?raw=0.0" (navi2ch-board-get-readcgi-url board)))

(defun navi2ch-board-update-file (board)
  (unless navi2ch-offline
    (let ((file (navi2ch-board-get-file-name board))
	  (time (cdr (assq 'time board))))
      (if navi2ch-board-enable-readcgi
	  (car (navi2ch-net-update-file-with-readcgi
		(navi2ch-board-get-readcgi-raw-url board) file time))
	(let ((url (navi2ch-board-get-url
		    board (if navi2ch-board-use-subback-html
			      navi2ch-board-subback-file-name)))
	      (func (if navi2ch-board-use-subback-html
			'navi2ch-board-make-subject-txt)))
	  (navi2ch-net-update-file url file time func))))))

(defun navi2ch-board-sync (&optional force first)
  (interactive "P")
  (run-hooks 'navi2ch-board-before-sync-hook)
  (save-excursion
    (let* ((buffer-read-only nil)
	   (navi2ch-net-force-update (or navi2ch-net-force-update
					 force))
	   (board navi2ch-board-current-board)
	   (url (navi2ch-board-get-url board))
	   (file (navi2ch-board-get-file-name board))
	   (old-file (navi2ch-board-get-file-name
		      board
		      navi2ch-board-old-subject-file-name))
	   time header)
      (unless navi2ch-offline
	(navi2ch-board-save-old-subject-file board)
	(setq header (navi2ch-board-update-file board))
	(setq time (or (cdr (assoc "Last-Modified" header))
		       (cdr (assoc "Date" header))))
	(when time
	  (setq board (navi2ch-put-alist 'time time board))))
      (setq navi2ch-board-current-board board)
      (when (or first time)
	(erase-buffer)
	(setq navi2ch-board-subject-list
	      (navi2ch-board-get-subject-list file))
	(setq navi2ch-board-subject-alist
	      (navi2ch-alist-list-to-alist
	       navi2ch-board-subject-list 'artid 'response))
	(setq navi2ch-board-old-subject-list
	      (navi2ch-board-get-subject-list old-file))
	(setq navi2ch-board-old-subject-alist
	      (navi2ch-alist-list-to-alist
	       navi2ch-board-old-subject-list 'artid 'response))
	(when time
	  (navi2ch-board-update-seen-articles))
	(navi2ch-board-insert-subjects navi2ch-board-subject-list)
	(navi2ch-board-save-info)
	(navi2ch-board-set-mode-line))))
  (run-hooks 'navi2ch-board-after-sync-hook))

(defun navi2ch-board-make-subject-txt (str)
  "subback.html から (navi2ch 用の) subject.txt を作る
`navi2ch-net-update-file' のハンドラ。"
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (with-temp-buffer
      (insert str)
      (let ((case-fold-search t)
	    str2)
	(goto-char (point-min))
	(while (re-search-forward
		;; この正規表現も仕様変更でだめになるのかも。
		;;   ……なりました。
		"<a +href=\"\\([0-9]+\\)[^>]*\">[0-9]+[^0-9].\\(.*\\)</a>" nil t)
	  (let ((dat (match-string 1))
		(title (match-string 2)))
	    (setq str2
		  (concat str2
			  (format "%s.dat<>%s\n" dat title)))))
	str2))))

(defun navi2ch-board-set-mode-line ()
  (let* ((board navi2ch-board-current-board)
	 (host (navi2ch-url-to-host (cdr (assq 'uri board)))))
    (setq navi2ch-mode-line-identification
	  (format "%s (%s) [%s]" (cdr (assq 'name board))
                  (cdr (assq 'id board))
                  host))
    (navi2ch-set-mode-line-identification)))

(defun navi2ch-board-write-message ()
  (interactive)
  (navi2ch-message-write-message navi2ch-board-current-board nil t))

(defun navi2ch-board-save-info (&optional board)
  (or board (setq board navi2ch-board-current-board))
  (when board
    (navi2ch-save-info
     (navi2ch-board-get-file-name
      board "board.info")
     (list (assq 'bookmark board)
	   (assq 'hide board)
	   (assq 'time board)
	   (assq 'logo board)))))

(defun navi2ch-board-load-info (&optional board)
  (or board (setq board navi2ch-board-current-board))
  (let ((alist (navi2ch-load-info
		(navi2ch-board-get-file-name
		 board "board.info"))))
    (dolist (x alist)
      (setq board
	    (navi2ch-put-alist (car x) (cdr x)
			       board)))
    board))

(defun navi2ch-board-save-spid (board spid)
  (navi2ch-save-info
   (navi2ch-board-get-file-name board "spid.txt")
   spid))

(defun navi2ch-board-load-spid (board)
  (navi2ch-load-info
   (navi2ch-board-get-file-name board "spid.txt")))

(defun navi2ch-board-select-view-range ()
  (interactive)
  (setq-default navi2ch-article-view-range
		(navi2ch-article-select-view-range-subr)))

(defun navi2ch-board-delete-line (sym func msg)
  (let ((artid (cdr (assq 'artid (get-text-property (point) 'article))))
	(list (cdr (assq sym navi2ch-board-current-board))))
    (if artid
	(progn
	  (setq list (funcall func artid list))
	  (setq navi2ch-board-current-board
		(navi2ch-put-alist sym list
				   navi2ch-board-current-board))
	  (let ((buffer-read-only nil))
	    (delete-region (save-excursion (beginning-of-line) (point))
			   (save-excursion (forward-line) (point))))
	  (and (eobp) (not (bobp))
	       (forward-line -1))
	  (message msg))
      (message "Can't select this line!"))))


;;; from Wanderlust(wl-expire.el)
(defsubst navi2ch-board-expire-date-p (key-datevec file access-time)
  (let ((datevec
	 (condition-case nil
	     (navi2ch-make-datevec
	      (or access-time
		  (nth 5 (file-attributes file))))
	   (error nil))))
    (and
     datevec (> (aref datevec 1) 0)
     (string<
      (navi2ch-make-sortable-date datevec)
      (navi2ch-make-sortable-date key-datevec)))))

(defun navi2ch-board-expire (&optional board ask)
  (interactive)
  (and (interactive-p) (setq ask t))
  (or board (setq board navi2ch-board-current-board))
  (unless (eq board navi2ch-board-current-board)
    (setq board (navi2ch-board-load-info board)))
  (let ((dir (navi2ch-board-get-file-name board "")))
    (when (and (file-exists-p dir)
	       (or (not ask) (y-or-n-p "Expire current borad?"))
	       navi2ch-board-expire-date)
      (let ((article-list (mapcar (lambda (file)
				    (when (string-match "[0-9]+" file)
				      (list (cons 'artid
						  (match-string 0 file)))))
				  (directory-files dir nil "\\.dat$")))
	    (summary (navi2ch-article-load-article-summary board))
	    (key-datevec (navi2ch-get-offset-datevec
			  (navi2ch-make-datevec (current-time))
			  navi2ch-board-expire-date))
	    (remove-list nil))
	(message "expiring %s..." (cdr (assq 'name board)))
	(dolist (article article-list)
	  (let ((artid (cdr (assq 'artid article)))
		(file (navi2ch-article-get-file-name board article)))
	    (when (and (or navi2ch-board-expire-bookmark-p
			   (not (or (member artid (cdr (assq 'bookmark board)))
				    (navi2ch-bookmark-exist-all board
								article))))
		       (navi2ch-board-expire-date-p
			key-datevec file
			(navi2ch-article-summary-element-access-time
			 (cdr (assoc artid summary)))))
	      (push article remove-list)
	      (when (assq 'hide board)
		(setcdr (assq 'hide board)
			(delq artid (cdr (assq 'hide board)))))
	      (when navi2ch-board-expire-bookmark-p
		(when (assq 'bookmark board)
		  (setcdr (assq 'bookmark board)
			  (delq artid (cdr (assq 'bookmark board)))))
		(navi2ch-bookmark-delete-article-all board article)))))
	(navi2ch-bm-remove-article-subr board remove-list)
	(navi2ch-board-save-info board)
	(message "expiring %s...done" (cdr (assq 'name board)))))))
  
(defun navi2ch-board-toggle-minor-mode (mode)
  (dolist (m navi2ch-board-minor-mode-list)
    (if (eq m mode)
	(set m (not (symbol-value m)))
      (set m nil)))
  (force-mode-line-update)
  (let ((buffer-read-only nil))
    (save-excursion
      (erase-buffer)
      (navi2ch-board-insert-subjects navi2ch-board-subject-list))))

;;; bookmark mode
(navi2ch-set-minor-mode 'navi2ch-board-bookmark-mode
			" Bookmark"
			navi2ch-board-bookmark-mode-map)

(defun navi2ch-board-add-bookmark ()
  (interactive)
  (unless navi2ch-board-bookmark-mode
    (let ((artid (cdr (assq 'artid (get-text-property (point) 'article))))
	  (list (cdr (assq 'bookmark navi2ch-board-current-board))))
      (if artid
	  (unless (member artid list)
	    (setq list (cons artid list))
	    (setq navi2ch-board-current-board
		  (navi2ch-put-alist 'bookmark list
				     navi2ch-board-current-board))
	    (message "Add bookmark"))
	(message "can't select this line!")))))

(defun navi2ch-board-delete-bookmark ()
  (interactive)
  (navi2ch-board-delete-line 'bookmark 'delete
			     "Delete bookmark"))

(defun navi2ch-board-delete-bookmark-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-board-delete-bookmark))

(defun navi2ch-board-toggle-bookmark ()
  (interactive)
  (navi2ch-board-toggle-minor-mode 'navi2ch-board-bookmark-mode))

;;; hide mode
(navi2ch-set-minor-mode 'navi2ch-board-hide-mode
			" Hide"
			navi2ch-board-hide-mode-map)

(defun navi2ch-board-hide-article ()
  (interactive)
  (navi2ch-board-delete-line 'hide
			     (lambda (artid list)
			       (if (member artid list)
				   list
				 (cons artid list)))
			     "Hide article"))

(defun navi2ch-board-hide-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-board-hide-article))

(defun navi2ch-board-cancel-hide ()
  (interactive)
  (navi2ch-board-delete-line 'hide 'delete
			     "Cancel hide article"))

(defun navi2ch-board-cancel-hide-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-board-cancel-hide))

(defun navi2ch-board-toggle-hide ()
  (interactive)
  (navi2ch-board-toggle-minor-mode 'navi2ch-board-hide-mode))

;;; updated mode
(navi2ch-set-minor-mode 'navi2ch-board-updated-mode
			" Updated"
			navi2ch-board-updated-mode-map)

(defun navi2ch-board-toggle-updated ()
  (interactive)
  (navi2ch-board-toggle-minor-mode 'navi2ch-board-updated-mode))

(run-hooks 'navi2ch-board-load-hook)
;;; navi2ch-board.el ends here
