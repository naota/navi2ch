;;; navi2ch-board.el --- subject list module for navi2ch

;; Copyright (C) 2000 by 2ちゃんねる

;; Author: (not 1)
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

(eval-when-compile (require 'cl))
(require 'navi2ch-article)
(require 'navi2ch-board-misc)
(require 'navi2ch-net)

(defvar navi2ch-board-mode-map nil)
(unless navi2ch-board-mode-map
  (let ((map (copy-keymap navi2ch-bm-mode-map)))
    ;; (define-key map "q" 'navi2ch-board-exit)
    (define-key map "s" 'navi2ch-board-sync)
    (define-key map "r" 'navi2ch-board-select-view-range)
    (define-key map "a" 'navi2ch-board-add-bookmark)
    (define-key map "d" 'navi2ch-board-hide-article)
    (define-key map "h" 'navi2ch-board-toggle-hide)
    (define-key map "+" 'navi2ch-board-toggle-updated)
    (define-key map "b" 'navi2ch-board-toggle-bookmark)
    (define-key map "w" 'navi2ch-board-write-message)
    (define-key map "2" 'navi2ch-board-two-pain)
    (define-key map "\C-c\C-f" 'navi2ch-article-find-file)
    (define-key map "\M-e" 'navi2ch-board-expire)
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

(defvar navi2ch-board-minor-mode-list
  '(navi2ch-board-bookmark-mode
    navi2ch-board-hide-mode
    navi2ch-board-updated-mode))

;; bookmark mode
(defvar navi2ch-board-bookmark-mode nil)
(defvar navi2ch-board-bookmark-mode-map nil)
(unless navi2ch-board-bookmark-mode-map
  (setq navi2ch-board-bookmark-mode-map (make-sparse-keymap))
  (define-key navi2ch-board-bookmark-mode-map "d"
    'navi2ch-board-delete-bookmark)
  (define-key navi2ch-board-bookmark-mode-map "a" 'undefined))

;; hide mode
(defvar navi2ch-board-hide-mode nil)
(defvar navi2ch-board-hide-mode-map nil)
(unless navi2ch-board-hide-mode-map
  (setq navi2ch-board-hide-mode-map (make-sparse-keymap))
  (define-key navi2ch-board-hide-mode-map "d"
    'navi2ch-board-cancel-hide)
  (define-key navi2ch-board-hide-mode-map "a" 'undefined))

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
  (navi2ch-board-save-info))
 
;;; navi2ch-board functions
(defsubst navi2ch-board-get-uri (board)
  "後ろの / が付いた uri を返す。
kako ならばそれに対応した uri にする"
  (let ((uri (cdr (assq 'uri board))))
    (when uri
      (when (string-match "[^/]$" uri)
	(setq uri (concat uri "/")))
      (concat uri (cdr (assq 'kako board))))))

(defsubst navi2ch-board-get-host (board)
  (let ((uri (cdr (assq 'uri board))))
    (when (string-match "http://\\([^/]+\\)" uri)
      (match-string 1 uri))))

(defsubst navi2ch-board-get-home-uri (board)
  "後ろの / が付いた home ディレクトリな uri を返す。"
  (let ((uri (cdr (assq 'uri board))))
    (when (string-match "http://.+/~[^/]+" uri)
      (concat (match-string 0 uri) "/"))))
	       
(defsubst navi2ch-board-get-url (board &optional file-name)
  (if (and file-name (string-match "^/" file-name))
      (concat "http://" (navi2ch-board-get-host board) file-name)
    (concat (navi2ch-board-get-uri board)
	    (or file-name navi2ch-board-subject-file-name))))

(defsubst navi2ch-board-get-readcgi-url (board)
  "read.cgi の板名までの url を返す。"
  (let ((uri (navi2ch-board-get-home-uri board)))
    (unless uri
      (setq uri (format "http://%s/" (navi2ch-board-get-host board))))
    (format "%stest/read.cgi/%s/" uri (cdr (assq 'id board)))))
	  
(defsubst navi2ch-board-equal (board1 board2)
  (string= (cdr (assq 'uri board1))
	   (cdr (assq 'uri board2))))
  
(defsubst navi2ch-board-get-file-name (board &optional file-name)
  (let ((uri (navi2ch-board-get-uri board)))
     (when (and uri (string-match "http://\\(.+\\)" uri))
       (setq uri (match-string 1 uri))
       (when (string-match ".+kako/" uri)
	 (setq uri (match-string 0 uri)))
       (navi2ch-expand-file-name
	(concat uri (or file-name navi2ch-board-subject-file-name))))))

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
		     (navi2ch-board-updated-article-p article seen))
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
	(setq navi2ch-board-current-board board)
	(run-hooks 'navi2ch-board-select-board-hook)
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
  (run-hooks 'navi2ch-board-mode-hook)
  (force-mode-line-update))

(defun navi2ch-board-save-old-subject-file (board)
  (let ((file (navi2ch-board-get-file-name board)))
    (when (file-exists-p file)
      (let ((coding-system-for-write navi2ch-net-coding-system))
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
	  (navi2ch-article-summary-element-set-seen element nil)
	  (setq changed t))
	(navi2ch-put-alist artid element summary)))
    (navi2ch-article-save-article-summary
     navi2ch-board-current-board summary)))

(defun navi2ch-board-get-readcgi-raw-url (board)
  (format "%s?raw=0.0" (navi2ch-board-get-readcgi-url board)))

(defun navi2ch-board-update-file (board)
  (unless navi2ch-offline
    (let ((file (navi2ch-board-get-file-name board))
	  (time (cdr (assq 'time board))))
      (navi2ch-net-update-file (navi2ch-board-get-url board) file time))))
  
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
      (when first
	(setq board (navi2ch-board-load-info board)))
      (unless navi2ch-offline
	(navi2ch-board-save-old-subject-file board)
	(setq header (navi2ch-board-update-file board))
	(setq time (cdr (assoc "Last-Modified" header)))
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

(defun navi2ch-board-set-mode-line ()
  (let* ((board navi2ch-board-current-board)
	 (host (cdr (assq 'uri board))))
    (string-match "http://\\([^/]+\\)" host)
    (setq host (match-string 1 host))
    (setq navi2ch-mode-line-identification
	  (format "%s (%s)" (cdr (assq 'name board)) host))
    (navi2ch-set-mode-line-identification)))

(defun navi2ch-board-write-message ()
  (interactive)
  (navi2ch-message-write-message navi2ch-board-current-board nil t))

(defun navi2ch-board-two-pain ()
  (interactive)
  (let* ((list-buf (get-buffer navi2ch-list-buffer-name))
	 (board-buf (get-buffer navi2ch-board-buffer-name))
	 (art-buf (navi2ch-article-current-buffer))
	 (list-win (get-buffer-window (or list-buf "")))
	 (art-win (get-buffer-window (or art-buf "")))
	 buf)
    (when board-buf
      (delete-other-windows)
      (switch-to-buffer board-buf)
      (setq buf
	    (cond ((and list-buf art-buf)
		   (cond ((and list-win art-win) art-buf)
			 (list-win art-buf)
			 (art-win list-buf)
			 (t art-buf)))
		  (list-buf list-buf)
		  (art-buf art-buf)))
      (when buf
	(if (eq buf list-buf)
	    (split-window-horizontally navi2ch-list-window-width)
	  (split-window-vertically navi2ch-board-window-height)
	  (other-window 1))
	(switch-to-buffer buf))
      (other-window 1))))
  

(defun navi2ch-board-save-info (&optional board)
  (or board (setq board navi2ch-board-current-board))
  (when board
    (navi2ch-save-info
     (navi2ch-board-get-file-name
      board "board.info")
     (list (assq 'bookmark board)
	   (assq 'hide board)
	   (assq 'time board)
	   (assq 'logo board)
	   (assq 'seen board)))))
   

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
	  (message msg))
      (message "can't select this line!"))))

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

;; まだテストしてない
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
				      (list (cons 'artid (match-string 0 file)))))
				  (directory-files dir nil "\\.dat$")))
	    (summary (navi2ch-article-load-article-summary board))
	    (key-datevec (navi2ch-get-offset-datevec
			  (navi2ch-make-datevec (current-time))
			  navi2ch-board-expire-date)))
	(message "expiring %s..." (cdr (assq 'name board)))
	(dolist (article article-list)
	  (let ((artid (cdr (assq 'artid article)))
		(file (navi2ch-article-get-file-name board article)))
	    (when (and (not (member artid (cdr (assq 'bookmark board))))
		       (not (navi2ch-bookmark-exist-all board article))
		       (navi2ch-board-expire-date-p
			key-datevec file
			(navi2ch-article-summary-element-access-time
			 (cdr (assoc artid summary)))))
	      (let ((info (navi2ch-article-get-info-file-name board article)))
		(when (file-exists-p info) (delete-file info))
		(delete-file file)
		(when (assq 'hide board)
		  (setcdr (assq 'hide board) (delq artid (cdr (assq 'hide board)))))
		(when (assoc artid summary)
		  (setq summary (delete (assoc artid summary) summary)))))))
	(navi2ch-board-save-info board)
	(navi2ch-article-save-article-summary board summary)
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

(defun navi2ch-board-cancel-hide ()
  (interactive)
  (navi2ch-board-delete-line 'hide 'delete
			     "Cancel hide article"))

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

(provide 'navi2ch-board)

;;; navi2ch-board.el ends here
