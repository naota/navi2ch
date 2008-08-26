;;; navi2ch-board.el --- subject list module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008 by
;; Navi2ch Project

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

;;; Commentary:

;;

;;; Code:
(provide 'navi2ch-board)
(defconst navi2ch-board-ident
  "$Id$")

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
    "^\\([0-9]+\\)\\.dat<>\\(.*\\)<\\([0-9]+\\)>\n"
    "^\\([0-9]+\\)\\.dat<>\\(.*\\)(\\([0-9]+\\))\n"
    "^\\([0-9]+\\)\\.dat,\\(.*\\)（\\([0-9]+\\)）\n"))

(defvar navi2ch-board-current-board nil)
(defvar navi2ch-board-subject-list nil)
(defvar navi2ch-board-old-subject-list nil)
(defvar navi2ch-board-subject-alist nil)
(defvar navi2ch-board-old-subject-alist nil)
(defvar navi2ch-board-subject-file-name "subject.txt")
(defvar navi2ch-board-old-subject-file-name "old-subject.txt")
(defvar navi2ch-board-last-seen-alist nil)

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
(defsubst navi2ch-board-set-property (begin end item)
  (put-text-property begin end 'article item))

(defsubst navi2ch-board-get-property (point)
  (get-text-property (save-excursion (goto-char point)
				     (beginning-of-line)
				     (point))
		     'article))

(defalias 'navi2ch-board-get-article 'identity)

(defsubst navi2ch-board-get-board (item)
  navi2ch-board-current-board)

(defsubst navi2ch-board-exit ()
  (run-hooks 'navi2ch-board-exit-hook)
  (navi2ch-board-save-info))

;; regist board
(navi2ch-bm-regist-board 'board 'navi2ch-board-select-board)

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-board-save-info)

;;; navi2ch-board functions
(defsubst navi2ch-board-get-uri (board)
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

(defcustom navi2ch-board-default-bbscgi-path "/test/bbs.cgi"
  "*bbs.cgi のデフォルトの path。"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-board-bbscgi-path-alist nil
  "*板 URL から bbs.cgi の path への alist。"
  :type '(repeat (cons (string :tag "URL") (string :tag "path")))
  :group 'navi2ch-board)

(defun navi2ch-board-get-bbscgi-path (board)
  "bbs.cgi の path を返す。"
  (let ((uri (navi2ch-board-get-uri board)))
    (or (cdr (assoc uri navi2ch-board-bbscgi-path-alist))
	navi2ch-board-default-bbscgi-path)))

(defun navi2ch-board-get-bbscgi-url (board)
  "bbs.cgi の url を返す。"
  (let ((uri (navi2ch-board-get-uri board)))
    (string-match "\\(.+\\)/[^/]+/$" uri)
    (concat (match-string 1 uri) (navi2ch-board-get-bbscgi-path board))))

(defsubst navi2ch-board-equal (board1 board2)
  (string= (cdr (assq 'uri board1))
	   (cdr (assq 'uri board2))))

(defsubst navi2ch-board-get-file-name (board &optional file-name)
  (navi2ch-multibbs-board-get-file-name board file-name))

(defsubst navi2ch-board-from-file-p (board)
  (string= (cdr (assq 'name board)) navi2ch-board-name-from-file))

(defsubst navi2ch-board-get-matched-article ()
  "match した結果から article を得る。"
  (let ((id (match-string 1))
	(str (match-string 2))
	(num (match-string 3)))
    ;; (setq str (navi2ch-replace-string "^ +" "" str)
    ;;       str (navi2ch-replace-string " +$" "" str))
    ;; は遅い。
    (when (string-match "^ +" str)
      (setq str (replace-match "" nil t str)))
    (when (string-match " +$" str)
      (setq str (replace-match "" nil t str)))
    (list (cons 'subject str)
	  (cons 'response num)
	  (cons 'artid id))))

(defun navi2ch-board-url-to-board (url)
  "URL から board を得る。"
  (let* ((alist (navi2ch-multibbs-url-to-board url))
	 (uri  (cdr (assq 'uri alist)))
	 (id   (cdr (assq 'id alist)))
	 board)
    (when id
      (dolist (x navi2ch-list-board-name-list)
	(when (string= (cdr (assq 'uri x)) uri)
	  (setq board x)))
      (or board
	  (let* ((alist (navi2ch-alist-list-to-alist
			 navi2ch-list-board-name-list
			 'id 'name))
		 (host (navi2ch-url-to-host uri))
		 (name (concat (or (cdr (assoc id alist))
				   "No Name")
			       "(" host ")")))
	    (list (cons 'uri uri)
		  (cons 'id id)
		  (cons 'type 'board)
		  (cons 'name name)))))))

(defun navi2ch-board-to-url (board)
  "BOARD から url に変換。"
  (navi2ch-board-get-uri board))

(defun navi2ch-board-get-subject-list (file)
  "FILE からスレの list を作る。"
  (when (file-exists-p file)
    (with-temp-buffer
      (navi2ch-board-insert-file-contents navi2ch-board-current-board file nil nil)
      (run-hooks 'navi2ch-board-get-subject-list-hook)
      (navi2ch-apply-filters navi2ch-board-current-board navi2ch-board-filter-list)
      (navi2ch-replace-html-tag-with-buffer)
      (goto-char (point-min))
      (let ((regexp (navi2ch-board-regexp-test))
	    ;; (file-list (directory-files
	    ;;		   (navi2ch-board-get-file-name
	    ;;		    navi2ch-board-current-board "") nil "\\.dat$"))
	    list)
	(if (null regexp)
	    nil
	  (while (re-search-forward regexp nil t)
	    (setq list (cons (navi2ch-board-get-matched-article) list)))
	  (nreverse list))))))

(defun navi2ch-board-get-updated-subject-list (board)
  "一時的に update してからスレの list を作る。"
  (let ((file (navi2ch-board-get-file-name board)))
    (unwind-protect
	(progn
	  (navi2ch-board-save-old-subject-file board)
	  (navi2ch-board-update-file board)
	  (navi2ch-board-get-subject-list file))
      (ignore-errors (navi2ch-board-save-old-subject-file board 'restore)))))

(defun navi2ch-board-updated-article-p (article seen)
  (let* ((artid (cdr (assq 'artid article)))
	 (res (cdr (assoc artid navi2ch-board-subject-alist)))
	 old-res)
    (when res
      (if (setq old-res (cdr (assoc artid navi2ch-board-old-subject-alist)))
	  (when (> (string-to-number res)
		   (or seen (string-to-number old-res)))
	    'updated)
	'new))))

(defun navi2ch-board-regexp-test ()
  (save-excursion
    (beginning-of-line)
    (catch 'loop
      (dolist (regexp navi2ch-board-regexp-list)
	(when (looking-at regexp)
	  (throw 'loop regexp))))))


(defun navi2ch-board-insert-subjects (list)
  (let ((bookmark (cdr (assq 'bookmark navi2ch-board-current-board)))
	(hide (cdr (assq 'hide navi2ch-board-current-board)))
	(summary (navi2ch-article-load-article-summary
		  navi2ch-board-current-board))
	(i 1))
    (dolist (article list)
      (let* ((artid (cdr (assq 'artid article)))
	     (seen (navi2ch-article-summary-element-seen
		    (cdr (assoc artid summary))))
	     updated)
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
	   (let ((res (cdr (assq 'response article)))
		 (last (and navi2ch-board-insert-subject-with-diff
			    (or seen
				(cdr (assoc artid navi2ch-board-last-seen-alist))
				(catch 'break
				  (string-to-number
				   (or (cdr (assoc artid navi2ch-board-old-subject-alist))
				       (throw 'break nil)))))))
		 (read (and navi2ch-board-insert-subject-with-unread
			    (navi2ch-article-get-last-read-number
			     navi2ch-board-current-board
			     article))))
	     (concat "("
		     (format "%4s" res)
		     (and navi2ch-board-insert-subject-with-diff
			  (concat
			   "/"
			   (if last
			       (format "%5s"
				       (format "+%d"
					       (- (string-to-number res) last)))
			     "    -")))
		     (and navi2ch-board-insert-subject-with-unread
			  (concat
			   "/"
			   (if read
			       (substring
				(format "   Δ%d"
					(max 0
					     (- (string-to-number res) read)))
				-5)
			     "     -")))
		     ")"))
	   (cond ((and navi2ch-board-check-updated-article-p
		       (setq updated
			     (navi2ch-board-updated-article-p article seen)))
		  updated)
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
	(setq navi2ch-board-current-board (navi2ch-board-load-info board))
	(run-hooks 'navi2ch-board-select-board-hook)
	(navi2ch-board-sync force 'first)))))

(easy-menu-define navi2ch-board-mode-menu
  navi2ch-board-mode-map
  "Menu used in navi2ch-board"
  navi2ch-board-mode-menu-spec)

(defun navi2ch-board-setup-menu ()
  (easy-menu-add navi2ch-board-mode-menu))

(defun navi2ch-board-mode ()
  "\\{navi2ch-board-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-board-mode)
  (setq mode-name "Navi2ch Board")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-board-mode-map)
  (navi2ch-board-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-board-mode-hook)
  (force-mode-line-update))

(defun navi2ch-board-save-old-subject-file (board &optional restore)
  (let ((from (navi2ch-board-get-file-name board))
	(to (navi2ch-board-get-file-name
	     board navi2ch-board-old-subject-file-name)))
    (when restore
      (setq from (prog1 to
		   (setq to from))))
    (when (file-exists-p from)
      (let ((coding-system-for-write (navi2ch-board-get-coding-system board)))
	(with-temp-file to
	  (navi2ch-board-insert-file-contents board from))))))

(defun navi2ch-board-update-seen-articles ()
  (let ((summary (navi2ch-article-load-article-summary
		  navi2ch-board-current-board)))
    (dolist (x summary)
      (let* ((element (cdr x))
	     (artid (car x))
	     (seen (navi2ch-article-summary-element-seen element)))
	(when (navi2ch-board-updated-article-p
	       (list (cons 'artid artid))
	       seen)
	  (setq navi2ch-board-last-seen-alist
		(navi2ch-put-alist
		 artid seen navi2ch-board-last-seen-alist))
	  (navi2ch-article-summary-element-set-seen element nil))
	(navi2ch-put-alist artid element summary)))
    (navi2ch-article-save-article-summary
     navi2ch-board-current-board summary)))

(defun navi2ch-board-update-file (board)
  (unless navi2ch-offline
    (navi2ch-multibbs-board-update board)))

(defun navi2ch-board-sync (&optional force first)
  (interactive "P")
  (run-hooks 'navi2ch-board-before-sync-hook)
  (save-excursion
    (let* ((buffer-read-only nil)
	   (navi2ch-net-force-update (or navi2ch-net-force-update
					 force))
	   (board navi2ch-board-current-board)
	   (file (navi2ch-board-get-file-name board))
	   (old-file (navi2ch-board-get-file-name
		      board
		      navi2ch-board-old-subject-file-name))
	   time header)
      (unless navi2ch-offline
	(navi2ch-board-save-old-subject-file board)
	(setq header (navi2ch-board-update-file board))
	(setq time (and (not (navi2ch-net-get-state 'not-updated header))
			(not (navi2ch-net-get-state 'error header))
			(or (cdr (assq 'last-modified header))
			    (cdr (assq 'date header)))))
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
	(setq navi2ch-board-last-seen-alist nil)
	(when time
	  (navi2ch-board-update-seen-articles))
	(navi2ch-board-insert-subjects navi2ch-board-subject-list)
	(navi2ch-board-save-info)
	(navi2ch-board-set-mode-line))))
  (run-hooks 'navi2ch-board-after-sync-hook))

(defun navi2ch-board-make-subject-txt ()
  "subback.html から (navi2ch 用の) subject.txt を作る。
`navi2ch-net-update-file' のハンドラ。"
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(case-fold-search t)
	(beg (point)))
    (while (re-search-forward
	    ;; この正規表現も仕様変更でだめになるのかも。
	    ;;   ……なりました。
	    "<a +href=\"\\([0-9]+\\)[^>]*\">[0-9]+[^0-9].\\(.*\\)</a>" nil t)
      (let ((dat (match-string 1))
	    (title (match-string 2)))
	(delete-region beg (point))
	(insert (format "%s.dat<>%s\n" dat title))
	(setq beg (point))))
    (delete-region beg (point-max))))

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

(defun navi2ch-board-expire-date-p (key-time file access-time)
  (let ((time (ignore-errors (or access-time
				 (navi2ch-file-mtime file)))))
    (and time key-time
	 (navi2ch-compare-times key-time time))))

(defun navi2ch-board-expire (&optional board ask)
  (interactive)
  (and (interactive-p) (setq ask t))
  (or board (setq board navi2ch-board-current-board))
  (unless (eq board navi2ch-board-current-board)
    (setq board (navi2ch-board-load-info board)))
  (let ((dir (navi2ch-board-get-file-name board "")))
    (when (and (file-exists-p dir)
	       (or (not ask) (y-or-n-p "Expire current borad? "))
	       navi2ch-board-expire-date)
      (let ((article-list (mapcar (lambda (file)
				    (list (cons 'artid
						(navi2ch-article-file-name-to-artid file))))
				  (directory-files dir nil navi2ch-article-local-dat-regexp)))
	    (summary (navi2ch-article-load-article-summary board))
	    (key-time (navi2ch-add-days-to-time (current-time)
						(- navi2ch-board-expire-date)))
	    (remove-list nil))
	(message "Expiring %s..." (cdr (assq 'name board)))
	(dolist (article article-list)
	  (let ((artid (cdr (assq 'artid article)))
		(file (navi2ch-article-get-file-name board article)))
	    (when (and (or navi2ch-board-expire-bookmark-p
			   (not (or (member artid (cdr (assq 'bookmark board)))
				    (navi2ch-bookmark-exist-all board
								article))))
		       (or (not navi2ch-board-expire-orphan-only)
			   (navi2ch-article-orphan-p board article))
		       (navi2ch-board-expire-date-p
			key-time file
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
	(message "Expiring %s...done" (cdr (assq 'name board)))))))

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

(defun navi2ch-board-update-buffer (buffer)
  (let (point)
    (with-current-buffer buffer
      (let ((buffer-read-only nil)
	    (artid (cdr (assq 'artid (get-text-property (point) 'article))))
	    p)
	(erase-buffer)
	(navi2ch-board-insert-subjects navi2ch-board-subject-list)
	(setq p (point-min))
	(while p
	  (if (equal artid (cdr (assq 'artid
				      (get-text-property p 'article))))
	      (progn
		(goto-char p)
		(setq point p
		      p nil))
	    (setq p (next-single-property-change p 'article))))))
    (if point
	;; 表示してるバッファのカーソル位置をすべて更新する。
	(save-selected-window
	  (dolist (win (get-buffer-window-list buffer nil t))
	    (select-window win)
	    (goto-char point))))))

(defun navi2ch-board-add-bookmark-subr (board article)
  (let ((buffer (get-buffer navi2ch-board-buffer-name))
	(artid (cdr (assq 'artid article)))
	current-board list)
    (setq board
	  (if (and (bufferp buffer)
		   (eq (with-current-buffer buffer major-mode)
		       'navi2ch-board-mode)
		   (equal (cdr (assq 'id navi2ch-board-current-board))
			  (cdr (assq 'id board))))
	      (progn
		(setq current-board t)
		navi2ch-board-current-board)
	    (navi2ch-board-load-info board)))
    (setq list (cdr (assq 'bookmark board)))
    (when (and artid (not (member artid list)))
      (setq board (navi2ch-put-alist 'bookmark (cons artid list) board))
      (message "Add bookmark")
      (if current-board
	  (with-current-buffer buffer
	    (setq navi2ch-board-current-board board)
	    (if navi2ch-board-bookmark-mode
		(navi2ch-board-update-buffer buffer)))
	(navi2ch-board-save-info board)))))

(defun navi2ch-board-add-bookmark ()
  (interactive)
  (unless navi2ch-board-bookmark-mode
    (let ((article (get-text-property (point) 'article)))
      (if article
	  (navi2ch-board-add-bookmark-subr navi2ch-board-current-board
					   article)
	(message "Can't select this line!")))))

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

(defun navi2ch-board-get-coding-system (board)
  "`navi2ch-board-coding-system-alist' に BOARD に対応する coding-system があ
ればそれを返す。
ない場合は `navi2ch-coding-system' を返す。"
  (or (cdr (assoc (cdr (assq 'id board)) navi2ch-board-coding-system-alist))
      navi2ch-coding-system))

(defun navi2ch-board-insert-file-contents (board file &optional begin end)
  "`navi2ch-board-get-coding-system' で特定される coding-system で
`navi2ch-insert-file-contents' する。"
  (navi2ch-insert-file-contents
   file begin end
   (navi2ch-board-get-coding-system board)))

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
