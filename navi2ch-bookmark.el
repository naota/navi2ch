;;; navi2ch-bookmark.el --- global bookmark module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008 by Navi2ch
;; Project

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
(provide 'navi2ch-bookmark)
(defconst navi2ch-bookmark-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-bookmark-mode-map nil)
(unless navi2ch-bookmark-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    ;; (define-key map "q" 'navi2ch-bookmark-exit)
    (define-key map "i" 'navi2ch-bookmark-fetch-article)
    (define-key map "D" 'navi2ch-bookmark-delete)
    (define-key map "d" 'navi2ch-bookmark-cut)
    (define-key map "\C-k" 'navi2ch-bookmark-cut)
    (define-key map "\C-y" 'navi2ch-bookmark-yank)
    (define-key map "o" 'navi2ch-bookmark-move)
    (define-key map "O" 'navi2ch-bookmark-copy)
    (define-key map "mi" 'navi2ch-bookmark-fetch-mark-article)
    (define-key map "md" 'navi2ch-bookmark-cut-mark-article)
    (define-key map "mD" 'navi2ch-bookmark-delete-mark-article)
    (define-key map "mo" 'navi2ch-bookmark-move-mark-article)
    (define-key map "s" 'navi2ch-bookmark-sync)
    (define-key map "X" 'navi2ch-bookmark-remember-order)
    (define-key map "S" 'navi2ch-bookmark-sort)
    (setq navi2ch-bookmark-mode-map map)))

(defvar navi2ch-bookmark-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Bookmark"
   nil))

(defvar navi2ch-bookmark-list nil
  "bookmark を表すリスト。
\((BOOKMARK-ID BOOKMARK-NAME
  (KEY
   (board BOARD)
   (article ARTICLE))...)
 ...)
という形をしている。
KEY は (concat URI ARTID)")
(defvar navi2ch-bookmark-cut-stack nil)
(defvar navi2ch-bookmark-current-bookmark-id nil)
(defvar navi2ch-bookmark-fetch-mark-article-check-update t)
(defvar navi2ch-bookmark-fetch-mark-article-no-check-regexp nil)

;;; navi2ch-bm callbacks
(defun navi2ch-bookmark-set-property (begin end item)
  (put-text-property begin end 'item item))

(defsubst navi2ch-bookmark-get-property (point)
  (get-text-property
   (save-excursion (goto-char point)
		   (beginning-of-line)
		   (point))
   'item))

(defsubst navi2ch-bookmark-get-article (item)
  (cdr (assq 'article
	     (cdr (assoc item
			 (cddr (assoc navi2ch-bookmark-current-bookmark-id
				      navi2ch-bookmark-list)))))))

(defsubst navi2ch-bookmark-get-board (item)
  (cdr (assq 'board
	     (cdr (assoc item
			 (cddr (assoc navi2ch-bookmark-current-bookmark-id
				      navi2ch-bookmark-list)))))))

(defsubst navi2ch-bookmark-exit ()
  (run-hooks 'navi2ch-bookmark-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'bookmark 'navi2ch-bookmark)

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-bookmark-save-info)
(add-hook 'navi2ch-load-status-hook 'navi2ch-bookmark-load-info)

;;; navi2ch-bookmark functions
(defun navi2ch-bookmark-convert ()
  (interactive)
  (unless navi2ch-bookmark-list
    (navi2ch-bookmark-load-info))
  (let ((id (navi2ch-read-string "input bookmark id: " "bmark"))
	(name (navi2ch-read-string "input bookmark name: " "ブックマーク"))
	(list navi2ch-bookmark-list))
    (setq navi2ch-bookmark-list
	  (list (append (list id name)
			list))))
  (navi2ch-bookmark-save-info))

(defun navi2ch-bookmark-fix ()
  (interactive)
  (dolist (bookmark navi2ch-bookmark-list)
    (while (assoc "" bookmark)
      (delete (assoc "" bookmark) bookmark))))

(defun navi2ch-bookmark-read-id (prompt)
  (completing-read prompt navi2ch-bookmark-list nil nil))

(defun navi2ch-bookmark-get-key (board article)
  (concat (cdr (assq 'uri board))
	  (cdr (assq 'artid article))))

(defun navi2ch-bookmark-exist (bookmark-id board article)
  (assoc (navi2ch-bookmark-get-key board article)
	 (cddr (assoc bookmark-id navi2ch-bookmark-list))))

(defun navi2ch-bookmark-exist-all (board article)
  (catch 'exist
    (dolist (id (mapcar 'car navi2ch-bookmark-list))
      (when (navi2ch-bookmark-exist id board article)
	(throw 'exist t)))))

(defun navi2ch-bookmark-create-bookmark (bookmark-id)
  "ブックマーク BOOKMARK-ID を追加する。"
  (unless (assoc bookmark-id navi2ch-bookmark-list)
    (when (member bookmark-id
		  (mapcar (lambda (x) (cdr (assq 'id x)))
			  navi2ch-list-board-name-list))
      (error "Can't create this id's bookmark!"))
    (let ((name (read-string
		 (concat "Input bookmark name for [" bookmark-id "]: ")
		 bookmark-id)))
      (push (list bookmark-id name)
	    navi2ch-bookmark-list)
      (save-excursion
	(set-buffer navi2ch-list-buffer-name)
	(navi2ch-list-sync-global-bookmark-category))
      (navi2ch-bookmark-save-info))))

(defun navi2ch-bookmark-delete-bookmark (bookmark-id)
  "ブックマーク BOOKMARK-ID を削除する。"
  (let ((bookmark (assoc bookmark-id navi2ch-bookmark-list)))
    (when (y-or-n-p (concat "delete global bookmark "
			    (cadr bookmark) "? "))
      (setq navi2ch-bookmark-list (delete bookmark navi2ch-bookmark-list))
      (save-current-buffer
	(set-buffer navi2ch-list-buffer-name)
	(navi2ch-list-sync-global-bookmark-category))
      (navi2ch-bookmark-save-info))))

(defun navi2ch-bookmark-change-bookmark (bookmark-id)
  "ブックマーク BOOKMARK-ID の ID、名称を変更する。"
  (let* ((bookmark (assoc bookmark-id navi2ch-bookmark-list))
	 (id (navi2ch-read-string "new bookmark ID: " (car bookmark)))
	 (name (navi2ch-read-string "new bookmark name: " (cadr bookmark))))
    (setcar bookmark id)
    (setcar (cdr bookmark) name)
    (save-excursion
      (set-buffer navi2ch-list-buffer-name)
      (navi2ch-list-sync-global-bookmark-category))
    (navi2ch-bookmark-save-info)))

(defun navi2ch-bookmark-add-subr (bookmark-id board article)
  "BOARD と ARTICLE で表される スレッドを追加。"
  (unless (assoc bookmark-id navi2ch-bookmark-list)
    (navi2ch-bookmark-create-bookmark bookmark-id))
  (let ((bookmark (assoc bookmark-id navi2ch-bookmark-list)))
    (setcdr (cdr bookmark)
	    (navi2ch-put-alist
	     (navi2ch-bookmark-get-key board article)
	     (list (cons 'board board)
		   (cons 'article article))
	     (cddr bookmark))))
  (navi2ch-bookmark-save-info))

(defun navi2ch-bookmark-add (bookmark-id board article)
  (navi2ch-bookmark-add-subr bookmark-id board article)
  (message "Add current article to global bookmark"))

(defun navi2ch-bookmark-insert-subject (num item)
  (navi2ch-bm-insert-subject
   item num
   (cdr (assq 'subject (navi2ch-bookmark-get-article item)))
   (format "[%s]" (cdr (assq 'name (navi2ch-bookmark-get-board item))))))

(defun navi2ch-bookmark-insert-subjects ()
  (let ((i 1))
    (dolist (x (cddr (assoc navi2ch-bookmark-current-bookmark-id
			    navi2ch-bookmark-list)))
      (navi2ch-bookmark-insert-subject i (car x))
      (setq i (1+ i)))))

(defun navi2ch-bookmark-delete-key (bookmark-id key)
  "ブックマーク BOOKMARK-ID からスレッドを削除する。
削除されるのは KEY で表わされるスレッド。"
  (let* ((bookmark (assoc bookmark-id navi2ch-bookmark-list))
	 (node (assoc key (cddr bookmark))))
    (when bookmark
      (setcdr bookmark
	      (delete node (cdr bookmark)))
      (navi2ch-bookmark-save-info)
      t)))

(defun navi2ch-bookmark-delete-article (bookmark-id board article)
  "ブックマーク BOOKMARK-ID からスレッドを削除する。
削除されるのは BOARD, ARTICLE で表わされるスレッド。"
  (navi2ch-bookmark-delete-key bookmark-id
			       (navi2ch-bookmark-get-key board article)))

(defun navi2ch-bookmark-delete-article-all (board article)
  "BOARD, ARTICLE で表わされるスレッドを全てのブックマークから削除する。"
  (dolist (node navi2ch-bookmark-list)
    (navi2ch-bookmark-delete-article (car node) board article)))

(defun navi2ch-bookmark-delete-subr ()
  "その行を bookmark から確認なしで削除する。"
  (let ((item (save-excursion (beginning-of-line)
			      (navi2ch-bookmark-get-property (point))))
	(buffer-read-only nil))
    (if item
	(when (navi2ch-bookmark-delete-key
	       navi2ch-bookmark-current-bookmark-id item)
	  (delete-region (save-excursion (beginning-of-line) (point))
			 (save-excursion (forward-line) (point)))
	  (navi2ch-bm-renumber)
	  (and (eobp) (not (bobp))
	       (forward-line -1)))
      (message "Can't select this line!"))))

(defun navi2ch-bookmark-delete ()
  "その行を bookmark から削除する。"
  (interactive)
  (if (y-or-n-p "Delete this line? ")
      (navi2ch-bookmark-delete-subr)))

(defun navi2ch-bookmark-delete-mark-article ()
  (interactive)
  (if (y-or-n-p "Delete these lines? ")
      (navi2ch-bm-exec-subr 'navi2ch-bookmark-delete-subr)))

(defun navi2ch-bookmark-copy (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Copy to: ")))
  (if (equal bookmark-id
	     navi2ch-bookmark-current-bookmark-id)
      (message "Same bookmark")
    (save-excursion
      (beginning-of-line)
      (let ((item (navi2ch-bookmark-get-property (point))))
	(navi2ch-bookmark-add-subr bookmark-id
				   (navi2ch-bookmark-get-board item)
				   (navi2ch-bookmark-get-article item))
	(navi2ch-bookmark-save-info)))))

(defun navi2ch-bookmark-move (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Move to: ")))
  (if (equal bookmark-id
	     navi2ch-bookmark-current-bookmark-id)
      (message "Same bookmark")
    (navi2ch-bookmark-copy bookmark-id)
    (navi2ch-bookmark-delete-subr)))

(defun navi2ch-bookmark-move-mark-article (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Move to: ")))
  (navi2ch-bm-exec-subr 'navi2ch-bookmark-move bookmark-id))

(defun navi2ch-bookmark-cut ()
  (interactive)
  (let ((item (save-excursion (beginning-of-line)
			      (navi2ch-bookmark-get-property (point))))
	(bookmark (assoc navi2ch-bookmark-current-bookmark-id
			 navi2ch-bookmark-list)))
    (if item
	(progn
	  (push (assoc item (cddr bookmark))
		navi2ch-bookmark-cut-stack)
	  (navi2ch-bookmark-delete-subr))
      (message "Can't select this line!"))))

(defun navi2ch-bookmark-cut-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bookmark-cut))

(defun navi2ch-bookmark-yank ()
  (interactive)
  (let ((pair (pop navi2ch-bookmark-cut-stack))
	(bookmark (assoc navi2ch-bookmark-current-bookmark-id
			 navi2ch-bookmark-list)))
    (if pair
	(progn
	  (if (eobp)
	      (setcdr (last bookmark)
		      (list pair))
	    (let ((list (member (assoc (navi2ch-bookmark-get-property (point))
				       (cddr bookmark))
				(cddr bookmark))))
	      (setcdr list (copy-sequence list))
	      (setcar list pair)))
	  (beginning-of-line)
	  (let ((buffer-read-only nil))
	    (navi2ch-bookmark-insert-subject 0 (car pair)))
	  (navi2ch-bm-renumber)
	  (forward-line -1))
      (message "Stack is empty"))))

;; (defun navi2ch-bookmark-exit ()
;;   (interactive)
;;   (bury-buffer)
;;   (set-window-configuration navi2ch-bookmark-window-configuration))

(defun navi2ch-bookmark-goto-bookmark (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Bookmark ID: ")))
  (let ((bookmark (assoc bookmark-id
			 navi2ch-bookmark-list)))
    (navi2ch-list-select-board
     (list (cons 'id (car bookmark))
	   (cons 'name (cadr bookmark))
	   (cons 'type 'bookmark)))))

(defun navi2ch-bookmark (board &rest args)
  "bookmark を表示する。"
  (let ((bookmark-id (cdr (assq 'id board))))
    (navi2ch-bookmark-mode)
    (setq navi2ch-bookmark-current-bookmark-id bookmark-id)
    (navi2ch-bm-setup 'navi2ch-bookmark)
    (navi2ch-bookmark-sync)))

(defun navi2ch-bookmark-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-bookmark-insert-subjects))))

(easy-menu-define navi2ch-bookmark-mode-menu
  navi2ch-bookmark-mode-map
  "Menu used in navi2ch-bookmark"
  navi2ch-bookmark-mode-menu-spec)

(defun navi2ch-bookmark-setup-menu ()
  (easy-menu-add navi2ch-bookmark-mode-menu))

(defun navi2ch-bookmark-mode ()
  "\\{navi2ch-bookmark-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-bookmark-mode)
  (setq mode-name "Navi2ch Bookmark")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-bookmark-mode-map)
  (navi2ch-bookmark-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-bookmark-mode-hook))

(defun navi2ch-bookmark-save-info ()
  (let ((info (mapcar (lambda (y)
			(append
			 (list (car y) (cadr y))
			 (mapcar
			  (lambda (x)
			    (let ((board (cdr (assq 'board (cdr x))))
				  (article (cdr (assq 'article (cdr x)))))
			      (cons (car x)
				    (list (cons 'board
						(list
						 (assq 'name board)
						 (assq 'uri board)
						 (assq 'id board)))
					  (cons 'article
						(list
						 (assq 'subject article)
						 (assq 'artid article)))))))
			  (cddr y))))
		      navi2ch-bookmark-list)))
    (navi2ch-save-info navi2ch-bookmark-file info t)))

(defun navi2ch-bookmark-load-info ()
  (setq navi2ch-bookmark-list
	(navi2ch-load-info navi2ch-bookmark-file)))

(defun navi2ch-bookmark-fetch-article (&optional force)
  ;; navi2ch-bm-fetch-article の wrapper として働き、subject が nil なら
  ;; ファイルから subject を見つけて更新する。
  (interactive "P")
  (let* ((item (navi2ch-bookmark-get-property (point)))
         (board (navi2ch-bookmark-get-board item))
         (article (navi2ch-bookmark-get-article item))
         state)
    (setq state (navi2ch-bm-fetch-article force))
    (unless (assq 'subject article)
      (let ((newsubject
	     (let ((file (navi2ch-article-get-file-name board article)))
	       (when (and file
			  (file-exists-p file))
                 ;; dat を 全部読まず、頭の1行だけ読むのはどうすれば?
		 ;; 俺も知らないです:-)
		 (cdr (assq 'subject
			    (navi2ch-article-get-first-message-from-file file board)))))))
	(when newsubject
	  (setq article (navi2ch-put-alist 'subject newsubject article))
	  (navi2ch-bookmark-add-subr
	   navi2ch-bookmark-current-bookmark-id board article))))
    state))

(defun navi2ch-bookmark-fetch-mark-article (&optional force)
  (interactive "P")
  (unless navi2ch-offline
    (when navi2ch-bookmark-fetch-mark-article-check-update
      (save-excursion
	(let ((board-data-cache (navi2ch-make-cache nil 'equal)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (navi2ch-bm-goto-mark-column)
	    (when (looking-at "\\*")
	      (let* ((item (navi2ch-bm-get-property-internal (point)))
		     (board (navi2ch-bm-get-board-internal item))
		     (article (navi2ch-bm-get-article-internal item))
		     ;; (bbstype (cdr (assq 'bbstype board)))
		     (board-uri (cdr (assq 'uri board)))
		     (art-id (cdr (assq 'artid article)))
		     (res (string-to-number
			   (or (cdr (assq 'response
					  (navi2ch-article-load-info board
								     article)))
			       "0")))
		     new-res board-data)
		(when (or (null navi2ch-bookmark-fetch-mark-article-no-check-regexp)
			  (not (string-match navi2ch-bookmark-fetch-mark-article-no-check-regexp
					     board-uri)))
		  (setq board-data
			(navi2ch-cache-get board-uri
					   (mapcar
					    (lambda (x)
					      (cons (cdr (assq 'artid x))
						    (string-to-number (cdr (assq 'response x)))))
					    (navi2ch-board-get-updated-subject-list board))
					   board-data-cache))
		  (when (and res
			     (setq new-res (cdr (assoc art-id board-data)))
			     (navi2ch-bm-get-state)
			     (<= new-res res))
		    (navi2ch-bm-unmark)))))
	    (forward-line)))))
    (navi2ch-bm-exec-subr #'navi2ch-bookmark-fetch-article force)))

(defun navi2ch-bookmark-change (changed-list)
  "変更された板の bookmark を修正する。
CHANGED-LIST については `navi2ch-list-get-changed-status' を参照。"
  (setq navi2ch-bookmark-list
	(mapcar
	 (lambda (bookmark)
	   (append (list (car bookmark)
			 (cadr bookmark))
		   (mapcar
		    (lambda (node)
		      (let* ((board (cdr (assq 'board node)))
			     (article (cdr (assq 'article node)))
			     (changed (assoc (cdr (assq 'id board))
					     changed-list)))
			(if changed
			    (let ((new-board (caddr changed)))
			      (list
			       (navi2ch-bookmark-get-key new-board article)
			       (cons 'board new-board)
			       (cons 'article article)))
			  node)))
		    (cddr bookmark))))
	 navi2ch-bookmark-list))
  (navi2ch-bookmark-save-info))

(defun navi2ch-bookmark-get-buffer (bookmark-id)
  "BOOKMARK-ID で指定する bookmark バッファを返す。"
  (save-current-buffer
    (let ((buf (get-buffer navi2ch-board-buffer-name)))
      (when (and buf
		 (progn		      ; 囲む必要なさそうだけど念のため
		   (set-buffer buf)
		   (eq major-mode 'navi2ch-bookmark-mode))
		 bookmark-id
		 (equal navi2ch-bookmark-current-bookmark-id bookmark-id))
	buf))))

(defun navi2ch-bookmark-remember-order ()
  "ブックマークの現在のスレの並び順を記憶する。"
  (interactive)
  (let ((bookmark (assoc navi2ch-bookmark-current-bookmark-id
			 navi2ch-bookmark-list))
	list item)
    (save-excursion
      (goto-char (point-max))
      (while (eq 0 (forward-line -1))
	(and (setq item
		   (navi2ch-bookmark-get-property (point)))
	     (push (assoc item (cddr bookmark))
		   list))))
    (setcdr (cdr bookmark) list)
    (navi2ch-bm-renumber)))

(defun navi2ch-bookmark-sort (&optional arg)
  (interactive "P")
  (navi2ch-bm-sort arg)
  (and navi2ch-bookmark-remember-order-after-sort
       (navi2ch-bookmark-remember-order)))

(run-hooks 'navi2ch-bookmark-load-hook)
;;; navi2ch-bookmark.el ends here
