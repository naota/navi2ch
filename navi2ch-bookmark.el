;;; navi2ch-bookmark.el --- global bookmark module for navi2ch

;; Copyright (C) 2001 by 2ちゃんねる

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

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))
(require 'navi2ch-board-misc)

(defvar navi2ch-bookmark-mode-map nil)
(unless navi2ch-bookmark-mode-map
  (let ((map (copy-keymap navi2ch-bm-mode-map)))
    ;; (define-key map "q" 'navi2ch-bookmark-exit)
    (define-key map "i" 'navi2ch-bookmark-fetch-article)
    (define-key map "mi" 'navi2ch-bookmark-fetch-mark-article)
    (define-key map "d" 'navi2ch-bookmark-delete)
    (define-key map "\C-k" 'navi2ch-bookmark-cut)
    (define-key map "\C-y" 'navi2ch-bookmark-yank)
    (define-key map "o" 'navi2ch-bookmark-move)
    (define-key map "O" 'navi2ch-bookmark-copy)
    (setq navi2ch-bookmark-mode-map map)))
    
(defvar navi2ch-bookmark-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Bookmark"
   nil))

(defvar navi2ch-bookmark-list nil "bookmark を表すリスト")
(defvar navi2ch-bookmark-cut-stack nil)
(defvar navi2ch-bookmark-current-bookmark-id nil)

;;; navi2ch-bm callbacks
(defun navi2ch-bookmark-set-property (begin end item)
  (put-text-property begin end 'item item))

(defun navi2ch-bookmark-get-property (point)
  (get-text-property
   (save-excursion (goto-char point)
		   (beginning-of-line)
		   (point))
   'item))

(defun navi2ch-bookmark-get-article (item)
  (cdr (assq 'article
	     (cdr (assoc item
			 (cddr (assoc navi2ch-bookmark-current-bookmark-id
				      navi2ch-bookmark-list)))))))

(defun navi2ch-bookmark-get-board (item)
  (cdr (assq 'board
	     (cdr (assoc item
			 (cddr (assoc navi2ch-bookmark-current-bookmark-id
				      navi2ch-bookmark-list)))))))

(defun navi2ch-bookmark-exit ())

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
  (let ((id (read-string "input bookmark id: " "bmark"))
	(name (read-string "input bookmark name: " "ブックマーク"))
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

(defun navi2ch-bookmark-exist (bookmark-id board article)
  (assoc (concat (cdr (assq 'uri board))
		 (cdr (assq 'artid article)))
	 (cddr (assoc bookmark-id navi2ch-bookmark-list))))

(defun navi2ch-bookmark-exist-all (board article)
  (catch 'exist
    (dolist (id (mapcar 'car navi2ch-bookmark-list))
      (when (navi2ch-bookmark-exist id board article)
	(throw 'exist t)))))

(defun navi2ch-bookmark-create-bookmark (bookmark-id)
  (unless (assoc bookmark-id navi2ch-bookmark-list)
    (when (member bookmark-id
		  (mapcar (lambda (x) (cdr (assq 'id x)))
			  (navi2ch-list-get-board-name-list
			   navi2ch-list-category-list)))
      (error "Can't create this id's bookmark!"))
    (let ((name (read-string (concat "Input bookmark name for [" bookmark-id "]: ")
			     bookmark-id)))
      (push (list bookmark-id name)
	    navi2ch-bookmark-list)
      (save-excursion
	(set-buffer navi2ch-list-buffer-name)
	(navi2ch-list-sync-global-bookmark-category)))))

(defun navi2ch-bookmark-delete-bookmark (bookmark-id)
  (let ((bookmark (assoc bookmark-id navi2ch-bookmark-list)))
    (when (y-or-n-p (concat "delete global bookmark "
			    (cadr bookmark) "? "))
      (setq navi2ch-bookmark-list (delete bookmark navi2ch-bookmark-list))
      (save-excursion
	(set-buffer navi2ch-list-buffer-name)
	(navi2ch-list-sync-global-bookmark-category)))))
  
(defun navi2ch-bookmark-change-bookmark (bookmark-id)
  (let* ((bookmark (assoc bookmark-id navi2ch-bookmark-list))
	 (id (read-string "new bookmark ID: " (car bookmark)))
	 (name (read-string "new bookmark name: " (cadr bookmark))))
    (setcar bookmark id)
    (setcar (cdr bookmark) name)
    (save-excursion
      (set-buffer navi2ch-list-buffer-name)
      (navi2ch-list-sync-global-bookmark-category))))
    
(defun navi2ch-bookmark-add-subr (bookmark-id board article)
  "BOARD と ARTICLE で表される スレッドを追加"
  (unless (assoc bookmark-id navi2ch-bookmark-list)
    (navi2ch-bookmark-create-bookmark bookmark-id))
  (let* ((item (concat (cdr (assq 'uri board))
		     (cdr (assq 'artid article))))
	 (bookmark (assoc bookmark-id navi2ch-bookmark-list)))
    (setcdr (cdr bookmark)
	    (navi2ch-put-alist
	     item (list
		   (cons 'board board)
		   (cons 'article article))
	     (cddr bookmark))))
  (message "Add current article to global bookmark."))

(defun navi2ch-bookmark-add (bookmark-id board article)
  (navi2ch-bookmark-add-subr bookmark-id board article))
   
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

(defun navi2ch-bookmark-delete ()
  "その行を bookmark から削除する"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((item (navi2ch-bookmark-get-property (point)))
	  (bookmark (assoc navi2ch-bookmark-current-bookmark-id
			   navi2ch-bookmark-list)))
      (if item
	  (let ((pair (assoc item (cddr bookmark)))
		(buffer-read-only nil))
	    (setcdr (cdr bookmark)
		    (delq pair (cddr bookmark)))
	    (delete-region (point)
			   (save-excursion (forward-line) (point)))
	    (navi2ch-bm-renumber))
	(message "Can't select this line!")))))

(defun navi2ch-bookmark-copy (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "copy to: ")))
  (save-excursion
    (beginning-of-line)
    (let ((item (navi2ch-bookmark-get-property (point))))
      (navi2ch-bookmark-add-subr bookmark-id
				 (navi2ch-bookmark-get-board item)
				 (navi2ch-bookmark-get-article item)))))

(defun navi2ch-bookmark-move (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "move to: ")))
  (navi2ch-bookmark-copy bookmark-id)
  (navi2ch-bookmark-delete))
  
(defun navi2ch-bookmark-cut ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((item (navi2ch-bookmark-get-property (point)))
	  (bookmark (assoc navi2ch-bookmark-current-bookmark-id
			   navi2ch-bookmark-list)))
      (if item
	  (progn
	    (push (assoc item (cddr bookmark))
		  navi2ch-bookmark-cut-stack)
	    (navi2ch-bookmark-delete))
	(message "Can't select this line!")))))

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
	  (let ((buffer-read-only nil))
	    (navi2ch-bookmark-insert-subject 0 (car pair)))
	  (navi2ch-bm-renumber))
      (message "stack is empty"))))

;; (defun navi2ch-bookmark-exit ()
;;   (interactive)
;;   (bury-buffer)
;;   (set-window-configuration navi2ch-bookmark-window-configuration))

(defun navi2ch-bookmark-goto-bookmark (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "bookmark id: ")))
  (let ((bookmark (assoc bookmark-id
			 navi2ch-bookmark-list)))
    (navi2ch-list-select-board
     (list (cons 'id (car bookmark))
	   (cons 'name (cadr bookmark))
	   (cons 'type 'bookmark)))))
  
(defun navi2ch-bookmark (board &rest args)
  "bookmark を表示する"
  (let ((bookmark-id (cdr (assq 'id board))))
    (navi2ch-bookmark-mode)
    (setq navi2ch-bookmark-current-bookmark-id bookmark-id)
    (navi2ch-bm-setup 'navi2ch-bookmark)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (save-excursion
	(navi2ch-bookmark-insert-subjects)))))

(defun navi2ch-bookmark-setup-menu ()
  (easy-menu-define navi2ch-bookmark-mode-menu
		    navi2ch-bookmark-mode-map
		    "Menu used in navi2ch-bookmark"
		    navi2ch-bookmark-mode-menu-spec)
  (easy-menu-add navi2ch-bookmark-mode-menu))

(defun navi2ch-bookmark-mode ()
  "\\{navi2ch-bookmark-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-bookmark-mode)
  (setq mode-name "Navi2ch Bookmark")
  (setq buffer-read-only t)
  (use-local-map navi2ch-bookmark-mode-map)
  (navi2ch-bookmark-setup-menu)
  (run-hooks 'navi2ch-bookmark-mode-hook))

(defun navi2ch-bookmark-save-info ()
  (navi2ch-save-info
   navi2ch-bookmark-file
   (mapcar (lambda (y)
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

(defun navi2ch-bookmark-load-info ()
  (setq navi2ch-bookmark-list
	(navi2ch-load-info navi2ch-bookmark-file)))

(defun navi2ch-bookmark-fetch-article (&optional max-line)
;; navi2ch-bm-fetch-article の wrapper として働き、subject が nil なら
;; ファイルから subject を見つけて更新する。
  (interactive "P")
  (let* ((item (navi2ch-bookmark-get-property (point)))
         (board (navi2ch-bookmark-get-board item))
         (article (navi2ch-bookmark-get-article item))
         state)
    (setq state (navi2ch-bm-fetch-article max-line))
    (unless (assq 'subject article)
      (let ((newsubject
	     (let ((file (navi2ch-article-get-file-name board article))
		   sep)
	       (when (file-exists-p file)
                 ;; dat を 全部読まず、頭の1行だけ読むのはどうすれば?
		 ;; 俺も知らないです:-)
		 (with-temp-buffer
		   (navi2ch-insert-file-contents file)
		   (goto-char (point-min))
		   (setq sep (navi2ch-article-get-separator))
		   (cdr (assq 'subject (navi2ch-article-parse-message
					(buffer-substring (point)
							  (progn (end-of-line) (point)))
					sep))))))))
	(when newsubject 
	  (setq article (navi2ch-put-alist 'subject newsubject article))
	  (navi2ch-bookmark-add-subr 
	   navi2ch-bookmark-current-bookmark-id board article))))
    state))

(defun navi2ch-bookmark-fetch-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bookmark-fetch-article))

(provide 'navi2ch-bookmark)
	
;;; navi2ch-bookmark.el ends here
