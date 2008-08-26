;;; navi2ch-search.el --- Search Module for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
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

;; namazu(大先生の検索) を使うには
;; (navi2ch-net-send-request
;; (format "http://64.124.197.202/cgi-bin/search/namazu.cgi?%s"
;;         (navi2ch-net-get-param-string
;;          '(("query" . "navi2ch")
;;            ("submit" . "検索")
;;            ("whence" . "0")
;;            ("idxname" . "2chpc")
;;            ("max" . "10")
;;            ("result" . "normal")
;;            ("sort" . "score"))))
;; "GET")
;; な感じで。




;;; Code:
(provide 'navi2ch-search)
(defconst navi2ch-search-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-search-mode-map nil)
(unless navi2ch-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    (define-key map "Q" 'navi2ch-search-return-previous-board-maybe)
    (define-key map "s" 'navi2ch-search-sync)
    (setq navi2ch-search-mode-map map)))

(defvar navi2ch-search-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Search"
   nil))

(defvar navi2ch-search-searched-subject-list nil)
(defvar navi2ch-search-board
  '((name . "検索一覧")
    (type . search)
    (id . "#search")))

(defvar navi2ch-search-history nil)

;;; navi2ch-bm callbacks
(defun navi2ch-search-set-property (begin end item)
  (put-text-property begin end 'item item))

(defun navi2ch-search-get-property (point)
  (get-text-property (save-excursion (goto-char point)
				     (beginning-of-line)
				     (point))
		     'item))

(defun navi2ch-search-get-board (item)
  (car item))

(defun navi2ch-search-get-article (item)
  (cdr item))

(defun navi2ch-search-exit ()
  (run-hooks 'navi2ch-search-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'search 'navi2ch-search
			 navi2ch-search-board)

;;; navi2ch-search functions
(defun navi2ch-search-insert-subjects ()
  (let ((i 1))
    (dolist (x navi2ch-search-searched-subject-list)
      (let ((article (navi2ch-search-get-article x))
            (board (navi2ch-search-get-board x)))
        (navi2ch-bm-insert-subject
         x i
         (cdr (assq 'subject article))
         (format "[%s]" (cdr (assq 'name board))))
        (setq i (1+ i))))))

(defun navi2ch-search-for-each-board (board-func board-list)
  (let (alist)
    (dolist (board board-list)
      (message "Searching in %s..." (cdr (assq 'name board)))
      (setq alist (nconc (funcall board-func board)
			 alist)))
    (message "Searching...%s" (if alist "done" "not found"))
    (nreverse alist)))

(defun navi2ch-search-for-each-article (article-func board-list)
  (navi2ch-search-for-each-board
   (lambda (board)
     (let ((default-directory (navi2ch-board-get-file-name board "")))
       (delq nil
	     (mapcar (lambda (file)
		       (funcall article-func board file))
		     (and default-directory
			  (file-directory-p default-directory)
			  (sort (directory-files default-directory
						 nil navi2ch-article-local-dat-regexp)
				#'navi2ch-right-aligned-string<))))))
   board-list))

(defun navi2ch-search-board-subject-regexp (board-list regexp)
  (navi2ch-search-for-each-board
   (lambda (board)
     (let* ((file (navi2ch-board-get-file-name board))
	    (subject-list (and file
			       (navi2ch-board-get-subject-list file)))
	    alist)
       (dolist (article subject-list)
	 (let ((subject (cdr (assq 'subject article))))
	   (when (string-match regexp subject)
	     (push (cons board article) alist))))
       alist))
   board-list))

(defun navi2ch-search-article-regexp (board-list regexp)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (with-temp-buffer
       (navi2ch-board-insert-file-contents board file)
       (navi2ch-apply-filters board navi2ch-article-filter-list)
       (goto-char (point-min))
       (when (re-search-forward regexp nil t)
	 (let ((subject
		(cdr (assq 'subject
			   (navi2ch-article-get-first-message)))))
	   (list board
		 (cons 'subject subject)
		 (cons 'artid
		       (navi2ch-article-file-name-to-artid file)))))))
   board-list))

(defun navi2ch-search-cache (board-list)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (let ((subject (assq 'subject
			  (navi2ch-article-get-first-message-from-file
			   file board))))
       (list board subject
	     (cons 'artid (navi2ch-article-file-name-to-artid file)))))
   board-list))

(defun navi2ch-search-orphan (board-list)
  (navi2ch-search-for-each-article
   (lambda (board file)
     (let ((article (list (cons 'artid (navi2ch-article-file-name-to-artid file)))))
       (if (navi2ch-article-orphan-p board article)
	   (let ((subject (assq 'subject
				(navi2ch-article-get-first-message-from-file
				 file board))))
	     (nconc (list board subject)
		    article)))))
   board-list))

(easy-menu-define navi2ch-search-mode-menu
  navi2ch-search-mode-map
  "Menu used in navi2ch-search"
  navi2ch-search-mode-menu-spec)

(defun navi2ch-search-setup-menu ()
  (easy-menu-add navi2ch-search-mode-menu))

(defun navi2ch-search-mode ()
  "\\{navi2ch-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-search-mode)
  (setq mode-name "Navi2ch Search")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-search-mode-map)
  (navi2ch-search-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-search-mode-hook))

(defun navi2ch-search (&rest args)
  (navi2ch-search-mode)
  (navi2ch-bm-setup 'navi2ch-search)
  (navi2ch-search-sync))

(defun navi2ch-search-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-search-insert-subjects))))

(defun navi2ch-search-return-previous-board-maybe ()
  (interactive)
  (if navi2ch-board-current-board
      (navi2ch-board-select-board	; 汚ねぇ・・・
       (prog1
	   navi2ch-board-current-board
	 (setq navi2ch-board-current-board nil)))
    (navi2ch-bm-exit)))

(defun navi2ch-search-subject-subr (board-list-or-function)
  (let ((regexp (navi2ch-read-string "Subject regexp: " nil
				     'navi2ch-search-history))
	(board-list (if (functionp board-list-or-function)
			(funcall board-list-or-function)
		      board-list-or-function)))
    (setq navi2ch-search-searched-subject-list
	  (navi2ch-search-board-subject-regexp board-list regexp)))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-for-each-directory-recursive (function directory)
  (let ((default-directory directory))
    (dolist (file (directory-files "."))
      (if (and (file-directory-p file)
	       (not (member file '("." ".."))))
	  (navi2ch-search-for-each-directory-recursive
	   function (expand-file-name file)))))
  (funcall function directory))

(defun navi2ch-search-directory-to-board (directory directory-to-board-alist)
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (or (cdr (assoc dir directory-to-board-alist))
	(navi2ch-board-url-to-board
	 (concat "http://"
		 (file-relative-name dir navi2ch-directory))))))

(defun navi2ch-search-all-board-list ()
  (let ((directory-to-board-alist
	 (mapcar (lambda (board)
		   (cons (navi2ch-board-get-file-name board "")
			 board))
		 (navi2ch-list-get-board-name-list
		  (navi2ch-list-get-normal-category-list
		   navi2ch-list-category-list))))
	l)
    (navi2ch-search-for-each-directory-recursive
     (lambda (directory)
       (if (file-exists-p (expand-file-name navi2ch-article-summary-file-name
					    directory))
	   (push (navi2ch-search-directory-to-board directory
						    directory-to-board-alist)
		 l)))
     navi2ch-directory)
    (nreverse l)))

(defun navi2ch-search-all-subject ()
  (interactive)
  (navi2ch-search-subject-subr #'navi2ch-search-all-board-list))

(defun navi2ch-search-article-subr (board-list-or-function)
  (let ((regexp (navi2ch-read-string "Search regexp: " nil
				     'navi2ch-search-history))
	(board-list (if (functionp board-list-or-function)
			(funcall board-list-or-function)
		      board-list-or-function)))
    (setq navi2ch-search-searched-subject-list
	  (navi2ch-search-article-regexp board-list regexp)))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-article ()
  (interactive)
  (navi2ch-search-article-subr #'navi2ch-search-all-board-list))

(defun navi2ch-search-cache-subr (board-list)
  (setq navi2ch-search-searched-subject-list
	(navi2ch-search-cache board-list))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-cache ()
  (interactive)
  (navi2ch-search-cache-subr (navi2ch-search-all-board-list)))

(defun navi2ch-search-orphan-subr (board-list)
  (setq navi2ch-search-searched-subject-list
	(navi2ch-search-orphan board-list))
  (navi2ch-bm-select-board navi2ch-search-board))

(defun navi2ch-search-all-orphan ()
  (interactive)
  (navi2ch-search-orphan-subr (navi2ch-search-all-board-list)))

(run-hooks 'navi2ch-search-load-hook)
;;; navi2ch-search.el ends here
