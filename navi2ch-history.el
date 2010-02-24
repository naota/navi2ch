;;; navi2ch-history.el --- global history module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2006, 2008 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network, 2ch

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
(provide 'navi2ch-history)
(defconst navi2ch-history-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-history-mode-map nil)
(unless navi2ch-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    ;; (define-key map "q" 'navi2ch-history-exit)
    (define-key map "\C-k" 'navi2ch-history-cut)
    (define-key map "\C-y" 'navi2ch-history-yank)
    (define-key map "d" 'navi2ch-history-cut)
    (define-key map "D" 'navi2ch-history-delete)
    (define-key map "md" 'navi2ch-history-cut-mark-article)
    (define-key map "mD" 'navi2ch-history-delete-mark-article)
    (define-key map "s" 'navi2ch-history-sync)
    (setq navi2ch-history-mode-map map)))

(defvar navi2ch-history-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "History"
   nil))

(defvar navi2ch-history-alist nil
  "history を表す連想リスト。
'((key board article) ...) という形をしている。
key は (concat uri artid)。")
(defvar navi2ch-history-buffer-name "*navi2ch history*")

(defvar navi2ch-history-board
  '((name . "履歴")
    (type . history)
    (id . "#hist")))
(defvar navi2ch-history-cut-stack nil)

;;; navi2ch-bm callbacks
(defun navi2ch-history-set-property (begin end item)
  (put-text-property begin end 'item item))

(defun navi2ch-history-get-property (point)
  (get-text-property (save-excursion (goto-char point)
				     (beginning-of-line)
				     (point))
		     'item))

(defun navi2ch-history-get-board (item)
  (nth 1 (assoc item navi2ch-history-alist)))

(defun navi2ch-history-get-article (item)
  (nth 2 (assoc item navi2ch-history-alist)))

(defun navi2ch-history-exit ()
  (run-hooks 'navi2ch-history-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'history 'navi2ch-history
			 navi2ch-history-board)

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-history-save-info)
(add-hook 'navi2ch-load-status-hook 'navi2ch-history-load-info)

;;; navi2ch-history functions
(defun navi2ch-history-get-key (board article)
  (concat (cdr (assq 'uri board))
	  (cdr (assq 'artid article))))

(defun navi2ch-history-add (board article)
  "BOARD と ARTICLE で表される スレッドを追加。"
  (let* ((key (navi2ch-history-get-key board article))
	 (article (list (assq 'subject article)
			(assq 'artid article)))
	 (old-node (assoc key navi2ch-history-alist))
	 (old-subject (cdr (assq 'subject (nth 2 old-node))))
	 (subject (cdr (assq 'subject article))))
    (setq navi2ch-history-alist (delete old-node navi2ch-history-alist))
    (setq navi2ch-history-alist
	  (cons (if (or subject (not old-subject))
		    (list key board article)
		  old-node)
		navi2ch-history-alist)))
  (when (and navi2ch-history-max-line
	     (> (length navi2ch-history-alist)
		navi2ch-history-max-line))
    (setcdr (nthcdr (1- navi2ch-history-max-line)
		    navi2ch-history-alist)
	    nil)))

(defun navi2ch-history-insert-subject (num item)
  (navi2ch-bm-insert-subject
   item num
   (or (cdr (assq 'subject (navi2ch-history-get-article item)))
       (navi2ch-history-get-key
	(navi2ch-history-get-board item)
	(navi2ch-history-get-article item)))
   (format "[%s]" (cdr (assq 'name (navi2ch-history-get-board item))))))

(defun navi2ch-history-insert-subjects ()
  (let ((i 1))
    (dolist (x navi2ch-history-alist)
      (navi2ch-history-insert-subject i (car x))
      (setq i (1+ i)))))

(defun navi2ch-history (&rest args)
  "history を表示する。"
  (navi2ch-history-mode)
  (navi2ch-bm-setup 'navi2ch-history)
  (navi2ch-history-sync))

(defun navi2ch-history-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-history-insert-subjects))))

(easy-menu-define navi2ch-history-mode-menu
  navi2ch-history-mode-map
  "Menu used in navi2ch-history"
  navi2ch-history-mode-menu-spec)

(defun navi2ch-history-setup-menu ()
  (easy-menu-add navi2ch-history-mode-menu))

(defun navi2ch-history-mode ()
  "\\{navi2ch-history-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-history-mode)
  (setq mode-name "Navi2ch History")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-history-mode-map)
  (navi2ch-history-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-history-mode-hook))

(defun navi2ch-history-save-info ()
  (let ((info (mapcar
	       (lambda (x)
		 (let ((board (nth 1 x))
		       (article (nth 2 x)))
		   (list (list
			  (assq 'name board)
			  (assq 'uri board)
			  (assq 'id board))
			 (list
			  (assq 'subject article)
			  (assq 'artid article)))))
	       navi2ch-history-alist)))
    (navi2ch-save-info navi2ch-history-file info t)))

(defun navi2ch-history-load-info ()
  (setq navi2ch-history-alist
	(mapcar
	 (lambda (x)
	   (cons (navi2ch-history-get-key (car x) (cadr x)) x))
	 (navi2ch-load-info navi2ch-history-file))))

(defun navi2ch-history-delete ()
  "その行を history から削除する。"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((item (navi2ch-history-get-property (point))))
      (if item
	  (let ((pair (assoc item navi2ch-history-alist))
		(buffer-read-only nil))
	    (setq navi2ch-history-alist (delq pair navi2ch-history-alist))
	    (delete-region (point) (save-excursion (forward-line) (point)))
	    (navi2ch-bm-renumber))
	(message "Can't select this line!")))))

(defun navi2ch-history-delete-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-history-delete))

(defun navi2ch-history-cut ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((item (navi2ch-history-get-property (point))))
      (if item
	  (progn
	    (push (assoc item navi2ch-history-alist) navi2ch-history-cut-stack)
	    (navi2ch-history-delete))
	(message "Can't select this line!")))))

(defun navi2ch-history-cut-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-history-cut))

(defun navi2ch-history-yank ()
  (interactive)
  (let ((pair (pop navi2ch-history-cut-stack)))
    (if pair
	(progn
	  (if (eobp)
	      (if navi2ch-history-alist
		  (setcdr (last navi2ch-history-alist)
			  (list pair))
		(setq navi2ch-history-alist (list pair)))
	    (let ((list (member (assoc (navi2ch-history-get-property (point))
				       navi2ch-history-alist)
				navi2ch-history-alist)))
	      (setcdr list (copy-sequence list))
	      (setcar list pair)))
	  (let ((buffer-read-only nil))
	    (navi2ch-history-insert-subject 0 (car pair)))
	  (navi2ch-bm-renumber))
      (message "Stack is empty"))))

(defun navi2ch-history-change (changed-list)
  "変更された板の履歴を修正する。
CHANGED-LIST については `navi2ch-list-get-changed-status' を参照。"
  (setq navi2ch-history-alist
	(mapcar
	 (lambda (node)
	   (let* ((board (cadr node))
		  (article (caddr node))
		  (changed (assoc (cdr (assq 'id board)) changed-list)))
	     (if changed
		 (let ((new-board (caddr changed)))
		   (list (navi2ch-history-get-key new-board article)
			 new-board
			 article))
	       node)))
	 navi2ch-history-alist))
  (navi2ch-history-save-info))

(run-hooks 'navi2ch-history-load-hook)
;;; navi2ch-history.el ends here
