;;; navi2ch-history.el --- global history module for navi2ch

;; Copyright (C) 2001 by 2ちゃんねる

;; Author: (not 1)
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

(eval-when-compile (require 'cl))
(require 'navi2ch-board-misc)

(defvar navi2ch-history-mode-map nil)
(unless navi2ch-history-mode-map
  (let ((map (copy-keymap navi2ch-bm-mode-map)))
    ;; (define-key map "q" 'navi2ch-history-exit)
    (define-key map "\C-k" 'navi2ch-history-cut)
    (define-key map "\C-y" 'navi2ch-history-yank)
    (define-key map "d" 'navi2ch-history-delete)
    (setq navi2ch-history-mode-map map)))
  
(defvar navi2ch-history-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "History"
   nil))

(defvar navi2ch-history-alist nil "history を表す連想リスト")
(defvar navi2ch-history-buffer-name "*navi2ch history*")
(defvar navi2ch-history-file (navi2ch-expand-file-name
                               "history.txt"))

(defvar navi2ch-history-board
  '((name . "履歴")
    (type . history)
    (id . "hist")))
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

(defun navi2ch-history-exit ())

;;; navi2ch-history functions
(defun navi2ch-history-get-key (board article)
  (concat (cdr (assq 'uri board))
	  (cdr (assq 'artid article))))

(defun navi2ch-history-add (board article)
  "BOARD と ARTICLE で表される スレッドを追加"
  (when (assq 'subject article)
    (let ((key (navi2ch-history-get-key board article)))
      (setq navi2ch-history-alist
	    (delete (assoc key navi2ch-history-alist)
		    navi2ch-history-alist))
      (setq navi2ch-history-alist
	    (cons (list key board article)
		  navi2ch-history-alist)))
    (when (and navi2ch-history-max-line
	       (> (length navi2ch-history-alist)
		  navi2ch-history-max-line))
      (setcdr (nthcdr (1- navi2ch-history-max-line)
		      navi2ch-history-alist)
	      nil))))

(defun navi2ch-history-insert-subject (num item)
  (navi2ch-bm-insert-subject
   item num
   (cdr (assq 'subject (navi2ch-history-get-article item)))
   (format "[%s]" (cdr (assq 'name (navi2ch-history-get-board item))))))

(defun navi2ch-history-insert-subjects ()
  (let ((i 1))
    (dolist (x navi2ch-history-alist)
      (navi2ch-history-insert-subject i (car x))
      (setq i (1+ i)))))

(defun navi2ch-history ()
  "history を表示する"
  (navi2ch-history-mode)
  (navi2ch-bm-setup 'navi2ch-history)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-history-insert-subjects))))

(defun navi2ch-history-setup-menu ()
  (easy-menu-define navi2ch-history-mode-menu
		    navi2ch-history-mode-map
		    "Menu used in navi2ch-history"
		    navi2ch-history-mode-menu-spec)
  (easy-menu-add navi2ch-history-mode-menu))

(defun navi2ch-history-mode ()
  "\\{navi2ch-history-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-history-mode)
  (setq mode-name "Navi2ch History")
  (setq buffer-read-only t)
  (use-local-map navi2ch-history-mode-map)
  (navi2ch-history-setup-menu)
  (run-hooks 'navi2ch-history-mode-hook))

(defun navi2ch-history-save-info ()
  (navi2ch-save-info navi2ch-history-file
		     (mapcar
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

(defun navi2ch-history-load-info ()
  (setq navi2ch-history-alist
	(mapcar
	 (lambda (x)
	   (cons (navi2ch-history-get-key (car x) (cadr x)) x))
	 (navi2ch-load-info navi2ch-history-file))))

(defun navi2ch-history-delete ()
  "その行を history から削除する"
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

(defun navi2ch-history-yank ()
  (interactive)
  (let ((pair (pop navi2ch-history-cut-stack)))
    (if pair
	(progn
	  (if (eobp)
	      (setcdr (last navi2ch-history-alist)
		      (list pair))
	    (let ((list (member (assoc (navi2ch-history-get-property (point))
				       navi2ch-history-alist)
				navi2ch-history-alist)))
	      (setcdr list (copy-sequence list))
	      (setcar list pair)))
	  (let ((buffer-read-only nil))
	    (navi2ch-history-insert-subject 0 (car pair)))
	  (navi2ch-bm-renumber))
      (message "stack is empty"))))

(provide 'navi2ch-history)
        
;;; navi2ch-history.el ends here
