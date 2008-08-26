;;; navi2ch-head.el --- View a local rule mode for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2008 by Navi2ch Project

;; Author: 膝を打つ者
;; Keywords: www, 2ch

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

;;;; navi2ch-head.el

;; Preamble
(provide 'navi2ch-head)
(defconst navi2ch-head-ident
  "$Id$")

(require 'navi2ch)

(eval-when-compile
  (autoload 'w3m-region "w3m")
  (autoload 'w3m-minor-mode "w3m"))
 
;; navi2ch-head-mode

(defvar navi2ch-head-mode-map nil
  "ローカルルールのビュワーのキーマップ。")
(unless navi2ch-head-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map "q" 'navi2ch-head-exit)
    (define-key map "l" 'navi2ch-head-exit)
    (define-key map "M" 'navi2ch-head-select-current-w3m-link)
    (setq navi2ch-head-mode-map map)))

(defvar navi2ch-head-file-name "head.txt")
(defvar navi2ch-head-buffer-name "*navi2ch head*")
(defvar navi2ch-head-current-board nil)
(defvar navi2ch-head-current-article nil)

(add-hook 'navi2ch-exit-hook 'navi2ch-head-kill-buffer)

(defun navi2ch-head-mode ()
  "\\{navi2ch-head-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-head-mode)
  (setq mode-name "Navi2ch Head")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-head-mode-map)
  (run-hooks 'navi2ch-head-mode-hook))

;; Functions

(defun navi2ch-head-save-time (time &optional board)
  (or board (setq board navi2ch-head-current-board))
  (when board
    (navi2ch-save-info (navi2ch-board-get-file-name board "head.info") time)))

(defun navi2ch-head-load-time (&optional board)
  (or board (setq board navi2ch-head-current-board))
  (navi2ch-load-info (navi2ch-board-get-file-name board "head.info")))

(defun navi2ch-head-kill-buffer ()
  (let ((buf (get-buffer navi2ch-head-buffer-name)))
    (when buf
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun navi2ch-head-exit ()
  "ローカルルールバッファを消す。"
  (interactive)
  (run-hooks 'navi2ch-head-exit-hook)
  (let ((exit (get-text-property (point-min) 'navi2ch-head-exit))
	(board navi2ch-head-current-board)
	(article navi2ch-head-current-article)
	win buf)
    (cond
     ((eq exit 'navi2ch-article-mode)
      (setq buf (get-buffer (navi2ch-article-get-buffer-name board article)))
      (if buf
	  (if (setq win (get-buffer-window buf))
	      (select-window win)
	    (switch-to-buffer buf))
	(navi2ch-article-view-article board article)))
     ((eq exit 'navi2ch-board-mode)
      (setq buf (get-buffer navi2ch-board-buffer-name))
      (if buf
	  (if (setq win (get-buffer-window buf))
	      (select-window win)
	    (switch-to-buffer buf))
	(navi2ch-bm-select-board board)))
     ((eq exit 'navi2ch-list-mode)
      (setq buf (get-buffer navi2ch-list-buffer-name))
      (if buf
	  (if (setq win (get-buffer-window buf))
	      (select-window win)
	    (switch-to-buffer buf))
	(navi2ch-list))))
    (navi2ch-head-kill-buffer)))

(defun navi2ch-head-select-current-w3m-link (&optional browse-p)
  "Emacs-w3m のリンクを navi2ch を使ってたどる。"
  (interactive "P")
  (let ((url (get-text-property (point) 'w3m-href-anchor)))
    (if url
	(if (and (navi2ch-2ch-url-p url)
		 (or (navi2ch-board-url-to-board url)
		     (navi2ch-article-url-to-article url))
		 (not browse-p))
	    (navi2ch-goto-url url)
	  (navi2ch-browse-url-internal url))
      (message "No URL at point"))))

(defun navi2ch-head-set-mode-line ()
  (setq navi2ch-mode-line-identification
	(format "[%s]" (cdr (assq 'name navi2ch-head-current-board))))
  (navi2ch-set-mode-line-identification))

;; Entry points from navi2ch-{article,board,list}-mode

(define-key navi2ch-article-mode-map "H" 'navi2ch-head-get-head-txt)
(define-key navi2ch-board-mode-map "H" 'navi2ch-head-get-head-txt)
(define-key navi2ch-list-mode-map "H" 'navi2ch-head-get-head-txt)

(defun navi2ch-head-get-head-txt (&optional force)
  "ローカルルールを持ってきて表示。head.txt に保存しちゃうよ。
emacs-w3m があれば w3m で表示しまつ。"
  (interactive "P")
  (cond ((eq major-mode 'navi2ch-article-mode)
	 (setq navi2ch-head-current-board navi2ch-article-current-board
	       navi2ch-head-current-article navi2ch-article-current-article))
	((eq major-mode 'navi2ch-board-mode)
	 (setq navi2ch-head-current-board navi2ch-board-current-board
	       navi2ch-head-current-article nil))
	((eq major-mode 'navi2ch-list-mode)
	 (setq navi2ch-head-current-board
	       (get-text-property (navi2ch-line-beginning-position) 'board)
	       navi2ch-head-current-article nil)
	 (unless (eq (cdr (assq 'type navi2ch-head-current-board)) 'board)
	   (setq navi2ch-head-current-board nil))))
  (unless (or navi2ch-head-current-board navi2ch-head-current-article)
    (error "Cannot get local rule here"))
  (let* ((navi2ch-net-force-update (or navi2ch-net-force-update force))
	 (board navi2ch-head-current-board)
	 (uri (navi2ch-board-get-url board navi2ch-head-file-name))
	 (file (navi2ch-board-get-file-name board navi2ch-head-file-name))
	 (exit major-mode)
	 (win (or (get-buffer-window navi2ch-head-buffer-name)
		  (and (navi2ch-article-current-buffer)
		       (get-buffer-window (navi2ch-article-current-buffer)))
		  (get-buffer-window navi2ch-board-buffer-name)))
	 time header)
    (unless (or navi2ch-offline
		;; navi2ch-multibbs-head-update 必要？
		(eq (navi2ch-multibbs-get-bbstype board) 'localfile))
      (setq time (navi2ch-head-load-time))
      (setq header (navi2ch-net-update-file uri file time))
      (setq time (and (not (navi2ch-net-get-state 'not-updated header))
		      (not (navi2ch-net-get-state 'error header))
		      (or (cdr (assq 'last-modified header))
			  (cdr (assq 'date header)))))
      (when time (navi2ch-head-save-time time)))
    (when win (select-window win))
    (set-buffer (get-buffer-create navi2ch-head-buffer-name))
    (navi2ch-head-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (when (file-exists-p file)
	(navi2ch-insert-file-contents file))
      (if (eq (point-max) (point-min))
	  (insert "'H'を押す。\n"
		  "  ↓\n"
		  "板ローカルルールを見る。\n"
		  "  ↓\n"
		  "板ローカルルールはない!\n"
		  "  ↓\n"
		  "(ﾟдﾟ)ﾏｼﾞｳﾏｰ\n")
	(when (locate-library "w3m")
	  (require 'w3m)
	  (w3m-region (point-min) (point-max) uri)
	  (w3m-minor-mode 1)))
      (goto-char (point-min))
      (put-text-property (point) (1+ (point)) 'navi2ch-head-exit exit)
      (set-buffer-modified-p nil))
    (switch-to-buffer (current-buffer))
    (navi2ch-head-set-mode-line)))

(run-hooks 'navi2ch-head-load-hook)
;;; navi2ch-head.el ends here
