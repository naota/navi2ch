;;; navi2ch-directory.el --- List directory files Module for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2008 by Navi2ch Project

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

;;

;;; Code:
(provide 'navi2ch-directory)
(defconst navi2ch-directory-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-directory-mode-map nil)
(unless navi2ch-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    (define-key map "s" 'navi2ch-directory-sync)
    (setq navi2ch-directory-mode-map map)))

(defvar navi2ch-directory-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Directory"
   nil))

(defvar navi2ch-directory-board
  '((name . "ファイル一覧")
    (type . directory)
    (id . "#directory")))

(defvar navi2ch-directory-current-board nil)
(defvar navi2ch-directory-subject-list nil)

;;; navi2ch-bm callbacks
(defun navi2ch-directory-set-property (begin end item)
  (put-text-property begin end 'item item))

(defun navi2ch-directory-get-property (point)
  (get-text-property
   (save-excursion (goto-char point)
		   (beginning-of-line)
		   (point))
   'item))

(defun navi2ch-directory-get-article (item)
  item)

(defun navi2ch-directory-get-board (item)
  navi2ch-directory-current-board)

(defun navi2ch-directory-exit ()
  (run-hooks 'navi2ch-directory-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'directory 'navi2ch-directory
			 navi2ch-directory-board)

;;; navi2ch-directory functions
(defun navi2ch-directory-insert-subjects ()
  (let ((i 1))
    (dolist (article navi2ch-directory-subject-list)
      (navi2ch-bm-insert-subject
       article i
       (cdr (assq 'subject article))
       (format "[%s]" (cdr (assq 'artid article))))
      (setq i (1+ i)))))

(defun navi2ch-directory-set-current-board (directory)
  (setq directory (expand-file-name directory))
  (setq navi2ch-directory-current-board
	(list (cons 'name navi2ch-board-name-from-file)
	      (cons 'uri (navi2ch-filename-to-url directory))
	      (cons 'id "navi2ch"))))

(defun navi2ch-directory-set-subject-list (directory)
  (setq directory (file-name-as-directory
		   (expand-file-name directory)))
  (setq navi2ch-directory-subject-list
	(mapcar
	 (lambda (file)
	   (setq file (concat directory file))
	   (list
	    (cons 'subject
		  (cdr (assq 'subject
			     (navi2ch-article-get-first-message-from-file
			      file))))
	    (cons 'artid
		  (navi2ch-article-file-name-to-artid file))))
	 (sort (directory-files directory nil navi2ch-article-local-dat-regexp t)
	       (lambda (x y)
		 (not (navi2ch-right-aligned-string< x y)))))))

(defun navi2ch-directory-find-directory (directory)
  (interactive "DDirectory: ")
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (setq default-directory directory)
    (navi2ch-directory-set-current-board directory)
    (navi2ch-directory-set-subject-list directory)
    (navi2ch-bm-select-board navi2ch-directory-board)))

(defun navi2ch-directory (&rest args)
  "directory を表示する。"
  (navi2ch-directory-mode)
  (navi2ch-bm-setup 'navi2ch-directory)
  (navi2ch-directory-sync))


(defun navi2ch-directory-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-directory-insert-subjects))))

(easy-menu-define navi2ch-directory-mode-menu
  navi2ch-directory-mode-map
  "Menu used in navi2ch-directory"
  navi2ch-directory-mode-menu-spec)

(defun navi2ch-directory-setup-menu ()
  (easy-menu-add navi2ch-directory-mode-menu))

(defun navi2ch-directory-mode ()
  "\\{navi2ch-directory-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-directory-mode)
  (setq mode-name "Navi2ch Directory")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-directory-mode-map)
  (navi2ch-directory-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-directory-mode-hook))

(run-hooks 'navi2ch-directory-load-hook)
;;; navi2ch-directory.el ends here
