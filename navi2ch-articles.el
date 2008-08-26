;;; navi2ch-articles.el --- Article List Module for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2008 by Navi2ch Project

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
(provide 'navi2ch-articles)
(defconst navi2ch-articles-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-articles-mode-map nil)
(unless navi2ch-articles-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-bm-mode-map)
    ;; (define-key navi2ch-articles-mode-map "q" 'navi2ch-articles-exit)
    (define-key map "d" 'navi2ch-articles-delete)
    (define-key map "md" 'navi2ch-articles-delete-mark-aritcle)
    (define-key map "R" 'navi2ch-articles-remove-article)
    (define-key map "mR" 'navi2ch-articles-remove-mark-article)
    (define-key map "s" 'navi2ch-articles-sync)
    (setq navi2ch-articles-mode-map map)))

(defvar navi2ch-articles-mode-menu-spec
  (navi2ch-bm-make-menu-spec
   "Articles"
   nil))

(defvar navi2ch-articles-board
  '((name . "表示スレ一覧")
    (type . articles)
    (id . "#articles")))

;;; navi2ch-bm callbacks
(defun navi2ch-articles-set-property (begin end item)
  (put-text-property begin end 'buffer item))

(defun navi2ch-articles-get-property (point)
  (get-text-property point 'buffer))

(defun navi2ch-articles-get-article (item)
  (when (and item
	     (buffer-live-p item))
    (save-excursion
      (set-buffer item)
      navi2ch-article-current-article)))

(defun navi2ch-articles-get-board (item)
  (when (and item
	     (buffer-live-p item))
    (save-excursion
      (set-buffer item)
      navi2ch-article-current-board)))

(defun navi2ch-articles-exit ()
  (run-hooks 'navi2ch-articles-exit-hook))

;; regist board
(navi2ch-bm-regist-board 'articles 'navi2ch-articles
			 navi2ch-articles-board)

;;; navi2ch-articles functions
(defun navi2ch-articles-insert-subjects ()
  (let ((i 1))
    (dolist (x (navi2ch-article-buffer-list))
      (let ((article (navi2ch-articles-get-article x))
            (board (navi2ch-articles-get-board x)))
        (navi2ch-bm-insert-subject
         x i
         (cdr (assq 'subject article))
         (format "[%s]" (cdr (assq 'name board))))
        (setq i (1+ i))))))

(defun navi2ch-articles-delete ()
  "その行を articles から削除して、その article buffer も消す。"
  (interactive)
  (let ((buf (save-excursion (beginning-of-line)
			     (navi2ch-articles-get-property (point)))))
    (if buf
	(let ((buffer-read-only nil))
	  (kill-buffer buf)
	  (delete-region (save-excursion (beginning-of-line) (point))
			 (save-excursion (forward-line) (point)))
	  (and (eobp) (not (bobp))
	       (forward-line -1)))
      (message "Can't select this line!"))))

(defun navi2ch-articles-delete-mark-aritcle ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-articles-delete))

(defun navi2ch-articles-remove-article ()
  (interactive)
  (navi2ch-bm-remove-article)
  (navi2ch-articles-delete))

(defun navi2ch-articles-remove-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-articles-remove-article))

(defun navi2ch-articles (&rest args)
  "articles を表示する。"
  (navi2ch-articles-mode)
  (navi2ch-bm-setup 'navi2ch-articles)
  (navi2ch-articles-sync))

(defun navi2ch-articles-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (save-excursion
      (navi2ch-articles-insert-subjects))))

(easy-menu-define navi2ch-articles-mode-menu
  navi2ch-articles-mode-map
  "Menu used in navi2ch-articles"
  navi2ch-articles-mode-menu-spec)

(defun navi2ch-articles-setup-menu ()
  (easy-menu-add navi2ch-articles-mode-menu))

(defun navi2ch-articles-mode ()
  "\\{navi2ch-articles-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-articles-mode)
  (setq mode-name "Navi2ch Articles")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-articles-mode-map)
  (navi2ch-articles-setup-menu)
  (run-hooks 'navi2ch-bm-mode-hook 'navi2ch-articles-mode-hook))

(run-hooks 'navi2ch-articles-load-hook)
;;; navi2ch-articles.el ends here
