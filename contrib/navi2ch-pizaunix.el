;;; navi2ch-pizaunix.el --- View old Unix board module for Navi2ch.

;; Copyright (C) 2002 by Navi2ch Project

;; Author: Navi2ch Project
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

;; http://piza.2ch.net/log/unix/0008251/ の
;; Unix 板過去ログ倉庫を見るための multibbs module です。
;; http://pc.2ch.net/test/read.cgi/unix/972851555/ も参照。

;;; Code:
(provide 'navi2ch-pizaunix)
(defconst navi2ch-pizaunix-ident
  "$Id$")

(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(defvar navi2ch-pizaunix-func-alist
  '((bbs-p		. navi2ch-pizaunix-p)
    (article-to-url	. navi2ch-pizaunix-article-to-url)
    (article-update	. navi2ch-pizaunix-article-update)))

(defvar navi2ch-pizaunix-variable-alist
  '((coding-system	. shift_jis)))

(navi2ch-multibbs-regist 'pizaunix
			 navi2ch-pizaunix-func-alist
			 navi2ch-pizaunix-variable-alist)

;;-------------

(defun navi2ch-pizaunix-p (uri)
  "URI が pizaunix なら non-nilを返す。"
  (string-match "http://piza.2ch.net/log/unix/0008251/" uri))

(defun navi2ch-pizaunix-article-to-url
  (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (concat "http://piza.2ch.net/log/unix/0008251/"
	  (cdr (assq 'artid article))
	  ".html"))

(defun navi2ch-pizaunix-article-update (board article start)
  "BOARD, ARTICLE に対応するファイルを更新する。"
  (let ((file (navi2ch-article-get-file-name board article))
	(url (concat "http://piza.2ch.net/log/unix/0008251/"
		     (cdr (assq 'artid article))
		     ".dat")))
    (list (navi2ch-net-update-file url file) 'kako)))

;;; navi2ch-pizaunix.el ends here
