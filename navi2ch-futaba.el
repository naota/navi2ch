;;; navi2ch-futaba.el --- View futaba module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004 by Navi2ch Project

;; Author:

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
(provide 'navi2ch-futaba)

(eval-when-compile (require 'cl))
(require 'navi2ch-util)
(require 'navi2ch-multibbs)

(defvar navi2ch-futaba-func-alist
  '((bbs-p		. navi2ch-futaba-p)
    (send-success-p     . navi2ch-futaba-send-message-success-p)
    ))

(defvar navi2ch-futaba-variable-alist
  (list (cons 'coding-system navi2ch-coding-system)))

(navi2ch-multibbs-regist 'futaba
			 navi2ch-futaba-func-alist
			 navi2ch-futaba-variable-alist)

;;-------------

(defun navi2ch-futaba-p (uri)
  "URI がふたばちゃんねるならば non-nilを返す。"
  (string-match "http://www.2chan.net/" uri))

(defun navi2ch-futaba-send-message-success-p (proc)
  (string= "302" (navi2ch-net-get-status proc)))


(provide 'navi2ch-futaba)

;;; navi2ch-futaba.el ends here
