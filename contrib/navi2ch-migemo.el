;;; navi2ch-migemo.el --- migemo support for Navi2ch

;; Copyright (C) 2002 by Navi2ch Project

;; Author: Nanashi San <nanashi@users.sourceforge.net>
;; Keywords: 2ch, network, matching

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

;; To use this module, put following code into your `~/.navi2ch/init.el':
;; (require 'navi2ch-migemo)

;;; Code:
(provide 'navi2ch-migemo)

;; migemo がなくてもコンパイルできるように。
(eval '(require 'migemo))

(require 'navi2ch-search)

(defvar navi2ch-migemo-enable t
  "*検索パターンにmigemoを適用するかどうか。")

(defadvice navi2ch-search-board-subject-regexp
  (before navi2ch-migemo-search-board-subject-regexp activate)
  "検索パターンにmigemoを適用する。"
  (if navi2ch-migemo-enable
      (ad-set-arg 1 (migemo-get-pattern (ad-get-arg 1)))))

(defadvice navi2ch-search-article-regexp
  (before navi2ch-migemo-search-article-regexp activate)
  "検索パターンにmigemoを適用する。"
  (if navi2ch-migemo-enable
      (ad-set-arg 1 (migemo-get-pattern (ad-get-arg 1)))))

(defun navi2ch-migemo-toggle-enable ()
  (setq navi2ch-migemo-enable (not navi2ch-migemo-enable)))

;;; navi2ch-migemo.el ends here
