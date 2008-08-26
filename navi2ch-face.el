;;; navi2ch-face.el --- face definitions for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network 2ch

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
(provide 'navi2ch-face)
(defconst navi2ch-face-ident
  "$Id$")

(defgroup navi2ch-face nil
  "Navi2ch, Faces."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defface navi2ch-list-category-face
  '((((class color) (background light)) (:foreground "Gray30" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t)))
  "カテゴリの face"
  :group 'navi2ch-face)

(defface navi2ch-list-board-name-face
  '((((class color) (background light)) (:foreground "Navy"))
    (((class color) (background dark)) (:foreground "SkyBlue")))
  "板名の face"
  :group 'navi2ch-face)

(defface navi2ch-list-add-board-name-face
  '((((class color) (background light)) (:foreground "FireBrick" :bold t))
    (((class color) (background dark)) (:foreground "cyan" :bold t)))
  "追加された板名の face"
  :group 'navi2ch-face)

(defface navi2ch-list-change-board-name-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen" :bold t))
    (((class color) (background dark)) (:foreground "GreenYellow" :bold t)))
  "変更された板名の face"
  :group 'navi2ch-face)

(defface navi2ch-bm-unread-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen"))
    (((class color) (background dark)) (:foreground "GreenYellow")))
  "読んでないスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-view-face
  '((((class color) (background light)) (:foreground "FireBrick"))
    (((class color) (background dark)) (:foreground "cyan")))
  "表示しているスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-cache-face
  '((((class color) (background light)) (:foreground "Navy"))
    (((class color) (background dark)) (:foreground "SkyBlue")))
  "読んだ事があるスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-update-face
  '((((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark)) (:foreground "LightSkyBlue")))
  "更新したスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-mark-face
  '((((class color) (background light)) (:foreground "Tomato3"))
    (((class color) (background dark)) (:foreground "tomato")))
  "マークしたスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-new-unread-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen" :bold t))
    (((class color) (background dark)) (:foreground "GreenYellow" :bold t)))
  "新しい読んでないスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-new-view-face
  '((((class color) (background light)) (:foreground "FireBrick" :bold t))
    (((class color) (background dark)) (:foreground "PaleGreen" :bold t)))
  "新しい表示しているスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-new-cache-face
  '((((class color) (background light)) (:foreground "Navy" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t)))
  "新しい読んだ事があるスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-new-update-face
  '((((class color) (background light)) (:foreground "SaddleBrown" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t)))
  "新しい更新したスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-new-mark-face
  '((((class color) (background light)) (:foreground "Tomato3" :bold t))
    (((class color) (background dark)) (:foreground "tomato" :bold t)))
  "新しいマークしたスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-updated-unread-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen" :bold t))
    (((class color) (background dark)) (:foreground "GreenYellow" :bold t)))
  "レスがあった読んでないスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-updated-view-face
  '((((class color) (background light)) (:foreground "FireBrick" :bold t))
    (((class color) (background dark)) (:foreground "PaleGreen" :bold t)))
  "レスがあった表示しているスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-updated-cache-face
  '((((class color) (background light)) (:foreground "Navy" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t)))
  "レスがあった読んだ事があるスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-updated-update-face
  '((((class color) (background light)) (:foreground "SaddleBrown" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t)))
  "レスがあった更新したスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-updated-mark-face
  '((((class color) (background light)) (:foreground "Tomato3" :bold t))
    (((class color) (background dark)) (:foreground "tomato" :bold t)))
  "レスがあったマークしたスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-seen-unread-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen" :underline t))
    (((class color) (background dark)) (:foreground "GreenYellow" :underline t)))
  "すでに見た読んでないスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-seen-view-face
  '((((class color) (background light)) (:foreground "FireBrick" :underline t))
    (((class color) (background dark)) (:foreground "PaleGreen" :underline t)))
  "すでに見た表示しているスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-seen-cache-face
  '((((class color) (background light)) (:foreground "Navy" :underline t))
    (((class color) (background dark)) (:foreground "SkyBlue" :underline t)))
  "すでに見た読んだ事があるスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-seen-update-face
  '((((class color) (background light)) (:foreground "SaddleBrown" :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :underline t)))
  "すでに見た更新したスレの face"
  :group 'navi2ch-face)

(defface navi2ch-bm-seen-mark-face
  '((((class color) (background light)) (:foreground "Tomato3" :underline t))
    (((class color) (background dark)) (:foreground "tomato" :underline t)))
  "すでに見たマークしたスレの face"
  :group 'navi2ch-face)

(defface navi2ch-article-header-face
  '((((class color) (background light)) (:foreground "Gray30" :bold t))
    (((class color) (background dark)) (:foreground "gray" :bold t)))
  "ヘッダの From とかの部分の face"
  :group 'navi2ch-face)

(defface navi2ch-article-header-contents-face
  '((((class color) (background light)) (:foreground "Navy"))
    (((class color) (background dark)) (:foreground "yellow")))
  "ヘッダの内容の方の face"
  :group 'navi2ch-face)

(defface navi2ch-article-header-fusianasan-face
  '((((class color) (background light)) (:underline t :foreground "Navy"))
    (((class color) (background dark)) (:underline t :foreground "yellow")))
  "ふしあなさんを表示する際の face"
  :group 'navi2ch-face)

(defface navi2ch-article-link-face
  '((((class color) (background light)) (:bold t))
    (((class color) (background dark)) (:bold t)))
  "同じスレ中へのリンクの face"
  :group 'navi2ch-face)

(defface navi2ch-article-url-face
  '((((class color) (background light)) (:bold t))
    (((class color) (background dark)) (:bold t)))
  "url の face"
  :group 'navi2ch-face)

(defface navi2ch-article-citation-face
  '((((class color) (background light)) (:foreground "FireBrick"))
    (((class color) (background dark)) (:foreground "HotPink1")))
  "引用の face"
  :group 'navi2ch-face)

(defface navi2ch-article-face
  nil
  "スレの face"
  :group 'navi2ch-face)

(defface navi2ch-article-auto-decode-face
  '((((class color) (background light)) (:background "gray90"))
    (((class color) (background dark)) (:foreground "gray10")))
  "スレのエンコードされたセクションの face"
  :group 'navi2ch-face)

(defface navi2ch-article-message-separator-face
  '((((class color) (background light)) (:foreground "SpringGreen4"))
    (((class color) (background dark)) (:foreground "firebrick")))
  "レスの区切の face"
  :group 'navi2ch-face)

(defface navi2ch-splash-screen-face
  '((((type tty) (background dark)) (:foreground "cyan"))
    (((class color) (background dark)) (:foreground "SkyBlue"))
    (((class color) (background light)) (:foreground "SteelBlue")))
  "Face used for displaying splash screen."
  :group 'navi2ch-face)

(defface navi2ch-message-link-face
  '((((class color) (background light)) (:bold t))
    (((class color) (background dark)) (:bold t)))
  "同じスレ中へのリンクの face"
  :group 'navi2ch-face)

(defface navi2ch-message-url-face
  '((((class color) (background light)) (:bold t))
    (((class color) (background dark)) (:bold t)))
  "url の face"
  :group 'navi2ch-face)

(defface navi2ch-message-citation-face
  '((((class color) (background light)) (:foreground "FireBrick"))
    (((class color) (background dark)) (:foreground "HotPink1")))
  "引用の face"
  :group 'navi2ch-face)

(run-hooks 'navi2ch-face-load-hook)
;;; navi2ch-face.el ends here
