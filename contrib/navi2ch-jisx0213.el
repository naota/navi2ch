;;; navi2ch-jisx0213.el --- Define translate table win charset to jisx2013

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author:
;; Part 7 スレの 66 の名無しさん
;; <http://pc.2ch.net/test/read.cgi/unix/1031231315/66>
;; Keywords: 2ch, charset

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

(require 'navi2ch-vars)
(require 'navi2ch-util)

(unless navi2ch-on-xemacs		;誰かXEmacs対応お願いします
  (defun navi2ch-jisx0213-make-char-list (i js je)
    (let ((j js) list)
      (while (<= j je)
	(setq list (cons (make-char 'japanese-jisx0208 i j) list))
	(setq j (1+ j)))
      (nreverse list)))

  (defvar navi2ch-jisx0213-win-chars-list
    (append
     (navi2ch-jisx0213-make-char-list  45  33  52) ;まる数字
     (navi2ch-jisx0213-make-char-list  45  53  62) ;ローマ数字(大文字)
     (navi2ch-jisx0213-make-char-list 124 113 122) ;ローマ数字(小文字)
     (navi2ch-jisx0213-make-char-list  45  64  86) ;単位
     (navi2ch-jisx0213-make-char-list  45  95 111) ;元号など
     (navi2ch-jisx0213-make-char-list  45 112 124) ;数学記号
     ))

  (defvar navi2ch-jisx0213-jisx0123-chars-list
    '(?① ?② ?③ ?④ ?⑤ ?⑥ ?⑦ ?⑧ ?⑨ ?⑩
	  ?⑪ ?⑫ ?⑬ ?⑭ ?⑮ ?⑯ ?⑰ ?⑱ ?⑲ ?⑳
	  ?Ⅰ ?Ⅱ ?Ⅲ ?Ⅳ ?Ⅴ ?Ⅵ ?Ⅶ ?Ⅷ ?Ⅸ ?Ⅹ
	  ?ⅰ ?ⅱ ?ⅲ ?ⅳ ?ⅴ ?ⅵ ?ⅶ ?ⅷ ?ⅸ ?ⅹ
	  ?㍉ ?㌔ ?㌢ ?㍍ ?㌘ ?㌧ ?㌃ ?㌶ ?㍑ ?㍗ ?㌍ ?㌦
	  ?㌣ ?㌫ ?㍊ ?㌻ ?㎜ ?㎝ ?㎞ ?㎎ ?㎏ ?㏄ ?㎡
	  ?㍻ ?“ ?” ?№ ?㏍ ?℡ ?㊤ ?㊥ ?㊦ ?㊧ ?㊨ ?㈱ ?㈲ ?㈹ ?㍾ ?㍽ ?㍼
	  ?≒ ?≡ ?∫ ?∮ ?Σ ?√ ?⊥ ?∠ ?∟ ?⊿ ?∵ ?∩ ?∪))

  (defvar navi2ch-jisx0213-display-table nil)
  (let ((table (make-display-table))
	(from navi2ch-jisx0213-win-chars-list)
	(to navi2ch-jisx0213-jisx0123-chars-list))
    (while (and from to)
      (aset table (car from) (vector (car to)))
      (setq from (cdr from) to (cdr to)))
    (setq navi2ch-jisx0213-display-table table))

  (defun navi2ch-jisx0213-set-display-table ()
    (setq buffer-display-table
	  (copy-sequence navi2ch-jisx0213-display-table)))

  (add-hook 'navi2ch-bm-mode-hook      'navi2ch-jisx0213-set-display-table)
  (add-hook 'navi2ch-article-mode-hook 'navi2ch-jisx0213-set-display-table)

  ;; なんでこんなのが必要なの?
  (defadvice string-width (around display-table-hack activate)
    (let ((buffer-display-table nil))
      ad-do-it))
  )

;; とりあえず4つ
(setq navi2ch-replace-html-tag-alist
      (append navi2ch-replace-html-tag-alist
	      '(("&spades;" . "♠")
		("&clubs;"  . "♣")
		("&hearts;" . "♥")
		("&diams;"  . "♦"))))

;; 正規表現を作りなおす
(setq navi2ch-replace-html-tag-regexp
  (concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
          "\\|"
          (mapconcat 'car navi2ch-replace-html-tag-regexp-alist "\\|")))

(provide 'navi2ch-jisx0213)

;;; navi2ch-jisx0213.el ends here