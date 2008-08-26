;;; navi2ch-mona.el --- Mona Font Utils for Navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 by Navi2ch Project

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation,
;; Inc.

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; 431 の名無しさん
;; 874 の名無しさん
;; UEYAMA Rui <rui314159@users.sourceforge.net>
;; part5 スレの 26, 45 さん

;; The part of find-face is originated form apel (poe.el).
;; You can get the original apel from <ftp://ftp.m17n.org/pub/mule/apel>.
;; poe.el's Authors:  MORIOKA Tomohiko <tomo@m17n.org>
;;      Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; apel is also licened under GPL.

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

;; custom を使って M-x customize-group navi2ch-mona から
;; 設定するとラクチン。

;;; Code:
(provide 'navi2ch-mona)
(defconst navi2ch-mona-ident
  "$Id$")
(eval-when-compile (require 'cl))
(require 'base64)
(require 'navi2ch-util)

(eval-when-compile
  (navi2ch-defalias-maybe 'set-face-attribute 'ignore)
  (autoload 'x-decompose-font-name "fontset"))

(make-face 'navi2ch-mona-face)
(make-face 'navi2ch-mona12-face)
(make-face 'navi2ch-mona14-face)
(make-face 'navi2ch-mona16-face)

(eval-when-compile
  (navi2ch-defalias-maybe 'query-fontset 'ignore)
  (navi2ch-defalias-maybe 'new-fontset 'ignore))

;; カスタマイズ変数の defcustom に必要な関数
(defun navi2ch-mona-create-fontset-from-family-name (family-name height)
  "navi2ch が必要とするフォントセットを作り、その名前を返す。

FAMILY-NAME は \"foundry-family\" からなる文字列。HEIGHT は pixelsize。

XEmacs では明示的にフォントセットを作る必要がないので、
フォントセット名として意味のある文字列
 \"-<FAMILY-NAME>-medium-r-*--<height>-*-*-*-p-*-*-*\"
を返すだけ。"
  (let ((fontset-name (format "-%s-medium-r-*--%d-*-*-*-p-*-*-*"
                              family-name height)))
    (navi2ch-ifxemacs
	fontset-name
      (let* ((fields (x-decompose-font-name fontset-name))
	     (new-decompose-p (= (length fields) 12))
	     (slant (aref fields (if new-decompose-p 2 3)))
	     (swidth (or (aref fields (if new-decompose-p 3 4)) "normal"))
	     foundry family fontset-templ font-templ fontset)
	(if new-decompose-p
	    (let ((foundry-family (aref fields 0)))
	      (when (string-match "\\([^-]*\\)-\\([^-]*\\)" foundry-family)
		(setq foundry (match-string 1 foundry-family)
		      family (match-string 2 foundry-family))))
	  (setq foundry (aref fields 0)
		family  (aref fields 1)))
	(setq fontset-templ (format
			     "-%s-%s-%%s-%s-%s--%d-*-*-*-p-*-fontset-mona%d"
			     foundry family slant swidth height height)
	     font-templ (progn
			  (string-match "^\\(.*\\)\\(fontset-mona[^-]+\\)$"
					fontset-templ)
			  (concat (match-string 1 fontset-templ) "%s"))
	     fontset (format "-%s-%s-*-*-*--%d-*-*-*-*-*-%s"
			     foundry family height
			     (match-string 2 fontset-templ)))
	(setq fontset-name fontset)
	(dolist (weight '("medium" "bold"))
	  (let ((fontset (format fontset-templ weight))
		(font (format font-templ weight "%s")))
	    (unless (query-fontset fontset)
	      (new-fontset fontset
			   (list (cons 'ascii
				       (format font "iso8859-1"))
				 (cons 'latin-iso8859-1
				       (format font "iso8859-1"))
				 (cons 'katakana-jisx0201
				       (format font "jisx0201.1976-0"))
				 (cons 'latin-jisx0201
				       (format font "jisx0201.1976-0"))
				 (cons 'japanese-jisx0208
				       (format font "jisx0208.1990-0"))))))))
      fontset-name)))

(defun navi2ch-mona-create-face-from-family-name (family-name)
  "VALUE で指定されるフォントセットに応じてフェイスを作成する。"
  (dolist (height '(12 14 16))
    (ignore-errors
      (let ((fontset (navi2ch-mona-create-fontset-from-family-name
		      family-name height))
	    (face (intern (format "navi2ch-mona%d-face" height))))
	(set-face-font face fontset)))))

(defun navi2ch-mona-set-font-family-name (symbol value)
  (navi2ch-mona-create-face-from-family-name value)
  (set-default symbol value))

;; Customizable variables.
(defcustom navi2ch-mona-enable nil
  "*non-nil なら、モナーフォントを使ってスレを表示する。"
  :set (lambda (symbol value)
	 (if value
	     (navi2ch-mona-setup)
	   (navi2ch-mona-undo-setup))
	 (set-default symbol value))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-enable-board-list nil
  "*モナーフォントで表示する板のリスト。
nil のときは `navi2ch-mona-disable-board-list' で指定した板以外の
すべての板でモナーフォントを使用する。"
  :type '(repeat (string :tag "板"))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-disable-board-list nil
  "*モナーフォントを使わない板のリスト。
`navi2ch-mona-enable-board-list' よりも優先される。"
  :type '(repeat (string :tag "板"))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-enable-article-list nil
  "*モナーフォントで表示するスレのリスト。
`navi2ch-mona-disable-board-list' よりも優先される。"
  :type '(repeat (cons (string :tag "板")
		       (string :tag "スレ")))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-disable-article-list nil
  "*モナーフォントを使わないスレのリスト。
`navi2ch-mona-enable-board-list', `navi2ch-mona-enable-article-list'
よりも優先される。"
  :type '(repeat (cons (string :tag "板")
		       (string :tag "スレ")))
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-pack-space-p nil
  "*non-nil なら、Web ブラウザのように2つ以上の空白は1つにまとめて表示する。"
  :type 'boolean
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-font-family-name "mona-gothic"
  "*モナーフォントとして使うフォントの family 名。
XLFD でいう \`foundry-family\' を指定する。要するに X での
フォント名の最初の2フィールドを書けばいいってこった。

XEmacs では、指定された family に対して pixelsize: 12/14/16
の 3つのフォントセットを作る。

Emacs 21 では、それに加えて medium/bold なフォントを別々に作る。
たとえば引数 \`moga-gothic\' がわたされると、

 -mona-gothic-medium-r-*--12-*-*-*-*-*-fontset-mona12
 -mona-gothic-medium-r-*--14-*-*-*-*-*-fontset-mona14
 -mona-gothic-medium-r-*--16-*-*-*-*-*-fontset-mona16
 -mona-gothic-bold-r-*--12-*-*-*-*-*-fontset-mona12
 -mona-gothic-bold-r-*--14-*-*-*-*-*-fontset-mona14
 -mona-gothic-bold-r-*--16-*-*-*-*-*-fontset-mona16

という 6 つのフォントセットを作ることになる。

文字のかわりにトーフが表示されちゃうのは、たぶんフォントが
見つからなかったせいなので、\`xlsfonts\' を実行して

-<指定した文字列>-{medium,bold}-r-*--{12,14,16}-*-*\\
-*-*-*-{iso8859-1,jisx0201.1976-0,jisx0208.(1983|1990)-0}

があるかどうか確かめてね。"
  :type '(choice (const :tag "Mona Font"
			"mona-gothic")
		 (const :tag "MS P Gothic"
			"microsoft-pgothic")
		 (string :tag "family name"))
  :set 'navi2ch-mona-set-font-family-name
  :initialize 'custom-initialize-default
  :group 'navi2ch-mona)

(defconst navi2ch-mona-sample-string
  (concat "サンプルテキストゲットォ！！ ひらがな、カタカナ、Roman Alphabet。\n"
          (decode-coding-string
           (base64-decode-string
	    (concat
	     "gVCBUIFQgVCBUIHJgVCBUIFQgVCBUIFQgVCBUIFAgUAogUyBTAqBQIFAgUCBQCCB"
	     "yIHIgUCBQIFAgWqBQIFAgUCBQIFAgUAogUyB3CiBTAqBQIFAgbyBad+ERN+BvIHc"
	     "gU2CwoHfgd+B3yiBTIHcOzs7gd+B34HfCoFAgUCBQIFAgUCBQCCBUIFQgUAgKIFM"
	     "gdwogUyB3Ds7CoFAgUCBQIFAgUCBQL3eu9673rCwsLCwryK93rveCg=="))
	   'shift_jis)))

(defcustom navi2ch-mona-face-variable t
  "*デフォルトの Mona face を選ぶ。"
  :type `(radio (const :tag "navi2ch-mona16-face"
                       :sample-face navi2ch-mona16-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona16-face)
                (const :tag "navi2ch-mona14-face"
                       :sample-face navi2ch-mona14-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona14-face)
                (const :tag "navi2ch-mona12-face"
                       :sample-face navi2ch-mona12-face
                       :doc ,navi2ch-mona-sample-string
                       :format "%t:\n%{%d%}\n"
                       navi2ch-mona12-face)
                (const :tag "デフォルトのフォントと同じサイズの face を自動選択する"
                       t))
  :set (function (lambda (symbol value)
                   (set-default symbol value)
                   (navi2ch-mona-set-mona-face)))
  :initialize 'custom-initialize-default
  :group 'navi2ch-mona)

(defcustom navi2ch-mona-on-message-mode nil
  "*non-nil の場合、レスを書く時にもモナーフォントを使う。"
  :type 'boolean
  :group 'navi2ch-mona)

;; defun find-face for GNU Emacs
;; the code is originated from apel.
(defun navi2ch-find-face-subr (face-or-name)
  "Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned."
  (car (memq face-or-name (face-list))))

(defalias 'navi2ch-find-face
  (if (fboundp 'find-face)
      #'find-face
    #'navi2ch-find-face-subr))

(defun navi2ch-mona-char-height ()
  (navi2ch-ifxemacs
      (font-height (face-font 'default))
    (frame-char-height)))

(defun navi2ch-set-face-parent (face parent)
  (navi2ch-ifxemacs
      (set-face-parent face parent)
    (set-face-attribute face nil :inherit parent)))

;; functions
(defun navi2ch-mona-set-mona-face ()
  (let ((parent navi2ch-mona-face-variable))
    (when (eq t parent)
      (let* ((height (navi2ch-mona-char-height))
	     (face-name (if height
			    (format "navi2ch-mona%d-face" height)
			  "navi2ch-mona16-face")))
	(setq parent (intern face-name))))
    (when (navi2ch-find-face parent)
      (navi2ch-set-face-parent 'navi2ch-mona-face parent))))

(defun navi2ch-mona-put-face ()
  "face が特に指定されていない部分を mona-face にする。
`navi2ch-article-face' の部分も mona-face にする。"
  (save-excursion
    (goto-char (point-min))
    (let (p face)
      (while (not (eobp))
	(setq p (next-single-property-change (point)
					     'face nil (point-max)))
	(setq face (get-text-property (point) 'face))
	(if (or (null face)
		(eq face 'navi2ch-article-face))
	    (put-text-property (point) p 'face 'navi2ch-mona-face))
	(goto-char p)))))

(defun navi2ch-mona-pack-space ()
  "連続する2つ以上の空白を1つにまとめる。"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ +" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t)
      (replace-match " "))))

(defvar navi2ch-mona-enable-list nil
  "モナーフォントを有効にするリスト。
nil の場合はデフォルトで有効になる。
エレメントが関数: 呼び出した結果が non-nil なら有効
エレメントが文字列: その板で有効
エレメントが cons ペア: car を板、cdr をスレもしくはスレのリストとし、
その板のそのスレで有効

各エレメントは順に評価され、有効とみなされた時点で評価を終了する。")

(defvar navi2ch-mona-disable-list nil
  "モナーフォントを無効にするリスト。
`navi2ch-mona-enable-list' よりも優先される。
エレメントが関数: 呼び出した結果が non-nil なら無効
エレメントが文字列: その板で無効
エレメントが cons ペア: car を板、cdr をスレもしくはスレのリストとし、
その板のそのスレで無効

各エレメントは順に評価され、有効とみなされた時点で評価を終了する。")

(defun navi2ch-mona-match-p (list board-id article-id)
  (dolist (elt list)
    (when (cond ((functionp elt)
		 (funcall elt))
		((stringp elt)
		 (string= elt board-id))
		((and (consp elt)
		      (string= (car elt) board-id))
		 (if (stringp (cdr elt))
		     (string= (cdr elt) article-id)
		   (member article-id (cdr elt)))))
      (return t))))

(eval-when-compile
  (defvar navi2ch-popup-article-current-board)
  (defvar navi2ch-article-current-board)
  (defvar navi2ch-popup-article-current-article)
  (defvar navi2ch-article-current-article))

(defun navi2ch-mona-arrange-message ()
  "モナーフォントを使う板ならそのための関数を呼ぶ。"
  (let ((id (cdr (assq 'id (if (eq major-mode 'navi2ch-popup-article-mode)
			       navi2ch-popup-article-current-board
			     navi2ch-article-current-board))))
	(artid (cdr (assq 'artid (if (eq major-mode 'navi2ch-popup-article-mode)
				     navi2ch-popup-article-current-article
				   navi2ch-article-current-article)))))
    (when (cond ((navi2ch-mona-match-p navi2ch-mona-disable-list
				       id artid)
		 nil)
		((navi2ch-mona-match-p navi2ch-mona-enable-list
				       id artid)
		 t)
		((member (cons id artid) navi2ch-mona-disable-article-list)
		 nil)
		((member (cons id artid) navi2ch-mona-enable-article-list)
		 t)
		((member id navi2ch-mona-disable-board-list)
		 nil)
		((member id navi2ch-mona-enable-board-list)
		 t)
		((or navi2ch-mona-enable-article-list
		     navi2ch-mona-enable-board-list
		     navi2ch-mona-enable-list)
		 nil)
		(t t))
      (navi2ch-mona-put-face))
    (when navi2ch-mona-pack-space-p
      (navi2ch-mona-pack-space))))

(defun navi2ch-mona-message-mode-hook ()
  (if navi2ch-mona-on-message-mode
      (navi2ch-ifxemacs
	  (let ((extent (make-extent (point-min) (point-max))))
	    (set-extent-properties extent
				   '(face navi2ch-mona-face
					  start-closed t end-closed t)))
	(let ((overlay (make-overlay (point-min) (point-max) nil nil t)))
	  (overlay-put overlay 'face 'navi2ch-mona-face)))))

(defun navi2ch-mona-setup ()
  "*モナーフォントを使うためのフックを追加する。"
  (when (and (or (eq window-system 'x) (eq window-system 'w32))
	     (or navi2ch-on-emacs21 navi2ch-on-xemacs))
    (navi2ch-mona-create-face-from-family-name navi2ch-mona-font-family-name)
    (navi2ch-mona-set-mona-face)	; 何回呼んでも大丈夫
    (add-hook 'navi2ch-article-arrange-message-hook
	      'navi2ch-mona-arrange-message)
    (add-hook 'navi2ch-message-mode-hook
	      'navi2ch-mona-message-mode-hook)
    (run-hooks 'navi2ch-mona-setup-hook)))

(defun navi2ch-mona-undo-setup ()
  (run-hooks 'navi2ch-mona-undo-setup-hook)
  (remove-hook 'navi2ch-article-arrange-message-hook
	       'navi2ch-mona-arrange-message)
  (remove-hook 'navi2ch-message-mode-hook
	       'navi2ch-mona-message-mode-hook))

(run-hooks 'navi2ch-mona-load-hook)
;;; navi2ch-mona.el ends here
