;;; navi2ch-mona.el --- Mona Font Utils for Navi2ch

;; Copyright (C) 2001 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; 431 の名無しさん
;; 874 の名無しさん

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

;; (require 'navi2ch-mona)
;; (add-hook 'navi2ch-article-arrange-message-hook
;;           'navi2ch-mona-arrange-message)
;; (setq navi2ch-mona-enable t)
;; すれば取り敢えずおっけ。          

;; GNU emacs21 でフォントサイズを XXpt (== 12, 14, 16) に変える時は、
;; (setq navi2ch-mona-font "-mona-gothic-*-*-*--XX-*-*-*-*-*-fontset-monaXX")
;; を加えて。

;;; Code:
(provide 'navi2ch-mona)

(require 'navi2ch)

(defvar navi2ch-mona-enable nil
  "mona フォントを使用するかどうか")
(defvar navi2ch-mona-enable-board-list nil
  "mona フォントを使用する板のリスト.")
(defvar navi2ch-mona-disable-board-list nil
  "mona フォントを使用しない板のリスト")
(defvar navi2ch-mona-pack-space-p nil
  "2つ以上の空白をまとめるか")
(defmacro navi2ch-mona-font-height ()
  (if (featurep 'xemacs)
      '(font-height (face-font 'default))
    '(frame-char-height)))
(defvar navi2ch-mona-font
  (let ((font-size (navi2ch-mona-font-height)))
    (unless (memq font-size '(12 14 16))
      (setq font-size 16))
    (cond
     ((featurep 'xemacs)
      (format "-mona-gothic-medium-r-*--%d-*-p-*"
	      font-size))
     ((and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
      (format "-mona-gothic-*-*-*--%d-*-*-*-*-*-fontset-mona%d"
	      font-size font-size))))
  "使用する mona フォントの名前")

;; mona 用の face を作成。
(add-hook
 'navi2ch-hook
 '(lambda ()
    (make-face 'navi2ch-mona-face)
    (set-face-font 'navi2ch-mona-face navi2ch-mona-font)))

;; face が特に指定されていない部分を mona-face にする
;; navi2ch-article-face の部分も mona-face にする
(defun navi2ch-mona-put-face ()
  (save-excursion
    (goto-char (point-min))
    (let (p face)
      (while (not (eobp))
        (setq p (next-single-property-change (point)
                                             'face nil (point-max)))
	(setq face (get-text-property (point) 'face))
	(if (or (null face)
		(eq face 'navi2ch-article-face))
	    (put-text-property (point) (1- p)
			       'face 'navi2ch-mona-face))
        (goto-char p)))))

(defun navi2ch-mona-pack-space ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ +" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward"  +" nil t)
      (replace-match " "))))

(defun navi2ch-mona-arrange-message ()
  (let ((id (cdr (assq 'id navi2ch-article-current-board))))
    (when (or (member id navi2ch-mona-enable-board-list)
              (and (not (member id navi2ch-mona-disable-board-list))
                   navi2ch-mona-enable))
      (navi2ch-mona-put-face))
    (when navi2ch-mona-pack-space-p
      (navi2ch-mona-pack-space))))

;; GNU emacs21 では明示的に fontset を作る必要がある
(if (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21)
	 (null (featurep 'xemacs))
	 window-system)
    (progn
      ;; roman - normalface (16pt)
      (new-fontset
       "-mona-gothic-medium-r-normal--16-*-*-*-*-*-fontset-mona16"
       '((ascii
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-80-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-80-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-medium-r-normal--16-150-75-75-p-160-jisx0208.1990-0")
	 (japanese-jisx0208
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-160-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-medium-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 ))
      ;; roman - boldface (16pt)
      (new-fontset
       "-mona-gothic-bold-r-normal--16-*-*-*-*-*-fontset-mona16"
       '((ascii
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-80-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-80-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-bold-r-normal--16-150-75-75-p-160-jisx0208.1990-0")
	 (japanese-jisx0208
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-160-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-bold-r-normal--16-150-75-75-p-80-jisx0201.1976-0")
	 ))

      ;; roman - normalface (14pt)
      (new-fontset
       "-mona-gothic-medium-r-normal--14-*-*-*-*-*-fontset-mona14"
       '((ascii
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-70-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-70-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-medium-r-normal--14-130-75-75-p-140-jisx0208.1990-0")
	 (japanese-jisx0208
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-140-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-medium-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 ))
      ;; roman - boldface (14pt)
      (new-fontset
       "-mona-gothic-bold-r-normal--14-*-*-*-*-*-fontset-mona14"
       '((ascii
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-70-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-70-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-bold-r-normal--14-130-75-75-p-140-jisx0208.1990-0")
	 (japanese-jisx0208
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-140-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-bold-r-normal--14-130-75-75-p-70-jisx0201.1976-0")
	 ))

      ;; roman - normalface (12pt)
      (new-fontset
       "-mona-gothic-medium-r-normal--12-*-*-*-*-*-fontset-mona12"
       '((ascii
	  . "-mona-gothic-medium-r-normal--12-110-75-75-p-60-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-medium-r-normal--12-110-75-75-p-60-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-medium-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-medium-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-medium-r-normal--12-110-75-75-p-120-jisx0208.1990-0")
	 (japanese-jisx0208
          . "-mona-gothic-medium-r-normal--12-110-75-75-p-120-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-medium-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 ))

      ;; roman - boldface (12pt)
      (new-fontset
       "-mona-gothic-gothic-r-normal--12-*-*-*-*-*-fontset-mona12"
       '((ascii
	  . "-mona-gothic-bold-r-normal--12-110-75-75-p-60-iso8859-1")
	 (latin-iso8859-1
	  . "-mona-gothic-bold-r-normal--12-110-75-75-p-60-iso8859-1")
	 (katakana-jisx0201 
	  . "-mona-gothic-bold-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 (latin-jisx0201
	  . "-mona-gothic-bold-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 (japanese-jisx0208-1978
          . "-mona-gothic-bold-r-normal--12-110-75-75-p-120-jisx0208.1990-0")
	 (japanese-jisx0208
          . "-mona-gothic-bold-r-normal--12-110-75-75-p-120-jisx0208.1990-0")
	 (japanese-jisx0212
	  . "-mona-gothic-bold-r-normal--12-110-75-75-p-60-jisx0201.1976-0")
	 ))
      )
  )

(run-hooks 'navi2ch-mona-load-hook)
;;; navi2ch-mona.el ends here
