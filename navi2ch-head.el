;;; navi2ch-head.el --- View a local rule mode for navi2ch

;; Copyright (C) 2001, 2002 by Navi2ch Project

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
(defvar navi2ch-head-ident
  "$Id$")

(require 'navi2ch)

;; navi2ch-head-mode

(defvar navi2ch-head-mode-map nil
  "ローカルルールのビュワーのキーマップ")
(unless navi2ch-head-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map "q" 'navi2ch-head-exit)
    (define-key map "l" 'navi2ch-head-exit)
    (setq navi2ch-head-mode-map map)))

(defun navi2ch-head-mode ()
  "\\{navi2ch-head-mode-map}"
  (interactive)
  (setq major-mode 'navi2ch-head-mode)
  (setq mode-name "Navi2ch Head")
  (setq buffer-read-only t)
  (use-local-map navi2ch-head-mode-map)
  (run-hooks 'navi2ch-head-mode-hook))

;; Functions

(defun navi2ch-head-exit ()
  "ローカルルールバッファを消す。どこへ行くかはemacsまかせ。これ直さないと…FIXME"
  (interactive)
  (run-hooks 'navi2ch-head-exit-hook)
  (let* ((buf (current-buffer)))
    (delete-windows-on buf)
    (kill-buffer buf)))

(defun navi2ch-head-get-uri ()
  "ローカルルールの書いてあるURI"
  (cond ((eq major-mode 'navi2ch-article-mode)
	 (concat (cdr (assoc 'uri navi2ch-article-current-board)) "head.txt"))
	((eq major-mode 'navi2ch-board-mode)
	 (concat (cdr (assoc 'uri navi2ch-board-current-board)) "head.txt"))))

;; Entry points from navi2ch-article-mode/navi2ch-board-mode

(define-key navi2ch-article-mode-map "H" 'navi2ch-head-get-head-txt)
(define-key navi2ch-board-mode-map "H" 'navi2ch-head-get-head-txt)

(defun navi2ch-head-get-head-txt ()
  "ローカルルールを持ってきて表示。head.txtに保存しちゃうよ。"
  (interactive)
  (let* ((uri (navi2ch-head-get-uri))
	 (filename (concat  (expand-file-name navi2ch-directory) "/"
			    (progn
			      (string-match "^http://\\(.*\\)" uri)
			      (match-string 1 uri)))))
    (or navi2ch-offline
	(navi2ch-net-update-file uri filename))
    (message uri filename)
    (save-excursion
      (find-file filename)
      (when (eq (point-max) (point-min))
	(insert-string "'H'を押す。\n")
	(insert-string "  ↓\n")
	(insert-string "板ローカルルールを見る。\n")
	(insert-string "  ↓\n")
	(insert-string "板ローカルルールはない!\n")
	(insert-string "  ↓\n")
	(insert-string "(ﾟдﾟ)ﾏｼﾞｳﾏｰ\n"))
      (set-buffer-modified-p nil)
      (navi2ch-head-mode))))

(run-hooks 'navi2ch-head-load-hook)
;;; navi2ch-head.el ends here
