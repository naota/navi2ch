;;; navi2ch-logo.el --- Inline logo module for navi2ch

;; Copyright (C) 2002 by navi2ch Project

;; Author:
;;   (not 1)
;;   http://pc.2ch.net/test/read.cgi/unix/999166513/895 の名無しさん
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
;; レス一覧の先頭にその板のロゴを貼りつける。

;;; Code:

(require 'navi2ch-board)
(require 'navi2ch-board-misc)

(defun navi2ch-board-display-logo ()
  (interactive)
  (overlay-put inline-logo 'before-string nil)
  (when (eq major-mode 'navi2ch-board-mode)
    (let* ((file (navi2ch-bm-get-logo))
	   (image (navi2ch-board-create-logo-image file))
	   (string " "))
      (when image
	(setq string (concat (propertize string 'display image) "\n"))
	(let ((buffer-read-only nil))
	  (overlay-put inline-logo 'before-string string))))))

(defun navi2ch-board-init-logo ()
  (if (boundp 'inline-logo)
      (delete-overlay inline-logo))
  (make-variable-frame-local 'inline-logo)
  (setq inline-logo (make-overlay (point-min) (point-min))))

(defun navi2ch-board-create-logo-image (file &rest props)
  (and file
       (condition-case nil;;

	   (or (apply 'create-image (append (li;;;
;;; レス一覧の先頭にその板のロゴを貼りつける。
;;;
(defun navi2ch-board-display-logo ()
  (interactive)
  (overlay-put inline-logo 'before-string nil)
  (when (eq major-mode 'navi2ch-board-mode)
    (let* ((file (navi2ch-bm-get-logo))
	   (image (navi2ch-board-create-logo-image file))
	   (string " "))
      (when image
	(setq string (concat (propertize string 'display image) "\n"))
	(let ((buffer-read-only nil))
	  (overlay-put inline-logo 'before-string string))))))

(defun navi2ch-board-init-logo ()
  (if (boundp 'inline-logo)
      (delete-overlay inline-logo))
  (make-variable-frame-local 'inline-logo)
  (setq inline-logo (make-overlay (point-min) (point-min))))

(defun navi2ch-board-create-logo-image (file &rest props)
  (and file
       (condition-case nil
	   (or (apply 'create-image (append (list file nil) props))
	       ;; その画像フォーマットに Emacs が対応してないとき
	       (catch 'found
		 (let ((newfile))
		   (dolist (format '(png xpm xbm))
		     (setq newfile (concat (file-name-sans-extension file)
					   "." (symbol-name format)))
		     (when (file-newer-than-file-p file newfile)
		       (call-process "convert" nil nil nil file newfile)
		       (throw 'found
			      (create-image newfile format props)))))))
	 (error . nil)))) 

(add-hook 'navi2ch-board-select-board-hook
	  'navi2ch-board-init-logo)

(add-hook 'navi2ch-board-after-sync-hook
	  'navi2ch-board-display-logo)

(defun navi2ch-bm-get-logo ()
  "そのロゴを替新する。返り値はキャッシュのフルパス。
ロゴを取得できなくて、キャッシュにもないときは、nil を返す。"
  (interactive)
  (let ((board (funcall navi2ch-bm-get-board-function
			(funcall navi2ch-bm-get-property-function (point))))
	(board-mode-p (eq major-mode 'navi2ch-board-mode))
	file old-file)
    (unless board-mode-p
      (setq board (navi2ch-board-load-info board)))
    (setq old-file (cdr (assq 'logo board)))
    (if navi2ch-offline
	(setq file old-file)
      (setq file (file-name-nondirectory (navi2ch-net-download-logo board)))
      (when file
	(when (and old-file navi2ch-board-delete-old-logo
		   (not (string-equal file old-file)))
	  (delete-file (navi2ch-board-get-file-name board old-file)))
	(if board-mode-p
	    (setq navi2ch-board-current-board board)
	  (navi2ch-board-save-info board))))
    (if file
	(navi2ch-board-get-file-name board file))))

(defun navi2ch-bm-view-logo ()
  "その板のロゴを見る"
  (interactive)
  (let ((file (navi2ch-bm-get-logo)))
    (if file
	(apply 'start-process "navi2ch view logo"
	       nil navi2ch-board-view-logo-program
	       (append navi2ch-board-view-logo-args
		       (list file)))
      (message "Can't find logo file")))) 
st file nil) props))
	       ;; その画像フォーマットに Emacs が対応してないとき
	       (catch 'found
		 (let ((newfile))
		   (dolist (format '(png xpm xbm))
		     (setq newfile (concat (file-name-sans-extension file)
					   "." (symbol-name format)))
		     (when (file-newer-than-file-p file newfile)
		       (call-process "convert" nil nil nil file newfile)
		       (throw 'found
			      (create-image newfile format props)))))))
	 (error . nil)))) 

(add-hook 'navi2ch-board-select-board-hook
	  'navi2ch-board-init-logo)

(add-hook 'navi2ch-board-after-sync-hook
	  'navi2ch-board-display-logo)

(defun navi2ch-bm-get-logo ()
  "そのロゴを替新する。返り値はキャッシュのフルパス。
ロゴを取得できなくて、キャッシュにもないときは、nil を返す。"
  (interactive)
  (let ((board (funcall navi2ch-bm-get-board-function
			(funcall navi2ch-bm-get-property-function (point))))
	(board-mode-p (eq major-mode 'navi2ch-board-mode))
	file old-file)
    (unless board-mode-p
      (setq board (navi2ch-board-load-info board)))
    (setq old-file (cdr (assq 'logo board)))
    (if navi2ch-offline
	(setq file old-file)
      (setq file (file-name-nondirectory (navi2ch-net-download-logo board)))
      (when file
	(when (and old-file navi2ch-board-delete-old-logo
		   (not (string-equal file old-file)))
	  (delete-file (navi2ch-board-get-file-name board old-file)))
	(if board-mode-p
	    (setq navi2ch-board-current-board board)
	  (navi2ch-board-save-info board))))
    (if file
	(navi2ch-board-get-file-name board file))))

(defun navi2ch-bm-view-logo ()
  "その板のロゴを見る"
  (interactive)
  (let ((file (navi2ch-bm-get-logo)))
    (if file
	(apply 'start-process "navi2ch view logo"
	       nil navi2ch-board-view-logo-program
	       (append navi2ch-board-view-logo-args
		       (list file)))
      (message "Can't find logo file"))));;;
;;; レス一覧の先頭にその板のロゴを貼りつける。
;;;
(defun navi2ch-board-display-logo ()
  (interactive)
  (overlay-put inline-logo 'before-string nil)
  (when (eq major-mode 'navi2ch-board-mode)
    (let* ((file (navi2ch-bm-get-logo))
	   (image (navi2ch-board-create-logo-image file))
	   (string " "))
      (when image
	(setq string (concat (propertize string 'display image) "\n"))
	(let ((buffer-read-only nil))
	  (overlay-put inline-logo 'before-string string))))))

(defun navi2ch-board-init-logo ()
  (if (boundp 'inline-logo)
      (delete-overlay inline-logo))
  (make-variable-frame-local 'inline-logo)
  (setq inline-logo (make-overlay (point-min) (point-min))))

(defun navi2ch-board-create-logo-image (file &rest props)
  (and file
       (condition-case nil
	   (or (apply 'create-image (append (list file nil) props))
	       ;; その画像フォーマットに Emacs が対応してないとき
	       (catch 'found
		 (let ((newfile))
		   (dolist (format '(png xpm xbm))
		     (setq newfile (concat (file-name-sans-extension file)
					   "." (symbol-name format)))
		     (when (file-newer-than-file-p file newfile)
		       (call-process "convert" nil nil nil file newfile)
		       (throw 'found
			      (create-image newfile format props)))))))
	 (error . nil)))) 

(add-hook 'navi2ch-board-select-board-hook
	  'navi2ch-board-init-logo)

(add-hook 'navi2ch-board-after-sync-hook
	  'navi2ch-board-display-logo)

(defun navi2ch-bm-get-logo ()
  "そのロゴを替新する。返り値はキャッシュのフルパス。
ロゴを取得できなくて、キャッシュにもないときは、nil を返す。"
  (interactive)
  (let ((board (funcall navi2ch-bm-get-board-function
			(funcall navi2ch-bm-get-property-function (point))))
	(board-mode-p (eq major-mode 'navi2ch-board-mode))
	file old-file)
    (unless board-mode-p
      (setq board (navi2ch-board-load-info board)))
    (setq old-file (cdr (assq 'logo board)))
    (if navi2ch-offline
	(setq file old-file)
      (setq file (file-name-nondirectory (navi2ch-net-download-logo board)))
      (when file
	(when (and old-file navi2ch-board-delete-old-logo
		   (not (string-equal file old-file)))
	  (delete-file (navi2ch-board-get-file-name board old-file)))
	(if board-mode-p
	    (setq navi2ch-board-current-board board)
	  (navi2ch-board-save-info board))))
    (if file
	(navi2ch-board-get-file-name board file))))

(defun navi2ch-bm-view-logo ()
  "その板のロゴを見る"
  (interactive)
  (let ((file (navi2ch-bm-get-logo)))
    (if file
	(apply 'start-process "navi2ch view logo"
	       nil navi2ch-board-view-logo-program
	       (append navi2ch-board-view-logo-args
		       (list file)))
      (message "Can't find logo file")))) 

(provide 'navi2ch-logo)

;;; navi2ch-logo.el ends here