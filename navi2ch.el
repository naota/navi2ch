;;; navi2ch.el --- Navigator for 2ch for Emacsen

;; Copyright (C) 2000 by 2ちゃんねる

;; Author: (not 1)
;; Keywords: network, 2ch

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

;;; Code:
(provide 'navi2ch)

(eval-when-compile (require 'cl))

;; BEWARE: order is important.
(require 'navi2ch-vars)
(require 'navi2ch-face)
(require 'navi2ch-util)
(require 'navi2ch-net)
(require 'navi2ch-list)
(require 'navi2ch-article)
(require 'navi2ch-popup-article)
(require 'navi2ch-board-misc)
(require 'navi2ch-board)
(require 'navi2ch-articles)
(require 'navi2ch-bookmark)
(require 'navi2ch-history)
(require 'navi2ch-search)
(require 'navi2ch-message)
(require 'navi2ch-version)

(defgroup navi2ch nil
  "Navigator for 2ch."
  :group 'hypermedia)

(defvar navi2ch-ask-when-exit t)

(defvar navi2ch-init nil)

(defun navi2ch (&optional arg)
  "Navigator for 2ch for Emacs"
  (interactive "P")
  (run-hooks 'navi2ch-before-startup-hook)
  (unless navi2ch-init
    (when (file-exists-p navi2ch-init-file)
      (load-file navi2ch-init-file))
    (add-hook 'kill-emacs-hook 'navi2ch-save-status)
    (if arg (setq navi2ch-offline (not navi2ch-offline)))
    (run-hooks 'navi2ch-load-status-hook)
    (setq navi2ch-init t)
    (run-hooks 'navi2ch-hook))
  (navi2ch-list)
  (run-hooks 'navi2ch-after-startup-hook))

(defun navi2ch-version ()
  (interactive)
  (message "Navivagor for 2ch %s" navi2ch-version))

(defun navi2ch-save-status ()
  "list, board, article の状態を保存する"
  (interactive)
  (message "save status...")
  (run-hooks 'navi2ch-save-status-hook)
  (message "save status...done"))
  
(defun navi2ch-exit (&optional suspend)
  "navi2ch を終了する
SUSPEND が non-nil なら buffer を消さない"
  (interactive)
  (when (or suspend
	    (not navi2ch-ask-when-exit)
	    (y-or-n-p "really exit navi2ch?"))
    (run-hooks 'navi2ch-exit-hook)
    (navi2ch-save-status)
    (dolist (x (append
                (list
                 (get-buffer navi2ch-list-buffer-name)
                 (get-buffer navi2ch-board-buffer-name)
		 (get-buffer navi2ch-popup-article-buffer-name)
		 (get-buffer navi2ch-message-backup-buffer-name))
                (navi2ch-article-buffer-list)))
      (when x
        (delete-windows-on x)
        (if suspend
            (bury-buffer x)
          (kill-buffer x))))
    (unless suspend
      (setq navi2ch-init nil)
      (remove-hook 'kill-emacs-hook 'navi2ch-save-status))))

(defun navi2ch-suspend ()
  "navi2ch を一時的に終了する"
  (interactive)
  (navi2ch-exit 'suspend))

(defun navi2ch-three-pane ()
  (interactive)
  (let ((list-buf (get-buffer navi2ch-list-buffer-name))
	(board-buf (get-buffer navi2ch-board-buffer-name))
	(art-buf (navi2ch-article-current-buffer))
	(buf (current-buffer))
	(start (window-start)))
    (delete-other-windows)
    (if (not (and list-buf board-buf art-buf))
	(navi2ch-two-pane)
      (if (not (memq buf (list list-buf board-buf art-buf)))
	  (setq buf list-buf
		start nil))
      (switch-to-buffer list-buf)
      (select-window (split-window-horizontally navi2ch-list-window-width))
      (switch-to-buffer board-buf)
      (select-window (split-window-vertically navi2ch-board-window-height))
      (switch-to-buffer art-buf)
      (select-window (get-buffer-window buf))
      (if start
	  (set-window-start (selected-window) start)))))

(defun navi2ch-one-pane ()
  (interactive)
  (let ((list-buf (get-buffer navi2ch-list-buffer-name))
        (board-buf (get-buffer navi2ch-board-buffer-name))
        (art-buf (navi2ch-article-current-buffer))
	(buf (current-buffer)))
    (if (> (count-windows) 1)
	(let ((start (window-start)))
	  (delete-other-windows)
	  (set-window-start (selected-window) start))
      (delete-other-windows)
      (switch-to-buffer
       (cond ((eq buf list-buf)
              (or board-buf art-buf list-buf))
             ((eq buf board-buf)
              (or art-buf list-buf board-buf))
             ((eq buf art-buf)
              (or list-buf board-buf art-buf))
             (t
              (or list-buf board-buf art-buf buf)))))))

(defun navi2ch-two-pane-horizontally (buf-left buf-right)
  "画面を左右に分割して BUF-LEFT、BUF-RIGHT を割り当てる。
\(win-left win-right) のリストを返す"
  (delete-other-windows)
  (let ((win-left (selected-window))
	(win-right (split-window-horizontally navi2ch-list-window-width)))
    (set-window-buffer win-left buf-left)
    (set-window-buffer win-right buf-right)
    (list win-left win-right)))

(defun navi2ch-two-pane-vertically (buf-top buf-bottom)
  "画面を上下に分割して BUF-TOP、BUF-BOTTOM を割り当てる。
\(win-top win-bottom) のリストを返す"
  (delete-other-windows)
  (let ((win-top (selected-window))
	(win-bottom (split-window-vertically navi2ch-board-window-height)))
    (set-window-buffer win-top buf-top)
    (set-window-buffer win-bottom buf-bottom)
    (list win-top win-bottom)))

(defun navi2ch-two-pane ()
  (interactive)
  (let* ((list-buf (get-buffer navi2ch-list-buffer-name))
	 (board-buf (get-buffer navi2ch-board-buffer-name))
	 (art-buf (navi2ch-article-current-buffer))
	 (list-win (get-buffer-window (or list-buf "")))
	 (board-win (get-buffer-window (or board-buf "")))
	 (art-win (get-buffer-window (or art-buf "")))
	 (buf (current-buffer))
	 (start (window-start)))
    (when (not (memq buf (list list-buf board-buf art-buf)))
      (setq buf (or list-buf board-buf art-buf))
      (unless buf
	(error "No navi2ch buffer"))
      (switch-to-buffer buf)
      (setq start (window-start)))
    (cond ((and (eq buf list-buf)
		(or board-buf art-buf))
	   (navi2ch-two-pane-horizontally buf
					  (if art-win
					      (or board-buf art-buf)
					    (or art-buf board-buf))))
	  ((and (eq buf board-buf)
		list-buf
		(or art-win
		    (null art-buf)))
	   (navi2ch-two-pane-horizontally list-buf buf))
	  ((and (eq buf board-buf)
		art-buf)
	   (navi2ch-two-pane-vertically buf art-buf))
	  ((and (eq buf art-buf)
		list-buf
		(or board-win
		    (null board-buf)))
	   (navi2ch-two-pane-horizontally list-buf buf))
	  ((and (eq buf art-buf)
		board-buf)
	   (navi2ch-two-pane-vertically board-buf buf)))
    (select-window (get-buffer-window buf))
    (set-window-start (selected-window) start)))

(defun navi2ch-save-info (file info)
  (setq info (navi2ch-strip-properties info))
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (when (or (file-regular-p file)
	    (not (file-exists-p file)))
    (let ((coding-system-for-write navi2ch-coding-system))
      (with-temp-file file
	(let ((standard-output (current-buffer)))
	  (prin1 info))))))

(defun navi2ch-load-info (file)
  (when (file-regular-p file)
    (let ((coding-system-for-read navi2ch-coding-system))
      (with-temp-buffer
	(insert-file-contents file)
	(let ((standard-input (current-buffer)))
	  (read))))))

(defun navi2ch-goto-url (url &optional force)
  "URL からスレまたは板を選ぶ"
  (interactive "sURL: ")
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer))))
	(article (navi2ch-article-url-to-article url))
	(board (navi2ch-board-url-to-board url)))
    (when board
      (cond (art-win
	     (select-window art-win)
	     (unless article
	       (navi2ch-article-exit)))
	    (board-win
	     (select-window board-win)
	     (when article
	       (condition-case nil
		   (enlarge-window (frame-height))
		 (error nil))
	       (split-window-vertically navi2ch-board-window-height)
	       (other-window 1)))
	    (list-win
	     (select-window list-win)
	     (when navi2ch-list-stay-list-window
	       (split-window-horizontally navi2ch-list-window-width)
	       (other-window 1))))
      (if article
	  (progn
	    (navi2ch-article-view-article board
					  article
					  force
					  (cdr (assq 'number article))))
	(navi2ch-board-select-board board force)))))

(defun navi2ch-2ch-url-p (url)
  "URL が 2ch 内の url かを返す。"
  (let ((host (navi2ch-url-to-host url)))
    (or (member host navi2ch-2ch-host-list)
	(let (list)
	  (setq list
		(mapcar
		 (lambda (x)
		   (navi2ch-url-to-host (cdr (assq 'uri x))))
		 (navi2ch-list-get-board-name-list
		  navi2ch-list-category-list)))
	  (member host list)))))
                      
(run-hooks 'navi2ch-load-hook)
;;; navi2ch.el ends here
