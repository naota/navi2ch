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

(eval-when-compile (require 'cl))
(require 'navi2ch-util)
(require 'navi2ch-net)
(require 'navi2ch-board)
(require 'navi2ch-board-misc)
(require 'navi2ch-list)
(require 'navi2ch-article)
(require 'navi2ch-message)
(require 'navi2ch-version)

(defgroup navi2ch nil
  "Navigator for 2ch."
  :group 'hypermedia)

(defvar navi2ch-ask-when-exit t)

(defvar navi2ch-coding-system 'shift_jis)

(defvar navi2ch-offline nil "オフラインモードかどうか")
(defvar navi2ch-offline-on "[ON] ")
(defvar navi2ch-offline-off "[--] ")

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
    ;; それぞれの module で add-hook した方がいいんじゃ？
    (navi2ch-bookmark-load-info)
    (navi2ch-history-load-info)
    (navi2ch-bm-load-info)
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
  ;; それぞれの module で add-hook した方がいいんじゃ？
  (message "save status...")
  (dolist (x (navi2ch-article-buffer-list))
    (save-excursion
      (set-buffer x)
      (navi2ch-article-save-number)
      (navi2ch-article-save-info)))
  (navi2ch-board-save-info)
  (navi2ch-list-save-info)
  (navi2ch-bookmark-save-info)
  (navi2ch-history-save-info)
  (navi2ch-bm-save-info)
  (message "save status...done"))
  
(defun navi2ch-exit (&optional suspend)
  "navi2ch を終了する
SUSPEND が non-nil なら buffer を消さない"
  (interactive)
  (when (or suspend
	    (not navi2ch-ask-when-exit)
	    (y-or-n-p "really exit navi2ch?"))
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

(defun navi2ch-toggle-offline ()
  (interactive)
  (setq navi2ch-offline (not navi2ch-offline))
  (message (if navi2ch-offline
               "offline"
             "online"))
  (navi2ch-set-mode-line-identification))

(defun navi2ch-three-pain ()
  (interactive)
  (delete-other-windows)
  (let ((list-buf (get-buffer navi2ch-list-buffer-name))
         (board-buf (get-buffer navi2ch-board-buffer-name))
         (art-buf (navi2ch-article-current-buffer))
         buf)
    (if (or (eq (current-buffer) list-buf)
            (eq (current-buffer) board-buf)
            (eq (current-buffer) art-buf))
        (setq buf (current-buffer))
      (setq buf list-buf))
    (when list-buf
      (switch-to-buffer navi2ch-list-buffer-name)
      (when board-buf
        (split-window-horizontally navi2ch-list-window-width)
        (other-window 1)))
    (when board-buf
      (switch-to-buffer navi2ch-board-buffer-name)
      (when art-buf
        (split-window-vertically navi2ch-board-window-height)
        (other-window 1)))
    (when art-buf
      (switch-to-buffer art-buf))
    (select-window (get-buffer-window buf))))

(defun navi2ch-one-pain ()
  (interactive)
  (let ((list-buf (get-buffer navi2ch-list-buffer-name))
        (board-buf (get-buffer navi2ch-board-buffer-name))
        (art-buf (navi2ch-article-current-buffer)))
    (if (> (count-windows) 1)
        (delete-other-windows)
      (delete-other-windows)
      (switch-to-buffer
       (cond ((eq (current-buffer) list-buf)
              (or board-buf art-buf list-buf))
             ((eq (current-buffer) board-buf)
              (or art-buf list-buf board-buf))
             ((eq (current-buffer) art-buf)
              (or list-buf board-buf art-buf))
             (t
              (or list-buf board-buf art-buf (current-buffer))))))))

(defun navi2ch-save-info (file info)
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
  (let (list)
    (setq list
          (mapcar
           (lambda (x)
             (let ((str (cdr (assq 'uri x))))
               (and str
                    (string-match "http://\\([^/]+\\)" str)
                    (match-string 1 str))))
           (navi2ch-list-get-board-name-list
            navi2ch-list-category-list)))
    (when (string-match "http://\\([^/]+\\)" url)
      (setq url (match-string 1 url))
      (member url list))))
                      
(defun navi2ch-insert-file-contents (file &optional begin end)
  (let ((coding-system-for-read navi2ch-coding-system)
        (coding-system-for-write navi2ch-coding-system))
    (insert-file-contents file nil begin end)))

(defun navi2ch-expand-file-name (file)
  (expand-file-name file navi2ch-directory))
  
(provide 'navi2ch)

;;; navi2ch.el ends here
