;;; navi2ch-popup-article.el --- popup article module for navi2ch

;; Copyright (C) 2001 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
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
(provide 'navi2ch-popup-article)

(require 'navi2ch)

(defvar navi2ch-popup-article-buffer-name "*navi2ch popup article*")
(defvar navi2ch-popup-article-window-configuration nil)
(defvar navi2ch-popup-article-mode-map nil)
(unless navi2ch-popup-article-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map " " 'navi2ch-article-scroll-up)
    (define-key map [del] 'navi2ch-article-scroll-down)
    (define-key map [backspace] 'navi2ch-article-scroll-down)
    (define-key map "\177" 'navi2ch-article-scroll-down)
    ;; (define-key map "\r" 'navi2ch-popup-article-select-current-link)
    ;; (navi2ch-define-mouse-key map 2 'navi2ch-popup-article-mouse-select)
    (define-key map "g" 'navi2ch-article-goto-number)
    (define-key map "q" 'navi2ch-popup-article-exit)
    (define-key map "l" 'navi2ch-popup-article-pop-point-or-exit)
    (define-key map "L" 'navi2ch-article-pop-poped-point)
    (define-key map "m" 'navi2ch-article-push-point)
      
    (define-key map "." 'navi2ch-article-redisplay-current-message)
    (define-key map "p" 'navi2ch-article-previous-message)
    (define-key map "n" 'navi2ch-article-next-message)
    (define-key map [(shift tab)] 'navi2ch-article-previous-link)
    (define-key map "\e\C-i" 'navi2ch-article-previous-link)
    (define-key map "\C-\i" 'navi2ch-article-next-link)
    (define-key map ">" 'navi2ch-article-goto-last-message)
    (define-key map "<" 'navi2ch-article-goto-first-message)
    (define-key map "\eu" 'navi2ch-article-uudecode-message)
    (define-key map "\eb" 'navi2ch-article-base64-decode-message)
    (define-key map "v" 'navi2ch-article-view-aa)
    (setq navi2ch-popup-article-mode-map map)))
    
(defun navi2ch-popup-article-exit ()
  (interactive)
  (run-hooks 'navi2ch-popup-article-exit-hook)
  (bury-buffer)
  (set-window-configuration navi2ch-popup-article-window-configuration))
  
(defun navi2ch-popup-article-pop-point-or-exit ()
  (interactive)
  (if navi2ch-article-point-stack
      (navi2ch-article-pop-point)
    (navi2ch-popup-article-exit)))

(defun navi2ch-popup-article-mode ()
  (interactive)
  (setq major-mode 'navi2ch-popup-article-mode)
  (setq mode-name "Navi2ch PopUp Article")
  (setq buffer-read-only t)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)
  (use-local-map navi2ch-popup-article-mode-map)
  (setq navi2ch-article-point-stack nil)
  (run-hooks 'navi2ch-popup-article-mode-hook))

(defun navi2ch-popup-article (num-list)
  (let ((mlist navi2ch-article-message-list)
	(sep navi2ch-article-separator)
	(buf (get-buffer-create navi2ch-popup-article-buffer-name)))
    (setq navi2ch-popup-article-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (navi2ch-popup-article-mode)
    (setq navi2ch-article-message-list mlist)
    (setq navi2ch-article-message-list
	  (mapcar (lambda (x)
		    (let ((msg (navi2ch-article-get-message x)))
		      (cons x (if (stringp msg)
				  (navi2ch-article-parse-message msg)
				(copy-alist msg)))))
		  num-list))
    (setq navi2ch-article-separator sep)
    (setq navi2ch-article-point-stack nil)
    (setq navi2ch-article-poped-point-stack nil)
    (setq truncate-partial-width-windows nil)
    (setq navi2ch-article-view-range nil)
    (setq navi2ch-article-through-next-function 'navi2ch-popup-article-exit)
    (setq navi2ch-article-through-previous-function 'navi2ch-popup-article-exit)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (navi2ch-article-insert-messages
       navi2ch-article-message-list
       nil))
    (goto-char (point-min))))

(defun navi2ch-popup-article-scroll-up ()
  (interactive)
  (condition-case error
      (scroll-up)
    (end-of-buffer
     (navi2ch-popup-article-exit)))
  (force-mode-line-update t))

(run-hooks 'navi2ch-popup-article-load-hook)
;;; navi2ch-popup-article.el ends here
