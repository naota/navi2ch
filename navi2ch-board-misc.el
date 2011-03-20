;;; navi2ch-board-misc.el --- Miscellaneous Functions for Navi2ch Board Mode -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2008 by Navi2ch
;; Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
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

;;

;;; Code:
(provide 'navi2ch-board-misc)
(defconst navi2ch-board-misc-ident
  "$Id$")

(eval-when-compile 
  (require 'cl)
  (defvar navi2ch-board-last-seen-alist)
  (defvar navi2ch-board-subject-alist)
  (defvar navi2ch-board-current-board))

;; Avoid byte-compile warnings (contrib/izonmoji-mode.el).
(eval-when-compile (defvar izonmoji-mode nil))

(require 'navi2ch)

(defvar navi2ch-bm-mode-map nil)
(unless navi2ch-bm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map "\r" 'navi2ch-bm-select-article)
    (navi2ch-define-mouse-key map 2 'navi2ch-bm-mouse-select)
    (define-key map " " 'navi2ch-bm-select-article-or-scroll-up)
    (define-key map "." 'navi2ch-bm-display-article)
    (define-key map "i" 'navi2ch-bm-fetch-article)
    (define-key map "e" 'navi2ch-bm-textize-article)
    (navi2ch-define-delete-keys map 'navi2ch-bm-select-article-or-scroll-down)
    (define-key map "n" 'navi2ch-bm-next-line)
    (define-key map "p" 'navi2ch-bm-previous-line)
    (define-key map "U" 'navi2ch-bm-show-url)
    (define-key map "l" 'navi2ch-bm-view-logo)
    (define-key map "A" 'navi2ch-bm-add-global-bookmark)
    (define-key map "g" 'navi2ch-bm-goto-board)
    (define-key map "q" 'navi2ch-bm-exit)
    (define-key map "S" 'navi2ch-bm-sort)
    (define-key map "?" 'navi2ch-bm-search)
    (define-key map "\C-c\C-m" 'navi2ch-message-pop-message-buffer)
    (define-key map "R" 'navi2ch-bm-remove-article)
    (define-key map "\C-c\C-r" 'navi2ch-bm-remove-article)
    (define-key map "\C-o" 'navi2ch-bm-save-dat-file)
    (define-key map "I" 'navi2ch-bm-fetch-maybe-new-articles)

    ;; mark command
    (define-key map "*" 'navi2ch-bm-mark)
    (define-key map "u" 'navi2ch-bm-unmark)
    (define-key map "m" nil)
    (define-key map "mr" 'navi2ch-bm-mark-region)
    (define-key map "ma" 'navi2ch-bm-mark-all)
    (define-key map "mA" 'navi2ch-bm-add-global-bookmark-mark-article)
    (define-key map "m." 'navi2ch-bm-display-mark-article)
    (define-key map "mi" 'navi2ch-bm-fetch-mark-article)
    (define-key map "me" 'navi2ch-bm-textize-mark-article)
    (define-key map "mm" 'navi2ch-bm-mark-marks)
    (define-key map "m?" 'navi2ch-bm-mark-by-query)
    (define-key map "mb" 'navi2ch-bm-add-bookmark-mark-article)
    (define-key map "mR" 'navi2ch-bm-remove-mark-article)
    (setq navi2ch-bm-mode-map map)))

(defvar navi2ch-bm-mode-menu-spec
  '(["Toggle offline" navi2ch-toggle-offline]
    ["Exit" navi2ch-bm-exit]
    ["Sort" navi2ch-bm-sort]
    ["Search" navi2ch-bm-search])
  "Menu の元")

(defvar navi2ch-board-buffer-name "*navi2ch board*")

;; set by navi2ch-bm-setup
(defvar navi2ch-bm-get-property-function nil
  "その位置の text-property を得る関数。引数は POINT")
(defvar navi2ch-bm-set-property-function nil
  "text-property を設定する関数。引数は BEGIN END ITEM")
(defvar navi2ch-bm-get-board-function nil
  "板を得る関数。引数は ITEM")
(defvar navi2ch-bm-get-article-function nil
  "スレを得る関数。引数は ITEM")
(defvar navi2ch-bm-exit-function nil)

;; stub functions
;; set by navi2ch-bm-setup
(defun navi2ch-bm-get-property-internal (point))
(defun navi2ch-bm-set-property-internal (begin end item))
(defun navi2ch-bm-get-board-internal (item))
(defun navi2ch-bm-get-article-internal (item))
(defun navi2ch-bm-exit-internal ())

(defvar navi2ch-bm-fetched-article-list nil)
(defvar navi2ch-bm-board-type-alist nil)

(defvar navi2ch-bm-state-char-table
  (navi2ch-alist-to-hash
   '((view   . ?V)
     (cache  . ?C)
     (update . ?U)
     (nil    . ? ))
   :test 'eq))
  

(eval-and-compile
  (let ((state-list '(view cache update nil))
	(update-list '(nil new updated seen)))
    (let ((func (lambda (f)
		  (navi2ch-alist-to-hash
		   (mapcar (lambda (state)
			     (cons state
				   (navi2ch-alist-to-hash
				    (mapcar (lambda (update)
					      (cons update
						    (funcall f state update)))
					    update-list)
				    :test 'eq)))
			   state-list)
		   :test 'eq))))
      (defconst navi2ch-bm-state-face-table
	(funcall func
		 (lambda (state update)
		   (intern (format "navi2ch-bm%s-%s-face"
				   (if update
				       (format "-%s" update)
				     "")
				   (or state 'unread))))))
      (defconst navi2ch-bm-state-mark-face-table
	(funcall func
		 (lambda (state update)
		   (intern (format "navi2ch-bm%s-mark-face"
				   (if update
				       (format "-%s" update)
				     "")))))))))

(defconst navi2ch-bm-updated-mark-table
  (navi2ch-alist-to-hash '((new     . ?%)
			   (updated . ?+)
			   (seen    . ?=)
			   (nil     . ? ))
			 :test 'eq))

(defvar navi2ch-bm-move-downward t)

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-bm-save-info)
(add-hook 'navi2ch-load-status-hook 'navi2ch-bm-load-info)

(defmacro navi2ch-bm-set-func (sym val)
  `(let ((val-str (symbol-name ',val))
         (sym-str (symbol-name ,sym))
         func-str)
     (when (string-match "navi2ch-bm-\\(.+\\)" val-str)
       (setq func-str (format "%s-%s"
			      sym-str (match-string 1 val-str)))
       (set (intern (concat val-str "-function")) (intern func-str))
       (fset (intern (concat val-str "-internal")) (intern func-str)))))

(defun navi2ch-bm-setup (prefix)
  (navi2ch-bm-set-func prefix navi2ch-bm-get-property)
  (navi2ch-bm-set-func prefix navi2ch-bm-set-property)
  (navi2ch-bm-set-func prefix navi2ch-bm-get-board)
  (navi2ch-bm-set-func prefix navi2ch-bm-get-article)
  ;; (navi2ch-bm-set-func prefix navi2ch-bm-get-subject)
  (navi2ch-bm-set-func prefix navi2ch-bm-exit)
  (setq navi2ch-bm-move-downward t))

(defun navi2ch-bm-make-menu-spec (title menu-spec)
  "タイトルが TITLE で 内容が `navi2ch-bm-mode-menu-spec' と MENU-SPEC
を繋げたメニューを作る。"
  (append (list title)
	  navi2ch-bm-mode-menu-spec
	  '("----")
	  menu-spec))

;; (defvar navi2ch-list-navi2ch-category-alist nil) ; コンパイルを通す為

(defun navi2ch-bm-regist-board (type open-func &optional board)
  "TYPE な板を開く関数 OPEN-FUNC を `navi2ch-bm-board-type-alist' に登
録する。また、同時に BOARD を `navi2ch-list-navi2ch-category-alist' に
登録する。"
  (setq navi2ch-bm-board-type-alist
	(navi2ch-put-alist type open-func
			   navi2ch-bm-board-type-alist))
  (when board
    (add-to-list 'navi2ch-list-navi2ch-category-alist board)))

(defun navi2ch-bm-select-board (board &optional force)
  (let ((buf (get-buffer-create navi2ch-board-buffer-name))
	(type (cdr (assq 'type board))))
    (set-buffer buf)
    (funcall (cdr (assq type navi2ch-bm-board-type-alist))
	     board force)
    (switch-to-buffer buf))
  (run-hooks 'navi2ch-bm-select-board-hook)
  (navi2ch-set-mode-line-identification))

(defun navi2ch-bm-set-property (begin end item state &optional updated mark)
  (navi2ch-bm-set-property-internal begin end item)
  (let ((updated (or updated 
		     (get-text-property begin 'navi2ch-bm-updated)))
	(face-table (if mark
			navi2ch-bm-state-mark-face-table
		      navi2ch-bm-state-face-table)))
    (add-text-properties begin end
			 (list 'navi2ch-bm-updated updated
			       'navi2ch-bm-state state
			       'navi2ch-bm-mark mark
			       'mouse-face navi2ch-bm-mouse-face
			       'face
			       (gethash updated
					(gethash state face-table))))))

(defun navi2ch-bm-get-state-from-article (board article)
  (cond ((navi2ch-board-from-file-p board)
	 (cond ((get-buffer (navi2ch-article-get-buffer-name
			     board article))
		'view)
	       ((file-exists-p (navi2ch-article-get-file-name board article))
		'cache)
	       (t nil)))
	((navi2ch-bm-fetched-article-p board article)
	 'update)
	(t
	 (navi2ch-article-check-cached board article))))

(defun navi2ch-bm-format-subject
  (number updated-char state-char subject other)
  (format (concat "%" (number-to-string navi2ch-bm-number-width)
		  "d %c%c %s%s%s\n")
	  number updated-char state-char subject
	  (make-string (max (- navi2ch-bm-subject-width
			       (string-width subject))
			    1)
		       ? )
	  other))

(defun navi2ch-bm-insert-subject (item number subject other
				       &optional updated)
  (let* ((article (navi2ch-bm-get-article-internal item))
	 (board (navi2ch-bm-get-board-internal item))
	 (point (point))
	 (state (navi2ch-bm-get-state-from-article board article))
	 (string (navi2ch-bm-format-subject
		  number
		  (gethash updated navi2ch-bm-updated-mark-table)
		  (gethash state navi2ch-bm-state-char-table)
		  (or subject navi2ch-bm-empty-subject)
		  other)))
    ;; for contrib/izonmoji-mode.el
    (navi2ch-ifxemacs
	(insert string)
      (let ((buffer-display-table (if (and (boundp 'izonmoji-mode)
					   izonmoji-mode)
				      nil
				    buffer-display-table)))
	(insert string)))
    (save-excursion
      (goto-char point)
      (set-text-properties (navi2ch-line-beginning-position)
			   (1+ (navi2ch-line-end-position))
			   nil)
      (navi2ch-bm-set-property (navi2ch-line-beginning-position)
			       (navi2ch-line-end-position)
			       item state updated))))

(defun navi2ch-bm-exit ()
  (interactive)
  (dolist (x (navi2ch-article-buffer-list))
    (when x
      (delete-windows-on x)))
  (navi2ch-bm-exit-internal)
  (run-hooks 'navi2ch-bm-exit-hook)
  (when (get-buffer navi2ch-board-buffer-name)
    (delete-windows-on navi2ch-board-buffer-name)
    (bury-buffer navi2ch-board-buffer-name))
  (when navi2ch-list-buffer-name
    (let ((win (get-buffer-window navi2ch-list-buffer-name)))
      (if win
	  (select-window win)
	(navi2ch-list)))))

;;; goto-*-column
(defsubst navi2ch-bm-goto-updated-mark-column ()
  (beginning-of-line)
  (when (looking-at " *[0-9]+ ")
    (goto-char (match-end 0))))

(defsubst navi2ch-bm-goto-state-column ()
  (when (navi2ch-bm-goto-updated-mark-column)
    (forward-char 1)))

(defsubst navi2ch-bm-goto-mark-column ()
  (when (navi2ch-bm-goto-updated-mark-column)
    (forward-char 2)))

(defun navi2ch-bm-goto-other-column ()
  (let ((sbj (cdr
              (assq 'subject
		    (navi2ch-bm-get-article-internal
                     (navi2ch-bm-get-property-internal (point)))))))
    (navi2ch-bm-goto-mark-column)
    (forward-char 1)
    (unless sbj (setq sbj navi2ch-bm-empty-subject))
    (when (and (not (string= sbj ""))
               (search-forward sbj nil t))
      (goto-char (match-end 0)))
    (skip-chars-forward " ")))


(defun navi2ch-bm-insert-state (item state &optional updated)
  ;; (setq article (navi2ch-put-alist 'cache 'view article))
  (let ((buffer-read-only nil))
    (save-excursion
      (navi2ch-bm-goto-state-column)
      (backward-char 1)
      (delete-char 2)
      (insert (gethash updated navi2ch-bm-updated-mark-table)
	      (gethash state navi2ch-bm-state-char-table))
      (navi2ch-bm-set-property (navi2ch-line-beginning-position)
			       (navi2ch-line-end-position)
			       item state updated))))

(defsubst navi2ch-bm-get-state (&optional point)
  "その位置の state を調べる。"
  (get-text-property (or point (point)) 'navi2ch-bm-state))

(defsubst navi2ch-bm-get-updated-mark (&optional point)
  "その位置の updated-mark を調べる。"
  (get-text-property (or point (point)) 'navi2ch-bm-updated))

(defun navi2ch-bm-select-article (&optional max-line)
  (interactive "P")
  (let* ((item (navi2ch-bm-get-property-internal (point)))
         (article (navi2ch-bm-get-article-internal item))
         (board (navi2ch-bm-get-board-internal item))
         (buf (current-buffer))
	 (window-configuration (current-window-configuration)))
    (unwind-protect
	(if article
	    (progn
	      (navi2ch-split-window 'article)
	      (let (state)
		(setq state
		      (if (navi2ch-board-from-file-p board)
			  (navi2ch-article-view-article-from-file
			   (navi2ch-article-get-file-name board article))
			(navi2ch-article-view-article
			 board article nil nil max-line)))
		(save-excursion
		  (set-buffer buf)
		  (when (or state
			    (navi2ch-bm-fetched-article-p board article)
			    (eq (navi2ch-bm-get-state) 'view))
		    (navi2ch-bm-remove-fetched-article board article)
		    (if (eq major-mode 'navi2ch-board-mode)
			(navi2ch-bm-insert-state item 'view 'seen)
		      (navi2ch-bm-insert-state item 'view)))))
	      (setq window-configuration (current-window-configuration)))
	  (message "Can't select this line!"))
      (set-window-configuration window-configuration))))

(defun navi2ch-bm-show-url ()
  "板の url を表示して、その url を見るか kill ring にコピーする。"
  (interactive)
  (let* ((board (navi2ch-bm-get-board-internal
		 (navi2ch-bm-get-property-internal (point))))
	 (url (navi2ch-board-to-url board)))
    (if (not url)
	(message "Can't select this line!")
      (let ((char (navi2ch-read-char-with-retry
		   (format "c)opy v)iew t)itle? URL: %s: " url)
		   nil '(?c ?v ?t))))
	(if (eq char ?t)
	    (navi2ch-bm-copy-title board)
	  (setq url (navi2ch-bm-show-url-subr board))
	  (cond ((not url)
		 (message "Can't select this line!"))
		((eq char ?c)
		 (kill-new url)
		 (message "Copy: %s" url))
		((eq char ?v)
		 (navi2ch-browse-url-internal url)
		 (message "View: %s" url))))))))

(defun navi2ch-bm-show-url-subr (board)
  "メニューを表示して、url を得る。"
  (let ((char (navi2ch-read-char-with-retry
	       (format "b)oard a)rticle l)ast%d: "
		       navi2ch-article-show-url-number)
	       nil '(?b ?a ?l)))
	(article (navi2ch-bm-get-article-internal
		  (navi2ch-bm-get-property-internal (point)))))
    (cond ((eq char ?b) (navi2ch-board-to-url board))
	  ((eq char ?a) (when article
			  (navi2ch-article-to-url board article)))
	  ((eq char ?l) (let ((l (format "l%d" navi2ch-article-show-url-number)))
			  (when article
			    (navi2ch-article-to-url board article l l)))))))

(defun navi2ch-bm-copy-title (board)
  "メニューを表示して、タイトルを得る。"
  (navi2ch-article-copy-title board
			      (navi2ch-bm-get-article-internal
			       (navi2ch-bm-get-property-internal
				(point)))))

(defun navi2ch-bm-display-article (&optional max-line)
  (interactive "P")
  (let ((win (selected-window)))
    (navi2ch-bm-select-article max-line)
    (select-window win)))

(defun navi2ch-bm-remember-fetched-article (board article)
  (let* ((uri (navi2ch-board-get-uri board))
	 (list (assoc uri navi2ch-bm-fetched-article-list))
	 (artid (cdr (assq 'artid article))))
    (if list
	(unless (member artid (cdr list))
	  (push artid (cdr list)))
      (push (list uri artid) navi2ch-bm-fetched-article-list))))

(defun navi2ch-bm-fetched-article-p (board article)
  (member (cdr (assq 'artid article))
	  (cdr (assoc (navi2ch-board-get-uri board)
		      navi2ch-bm-fetched-article-list))))

(defun navi2ch-bm-remove-fetched-article (board article)
  (let* ((uri (navi2ch-board-get-uri board))
	 (list (assoc uri navi2ch-bm-fetched-article-list))
	 (artid (cdr (assq 'artid article))))
    (when (member artid list)
      (setcdr list (delete artid (cdr list)))
      (unless (cdr list)
	(setq navi2ch-bm-fetched-article-list
	      (delq list navi2ch-bm-fetched-article-list))))))

(defun navi2ch-bm-fetch-article (&optional force)
  (interactive "P")
  (let* ((item (navi2ch-bm-get-property-internal (point)))
         (board (navi2ch-bm-get-board-internal item))
         (article (navi2ch-bm-get-article-internal item))
         state)
    (if (and article
	     (not (navi2ch-board-from-file-p board)))
	(let (summary artid element seen)
	  (when (and navi2ch-board-check-article-update-suppression-length
		     (not (navi2ch-bm-fetched-article-p board article)))
	    (setq summary (navi2ch-article-load-article-summary board))
	    (setq artid (cdr (assq 'artid article)))
	    (setq element (cdr (assoc artid summary)))
	    (setq seen (or (navi2ch-article-summary-element-seen element)
			   (cdr (assoc artid navi2ch-board-last-seen-alist))
			   0)))
	  (setq state (navi2ch-article-fetch-article board article force))
	  (when state
	    (let ((state-mark 'update)
		  (updated-mark (navi2ch-bm-get-updated-mark)))
	      (when seen
		(setq seen
		      (and (catch 'break
			     (<= (string-to-number
				  (or (cdr (assoc artid navi2ch-board-subject-alist))
				      (throw 'break t)))
				 (+ seen navi2ch-board-check-article-update-suppression-length)))
			   (navi2ch-article-check-message-suppression
			    board
			    article
			    (1+ seen)
			    (+ seen navi2ch-board-check-article-update-suppression-length)))))
	      (if seen
		  (progn
		    (navi2ch-article-summary-element-set-seen element seen)
		    (navi2ch-article-save-article-summary board summary)
		    (setq state-mark (navi2ch-bm-get-state))
		    (when (memq updated-mark '(new updated))
		      (setq updated-mark 'seen))
		    (message "No updates need seeing"))
		(navi2ch-bm-remember-fetched-article board article))
	      (navi2ch-bm-insert-state item state-mark updated-mark))))
      (message "Can't select this line!"))
    state))

(defun navi2ch-bm-textize-article (&optional dir-or-file buffer)
  (interactive)
  (let* ((navi2ch-article-view-range nil)
	 (navi2ch-article-auto-range nil)
	 window)
    (setq window (selected-window))
    (navi2ch-bm-display-article)
    (select-window (get-buffer-window (navi2ch-article-current-buffer)))
    (when navi2ch-article-view-range
      (setq navi2ch-article-view-range nil)
      (navi2ch-article-redraw))
    (navi2ch-article-textize-article dir-or-file buffer)
    (select-window window)))

(defun navi2ch-bm-select-article-or-scroll (way &optional max-line)
  (let ((article (navi2ch-bm-get-article-internal
		  (navi2ch-bm-get-property-internal (point)))))
    (if (and (navi2ch-article-current-buffer)
             (string= (cdr (assq 'artid article))
                      (save-excursion
                        (set-buffer (navi2ch-article-current-buffer))
                        (cdr (assq 'artid navi2ch-article-current-article))))
             (get-buffer-window (navi2ch-article-current-buffer)))
        (let ((win (selected-window)))
	  (unwind-protect
	      (progn
		(select-window
		 (get-buffer-window (navi2ch-article-current-buffer)))
		(cond
		 ((eq way 'up)
		  (navi2ch-article-scroll-up))
		 ((eq way 'down)
		  (navi2ch-article-scroll-down))))
	    (select-window win)))
      (navi2ch-bm-select-article max-line))))

(defun navi2ch-bm-select-article-or-scroll-up (&optional max-line)
  (interactive "P")
  (navi2ch-bm-select-article-or-scroll 'up max-line))

(defun navi2ch-bm-select-article-or-scroll-down (&optional max-line)
  (interactive "P")
  (navi2ch-bm-select-article-or-scroll 'down max-line))

(defun navi2ch-bm-mouse-select (e)
  (interactive "e")
  (mouse-set-point e)
  (save-excursion
    (beginning-of-line)
    (navi2ch-bm-select-article)))

(defun navi2ch-bm-goto-board ()
  (interactive)
  (navi2ch-list-goto-board
   (navi2ch-bm-get-board-internal
    (navi2ch-bm-get-property-internal (point)))))

(defun navi2ch-bm-renumber ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil)
          (i 1))
      (while (not (eobp))
        (let ((props (text-properties-at (point)))
	      (num-string (format
			   (concat "%" (number-to-string navi2ch-bm-number-width) "d")
			   i)))
          (delete-region (point)
			 (save-excursion
			   (navi2ch-bm-goto-state-column)
			   (- (point) 2)))
          (insert num-string)
	  (set-text-properties (- (point) (length num-string))
			       (point) props)
          (forward-line 1)
          (setq i (1+ i)))))))

(defun navi2ch-bm-view-logo ()
  "その板のロゴを見る。"
  (interactive)
  (let ((board (navi2ch-bm-get-board-internal
		(navi2ch-bm-get-property-internal (point))))
	(board-mode-p (eq major-mode 'navi2ch-board-mode))
	file old-file)
    (unless board-mode-p
      (setq board (navi2ch-board-load-info board)))
    (setq old-file (cdr (assq 'logo board)))
    (if navi2ch-offline
	(setq file old-file)
      (setq file (navi2ch-net-download-logo board))
      (when file
	(setq file (file-name-nondirectory (navi2ch-net-download-logo board)))
	(when (and old-file navi2ch-board-delete-old-logo
		   (not (string-equal file old-file)))
	  (delete-file (navi2ch-board-get-file-name board old-file)))
	(if board-mode-p
	    (setq navi2ch-board-current-board board)
	  (navi2ch-board-save-info board))))
    (if file
	(apply 'start-process "navi2ch view logo"
	       nil navi2ch-board-view-logo-program
	       (append navi2ch-board-view-logo-args
		       (list (navi2ch-board-get-file-name board file))))
      (message "Can't find logo file"))))

(defun navi2ch-bm-add-global-bookmark (&optional bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Bookmark ID: ")))
  (let* ((item (navi2ch-bm-get-property-internal (point)))
	 (board (navi2ch-bm-get-board-internal item))
	 (article (navi2ch-bm-get-article-internal item)))
    (if item
	(navi2ch-bookmark-add
	 bookmark-id
	 board
	 article)
      (message "Can't select this line!"))))

;;; move
(defun navi2ch-bm-forward-line (&optional n)
  (interactive "p")
  (let ((ret (forward-line n)))
    (when (eobp)
      (forward-line -1)
      (setq ret (1+ ret)))
    ret))

(defun navi2ch-bm-next-line (num)
  (interactive "p")
  (unless (zerop (navi2ch-bm-forward-line num))
    (message "No more articles"))
  (setq navi2ch-bm-move-downward t))

(defun navi2ch-bm-previous-line (num)
  (interactive "p")
  (unless (zerop (navi2ch-bm-forward-line (- num)))
    (message "No more articles"))
  (setq navi2ch-bm-move-downward nil))

;;; mark
(defun navi2ch-bm-mark-subr (mark &optional arg interactive)
  "mark する。
INTERACTIVE が non-nil なら mark したあと移動する。
ARG が non-nil なら移動方向を逆にする。"
  (let ((item (navi2ch-bm-get-property-internal (point)))
	(state (navi2ch-bm-get-state (point)))
	(table (and mark navi2ch-bm-state-mark-face-table)))
    (when item
      (let ((buffer-read-only nil)
	    (pos (point)))
	(navi2ch-bm-goto-mark-column)
	(delete-char 1)
	(insert (if mark ?* ? ))
	(navi2ch-bm-set-property (navi2ch-line-beginning-position)
				 (navi2ch-line-end-position)
				 item state nil table)
	(goto-char pos)))
    (when (and navi2ch-bm-mark-and-move interactive)
      (let (downward)
	(cond ((eq navi2ch-bm-mark-and-move 'follow)
	       (setq downward
		     (if arg
			 (not navi2ch-bm-move-downward)
		       navi2ch-bm-move-downward)))
	      ((eq navi2ch-bm-mark-and-move t)
	       (setq downward (not arg))))
	(navi2ch-bm-forward-line (if downward 1 -1))))))

(defun navi2ch-bm-mark (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-subr t arg (interactive-p)))

(defun navi2ch-bm-unmark (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-subr nil arg (interactive-p)))

(defun navi2ch-bm-exec-subr (func &rest args)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (navi2ch-bm-goto-mark-column)
      (if (looking-at "\\*")
	  (progn
	    (condition-case nil
		(save-excursion
		  (navi2ch-bm-unmark)
		  (apply func args))
	      (navi2ch-update-failed nil))
	    (sit-for 0)
	    (discard-input))
	(forward-line)))))

(defsubst navi2ch-bm-display-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-display-article))

(defun navi2ch-bm-fetch-mark-article (&optional force)
  (interactive "P")
  (unless navi2ch-offline
    (navi2ch-bm-exec-subr #'navi2ch-bm-fetch-article force)))

(defun navi2ch-bm-textize-mark-article (directory &optional file)
  (interactive "DDirectory: \nFList file: ")
  (let ((buffer (get-buffer-create (make-temp-name "*navi2ch "))))
    (navi2ch-bm-exec-subr 'navi2ch-bm-textize-article directory buffer)
    (save-excursion
      (set-buffer buffer)
      (when file
	(navi2ch-write-region (point-min) (point-max) file)))
    (kill-buffer buffer)))

(defun navi2ch-bm-add-global-bookmark-mark-article (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Bookmark ID: ")))
  (navi2ch-bm-exec-subr 'navi2ch-bm-add-global-bookmark bookmark-id))


;; add marked ones to the board bookmark
(defun navi2ch-bm-add-bookmark-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-board-add-bookmark))

(defun navi2ch-bm-mark-region-subr (begin end mark)
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (not (eobp))
        (navi2ch-bm-mark-subr mark)
        (forward-line)))))

(defun navi2ch-bm-mark-region (begin end &optional arg)
  (interactive "r\nP")
  (navi2ch-bm-mark-region-subr (save-excursion (goto-char begin)
					       (beginning-of-line)
					       (point))
			       (save-excursion (goto-char (max (1- end)
							       (point-min)))
					       (end-of-line)
					       (point))
			       (not arg)))

(defun navi2ch-bm-fetch-maybe-new-articles ()
  "更新されている可能性のあるスレを fetch する。"
  (interactive)
  (unless navi2ch-offline
    (navi2ch-bm-mark-states "[^=]")
    (sit-for 0)
    (navi2ch-bookmark-fetch-mark-article)))

(defun navi2ch-bm-mark-all (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-region (point-min) (point-max) arg))

(defun navi2ch-bm-mark-marks (mark &optional arg)
  (interactive "cInput mark: \nP")
  (navi2ch-bm-mark-states
   (format ".%c" (upcase mark))
   arg))

(defun navi2ch-bm-mark-states (regexp &optional arg)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (navi2ch-bm-goto-updated-mark-column)
      (when (looking-at regexp)
	(navi2ch-bm-mark-subr (not arg)))
      (forward-line))))

;; mark by regexp query
(defun navi2ch-bm-mark-by-query (query &optional arg)
  (interactive "MQuery (regexp): ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward query nil t)
      (navi2ch-bm-mark-subr (not arg)))))

;;; sort
(defun navi2ch-bm-sort-subr (rev start-key-fun end-key-fun)
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (sort-subr rev 'forward-line 'end-of-line
                 start-key-fun end-key-fun))))

(defun navi2ch-bm-sort-by-number (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (beginning-of-line)
     (save-match-data
       (if (looking-at "^ *\\([0-9]+\\)")
	   (string-to-number
	    (buffer-substring (match-beginning 1) (match-end 1)))
	 ;; not a number
	 -1)))
   nil))

(defun navi2ch-bm-sort-by-state (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (navi2ch-bm-goto-state-column)
     (backward-char)
     (or (cdr (assoc (buffer-substring (point) (+ (point) 2))
		     navi2ch-bm-sort-by-state-order))
	 ;; 未知の状態。
	 1000))
   nil))

(defun navi2ch-bm-sort-by-subject (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (navi2ch-bm-goto-mark-column)
     (forward-char 1))
   'navi2ch-bm-goto-other-column))

(defun navi2ch-bm-sort-by-other (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (navi2ch-bm-goto-other-column)
     nil) ; end-key-fun を呼ばせるには nil が欲しいらしい。はまった(泣)。
   'end-of-line))

(defun navi2ch-bm-sort-by-date (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   (not rev)
   (lambda ()
     (string-to-number
      (cdr (assq 'artid
		 (navi2ch-bm-get-article-internal
		  (navi2ch-bm-get-property-internal (point)))))))
   nil))

(defun navi2ch-bm-sort (&optional arg)
  (interactive "P")
  (let ((ch (navi2ch-read-char-with-retry
	     "Sort by n)umber s)tate t)itle o)ther d)ate? "
	     nil '(?n ?s ?t ?o ?d))))
    (message "Sorting...")
    (funcall
     (cond ((eq ch ?n) 'navi2ch-bm-sort-by-number)
           ((eq ch ?s) 'navi2ch-bm-sort-by-state)
           ((eq ch ?t) 'navi2ch-bm-sort-by-subject)
           ((eq ch ?o) 'navi2ch-bm-sort-by-other)
	   ((eq ch ?d) 'navi2ch-bm-sort-by-date))
     arg)
    (message "Sorting...done")))

;;; search
(defun navi2ch-bm-search-current-board-subject ()
  (interactive)
  (navi2ch-search-subject-subr
   (list (navi2ch-bm-get-board-internal
	  (navi2ch-bm-get-property-internal (point))))))

(defun navi2ch-bm-search-current-board-article ()
  (interactive)
  (navi2ch-search-article-subr
   (list (navi2ch-bm-get-board-internal
	  (navi2ch-bm-get-property-internal (point))))))

(defun navi2ch-bm-search-current-board-cache ()
  (interactive)
  (navi2ch-search-cache-subr
   (list (navi2ch-bm-get-board-internal
	  (navi2ch-bm-get-property-internal (point))))))

(defun navi2ch-bm-search-current-board-orphan ()
  (interactive)
  (navi2ch-search-orphan-subr
   (list (navi2ch-bm-get-board-internal
	  (navi2ch-bm-get-property-internal (point))))))

(defun navi2ch-bm-search ()
  (interactive)
  (let ((ch (navi2ch-read-char-with-retry
	     "Search for: s)ubject a)rticle c)ache o)rphan: "
	     nil '(?s ?a ?c ?o)))
	(ch2 (navi2ch-read-char-with-retry
	      "Search from: b)oard a)ll: " nil '(?b ?a))))
    (cond ((eq ch ?s)
           (cond ((eq ch2 ?b) (navi2ch-bm-search-current-board-subject))
                 ((eq ch2 ?a) (navi2ch-search-all-subject))))
          ((eq ch ?a)
           (cond ((eq ch2 ?b) (navi2ch-bm-search-current-board-article))
                 ((eq ch2 ?a) (navi2ch-search-all-article))))
	  ((eq ch ?c)
	   (cond ((eq ch2 ?b) (navi2ch-bm-search-current-board-cache))
		 ((eq ch2 ?a) (navi2ch-search-all-cache))))
	  ((eq ch ?o)
	   (cond ((eq ch2 ?b) (navi2ch-bm-search-current-board-orphan))
		 ((eq ch2 ?a) (navi2ch-search-all-orphan)))))))

;;; save and load info
(defun navi2ch-bm-save-info ()
  (navi2ch-save-info navi2ch-bm-fetched-info-file
		     navi2ch-bm-fetched-article-list
		     t))

(defun navi2ch-bm-load-info ()
  (setq navi2ch-bm-fetched-article-list
        (navi2ch-load-info navi2ch-bm-fetched-info-file)))

(defun navi2ch-bm-update-article (board article &optional state updated)
  "板バッファのうち、BOARD と ARTICLE にマッチする行を更新する。"
  (let ((buffer (get-buffer navi2ch-board-buffer-name)))
    (when buffer
      (with-current-buffer buffer
	(let ((buffer-read-only nil))
	  (save-excursion
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let* ((item (navi2ch-bm-get-property-internal (point)))
		     (item-article (navi2ch-bm-get-article-internal item))
		     (item-board (navi2ch-bm-get-board-internal item)))
		(when (and (equal (cdr (assq 'id board))
				  (cdr (assq 'id item-board)))
			   (equal (cdr (assq 'artid article))
				  (cdr (assq 'artid item-article))))
		  (let ((state (or state
				   (navi2ch-bm-get-state-from-article
				    board article)))
			(updated (or updated
				     (navi2ch-bm-get-updated-mark))))
		    (navi2ch-bm-insert-state item state updated)
		    (navi2ch-bm-set-property (navi2ch-line-beginning-position)
					     (navi2ch-line-end-position)
					     item state updated))))
	      (forward-line))))))))

(defun navi2ch-bm-remove-article-subr (board articles)
  "BOARD と ARTICLES で指定されるスレの情報を消す。
ARTILCES が alist の場合はそのスレのみを、alist の list の場合は指定さ
れるすべてのスレを対象にする。"
  (let ((summary (navi2ch-article-load-article-summary board)))
    (setq articles
	  (cond ((cdr (assq 'artid articles)) ; スレ alist
		 (list articles))
		((cdr (assq 'artid (car articles))) ; スレ alist の list
		 articles)))
    (dolist (article articles)
      (let ((artid (cdr (assq 'artid article)))
	    (buffer (get-buffer (navi2ch-article-get-buffer-name board
								 article)))
	    (info-file (navi2ch-article-get-info-file-name board article))
	    elt)
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer))
	(dolist (file (list info-file
			    (navi2ch-make-backup-file-name
			     info-file)
			    (navi2ch-article-get-file-name board article)
			    (navi2ch-article-get-message-filter-cache-file-name
			     board article)))
	  (condition-case nil
	      (if (file-exists-p file)
		  (delete-file file))
	    (file-error nil))
	  (navi2ch-cache-remove file navi2ch-info-cache))
	(navi2ch-bm-remove-fetched-article board article)
	(while (setq elt (assoc artid summary))
	  (setq summary (delq elt summary))))
      (navi2ch-bm-update-article board article))
    (navi2ch-article-save-article-summary board summary)))

(defun navi2ch-bm-remove-article ()
  (interactive)
  (let* ((item (navi2ch-bm-get-property-internal (point)))
	 (article (navi2ch-bm-get-article-internal item))
	 (board (navi2ch-bm-get-board-internal item)))
    (when (and board article)
      (navi2ch-bm-remove-article-subr board article))))

(defun navi2ch-bm-remove-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-remove-article))

(defun navi2ch-bm-save-dat-file ()
  (interactive)
  (let* ((item (navi2ch-bm-get-property-internal (point)))
	 (article (navi2ch-bm-get-article-internal item))
	 (board (navi2ch-bm-get-board-internal item)))
    (when (and board article)
      (navi2ch-article-save-dat-file board article))))

(defun navi2ch-bm-url-at-point (point)
  "POINT の下のリンクを指す URL を得る。"
  (let ((board (navi2ch-bm-get-board-internal
		(navi2ch-bm-get-property-internal point)))
	(article (navi2ch-bm-get-article-internal
		  (navi2ch-bm-get-property-internal point))))
    (navi2ch-article-to-url board article)))

(run-hooks 'navi2ch-board-misc-load-hook)
;;; navi2ch-board-misc.el ends here
