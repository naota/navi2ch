;;; navi2ch-board-misc.el --- Miscellaneous Functions for Navi2ch Board Mode 

;; Copyright (C) 2001 by 2ちゃんねる

;; Author: (not 1)
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

;;; Code:
(provide 'navi2ch-board-misc)

(eval-when-compile (require 'cl))

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
    (define-key map [del] 'navi2ch-bm-select-article-or-scroll-down)
    (define-key map [backspace] 'navi2ch-bm-select-article-or-scroll-down)
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

    ;; mark command
    (define-key map "*" 'navi2ch-bm-mark)
    (define-key map "u" 'navi2ch-bm-unmark)
    (define-key map "mr" 'navi2ch-bm-mark-region)
    (define-key map "ma" 'navi2ch-bm-mark-all)
    (define-key map "mA" 'navi2ch-bm-add-global-bookmark-mark-article)
    (define-key map "mo" 'navi2ch-bm-display-mark-article)
    (define-key map "mi" 'navi2ch-bm-fetch-mark-article)
    (define-key map "me" 'navi2ch-bm-textize-mark-article)
    (define-key map "mm" 'navi2ch-bm-mark-marks)
    (setq navi2ch-bm-mode-map map)))

(defvar navi2ch-bm-mode-menu-spec
  '(["Toggle offline" navi2ch-toggle-offline]
    ["Exit" navi2ch-bm-exit]
    ["Sort" navi2ch-bm-sort]
    ["Search" navi2ch-bm-search])
  "Menu の元")

(defvar navi2ch-board-buffer-name "*navi2ch board*")

(defvar navi2ch-bm-get-property-function nil
  "その位置の text-property を得る関数。引数は POINT")
(defvar navi2ch-bm-set-property-function nil
  "text-property を設定する関数。引数は BEGIN END ITEM")
(defvar navi2ch-bm-get-board-function nil
  "板を得る関数。引数は ITEM")
(defvar navi2ch-bm-get-article-function nil
  "スレを得る関数。引数は ITEM")
(defvar navi2ch-bm-exit-function nil)

(fset 'navi2ch-bm-get-property-internal nil)
(fset 'navi2ch-bm-set-property-internal nil)
(fset 'navi2ch-bm-get-board-internal nil)
(fset 'navi2ch-bm-get-article-internal nil)
(fset 'navi2ch-bm-exit-internal nil)

(defvar navi2ch-bm-fetched-article-list nil)
(defvar navi2ch-bm-board-type-alist nil)

(defvar navi2ch-bm-state-alist
  '((view "V"
	  navi2ch-bm-view-face
	  navi2ch-bm-updated-view-face
	  navi2ch-bm-seen-view-face)
    (cache "C"
	   navi2ch-bm-cache-face
	   navi2ch-bm-updated-cache-face
	   navi2ch-bm-seen-cache-face)
    (update "U"
	    navi2ch-bm-update-face
	    navi2ch-bm-updated-update-face
	    navi2ch-bm-seen-update-face)
    (nil " "
	 navi2ch-bm-unread-face
	 navi2ch-bm-updated-unread-face
	 navi2ch-bm-seen-unread-face)
    (mark " "
	  navi2ch-bm-mark-face
	  navi2ch-bm-updated-mark-face
	  navi2ch-bm-seen-mark-face)))

(defvar navi2ch-bm-updated-mark-alist
  '((updated . "+")
    (seen . "=")
    (nil . " ")))

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
    (setq navi2ch-list-navi2ch-category-alist
	  (cons board navi2ch-list-navi2ch-category-alist))))

(defun navi2ch-bm-select-board (board &optional force)
  (switch-to-buffer (get-buffer-create navi2ch-board-buffer-name))
  (let ((type (cdr (assq 'type board))))
    (funcall (cdr (assq type navi2ch-bm-board-type-alist))
	     board force))
  (navi2ch-set-mode-line-identification))

(defsubst navi2ch-bm-set-property (begin end item state &optional updated)
  (navi2ch-bm-set-property-internal begin end item)
  (setq updated (or updated (get-text-property (1+ begin) 'updated)))
  (put-text-property begin end 'updated updated)
  (put-text-property begin end 'mouse-face 'highlight)
  (put-text-property begin end 'face (nth (cond ((eq updated 'updated) 3)
						((eq updated 'seen) 4)
						((eq updated nil) 2))
					  (assq state
						navi2ch-bm-state-alist))))

(defsubst navi2ch-bm-insert-subject (item number subject other
					  &optional updated)
  (let* ((article (navi2ch-bm-get-article-internal item))
	 (board (navi2ch-bm-get-board-internal item))
	 (point (point))
	 (state (if (navi2ch-bm-fetched-article-p board article)
		    'update
		  (navi2ch-article-check-cached board article))))
    (unless subject (setq subject navi2ch-bm-empty-subject))
    (insert (format "%3d %s%s %s%s%s\n"
                    number
		    (cdr (assq updated navi2ch-bm-updated-mark-alist))
                    (cadr (assq state navi2ch-bm-state-alist))
                    subject
                    (make-string (max (- navi2ch-bm-subject-width
                                         (string-width subject))
                                      1)
                                 ? )
                    other))
    (navi2ch-bm-set-property point (1- (point)) item state updated)))

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
	(switch-to-buffer navi2ch-list-buffer-name)))))

(defun navi2ch-bm-goto-state-column ()
  (beginning-of-line)
  (forward-char 5))

(defun navi2ch-bm-insert-state (item state &optional updated)
  ;; (setq article (navi2ch-put-alist 'cache 'view article))
  (navi2ch-bm-goto-state-column)
  (backward-char 1)
  (delete-char 2)
  (insert (cdr (assq updated navi2ch-bm-updated-mark-alist)))
  (insert (cadr (assq state navi2ch-bm-state-alist)))
  (navi2ch-bm-set-property (save-excursion (beginning-of-line) (point))
			   (save-excursion (end-of-line) (point))
			   item state updated))
  
(defun navi2ch-bm-get-state (&optional point)
  "その位置の state を調べる"
  (save-excursion
    (and point (goto-char point))
    (navi2ch-bm-goto-state-column)
    (cdr (assoc (char-to-string (char-after))
		(mapcar (lambda (x)
			  (cons (cadr x) (car x)))
			navi2ch-bm-state-alist)))))

(defun navi2ch-bm-select-article (&optional max-line)
  (interactive "P")
  (let* ((item (navi2ch-bm-get-property-internal (point)))
         (article (navi2ch-bm-get-article-internal item))
         (board (navi2ch-bm-get-board-internal item))
         (buf (current-buffer)))
    (if article
        (progn
          (dolist (x (navi2ch-article-buffer-list))
            (when x
              (delete-windows-on x)))
	  (when navi2ch-bm-stay-board-window
	    (condition-case nil
		(enlarge-window (frame-height))
	      (error nil))
	    (split-window-vertically navi2ch-board-window-height)
	    (other-window 1))
          (let (state)
            (setq state (navi2ch-article-view-article
                         board article nil nil max-line))
            (save-excursion
              (set-buffer buf)
              (let ((buffer-read-only nil))
                (when (or state
			  (navi2ch-bm-fetched-article-p board article)
			  (eq (navi2ch-bm-get-state) 'view))
		  (navi2ch-bm-remove-fetched-article board article)
		  (if (eq major-mode 'navi2ch-board-mode)
		      (navi2ch-bm-insert-state item 'view 'seen)
		    (navi2ch-bm-insert-state item 'view)))))))
      (message "can't select this line!"))))

(defun navi2ch-bm-show-url ()
  "板のurl を表示して、その url を見るか kill ring にコピーする"
  (interactive)
  (let* ((board (navi2ch-bm-get-board-internal
		 (navi2ch-bm-get-property-internal (point))))
	 (url (navi2ch-board-to-url board)))
    (message "c)opy v)iew t)itle? URL: %s" url)
    (let ((char (read-char)))
      (if (eq char ?t) (navi2ch-bm-copy-title board)
	(funcall (cond ((eq char ?c)
			'(lambda (x) (message "copy: %s" (kill-new x))))
		       ((eq char ?v)
			'navi2ch-browse-url))
		 (navi2ch-bm-show-url-subr board))))))

(defun navi2ch-bm-show-url-subr (board)
  "メニューを表示して、url を得る"
  (message "b)oard a)rticle")
  (let ((char (read-char)))
    (cond ((eq char ?b) (navi2ch-board-to-url board))
	  ((eq char ?a)
	   (navi2ch-article-to-url
	    board
	    (navi2ch-bm-get-article-internal
	     (navi2ch-bm-get-property-internal (point))))))))

(defun navi2ch-bm-copy-title (board)
  "メニューを表示して、タイトルを得る"
  (message "b)oard a)rticle")
  (let ((char (read-char)))
    (message "copy: %s"
	     (kill-new
	      (cond ((eq char ?b) (cdr (assq 'name board)))
		    ((eq char ?a)
		     (cdr (assq 'subject
				(navi2ch-bm-get-article-internal
				 (navi2ch-bm-get-property-internal
				  (point)))))))))))

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
  
      
(defun navi2ch-bm-fetch-article (&optional max-line)
  (interactive "P")
  (let* ((item (navi2ch-bm-get-property-internal (point)))
         (board (navi2ch-bm-get-board-internal item))
         (article (navi2ch-bm-get-article-internal item))
         state)
    (if article
	(progn
	  (setq state (navi2ch-article-fetch-article board article))
	  (when state
	    (navi2ch-bm-remember-fetched-article board article)
	    (let ((buffer-read-only nil))
	      (save-excursion
		(navi2ch-bm-insert-state item 'update)))))
      (message "can't select this line!"))))

(defun navi2ch-bm-textize-article (directory &optional buffer)
  (interactive "Ddirectory: ")
  (let* ((navi2ch-article-view-range nil)
	 (navi2ch-article-auto-range nil)
	 window)
    (setq window (selected-window))
    (navi2ch-bm-display-article)
    (select-window (get-buffer-window (navi2ch-article-current-buffer)))
    (when navi2ch-article-view-range
      (setq navi2ch-article-view-range nil)
      (navi2ch-article-redraw))
    (let* ((article navi2ch-article-current-article)
	   (board navi2ch-article-current-board)
	   (id (cdr (assq 'id board)))
	   (file (format "%s_%s.txt" id (cdr (assq 'artid article))))
	   (subject (cdr (assq 'subject article))))
      (and buffer
	   (save-excursion
	     (set-buffer buffer)
	     (goto-char (point-max))
	     (insert (format "<a href=\"%s\">%s</a><br>\n" file subject))))
      (let ((coding-system-for-write navi2ch-coding-system))
	(navi2ch-write-region (point-min) (point-max)
			      (expand-file-name file directory))))
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
          (select-window
           (get-buffer-window (navi2ch-article-current-buffer)))
          (condition-case nil
              (cond
               ((eq way 'up)
                (navi2ch-article-scroll-up))
               ((eq way 'down)
                (navi2ch-article-scroll-down)))
            (error nil))
          (select-window win))
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
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil)
          (i 1))
      (while (not (eobp))
        (let ((props (text-properties-at (point))))
          (delete-char 3)
          (insert (format "%3d" i))
	  (set-text-properties (- (point) 3) (point) props)
          (forward-line 1)
          (setq i (1+ i)))))))

(defun navi2ch-bm-view-logo ()
  "その板のロゴを見る"
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
      (setq file (file-name-nondirectory (navi2ch-net-download-logo board)))
      (when file
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
  (interactive (list (navi2ch-bookmark-read-id "bookmark id: ")))
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
(defun navi2ch-bm-next-line ()
  (interactive)
  (forward-line 1)
  (setq navi2ch-bm-move-downward t))

(defun navi2ch-bm-previous-line ()
  (interactive)
  (forward-line -1)
  (setq navi2ch-bm-move-downward nil))

;;; mark
(defun navi2ch-bm-goto-mark-column ()
  (navi2ch-bm-goto-state-column)
  (forward-char 1))

(defun navi2ch-bm-mark-subr (mark &optional arg interactive)
  "mark する。
INTERACTIVE が non-nil なら mark したあと移動する。
ARG が non-nil なら移動方向を逆にする。"
  (let ((item (navi2ch-bm-get-property-internal (point)))
	(state 'mark)
	(alist (mapcar
		(lambda (x)
		  (cons (cadr x) (car x)))
		navi2ch-bm-state-alist)))
    (when item
      (save-excursion
        (let ((buffer-read-only nil))
	  (when (string= mark " ")
	    (navi2ch-bm-goto-state-column)
	    (setq state (cdr (assoc (char-to-string (char-after (point)))
				    alist))))
          (navi2ch-bm-goto-mark-column)
          (delete-char 1)
          (insert mark)
          (navi2ch-bm-set-property (progn (beginning-of-line) (point))
				   (progn (end-of-line) (point))
				   item state))))
    (when (and navi2ch-bm-mark-and-move interactive)
      (let (downward)
	(cond ((eq navi2ch-bm-mark-and-move 'follow)
	       (setq downward
		     (if arg
			 (not navi2ch-bm-move-downward)
		       navi2ch-bm-move-downward)))
	      ((eq navi2ch-bm-mark-and-move t)
	       (setq downward (not arg))))
	(if downward
	    (navi2ch-bm-next-line)
	  (navi2ch-bm-previous-line))))))

(defun navi2ch-bm-mark (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-subr "*" arg (interactive-p)))

(defun navi2ch-bm-unmark (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-subr " " arg (interactive-p)))

(defun navi2ch-bm-exec-subr (func &rest args)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (navi2ch-bm-goto-mark-column)
      (when (looking-at "\\*")
	(condition-case nil
	    (progn
	      (save-excursion
		(apply func args))
	      (navi2ch-bm-unmark))
	  (navi2ch-update-failed nil))
	(sit-for 0))
      (forward-line))))

(defun navi2ch-bm-display-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-display-article))

(defun navi2ch-bm-fetch-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-fetch-article))

(defun navi2ch-bm-textize-mark-article (directory &optional file)
  (interactive "Ddirectory: \nFlist file: ")
  (let ((buffer (get-buffer-create (make-temp-name "*navi2ch "))))
    (navi2ch-bm-exec-subr 'navi2ch-bm-textize-article directory buffer)
    (save-excursion
      (set-buffer buffer)
      (when file
	(navi2ch-write-region (point-min) (point-max) file)))
    (kill-buffer buffer)))

(defun navi2ch-bm-add-global-bookmark-mark-article (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "bookmark id: ")))
  (navi2ch-bm-exec-subr 'navi2ch-bm-add-global-bookmark bookmark-id))
   
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
			       (save-excursion (goto-char end)
					       (end-of-line)
					       (point))
			       (if arg " " "*")))

(defun navi2ch-bm-mark-all (&optional arg)
  (interactive "P")
  (navi2ch-bm-mark-region (point-min) (point-max) arg))

(defun navi2ch-bm-mark-marks (mark &optional arg)
  (interactive "cinput mark: \nP")
  (save-excursion
    (goto-char (point-min))
    (let ((rep (format "%c" (upcase mark))))
      (while (not (eobp))
        (navi2ch-bm-goto-state-column)
        (when (looking-at rep)
          (navi2ch-bm-mark-subr (if arg " " "*")))
        (forward-line)))))

;;; sort
(defun navi2ch-bm-sort-subr (rev start-key-fun end-key-fun)
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (sort-subr rev 'forward-line 'end-of-line
                 start-key-fun end-key-fun))))

(defun navi2ch-bm-sort-by-number (rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   'beginning-of-line
   'navi2ch-bm-goto-state-column))

(defun navi2ch-bm-sort-by-state (rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   (not rev)
   'navi2ch-bm-goto-state-column
   'forward-char))

(defun navi2ch-bm-goto-other-column ()
  (let ((sbj (cdr
              (assq 'subject
		    (navi2ch-bm-get-article-internal
                     (navi2ch-bm-get-property-internal (point)))))))
    (navi2ch-bm-goto-mark-column)
    (forward-char 1)
    (when (and (not (string= sbj ""))
               (search-forward sbj nil t))
      (goto-char (match-end 0)))
    (skip-chars-forward " ")))

(defun navi2ch-bm-sort-by-subject (rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (navi2ch-bm-goto-mark-column)
     (forward-char 1))
   'navi2ch-bm-goto-other-column))

(defun navi2ch-bm-sort-by-other (rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (navi2ch-bm-goto-other-column)
     nil) ; end-key-fun を呼ばせるには nil が欲しいらしい。はまった(泣)。
   'end-of-line))

(defun navi2ch-bm-sort (&optional arg)
  (interactive "P")
  (message "Sort by n)umber s)tate t)itle o)ther?")
  (let ((ch (read-char)))
    (funcall
     (cond ((eq ch ?n) 'navi2ch-bm-sort-by-number)
           ((eq ch ?s) 'navi2ch-bm-sort-by-state)
           ((eq ch ?t) 'navi2ch-bm-sort-by-subject)
           ((eq ch ?o) 'navi2ch-bm-sort-by-other))
     arg)))

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

(defun navi2ch-bm-search ()
  (interactive)
  (let (ch)
    (message "Search for: s)ubject a)rticle")
    (setq ch (read-char))
    (cond ((eq ch ?s)
           (message "Search from: b)oard a)ll")
           (setq ch (read-char))
           (cond ((eq ch ?b) (navi2ch-bm-search-current-board-subject))
                 ((eq ch ?a) (navi2ch-search-all-subject))))
          ((eq ch ?a)
           (message "Search from: b)oard a)ll")
           (setq ch (read-char))
           (cond ((eq ch ?b) (navi2ch-bm-search-current-board-article))
                 ((eq ch ?a) (navi2ch-search-all-article)))))))
  
;;; save and load info
(defun navi2ch-bm-save-info ()
  (navi2ch-save-info
   navi2ch-bm-fetched-info-file
   navi2ch-bm-fetched-article-list))

(defun navi2ch-bm-load-info ()
  (setq navi2ch-bm-fetched-article-list
	(navi2ch-load-info navi2ch-bm-fetched-info-file)))
   
(run-hooks 'navi2ch-board-misc-load-hook)
;;; navi2ch-board-misc.el ends here
