;;; navi2ch-board-misc.el --- Miscellaneous Functions for Navi2ch Board Mode

;; Copyright (C) 2001 by Navi2ch Project

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

;;; Code:
(provide 'navi2ch-board-misc)
(defvar navi2ch-board-misc-ident
  "$Id$")

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
    (define-key map "R" 'navi2ch-bm-remove-article)

    ;; mark command
    (define-key map "*" 'navi2ch-bm-mark)
    (define-key map "u" 'navi2ch-bm-unmark)
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
(defun navi2ch-bm-get-property-internal (point))
(defun navi2ch-bm-set-property-internal (begin end item))
(defun navi2ch-bm-get-board-internal (item))
(defun navi2ch-bm-get-article-internal (item))
(defun navi2ch-bm-exit-internal ())

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
  (run-hooks 'navi2ch-bm-select-board-hook)
  (navi2ch-set-mode-line-identification))

(defsubst navi2ch-bm-set-property (begin end item state &optional updated)
  (navi2ch-bm-set-property-internal begin end item)
  (setq updated (or updated (get-text-property (1+ begin) 'updated)))
  (put-text-property begin end 'updated updated)
  (put-text-property begin end 'mouse-face navi2ch-bm-mouse-face)
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
	 (state (cond ((navi2ch-board-from-file-p board)
					; navi2ch-article-check-cached で処理すべきか。
		       (cond ((get-buffer (navi2ch-article-get-buffer-name
					   board article))
			      'view)
			     ((file-exists-p
			       (navi2ch-article-get-file-name
				board article))
			      'cache)
			     (t
			      nil)))
		      ((navi2ch-bm-fetched-article-p board article)
		       'update)
		      (t
		       (navi2ch-article-check-cached board article)))))
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
	(navi2ch-list)))))

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
         (buf (current-buffer))
	 (window-configuration (current-window-configuration)))
    (condition-case err
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
		  (let ((buffer-read-only nil))
		    (when (or state
			      (navi2ch-bm-fetched-article-p board article)
			      (eq (navi2ch-bm-get-state) 'view))
		      (navi2ch-bm-remove-fetched-article board article)
		      (if (eq major-mode 'navi2ch-board-mode)
			  (navi2ch-bm-insert-state item 'view 'seen)
			(navi2ch-bm-insert-state item 'view)))))))
	  (message "Can't select this line!"))
      ((error quit)
       (set-window-configuration window-configuration)
       (signal (car err) (cdr err))))))

(defun navi2ch-bm-show-url ()
  "板のurl を表示して、その url を見るか kill ring にコピーする"
  (interactive)
  (let* ((board (navi2ch-bm-get-board-internal
		 (navi2ch-bm-get-property-internal (point))))
	 (url (navi2ch-board-to-url board)))
    (let ((char (navi2ch-read-char-with-retry
		 (format "c)opy v)iew t)itle? URL: %s: " url)
		 nil '(?c ?v ?t))))
      (if (eq char ?t) (navi2ch-bm-copy-title board)
	(funcall (cond ((eq char ?c)
			(lambda (x)
			  (kill-new x)
			  (message "copy: %s" x)))
		       ((eq char ?v)
			(lambda (x)
			  (navi2ch-browse-url x)
			  (message "view: %s" x))))
		 (navi2ch-bm-show-url-subr board))))))

(defun navi2ch-bm-show-url-subr (board)
  "メニューを表示して、url を得る"
  (let ((char (navi2ch-read-char-with-retry "b)oard a)rticle: "
					    nil '(?b ?a))))
    (cond ((eq char ?b) (navi2ch-board-to-url board))
	  ((eq char ?a)
	   (navi2ch-article-to-url
	    board
	    (navi2ch-bm-get-article-internal
	     (navi2ch-bm-get-property-internal (point))))))))

(defun navi2ch-bm-copy-title (board)
  "メニューを表示して、タイトルを得る"
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
	(progn
	  (setq state (navi2ch-article-fetch-article board article force))
	  (when state
	    (navi2ch-bm-remember-fetched-article board article)
	    (let ((buffer-read-only nil))
	      (save-excursion
		(navi2ch-bm-insert-state item 'update)))))
      (message "Can't select this line!"))))

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
    (let* ((article navi2ch-article-current-article)
	   (board navi2ch-article-current-board)
	   (id (cdr (assq 'id board)))
	   (subject (cdr (assq 'subject article)))
	   (basename (format "%s_%s.txt" id (cdr (assq 'artid article))))
	   (dir nil) file)
      (and dir-or-file
	   (file-directory-p dir-or-file)
	   (setq dir dir-or-file))
      (setq file
	    (if (or (not dir-or-file)
		    (and dir (interactive-p)))
		(expand-file-name
		 (read-file-name "Write thread to file: " dir nil nil basename))
	      (expand-file-name basename dir)))
      (and buffer
	   (save-excursion
	     (set-buffer buffer)
	     (goto-char (point-max))
	     (insert (format "<a href=\"%s\">%s</a><br>\n" file subject))))
      (let ((coding-system-for-write navi2ch-coding-system))
	(navi2ch-write-region (point-min) (point-max)
			      file))
      (message "Wrote %s" file))
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
  (interactive)
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

(defun navi2ch-bm-display-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-display-article))

(defun navi2ch-bm-fetch-mark-article (&optional force)
  (interactive "P")
  (navi2ch-bm-exec-subr 'navi2ch-bm-fetch-article force))

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
			       (save-excursion (goto-char end)
					       (backward-char 1)
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

;; mark by regexp query
(defun navi2ch-bm-mark-by-query (query &optional arg)
  (interactive "Mquery (regexp): ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward query nil t)
      (navi2ch-bm-mark-subr (if arg " " "*")))))

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
    (unless sbj (setq sbj navi2ch-bm-empty-subject))
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

(defun navi2ch-bm-sort-by-date (rev)
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
    (funcall
     (cond ((eq ch ?n) 'navi2ch-bm-sort-by-number)
           ((eq ch ?s) 'navi2ch-bm-sort-by-state)
           ((eq ch ?t) 'navi2ch-bm-sort-by-subject)
           ((eq ch ?o) 'navi2ch-bm-sort-by-other)
	   ((eq ch ?d) 'navi2ch-bm-sort-by-date))
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

(defun navi2ch-bm-search-current-board-cache ()
  (interactive)
  (navi2ch-search-cache-subr
   (list (navi2ch-bm-get-board-internal
	  (navi2ch-bm-get-property-internal (point))))))

(defun navi2ch-bm-search ()
  (interactive)
  (let ((ch (navi2ch-read-char-with-retry
	     "Search for: s)ubject a)rticle c)ache: " nil '(?s ?a ?c)))
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
		 ((eq ch2 ?a) (navi2ch-search-all-cache)))))))

;;; save and load info
(defun navi2ch-bm-save-info ()
  (if navi2ch-bm-fetched-article-list
      (navi2ch-save-info
       navi2ch-bm-fetched-info-file
       navi2ch-bm-fetched-article-list
       t)))

(defun navi2ch-bm-load-info ()
  (setq navi2ch-bm-fetched-article-list
        (navi2ch-load-info navi2ch-bm-fetched-info-file)))

(defun navi2ch-bm-remove-article-subr (board articles)
  "BOARD と ARTICLES で指定されるスレの情報を消す。
ARTILCES が alist の場合はそのスレのみを、alist の list の場合は指定さ
れるすべてのスレを対象にする。"
  (let ((summary (navi2ch-article-load-article-summary board)))
    (setq articles
	  (cond ((cdr (assq 'artid articles)) ; スレ alist
		 (setq articles (list articles)))
		((cdr (assq 'artid (car articles))) ; スレ alist の list
		 articles)
		(t nil)))
    (dolist (article articles)
      (let ((artid (cdr (assq 'artid article))))
	(let ((buffer (get-buffer (navi2ch-article-get-buffer-name board
								   article))))
	  (when buffer
	    (delete-windows-on buffer)
	    (kill-buffer buffer)))
	(dolist (file (list (navi2ch-article-get-info-file-name board article)
			    (navi2ch-article-get-file-name board article)))
	  (condition-case nil
	      (if (file-exists-p file)
		  (delete-file file))
	    (error nil)))
	(navi2ch-bm-remove-fetched-article board article)
	(when summary
	  (let (elt)
	    (while (setq elt (assoc artid summary)) ; クドい?
	      (setq summary (delq elt summary)))))))
    (when summary
      (navi2ch-article-save-article-summary board summary))))

(defun navi2ch-bm-remove-article ()
  (interactive)
  (let* ((item (navi2ch-bm-get-property-internal (point)))
	 (article (navi2ch-bm-get-article-internal item))
	 (board (navi2ch-bm-get-board-internal item)))
    (when (and board article)
      (navi2ch-bm-remove-article-subr board article)
      (save-excursion
	(let ((buffer-read-only nil))
	  (navi2ch-bm-insert-state item nil nil))))))

(defun navi2ch-bm-remove-mark-article ()
  (interactive)
  (navi2ch-bm-exec-subr 'navi2ch-bm-remove-article))

(run-hooks 'navi2ch-board-misc-load-hook)
;;; navi2ch-board-misc.el ends here
