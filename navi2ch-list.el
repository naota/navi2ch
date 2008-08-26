;;; navi2ch-list.el --- board list module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008 by Navi2ch
;; Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network, 2ch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; http://salad.2ch.net/bbstable.html から、作った方がいいんかな。

;;; Code:
(provide 'navi2ch-list)
(defconst navi2ch-list-ident
  "$Id$")

(eval-when-compile 
  (require 'cl)
  (defvar navi2ch-bookmark-list)
  (defvar navi2ch-list-bookmark-mode)
  (defvar navi2ch-board-buffer-name)
  (defvar navi2ch-board-current-board))

(require 'navi2ch)

(defvar navi2ch-list-mode-map nil)
(unless navi2ch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map "\r" 'navi2ch-list-select-current-board)
    (define-key map "q" 'navi2ch-exit)
    (define-key map "z" 'navi2ch-suspend)
    (define-key map "s" 'navi2ch-list-sync)
    (define-key map " " 'navi2ch-list-select-current-board)
    (navi2ch-define-delete-keys map 'scroll-down)
    (define-key map "a" 'navi2ch-list-add-bookmark)
    (define-key map "b" 'navi2ch-list-toggle-bookmark)
    (navi2ch-define-mouse-key map 2 'navi2ch-list-mouse-select)
    (define-key map "/" 'navi2ch-list-toggle-open)
    (define-key map "[" 'navi2ch-list-open-all-category)
    (define-key map "]" 'navi2ch-list-close-all-category)
    (define-key map "D" 'navi2ch-list-delete-global-bookmark)
    (define-key map "C" 'navi2ch-list-change-global-bookmark)
    (define-key map "?" 'navi2ch-list-search)
    (define-key map "e" 'navi2ch-list-expire)
    (define-key map "U" 'navi2ch-list-show-url)
    (setq navi2ch-list-mode-map map)))

(defvar navi2ch-list-mode-menu-spec
  '("List"
    ["Sync board list" navi2ch-list-sync]
    ["Toggle offline" navi2ch-toggle-offline]
    ["Open all category" navi2ch-list-open-all-category]
    ["Close all category" navi2ch-list-close-all-category]
    ["Toggle current category" navi2ch-list-toggle-open]
    ["Toggle bookmark" navi2ch-list-toggle-bookmark]
    ["Select current board" navi2ch-list-select-current-board])
  "Menu definition for navi2ch-list.")

(defvar navi2ch-list-ignore-category-list
  '("チャット" "お絵かき" "運営案内" "ツール類" "他の掲示板" "他のサイト" "特別企画"))
(defvar navi2ch-list-buffer-name "*navi2ch list*")
(defvar navi2ch-list-current-list nil)
(defvar navi2ch-list-category-list nil)
(defvar navi2ch-list-board-name-list nil)

(defvar navi2ch-list-navi2ch-category-name "Navi2ch")
(defvar navi2ch-list-changed-category-name "変わった板")

(defvar navi2ch-list-navi2ch-category-alist nil)

(defvar navi2ch-list-state-table
  (navi2ch-alist-to-hash
   '((add "A" navi2ch-list-add-board-name-face)
     (change "C" navi2ch-list-change-board-name-face)
     (nil " " navi2ch-list-board-name-face))))

(defconst navi2ch-list-bbstable-default-url
  "http://menu.2ch.net/bbsmenu.html")

;; add hook
(add-hook 'navi2ch-save-status-hook 'navi2ch-list-save-info)

(defun navi2ch-list-get-file-name (&optional name)
  (navi2ch-expand-file-name
   (or name "board.txt")))

(defun navi2ch-list-get-category-list-subr ()
  (let (list)
    (while (re-search-forward "\\(.+\\)\n\\(.+\\)\n\\(.+\\)" nil t)
      (setq list (cons
		  (list (cons 'name (match-string 1))
			(cons 'uri (match-string 2))
			(cons 'id (match-string 3))
			(cons 'type 'board)
			(cons 'seen nil))
		  list)))
    (nreverse list)))

(defun navi2ch-list-get-category (name list)
  (list name
	(cons 'open
	      (or navi2ch-list-init-open-category
		  (cdr (assq 'open
			     (cdr (assoc name navi2ch-list-category-list))))))
	(cons 'child list)))

(defun navi2ch-list-set-category (name list)
  (let ((category (assoc name navi2ch-list-category-list)))
    (setcdr category
	    (list
	     (cadr category)
	     (cons 'child list))))
  (setq navi2ch-list-board-name-list (navi2ch-list-get-board-name-list
				      navi2ch-list-category-list)))

(defun navi2ch-list-get-global-bookmark-board-list ()
  (mapcar (lambda (x)
	    (list (cons 'name (cadr x))
		  (cons 'type 'bookmark)
		  (cons 'id (car x))))
	  navi2ch-bookmark-list))

(defun navi2ch-list-get-global-bookmark-category ()
  (navi2ch-list-get-category
   navi2ch-list-global-bookmark-category-name
   (navi2ch-list-get-global-bookmark-board-list)))

(defun navi2ch-list-set-global-bookmark-category ()
  (navi2ch-list-set-category
   navi2ch-list-global-bookmark-category-name
   (navi2ch-list-get-global-bookmark-board-list)))

(defun navi2ch-list-sync-global-bookmark-category ()
  (navi2ch-list-set-global-bookmark-category)
  (let ((buffer-read-only nil)
	(p (point)))
    (erase-buffer)
    (navi2ch-list-insert-board-names
     navi2ch-list-category-list)
    (goto-char p)))

(defun navi2ch-list-delete-global-bookmark ()
  (interactive)
  (let ((board (get-text-property (point) 'board)))
    (if (eq (cdr (assq 'type board)) 'bookmark)
	(navi2ch-bookmark-delete-bookmark (cdr (assq 'id board)))
      (message "This line is not bookmark!"))))

(defun navi2ch-list-change-global-bookmark ()
  (interactive)
  (let ((board (get-text-property (point) 'board)))
    (if (eq (cdr (assq 'type board)) 'bookmark)
	(navi2ch-bookmark-change-bookmark (cdr (assq 'id board)))
      (message "This line is not bookmark!"))))

(defun navi2ch-list-get-category-list (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (navi2ch-insert-file-contents file)
      (run-hooks 'navi2ch-list-get-category-list-hook)
      (navi2ch-apply-filters navi2ch-list-current-list navi2ch-list-filter-list)
      (goto-char (point-min))
      (let (list)
	(while (re-search-forward "\\(.+\\)\n\n\n" nil t)
	  (setq list (cons (list (match-string 1)
				 (match-beginning 0) (match-end 0))
			   list)))
	(goto-char (point-min))
	(setq list (nreverse list))
	(let (list2)
	  (while list
	    (save-restriction
	      (narrow-to-region (nth 2 (car list))
				(or (nth 1 (cadr list))
				    (point-max)))
	      (setq list2 (cons
			   (navi2ch-list-get-category
			    (caar list)
			    (navi2ch-list-get-category-list-subr))
			   list2)))
	    (setq list (cdr list)))
	  (nreverse list2))))))

(defun navi2ch-list-get-etc-category ()
  (let ((file (expand-file-name navi2ch-list-etc-file-name
				navi2ch-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(navi2ch-list-get-category
	 navi2ch-list-etc-category-name
	 (navi2ch-list-get-category-list-subr))))))

(defun navi2ch-list-insert-board-names-subr (list)
  (let ((prev (point))
	(indent (make-string (1- navi2ch-list-indent-width) ?\ ))
	(change (cdr (assq 'change navi2ch-list-current-list))))
    (dolist (board list)
      (let* ((board-id (cdr (assq 'id board)))
	     (state (gethash (cdr (assoc board-id change))
			     navi2ch-list-state-table)))
	(insert (car state)
		indent
		(cdr (assq 'name board)))
	(when navi2ch-list-display-board-id-p
	  (insert " ")
	  (indent-to-column navi2ch-list-board-id-column)
	  (insert "(" board-id ")"))
	(insert "\n")
	(set-text-properties prev (point) nil)
	(set-text-properties
	 (+ prev
	    (length (car state))
	    (length indent))
	 (1- (point))
	 (list 'mouse-face navi2ch-list-mouse-face
	       'face (cadr state))))
      (put-text-property prev (point) 'board board)
      (setq prev (point)))))

(defun navi2ch-list-insert-board-names (list)
  "LIST の内容をバッファに挿入。"
  (if navi2ch-list-bookmark-mode
      (navi2ch-list-insert-bookmarks list)
    (let ((prev (point)))
      (dolist (pair list)
	(let* ((alist (cdr pair))
	       (open (cdr (assq 'open alist))))
	  (insert "[" (if open "-" "+") "]"
		  (car pair) "\n")
	  (set-text-properties prev (1- (point))
			       (list 'mouse-face navi2ch-list-mouse-face
				     'face 'navi2ch-list-category-face))
	  (put-text-property prev (point) 'genre (car pair))
	  (when open
	    (navi2ch-list-insert-board-names-subr (cdr (assq 'child alist))))
	  (setq prev (point)))))))

(defun navi2ch-list-bookmark-node (board)
  "BOARD から bookmark に格納する node を取得する。"
  (let ((uri (cdr (assq 'uri board)))
	(type (cdr (assq 'type board)))
	(id (cdr (assq 'id board))))
    (cond ((eq type 'board)
	   uri)
	  ((and type id)
	   (cons type id)))))

(defun navi2ch-list-insert-bookmarks (list)
  (let ((bookmark (cdr (assq 'bookmark navi2ch-list-current-list)))
	alist)
    (dolist (x (navi2ch-list-get-board-name-list list))
      (let ((node (navi2ch-list-bookmark-node x)))
	(when (member node bookmark)
	  ;; リストの後にあるノードを優先
	  (setq alist (delq (assoc node alist) alist))
	  (push (cons node x) alist))))
    (navi2ch-list-insert-board-names-subr (mapcar #'cdr
						  (nreverse alist)))))

(defun navi2ch-list-toggle-open ()
  "カテゴリを開いたり閉じたりする。"
  (interactive)
  (when (save-excursion
	  (end-of-line)
	  (re-search-backward "^\\[[+-]\\]" nil t))
    (goto-char (match-beginning 0))
    (let* ((category (get-text-property (point) 'genre))
	   (props (text-properties-at (point)))
	   (pair (assoc category navi2ch-list-category-list))
	   (alist (cdr pair))
	   (open (cdr (assq 'open alist)))
	   (buffer-read-only nil))
      (delete-region (point) (+ 3 (point)))
      (insert "[" (if open "+" "-") "]")
      (set-text-properties (- (point) 3) (point) props)
      (save-excursion
	(forward-line 1)
	(if open
	    (delete-region (point)
			   (if (re-search-forward "^\\[[+-]\\]" nil t)
			       (match-beginning 0)
			     (point-max)))
	  (navi2ch-list-insert-board-names-subr (cdr (assq 'child alist)))))
      (setcdr pair (navi2ch-put-alist 'open (not open) alist)))))

(defun navi2ch-list-select-current-board (&optional force)
  "板を選ぶ。またはカテゴリの開閉をする。"
  (interactive "P")
  (let (prop)
    (cond ((setq prop (get-text-property (point) 'board))
	   (navi2ch-list-select-board prop force))
	  ((get-text-property (point) 'genre)
	   (navi2ch-list-toggle-open))
	  (t
	   (message "Can't select this line!")))))

(defun navi2ch-list-open-all-category ()
  (interactive)
  (when (save-excursion
	  (end-of-line)
	  (re-search-backward "^\\[[+-]\\]" nil t))
    (let ((str (buffer-substring-no-properties
		(save-excursion (beginning-of-line) (point))
		(save-excursion (end-of-line) (point)))))
      (setq navi2ch-list-category-list
	    (mapcar (lambda (x)
		      (navi2ch-put-alist 'open t x))
		    navi2ch-list-category-list))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(navi2ch-list-insert-board-names
	 navi2ch-list-category-list))
      (goto-char (point-min))
      (re-search-forward (concat "^"
				 (regexp-quote
				  (navi2ch-replace-string "^\\[\\+\\]" "[-]"
							  str t))
				 "$")
			 nil t)
      (beginning-of-line)
      (if (looking-at "\\[-\\]")
	  (goto-char (match-end 0))
	(forward-char navi2ch-list-indent-width)))))

(defun navi2ch-list-close-all-category ()
  (interactive)
  (when (save-excursion
	  (end-of-line)
	  (re-search-backward "^\\[[+-]\\]" nil t))
    (goto-char (match-end 0))
    (let ((str (buffer-substring-no-properties
		(point)
		(save-excursion (end-of-line) (point)))))
      (setq navi2ch-list-category-list
	    (mapcar (lambda (x)
		      (navi2ch-put-alist 'open nil x))
		    navi2ch-list-category-list))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(navi2ch-list-insert-board-names
	 navi2ch-list-category-list))
      (goto-char (point-min))
      (re-search-forward (concat "^\\(\\[[+-]\\]\\)"
				 (regexp-quote str)
				 "$")
			 nil t)
      (goto-char (match-end 1)))))

(defun navi2ch-list-select-board (board &optional force)
  (let ((flag (eq (current-buffer)
		  (get-buffer navi2ch-list-buffer-name))))
    (when (and (get-buffer navi2ch-board-buffer-name)
	       flag)
      (delete-windows-on navi2ch-board-buffer-name))
    (dolist (x (navi2ch-article-buffer-list))
      (when x
	(delete-windows-on x)))
    (navi2ch-split-window 'board)
    (navi2ch-bm-select-board board force)))

(easy-menu-define navi2ch-list-mode-menu
  navi2ch-list-mode-map
  "Menu used in navi2ch-list"
  navi2ch-list-mode-menu-spec)

(defun navi2ch-list-setup-menu ()
  (easy-menu-add navi2ch-list-mode-menu))

(defun navi2ch-list-mode ()
  "\\{navi2ch-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-list-mode)
  (setq mode-name "Navi2ch List")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map navi2ch-list-mode-map)
  (navi2ch-list-setup-menu)
  (run-hooks 'navi2ch-list-mode-hook)
  (force-mode-line-update))

(defun navi2ch-list ()
  (interactive)
  (if (get-buffer navi2ch-list-buffer-name)
      (switch-to-buffer navi2ch-list-buffer-name)
    (switch-to-buffer (get-buffer-create navi2ch-list-buffer-name))
    (navi2ch-list-mode)
    (save-excursion
      (navi2ch-list-sync nil t))))

(defun navi2ch-list-get-changed-status (old-category-list category-list)
  "以前の板の一覧 OLD-CATEGORY-LIST と現在の板の一覧 CATEGORY-LIST を
比べて、追加、変更のあった板を
'((add . added-list)
  (change . changed-list))
の alist にして返す。
added-list は '(board-id ...) な list。
changed-list は '((board-id old-board new-board) ...) な alist。"
  (let ( ;; 現在の板一覧の uri の alist
	(list (navi2ch-alist-list-to-alist
	       (navi2ch-list-get-board-name-list category-list)
	       'uri))
	;; 以前の板一覧の uri の alist
	(old-list (navi2ch-alist-list-to-alist
 		   (navi2ch-list-get-board-name-list old-category-list)
		   'uri))
	added-list changed-list)
    (dolist (new list)
      (when (car new)			; uri があるときのみ処理する。
	(let ((old (assoc (car new) old-list)))
	  ;; 現在の板一覧の uri が以前の板一覧から見つからなかったら
	  (unless old
	    (let ((new-name (cdr (assq 'name (cdr new))))
		  (new-pure-id (navi2ch-replace-string
				":.*" "" (cdr (assq 'id (cdr new)))))
		  old-name old-pure-id)
	      (catch 'break
		(dolist (x old-list)
		  (setq old-name (cdr (assq 'name (cdr x)))
			old-pure-id (navi2ch-replace-string
				     ":.*" "" (cdr (assq 'id (cdr new)))))
		  ;; 名前と id の : 以前が同じだったら変わった板に追加する
		  (when (and (string= new-name old-name)
			     (string= new-pure-id old-pure-id))
		    (push (list (cdr (assq 'id (cdr new)))
				(cdr x) (cdr new))
			  changed-list)
		    (throw 'break nil)))
		;; 現在の板一覧の id が以前の板一覧から見つからなかったら
		;; 追加された事にする
		(push (cdr (assq 'id (cdr new))) added-list)))))))
    (list (cons 'add added-list)
 	  (cons 'change changed-list))))

(defun navi2ch-list-change (changed-list)
  "CHANGED-LIST をもとに板ブックマークを更新。"
  (let ((changed-alist (mapcar
			(lambda (elt)
			  (cons (navi2ch-list-bookmark-node (nth 1 elt))
				(navi2ch-list-bookmark-node (nth 2 elt))))
			changed-list)))
    (setq navi2ch-list-current-list
	  (navi2ch-put-alist 'bookmark
			     (mapcar (lambda (node)
				       (or (cdr (assoc node changed-alist))
					   node))
				     (cdr (assq 'bookmark
						navi2ch-list-current-list)))
			     navi2ch-list-current-list))))

(defun navi2ch-list-apply-changed-status (changed-status)
  "CHANGED-STATUS をもとに板の変更をいろんな所に反映する。"
  (message "Applying board changes...")
  (let ((added-list (cdr (assq 'add changed-status)))
	(changed-list (cdr (assq 'change changed-status))))
    (when changed-status
      (setq navi2ch-list-current-list
	    (navi2ch-put-alist 'change
			       (append (mapcar (lambda (id)
						 (cons id 'add))
					       added-list)
				       (mapcar (lambda (pair)
						 (cons (car pair) 'change))
					       changed-list))
			       navi2ch-list-current-list))
      (navi2ch-change-log-directory changed-list)
      (navi2ch-bookmark-change changed-list)
      (navi2ch-history-change changed-list)
      (navi2ch-list-change changed-list)
      (message "Applying board changes...done"))))

(defun navi2ch-list-get-changed-category (category-list)
  (let ((alist (navi2ch-alist-list-to-alist
		(navi2ch-list-get-board-name-list category-list)
		'id)))
    (navi2ch-list-get-category
     navi2ch-list-changed-category-name
     (mapcar (lambda (pair)
	       (cdr (assoc (car pair) alist)))
	     (cdr (assq 'change navi2ch-list-current-list))))))

(defun navi2ch-list-sync (&optional force first)
  (interactive "P")
  (save-excursion
    (let ((buffer-read-only nil)
	  (navi2ch-net-force-update (or navi2ch-net-force-update
					force))
	  (file (navi2ch-list-get-file-name))
	  (bbstable (or navi2ch-list-bbstable-url
			navi2ch-list-bbstable-default-url))
	  (change (cdr (assq 'change navi2ch-list-current-list)))
	  updated header time old-category-list)
      (when first
	(navi2ch-list-load-info))
      (navi2ch-set-mode-line-identification)
      (setq old-category-list (navi2ch-list-get-normal-category-list
			       navi2ch-list-category-list))
      (unless (or navi2ch-offline
		  (and first
		       (not navi2ch-list-sync-update-on-boot)
		       (file-exists-p file)))
	(setq time (and (equal (cdr (assq 'bbstable navi2ch-list-current-list))
			       bbstable)
			(cdr (assq 'time navi2ch-list-current-list))))
	(setq header (navi2ch-net-update-file bbstable file time
					      'navi2ch-list-make-board-txt))
	(setq updated (and header
			   (not (navi2ch-net-get-state 'not-updated header))
			   (not (navi2ch-net-get-state 'error header)))))
      (when updated
	(setq navi2ch-list-current-list
	      (navi2ch-put-alist 'time
				 (or (cdr (assq 'last-modified header))
				     (cdr (assq 'date header)))
				 navi2ch-list-current-list))
	(setq navi2ch-list-current-list
	      (navi2ch-put-alist 'bbstable bbstable
				 navi2ch-list-current-list)))
      ;; bbstable, etc.txt, navi2ch-list-navi2ch-category-alist
      ;; のいずれかが更新されていれば 以下の処理が必要。
      ;; とりあえず、常に実行しておく。
      (let ((category-list (navi2ch-list-get-category-list file)))
	(when (or updated change)
	  (navi2ch-list-apply-changed-status
	   (navi2ch-list-get-changed-status old-category-list category-list)))
	(setq navi2ch-list-category-list
	      (append
	       (delq nil
		     (list (navi2ch-list-get-category
			    navi2ch-list-navi2ch-category-name
			    navi2ch-list-navi2ch-category-alist)
			   (navi2ch-list-get-global-bookmark-category)
			   (navi2ch-list-get-etc-category)
			   (navi2ch-list-get-changed-category
			    category-list)))
	       category-list))
	(setq navi2ch-list-board-name-list
	      (navi2ch-list-get-board-name-list
	       navi2ch-list-category-list)))
      (erase-buffer)
      (navi2ch-list-insert-board-names navi2ch-list-category-list)))
  (run-hooks 'navi2ch-list-after-sync-hook))

(defun navi2ch-list-board-id-from-url (url)
  "URL から board-id を得る。"
  (let ((board-id (cdr (assoc url navi2ch-list-board-id-alist))))
    (or board-id
	(save-match-data
	  (if (string-match "\\`http://.+/\\([^/]+\\)/\\'" url)
	      (match-string 1 url))))))

(defun navi2ch-list-make-board-txt ()
  "bbstable.html から (navi2ch 用の) board.txt を作る。
`navi2ch-net-update-file' のハンドラ。"
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(case-fold-search t)
	(beg (point))
	(id-to-url-table (make-hash-table :test 'eq))
	ignore)
    (when (re-search-forward "<b>[^>]+</b>" nil t)
      (goto-char (match-beginning 0))
      (while (re-search-forward
	      "<\\([ab]\\)\\([^>]*\\)>\\([^<]+\\)</\\1>" nil t)
	(let ((tag (match-string 1))
	      (attr (match-string 2))
	      (cont (match-string 3)))
	  (delete-region beg (point))
	  (if (string-match "a" tag)
	      (let (url board-id id u)
		(when (and (not ignore)
			   (string-match "href=\\(.+/\\([^/]+\\)/\\)" attr))
		  (setq url (match-string 1 attr))
		  (setq url (or (cdr (assoc
				      url
				      navi2ch-list-moved-board-alist))
				url))
		  (when (and (navi2ch-list-valid-board url)
			     (setq board-id (navi2ch-list-board-id-from-url url)))
		    (setq id (intern board-id))
		    (when (and (setq u (gethash id id-to-url-table))
			       (not (string= u url)))
		      ;; 同じ ID で URL が違う板がある場合
		      (let ((i 2)
			    newid)
			(while (and (setq newid (intern (format "%s:%d" id i)))
				    (setq u (gethash newid id-to-url-table))
				    (not (string= u url)))
			  (setq i (1+ i)))
			(setq id newid)))
		    (puthash id url id-to-url-table)
		    (insert cont "\n"
			    url "\n"
			    (symbol-name id) "\n"))))
	    (setq ignore
		  (member (decode-coding-string
			   cont navi2ch-coding-system)
			  navi2ch-list-ignore-category-list))
	    (when (not ignore)
	      (insert cont "\n\n\n"))))
	(setq beg (point))))
    (delete-region beg (point-max))))

(defun navi2ch-list-valid-board (uri)
  (save-match-data
    (when (string-match "http://\\([^/]+\\)/\\([^/]+\\)/" uri)
      (let ((host (match-string 1 uri)))
	(and (not (string-match navi2ch-list-invalid-host-regexp host))
	     (string-match navi2ch-list-valid-host-regexp host))))))

(defun navi2ch-list-mouse-select (e)
  (interactive "e")
  (beginning-of-line)
  (mouse-set-point e)
  (save-excursion
    (navi2ch-list-select-current-board)))

(defun navi2ch-list-goto-board (&optional default)
  (interactive)
  (let (alist board)
    (setq alist (mapcar (lambda (x) (cons (cdr (assq 'id x)) x))
			navi2ch-list-board-name-list))
    (save-window-excursion
      (setq board (cdr (assoc
			(completing-read
			 (concat "Board name"
				 (when default
				   (format " (%s)" (cdr (assq 'id default))))
				 ": ")
			 alist nil t)
			alist))))
    (setq board (or board
		    (assoc (cdr (assq 'id default)) alist)))
    (when board
      (when (eq (navi2ch-get-major-mode navi2ch-board-buffer-name)
		'navi2ch-board-mode)
	(navi2ch-board-save-info))
      (navi2ch-list-select-board board))))

(defun navi2ch-list-get-normal-category-list (list)
  (setq list (copy-sequence list))	; delq するから
  (when (assoc navi2ch-list-navi2ch-category-name list)
    (setq list (delq (assoc navi2ch-list-navi2ch-category-name list) list)))
  (when (assoc navi2ch-list-global-bookmark-category-name list)
    (setq list (delq (assoc navi2ch-list-global-bookmark-category-name list) list))))

(defun navi2ch-list-get-board-name-list (list)
  (let (alist id)
    (dolist (x list)
      (unless (string= (car x) navi2ch-list-changed-category-name)
	(dolist (y (cdr (assq 'child x)))
	  (setq id (cdr (assq 'id y)))
	  ;; 同じ id に対しては一つのみ返す。
	  (setq alist (cons (cons id y)
			    (delq (assoc id alist) alist))))))
    (mapcar #'cdr (nreverse alist))))

(defun navi2ch-list-normalize-bookmark (list)
  (let ((bookmark (cdr (assq 'bookmark list)))
	ret)
    (dolist (x (navi2ch-list-get-board-name-list navi2ch-list-category-list))
      (let ((node (navi2ch-list-bookmark-node x)))
	(when (member node bookmark)
	  (setq ret (cons node (delq node ret))))))
    (nreverse ret)))

(defun navi2ch-list-save-info ()
  (when navi2ch-list-category-list
    (let ((list (mapcar (lambda (elt)
			  (list (car elt)
				(assq 'open (cdr elt))))
			navi2ch-list-category-list)))
      (setq navi2ch-list-current-list
	    (navi2ch-put-alist 'category list
			       navi2ch-list-current-list))))
  (when navi2ch-list-current-list
    (navi2ch-save-info
     (navi2ch-list-get-file-name "list.info")
     (list (cons 'bookmark (navi2ch-list-normalize-bookmark
			    navi2ch-list-current-list))
	   (assq 'category navi2ch-list-current-list)
	   (assq 'change navi2ch-list-current-list)
	   (assq 'bbstable navi2ch-list-current-list)
	   (assq 'time navi2ch-list-current-list))
     t)))

(defun navi2ch-list-load-info ()
  (setq navi2ch-list-current-list
	(navi2ch-load-info (navi2ch-list-get-file-name "list.info")))
  (if navi2ch-list-load-category-list
      (setq navi2ch-list-category-list
	    (cdr (assq 'category navi2ch-list-current-list))))
  (let* ((file (navi2ch-list-get-file-name))
	 (category-list (navi2ch-list-get-category-list file)))
    (setq navi2ch-list-category-list
	  (append
	   (delq nil
		 (list (navi2ch-list-get-category
			navi2ch-list-navi2ch-category-name
			navi2ch-list-navi2ch-category-alist)
		       (navi2ch-list-get-global-bookmark-category)
		       (navi2ch-list-get-etc-category)
		       (navi2ch-list-get-changed-category category-list)))
	   category-list))
    (setq navi2ch-list-board-name-list
	  (navi2ch-list-get-board-name-list navi2ch-list-category-list))))

(defun navi2ch-list-get-current-category-list ()
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^\\[[+-]\\]" nil t)
      (let ((category (get-text-property (point) 'genre)))
	(cdr (assq 'child (cdr
			   (assoc category
				  navi2ch-list-category-list))))))))

;;; bookmark mode
(defvar navi2ch-list-bookmark-mode nil)
(defvar navi2ch-list-bookmark-mode-map nil)
(unless navi2ch-list-bookmark-mode-map
  (setq navi2ch-list-bookmark-mode-map (make-sparse-keymap))
  (define-key navi2ch-list-bookmark-mode-map "d"
    'navi2ch-list-delete-bookmark)
  (define-key navi2ch-list-bookmark-mode-map "a" 'undefined))

(navi2ch-set-minor-mode 'navi2ch-list-bookmark-mode
			" Bookmark"
			navi2ch-list-bookmark-mode-map)

(defun navi2ch-list-add-bookmark ()
  (interactive)
  (let ((node (navi2ch-list-bookmark-node (get-text-property (point)
							     'board)))
	(list (cdr (assq 'bookmark navi2ch-list-current-list))))
    (if node
	(unless (member node list)
	  (setq list (cons node list))
	  (setq navi2ch-list-current-list
		(navi2ch-put-alist 'bookmark list
				   navi2ch-list-current-list))
	  (message "Add bookmark"))
      (message "Can't select this line!"))))

(defun navi2ch-list-delete-bookmark ()
  (interactive)
  (let ((node (navi2ch-list-bookmark-node (get-text-property (point)
							     'board)))
	(list (cdr (assq 'bookmark navi2ch-list-current-list))))
    (if node
	(progn
	  (setq list (delete node list))
	  (setq navi2ch-list-current-list
		(navi2ch-put-alist 'bookmark list
				   navi2ch-board-current-board))
	  (let ((buffer-read-only nil))
	    (delete-region (save-excursion (beginning-of-line) (point))
			   (save-excursion (forward-line) (point))))
	  (message "Delete bookmark"))
      (message "Can't select this line!"))))

(defun navi2ch-list-toggle-bookmark ()
  (interactive)
  (setq navi2ch-list-bookmark-mode (not navi2ch-list-bookmark-mode))
  (let ((buffer-read-only nil))
    (save-excursion
      (erase-buffer)
      (navi2ch-list-insert-board-names navi2ch-list-category-list))))

;;; search
(defun navi2ch-list-search-current-board-subject ()
  (interactive)
  (navi2ch-search-subject-subr (list (get-text-property (point) 'board))))

(defun navi2ch-list-search-current-category-subject ()
  (interactive)
  (navi2ch-search-subject-subr
   (navi2ch-list-get-current-category-list)))

(defun navi2ch-list-search-current-board-article ()
  (interactive)
  (navi2ch-search-article-subr (list (get-text-property (point) 'board))))

(defun navi2ch-list-search-current-category-article ()
  (interactive)
  (navi2ch-search-article-subr
   (navi2ch-list-get-current-category-list)))

(defun navi2ch-list-search-current-board-cache ()
  (interactive)
  (navi2ch-search-cache-subr (list (get-text-property (point) 'board))))

(defun navi2ch-list-search-current-category-cache ()
  (interactive)
  (navi2ch-search-cache-subr
   (navi2ch-list-get-current-category-list)))

(defun navi2ch-list-search-current-board-orphan ()
  (interactive)
  (navi2ch-search-orphan-subr (list (get-text-property (point) 'board))))

(defun navi2ch-list-search-current-category-orphan ()
  (interactive)
  (navi2ch-search-orphan-subr
   (navi2ch-list-get-current-category-list)))

(defun navi2ch-list-search ()
  (interactive)
  (let* ((ch (navi2ch-read-char-with-retry
	      "Search for: s)ubject a)rticle c)ache o)rphan: "
	      nil '(?s ?a ?c ?o)))
	 (board (get-text-property (point) 'board))
	 (ch2 (if (and board
		       (navi2ch-board-get-file-name board))
		 (navi2ch-read-char-with-retry
		  "Search from: b)oard c)ategory a)ll: " nil '(?b ?c ?a))
	       (navi2ch-read-char-with-retry
		"Search from: c)ategory a)ll: " nil '(?c ?a)))))
    (cond ((eq ch ?s)
	   (cond ((eq ch2 ?b) (navi2ch-list-search-current-board-subject))
		 ((eq ch2 ?c) (navi2ch-list-search-current-category-subject))
		 ((eq ch2 ?a) (navi2ch-search-all-subject))))
	  ((eq ch ?a)
	   (cond ((eq ch2 ?b) (navi2ch-list-search-current-board-article))
		 ((eq ch2 ?c) (navi2ch-list-search-current-category-article))
		 ((eq ch2 ?a) (navi2ch-search-all-article))))
	  ((eq ch ?c)
	   (cond ((eq ch2 ?b) (navi2ch-list-search-current-board-cache))
		 ((eq ch2 ?c) (navi2ch-list-search-current-category-cache))
		 ((eq ch2 ?a) (navi2ch-search-all-cache))))
	  ((eq ch ?o)
	   (cond ((eq ch2 ?b) (navi2ch-list-search-current-board-orphan))
		 ((eq ch2 ?c) (navi2ch-list-search-current-category-orphan))
		 ((eq ch2 ?a) (navi2ch-search-all-orphan)))))))

;;; expire
(defun navi2ch-list-expire-current-board (&optional ask)
  (interactive)
  (navi2ch-board-expire
   (get-text-property (point) 'board) ask))

(defun navi2ch-list-expire-current-category (&optional ask)
  (interactive)
  (and (interactive-p) (setq ask t))
  (when (or (not ask)
	    (y-or-n-p "Expire current category boards? "))
    (dolist (board (navi2ch-list-get-current-category-list))
      (navi2ch-board-expire board))
    (message "Expiring current category is done")))

(defun navi2ch-list-expire-all (&optional ask)
  (interactive)
  (and (interactive-p) (setq ask t))
  (when (or (not ask)
	    (y-or-n-p "Expire all boards? "))
    (dolist (board navi2ch-list-board-name-list)
      (when (eq (cdr (assq 'type board)) 'board)
	(navi2ch-board-expire board)))
    (message "Expiring all board is done")))

(defun navi2ch-list-expire ()
  (interactive)
  (let ((ch (navi2ch-read-char-with-retry "Expire b)oard c)ategory a)ll? "
					  nil '(?b ?c ?a))))
    (cond ((eq ch ?b) (navi2ch-list-expire-current-board 'ask))
	  ((eq ch ?c) (navi2ch-list-expire-current-category 'ask))
	  ((eq ch ?a) (navi2ch-list-expire-all 'ask)))))

(defun navi2ch-list-show-url ()
  (interactive)
  (let* ((board (get-text-property (point) 'board))
	 (uri (cdr (assq 'uri board)))
	 (name (cdr (assq 'name board))))
    (if (not uri)
	(message "Can't select this line!")
      (let ((char (navi2ch-read-char-with-retry
		   (format "c)opy v)iew t)itle u)rl&title? URL: %s: " uri)
		   nil '(?c ?v ?t ?u))))
	(if (eq char ?v)
	    (navi2ch-browse-url-internal uri)
	  (let ((str (cond ((eq char ?c)
			    uri)
			   ((eq char ?t)
			    name)
			   ((eq char ?u)
			    (format "%s\n%s" name uri)))))
	    (if (not str)
		(ding)
	      (kill-new str)
	      (message "Copy: %s" str))))))))

(defun navi2ch-list-url-at-point (point)
  (let ((board (get-text-property point 'board)))
    (cdr (assq 'uri board))))

(run-hooks 'navi2ch-list-load-hook)
;;; navi2ch-list.el ends here
