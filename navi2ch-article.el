;;; navi2ch-article.el --- article view module for navi2ch

;; Copyright (C) 2000-2004 by Navi2ch Project

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

;;; Commentary:

;;

;;; Code:
(provide 'navi2ch-article)
(defconst navi2ch-article-ident
  "$Id$")

(eval-when-compile (require 'cl))
(require 'base64)

(require 'navi2ch)

(defvar navi2ch-article-mode-map nil)
(unless navi2ch-article-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-view-map)
    (define-key map "q" 'navi2ch-article-exit)
    (define-key map "Q" 'navi2ch-article-goto-current-board)
    (define-key map "s" 'navi2ch-article-sync)
    (define-key map "S" 'navi2ch-article-sync-disable-diff)
    (define-key map "r" 'navi2ch-article-redraw-range)
    (define-key map "j" 'navi2ch-article-few-scroll-up)
    (define-key map "k" 'navi2ch-article-few-scroll-down)
    (define-key map " " 'navi2ch-article-scroll-up)
    (navi2ch-define-delete-keys map 'navi2ch-article-scroll-down)
    (define-key map "w" 'navi2ch-article-write-message)
    (define-key map "W" 'navi2ch-article-write-sage-message)
    (define-key map "\r" 'navi2ch-article-select-current-link)
    (navi2ch-define-mouse-key map 2 'navi2ch-article-mouse-select)
    (define-key map "g" 'navi2ch-article-goto-number-or-board)
    ;; (define-key map "g" 'navi2ch-article-goto-number)
    (define-key map "l" 'navi2ch-article-pop-point)
    (define-key map "L" 'navi2ch-article-pop-poped-point)
    (define-key map "m" 'navi2ch-article-push-point)
    (define-key map "R" 'navi2ch-article-rotate-point)
    (define-key map "U" 'navi2ch-article-show-url)
    (define-key map "." 'navi2ch-article-redisplay-current-message)
    (define-key map "p" 'navi2ch-article-previous-message)
    (define-key map "n" 'navi2ch-article-next-message)
    (define-key map "P" 'navi2ch-article-through-previous)
    (define-key map "N" 'navi2ch-article-through-next)
    (define-key map [(shift tab)] 'navi2ch-article-previous-link)
    (define-key map [(shift iso-lefttab)] 'navi2ch-article-previous-link)
    (define-key map "\e\C-i" 'navi2ch-article-previous-link)
    (define-key map "\C-\i" 'navi2ch-article-next-link)
    (define-key map  "i" 'navi2ch-article-fetch-link)
    (define-key map ">" 'navi2ch-article-goto-last-message)
    (define-key map "<" 'navi2ch-article-goto-first-message)
    (define-key map "\ed" 'navi2ch-article-decode-message)
    (define-key map "\ei" 'navi2ch-article-auto-decode-toggle-text)
    (define-key map "v" 'navi2ch-article-view-aa)
    (define-key map "f" 'navi2ch-article-forward-buffer)
    (define-key map "b" 'navi2ch-article-backward-buffer)
    (define-key map "d" 'navi2ch-article-hide-message)
    (define-key map "a" 'navi2ch-article-add-important-message)
    (define-key map "h" 'navi2ch-article-toggle-hide)
    (define-key map "$" 'navi2ch-article-toggle-important)
    (define-key map "A" 'navi2ch-article-add-global-bookmark)
    (define-key map "\C-c\C-m" 'navi2ch-message-pop-message-buffer)
    (define-key map "G" 'navi2ch-article-goto-board)
    (define-key map "e" 'navi2ch-article-textize-article)
    (define-key map "?" 'navi2ch-article-search)
    (define-key map "\C-o" 'navi2ch-article-save-dat-file)
    (define-key map "F" 'navi2ch-article-toggle-message-filter)
    (define-key map "x" 'undefined)
    (define-key map "!" 'navi2ch-article-add-message-filter-rule)
    (define-key map "\C-c\C-r" 'navi2ch-article-remove-article)
    (navi2ch-ifxemacs
	(define-key map  "\C-c\C- " 'navi2ch-article-toggle-sticky)
      (define-key map [(control c) (control ? )] 'navi2ch-article-toggle-sticky))
    (define-key map "u" 'navi2ch-show-url-at-point)
    (setq navi2ch-article-mode-map map)))

(defvar navi2ch-article-mode-menu-spec
  '("Article"
    ["Toggle offline" navi2ch-toggle-offline]
    ["Sync" navi2ch-article-sync]
    ["Sync (no diff)" navi2ch-article-sync-disable-diff]
    ["Exit" navi2ch-article-exit]
    ["Write message" navi2ch-article-write-message]
    ["Write message (sage)" navi2ch-article-write-sage-message]
    ["Select Range" navi2ch-article-redraw-range]))

(defvar navi2ch-article-view-range nil
  "表示するスレッドの範囲。
書式は '(first . last) で、
first が最初からいくつ表示するか、
last が最後からいくつ表示するか。
例えば、(10 . 50) で、最初の10と最後の50を表示")

(defvar navi2ch-article-buffer-name-prefix "*navi2ch article ")
(defvar navi2ch-article-current-article nil)
(defvar navi2ch-article-current-board nil)
(defvar navi2ch-article-message-list nil)
(defvar navi2ch-article-point-stack nil "位置を覚えとく stack")
(defvar navi2ch-article-poped-point-stack nil)
(defvar navi2ch-article-separator nil)
(defvar navi2ch-article-hide-mode nil)
(defvar navi2ch-article-window-configuretion nil)
(defvar navi2ch-article-through-next-function 'navi2ch-article-through-next)
(defvar navi2ch-article-through-previous-function 'navi2ch-article-through-previous)
(defvar navi2ch-article-through-forward-line-function 'navi2ch-bm-forward-line
  "前後のスレに移動するために実行される関数。
一つの整数を引数にとり、その数だけ後ろ(引数が負のときは前)のスレに移動し、
移動できたら 0、できなければ 0 以外の整数を返す関数であること。")

(defvar navi2ch-article-save-info-keys
  '(number name time hide important unfilter mail kako))

(defvar navi2ch-article-insert-message-separator-function
  (if (and window-system
	   (eq emacs-major-version 20)
	   (not (featurep 'xemacs)))
      'navi2ch-article-insert-message-separator-by-face
    'navi2ch-article-insert-message-separator-by-char)
  "セパレータを挿入する関数")

(defvar navi2ch-article-summary-file-name "article-summary")

;; important mode
(defvar navi2ch-article-important-mode nil)
(defvar navi2ch-article-important-mode-map nil)
(unless navi2ch-article-important-mode-map
  (setq navi2ch-article-important-mode-map (make-sparse-keymap))
  (define-key navi2ch-article-important-mode-map "d" 'navi2ch-article-delete-important-message)
  (define-key navi2ch-article-important-mode-map "a" 'undefined))

;; hide mode
(defvar navi2ch-article-hide-mode nil)
(defvar navi2ch-article-hide-mode-map nil)
(unless navi2ch-article-hide-mode-map
  (setq navi2ch-article-hide-mode-map (make-sparse-keymap))
  (define-key navi2ch-article-hide-mode-map "d" 'navi2ch-article-cancel-hide-message)
  (define-key navi2ch-article-hide-mode-map "a" 'undefined))

;; filter mode
(defvar navi2ch-article-message-filter-mode nil)
(defvar navi2ch-article-message-filter-mode-map nil)
(unless navi2ch-article-message-filter-mode-map
  (setq navi2ch-article-message-filter-mode-map (make-sparse-keymap))
  (define-key navi2ch-article-message-filter-mode-map "x" 'navi2ch-article-toggle-replace-message))

(defvar navi2ch-article-message-filter-cache nil)
(defvar navi2ch-article-save-message-filter-cache-keys
  '(cache replace hide important aborn))

;; defcustom にしないでおく
(defvar navi2ch-article-default-message-filter-by-message-alist
  `(((,(regexp-opt '("(mule-caesar" "(base64-decode"
		     "(shell-command" "(call-process" "(delete-file")) r)
     . hide)))

;; sticky mode
(defvar navi2ch-article-sticky-mode nil)
(make-variable-buffer-local 'navi2ch-article-sticky-mode)
(add-to-list 'minor-mode-alist '(navi2ch-article-sticky-mode " Sticky"))

;; local variables
(make-variable-buffer-local 'navi2ch-article-current-article)
(make-variable-buffer-local 'navi2ch-article-current-board)
(make-variable-buffer-local 'navi2ch-article-message-list)
(make-variable-buffer-local 'navi2ch-article-message-filter-cache)
(make-variable-buffer-local 'navi2ch-article-point-stack)
(make-variable-buffer-local 'navi2ch-article-poped-point-stack)
(make-variable-buffer-local 'navi2ch-article-view-range)
(make-variable-buffer-local 'navi2ch-article-separator)
(make-variable-buffer-local 'navi2ch-article-through-next-function)
(make-variable-buffer-local 'navi2ch-article-through-previous-function)

;; add hook
(defun navi2ch-article-kill-emacs-hook ()
  (navi2ch-article-expunge-buffers -1))

(add-hook 'navi2ch-kill-emacs-hook 'navi2ch-article-kill-emacs-hook)

;;; navi2ch-article functions
(defun navi2ch-article-get-url (board article &optional no-kako)
  (let ((artid (cdr (assq 'artid article)))
	(url (navi2ch-board-get-uri board)))
    (if (and (not no-kako)
	     (cdr (assq 'kako article)))
	(navi2ch-article-get-kako-url board article)
      (concat url "dat/" artid ".dat"))))

(defun navi2ch-article-get-kako-url (board article)
  (let* ((artid (cdr (assq 'artid article)))
	 (url (navi2ch-board-get-uri board))
	 (length (length artid)))
    (cond
     ((= length 9)
      (concat url "kako/"
	      (substring artid 0 3)
	      "/" artid ".dat.gz"))
     ((= length 10)
      (concat url "kako/"
	      (substring artid 0 4) "/" (substring artid 0 5)
	      "/" artid ".dat.gz"))
     (t
      nil))))

(defun navi2ch-article-get-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat (cdr (assq 'artid article)) ".dat")))

(defun navi2ch-article-get-info-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat "info/" (cdr (assq 'artid article)))))

(defsubst navi2ch-article-inside-range-p (num range len)
  "NUM が RANGE で示す範囲に入ってるか
LEN は RANGE で範囲を指定される list の長さ"
  (or (not range)
      (<= num (car range))
      (> num (- len (cdr range)))))

(defsubst navi2ch-article-get-buffer-name (board article)
  (concat navi2ch-article-buffer-name-prefix
	  (navi2ch-article-get-url board article 'no-kako)))

(defsubst navi2ch-article-check-cached (board article)
  "BOARD と ARTICLE で指定されるスレッドがキャッシュされてるか。"
  (cond ((get-buffer (navi2ch-article-get-buffer-name board article))
         'view)
        ((file-exists-p (navi2ch-article-get-file-name board article))
	 ;; ((member (concat (cdr (assq 'artid article)) ".dat") list)
         'cache)))

(defmacro navi2ch-article-summary-element-seen (element)
  `(plist-get ,element :seen))

(defmacro navi2ch-article-summary-element-access-time (element)
  `(plist-get ,element :access-time))

(defmacro navi2ch-article-summary-element-set-seen (element seen)
  `(setq ,element (plist-put ,element :seen ,seen)))

(defmacro navi2ch-article-summary-element-set-access-time (element time)
  `(setq ,element (plist-put ,element :access-time ,time)))

(defmacro navi2ch-article-save-view (&rest body)
  "BODY 内で現在表示しているスレを表示しなおすときに、
ウィンドウ内のカーソルの位置をできるだけ維持する。"
  (let ((num (make-symbol "num"))
	(buf (make-symbol "buf"))
	(win (make-symbol "win"))
	(bol (make-symbol "bol"))
	(col (make-symbol "col"))
	(win-lin (make-symbol "win-lin"))
	(msg-lin (make-symbol "msg-lin"))
	(visible (make-symbol "visible")))
    `(let* ((,num (navi2ch-article-get-current-number))
	    (,buf (current-buffer))
	    (,win (if (eq (window-buffer) ,buf)
		      (selected-window)
		    (get-buffer-window ,buf)))
	    (,bol (navi2ch-line-beginning-position))
	    (,col (current-column))
	    ,win-lin ,msg-lin ,visible)
       (save-excursion
	 (goto-char (window-start ,win))
	 (setq ,win-lin (count-lines (navi2ch-line-beginning-position) ,bol))
	 (if (null ,num)
	     (setq ,msg-lin ,win-lin)
	   (navi2ch-article-goto-number ,num)
	   (setq ,msg-lin
		 (count-lines (navi2ch-line-beginning-position) ,bol))))
       (prog1 (progn ,@body)
	 (with-current-buffer ,buf
	   (when ,num
	     (setq ,visible (navi2ch-article-get-visible-numbers))
	     (while (and (cdr ,visible)
			 (< (car ,visible) ,num))
	       (setq ,visible (cdr ,visible))))
	   (navi2ch-article-goto-number (or (car ,visible) 1))
	   (forward-line ,msg-lin)
	   (move-to-column ,col)
	   (set-window-start ,win
			     (navi2ch-line-beginning-position (- 1 ,win-lin)))
	   (unless (eq (navi2ch-article-get-current-number) (car ,visible))
	     (navi2ch-article-goto-number (or (car ,visible) 1))))))))

(put 'navi2ch-article-save-view 'lisp-indent-function 0)

(defun navi2ch-article-url-to-article (url)
  "URL から article に変換。"
  (navi2ch-multibbs-url-to-article url))

(defun navi2ch-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (navi2ch-multibbs-article-to-url board article start end nofirst))

(defsubst navi2ch-article-cleanup-message ()
  (let (re)
    (when navi2ch-article-cleanup-trailing-newline ; レス末尾の空白を取り除く
      (goto-char (point-min))
      (when (re-search-forward "\\(<br> *\\)+<>" nil t)
	(replace-match "<>")))
    (when navi2ch-article-cleanup-white-space-after-old-br
      (goto-char (point-min))
      (unless (re-search-forward "<br>[^ ]" nil t)
	(setq re "<br> ")))
    (when navi2ch-article-cleanup-trailing-whitespace
      (setq re (concat " *" (or re "<br>"))))
    (unless (or (not re)
		(string= re "<br>"))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(replace-match "<br>")))))	; "\n" でもいいかも。

(defsubst navi2ch-article-parse-message (str &optional sep)
  (or sep (setq sep navi2ch-article-separator))
  (unless (string= str "")
;;;     (let ((strs (split-string str sep))
;;; 	  (syms '(name mail date data subject))
;;; 	  s)
;;;       (mapcar (lambda (sym)
;;; 		(setq s (or (car strs) "")
;;; 		      strs (cdr strs))
;;; 		(cons sym
;;; 		      (if (eq sym 'data)
;;; 			  (navi2ch-replace-html-tag-with-temp-buffer s)
;;; 			(navi2ch-replace-html-tag s))))
;;; 	      syms))))
    (with-temp-buffer
      (let ((syms '(name mail date data subject))
	    alist max)
	(insert str)
	(navi2ch-article-cleanup-message)
	(setq max (point-max-marker))
	(goto-char (point-min))
	(setq alist (mapcar
		     (lambda (sym)
		       (cons sym
			     (cons (point-marker)
				   (if (re-search-forward sep nil t)
				       (copy-marker (match-beginning 0))
				     (goto-char max)
				     max))))
		     syms))
	(let ((start (car (cdr (assq 'name alist))))
	      (end (cdr (cdr (assq 'name alist)))))
	  (when (and start end)
	    (goto-char start)
	    (while (re-search-forward "</b>[^<]+<b>" end t)
	      ;; fusianasan
	      (replace-match (navi2ch-propertize (match-string 0)
						 'navi2ch-fusianasan-flag t)
			     t t))))
	(navi2ch-replace-html-tag-with-buffer)
	(dolist (x alist)
	  (setcdr x (buffer-substring (cadr x) (cddr x))))
	alist))))

(defun navi2ch-article-get-separator ()
  (save-excursion
    (let ((string (buffer-substring (navi2ch-line-beginning-position)
				    (navi2ch-line-end-position))))
      (if (or (string-match "<>.*<>.*<>" string)
	      (not (string-match ",.*,.*," string)))
	  " *<> *"
	" *, *"))))

(defsubst navi2ch-article-get-first-message ()
  "current-buffer の article の最初の message を返す。"
  (goto-char (point-min))
  (navi2ch-article-parse-message
   (buffer-substring-no-properties (point)
				   (progn (forward-line 1)
					  (1- (point))))
   (navi2ch-article-get-separator)))

(defsubst navi2ch-article-get-first-message-from-file (file)
  "FILE で指定された article の最初の message を返す。"
  (with-temp-buffer
    (navi2ch-insert-file-contents file)
    (navi2ch-article-get-first-message)))

(defun navi2ch-article-get-message-list (file &optional begin end)
  "FILE の BEGIN から END までの範囲からスレの list を作る
空行は nil"
  (when (file-exists-p file)
    (let ((board navi2ch-article-current-board)
	  sep message-list)
      (with-temp-buffer
        (navi2ch-insert-file-contents file begin end)
	(run-hooks 'navi2ch-article-get-message-list-hook)
        (let ((i 1))
	  (navi2ch-apply-filters board navi2ch-article-filter-list)
          (message "splitting current messages...")
          (goto-char (point-min))
          (setq sep (navi2ch-article-get-separator))
          (while (not (eobp))
            (setq message-list
                  (cons (cons i
			      (let ((str (buffer-substring-no-properties
					  (point)
					  (progn (forward-line 1)
						 (1- (point))))))
				(unless (string= str "") str)))
                        message-list))
            (setq i (1+ i)))
          (message "splitting current messages...done")))
      (setq navi2ch-article-separator sep) ; it's a buffer local variable...
      (nreverse message-list))))

(defun navi2ch-article-append-message-list (list1 list2)
  (let ((num (length list1)))
    (append list1
	    (mapcar
	     (lambda (x)
	       (setq num (1+ num))
	       (cons num (cdr x)))
	     list2))))

(defun navi2ch-article-insert-message-separator-by-face ()
  (let ((p (point)))
    (insert "\n")
    (put-text-property p (point) 'face 'underline)))

(defun navi2ch-article-insert-message-separator-by-char ()
  (insert (make-string (eval navi2ch-article-message-separator-width)
		       navi2ch-article-message-separator) "\n"))

(defsubst navi2ch-article-set-link-property-subr (start end type value
							&optional object)
  (let ((face (cond ((eq type 'number) 'navi2ch-article-link-face)
		    ((eq type 'url) 'navi2ch-article-url-face))))
    (add-text-properties start end
			 (list 'face face
			       'help-echo #'navi2ch-article-help-echo
			       'link t
			       'mouse-face navi2ch-article-mouse-face
			       type value)
			 object)
    (add-text-properties start (1+ start)
			 (list 'link-head t)
			 object)))

(defsubst navi2ch-article-set-link-property ()
  ">>1 とか http:// に property を付ける"
  (goto-char (point-min))
  (let* ((pref-depth (regexp-opt-depth navi2ch-article-number-prefix-regexp))
	 (sep-depth (regexp-opt-depth navi2ch-article-number-separator-regexp))
	 (number-func
	  (lambda (match)
	    (navi2ch-article-set-link-property-subr
	     (match-beginning 0) (match-end 0)
	     'number (navi2ch-match-string-no-properties (1+ pref-depth)))
	    (while (looking-at (concat navi2ch-article-number-separator-regexp
				       navi2ch-article-number-number-regexp))
	      (navi2ch-article-set-link-property-subr
	       (match-beginning (1+ sep-depth)) (match-end (1+ sep-depth))
	       'number (navi2ch-match-string-no-properties (1+ sep-depth)))
	      (goto-char (max (1+ (match-beginning 0)) (match-end 0))))))
	 (url-func
	  (lambda (url)
	    (when (string-match "\\`\\(h?t?tp\\)\\(s?:\\)" url)
	      (replace-match "http\\2" nil nil url))))
	 (alist (navi2ch-regexp-alist-to-number-alist
		 (append
		  navi2ch-article-link-regexp-alist
		  (list (cons (concat navi2ch-article-number-prefix-regexp
				      navi2ch-article-number-number-regexp)
			      number-func)
			(cons navi2ch-article-url-regexp
			      url-func)))))
	 match rep literal)
    (while (setq match (navi2ch-re-search-forward-regexp-alist alist nil t))
      (setq rep (cdr match)
	    literal nil)
      (when (functionp rep)
	(save-match-data
	  (setq rep (funcall rep (navi2ch-match-string-no-properties 0))
		literal t)))
      (when (stringp rep)
	(let ((start (match-beginning 0))
	      (end (match-end 0))
	      (url (navi2ch-match-string-no-properties 0)))
	  (when (string-match (concat "\\`" (car match) "\\'") url)
	    (setq url (replace-match rep nil literal url))
	    (navi2ch-article-set-link-property-subr
	     start end 'url url))
	  (goto-char (max (1+ start) end)))))))

(defsubst navi2ch-article-put-cite-face ()
  (goto-char (point-min))
  (while (re-search-forward navi2ch-article-citation-regexp nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0)
		       'face 'navi2ch-article-citation-face)))

(defsubst navi2ch-article-arrange-message ()
  (goto-char (point-min))
  (let ((id (cdr (assq 'id navi2ch-article-current-board))))
    (when (or (member id navi2ch-article-enable-fill-list)
	      (and (not (member id navi2ch-article-disable-fill-list))
		   navi2ch-article-enable-fill))
      (set-hard-newline-properties (point-min) (point-max))
      (let ((fill-column (- (window-width) 5))
	    (use-hard-newlines t))
	(fill-region (point-min) (point-max)))))
  (run-hooks 'navi2ch-article-arrange-message-hook))

(defsubst navi2ch-article-insert-message (num alist)
  (let ((p (point)))
    (insert (funcall navi2ch-article-header-format-function
                     num
                     (cdr (assq 'name alist))
                     (cdr (assq 'mail alist))
                     (cdr (assq 'date alist))))
    (put-text-property p (1+ p) 'current-number num)
    (setq p (point))
    (insert (cdr (assq 'data alist)) "\n")
    (save-excursion
      (save-restriction
	(narrow-to-region p (point))
	;; (navi2ch-article-cleanup-message) ; やっぱ遅い
	(put-text-property (point-min) (point-max) 'face
			   'navi2ch-article-face)
	(navi2ch-article-put-cite-face)
	(navi2ch-article-set-link-property)
        (if navi2ch-article-auto-decode-p
            (navi2ch-article-auto-decode-encoded-section))
	(navi2ch-article-arrange-message))))
  (funcall navi2ch-article-insert-message-separator-function)
  (insert "\n"))

(defun navi2ch-article-insert-messages (list range)
  "LIST を整形して挿入する"
  (let ((msg (if navi2ch-article-message-filter-mode
		 "filtering and inserting current messages..."
	       "inserting current messages..."))
	(len (length list))
	(hide (cdr (assq 'hide navi2ch-article-current-article)))
	(imp (cdr (assq 'important navi2ch-article-current-article)))
	(unfilter (cdr (assq 'unfilter navi2ch-article-current-article)))
	(cache (cdr (assq 'cache navi2ch-article-message-filter-cache)))
	(reps (cdr (assq 'replace navi2ch-article-message-filter-cache)))
	(f-hide (cdr (assq 'hide navi2ch-article-message-filter-cache)))
	(f-imp (cdr (assq 'important navi2ch-article-message-filter-cache)))
	(aborn (cdr (assq 'aborn navi2ch-article-message-filter-cache)))
	(progress 0)
	(percent 0))
    (message msg)
    (if navi2ch-article-message-filter-mode
	(setq hide (navi2ch-union hide f-hide)
	      imp (navi2ch-union imp f-imp))
      (setq hide (navi2ch-set-difference hide f-hide)
	    imp (navi2ch-set-difference imp f-imp)))
    (dolist (x list)
      (let* ((num (car x))
	     (alist (cdr x))
	     (rep (cdr (assq num reps)))
	     suppress)
        (when (and alist
		   (cond (navi2ch-article-hide-mode
			  (memq num hide))
			 (navi2ch-article-important-mode
			  (memq num imp))
			 (t
			  (and (navi2ch-article-inside-range-p num range len)
			       (not (memq num hide))))))
          (when (stringp alist)
            (setq alist (navi2ch-article-parse-message alist))
	    ;; 新しく「あぼーん」されたレスはキャッシュをクリア
	    (when (and (not (memq num aborn))
		       (string= "あぼーん" (cdr (assq 'date alist))))
	      (setq cache (delq num cache)
		    unfilter (delq num unfilter))
	      (setq aborn (cons num aborn))
	      (setq navi2ch-article-message-filter-cache
		    (navi2ch-put-alist 'aborn
				       aborn
				       navi2ch-article-message-filter-cache)))
	    ;; 置換前のレスをキャッシュに退避
	    (when rep
	      (dolist (slot rep)
		(setcdr (cdr slot) (list (cdr (assq (car slot) alist)))))
	      (navi2ch-put-alist num rep reps)
	      (navi2ch-put-alist 'replace
				 reps
				 navi2ch-article-message-filter-cache)))
	  (if (or (not navi2ch-article-message-filter-mode)
		  (memq num unfilter))
	      ;; 置換前のレスをキャッシュから復元
	      (dolist (slot rep)
		(let ((org (cddr slot)))
		  (when org
		    (navi2ch-put-alist (car slot) (car org) alist))))
	    (if (and navi2ch-article-use-message-filter-cache
		     (memq num cache))
		;; 置換後のレスをキャッシュから抽出
		(dolist (slot rep)
		  (let ((new (cadr slot)))
		    (when new
		      (setq alist (navi2ch-put-alist (car slot) new
						     alist)))))
	      ;; フィルタ処理の本体
	      (let ((alist-old (copy-alist alist)))
		(let (filter-result)
		  (setq filter-result
			(let ((filtered (navi2ch-article-apply-message-filters
					 (navi2ch-put-alist 'number num alist))))
			  (when filtered
			    (cond ((stringp filtered)
				   (let ((case-fold-search nil))
				     (navi2ch-put-alist 'name filtered alist)
				     (navi2ch-put-alist 'data filtered alist)
				     (navi2ch-put-alist 'mail
							(if (string-match "sage"
									  (cdr (assq 'mail alist)))
							    "sage"
							  "")
							alist)
				     (navi2ch-put-alist 'date
							(navi2ch-replace-string " ID:[^ ]+"
										" ID:???"
										(cdr (assq 'date alist)))
							alist)))
				  ((eq filtered 'hide)
				   'hide)
				  ((eq filtered 'important)
				   'important)))))
		  (if (and (eq filter-result 'hide)
			   (not navi2ch-article-hide-mode))
		      (progn
			(setq hide (cons num hide))
			(setq navi2ch-article-current-article
			      (navi2ch-put-alist 'hide
						 hide
						 navi2ch-article-current-article))
			(setq suppress t)
			(when navi2ch-article-use-message-filter-cache
			  (setq f-hide (cons num f-hide))
			  (setq navi2ch-article-message-filter-cache
				(navi2ch-put-alist 'hide
						   f-hide
						   navi2ch-article-message-filter-cache))))
		    (when (and (eq filter-result 'important)
			       (not navi2ch-article-important-mode))
		      (setq imp (cons num imp))
		      (setq navi2ch-article-current-article
			    (navi2ch-put-alist 'important
					       imp
					       navi2ch-article-current-article))
		      (when navi2ch-article-use-message-filter-cache
			(setq f-imp (cons num f-imp))
			(setq navi2ch-article-message-filter-cache
			      (navi2ch-put-alist 'important
						 f-imp
						 navi2ch-article-message-filter-cache))))))
		;; フィルタ処理の結果をキャッシュに登録
		(when navi2ch-article-use-message-filter-cache
		  (unless (memq num cache)
		    (setq cache (cons num cache))
		    (setq navi2ch-article-message-filter-cache
			  (navi2ch-put-alist 'cache
					     cache
					     navi2ch-article-message-filter-cache)))
		  (dolist (slot alist-old)
		    (let ((new (cdr (assq (car slot) alist)))
			  (org (or (cddr (assq (car slot) rep))
				   (list (cdr slot)))))
		      (unless (equal new (car org))
			(setq rep
			      (navi2ch-put-alist (car slot)
						 (cons new org)
						 rep)))))
		  (when rep
		    (setq reps (navi2ch-put-alist num rep reps))
		    (setq navi2ch-article-message-filter-cache
			  (navi2ch-put-alist 'replace
					     reps
					     navi2ch-article-message-filter-cache)))))))
	  (setcdr x (navi2ch-put-alist 'point (point-marker) alist))
	  ;; (setcdr x (navi2ch-put-alist 'point (point) alist))
	  (unless suppress
	    (navi2ch-article-insert-message num alist)))
	;; 進捗表示
	(and (> (setq progress (+ progress 100)) 10000)
	     (/= (/ progress len) percent)
	     (navi2ch-no-logging-message "%s%d%%" msg (setq percent (/ progress len))))))
    (message "Garbage collecting...")
    (garbage-collect)	    ; navi2ch-parse-message は大量にゴミを残す
    (message "Garbage collecting...done")
    (message "%sdone" msg)))

(defun navi2ch-article-reinsert-partial-messages (start &optional end)
  "START 番目から、最後または END 番目までのレスを挿入しなおす。"
  (let* ((nums (mapcar #'car navi2ch-article-message-list))
	 (len (length nums))
	 (last (car (last nums)))
	 list visible start-point end-point)
    (when (minusp start)
      (setq start (+ start last 1)))
    (if (null end)
	(setq end last)
      (when (minusp end)
	(setq end (+ end last 1)))
      (when (> start end)
	(setq start (prog1 end
		      (setq end start)))))
    (catch 'loop
      (dolist (num (nreverse nums))
	(cond
	 ((< num start)
	  (throw 'loop nil))
	 ((and (<= num end)
	       (or navi2ch-article-hide-mode
		   navi2ch-article-important-mode
		   (navi2ch-article-inside-range-p num
						   navi2ch-article-view-range
						   len)))
	  (let ((slot (assq num navi2ch-article-message-list)))
	    (when slot
	      (setq list (cons slot list))))))))
    (setq visible (navi2ch-article-get-visible-numbers))
    (while (and visible
		(< (car visible) start))
      (setq visible (cdr visible)))
    (when visible
      (setq start-point
	    (cdr (assq 'point (navi2ch-article-get-message (car visible))))))
    (while (and visible
		(<= (car visible) end))
      (setq visible (cdr visible)))
    (when visible
      (setq end-point
	    (cdr (assq 'point (navi2ch-article-get-message (car visible)))))
      (set-marker-insertion-type end-point t))
    (if (null start-point)
	(goto-char (point-max))
      (goto-char start-point)
      (delete-region start-point (or end-point (point-max))))
    (prog1 (navi2ch-article-insert-messages list nil)
      (when end-point
	(set-marker-insertion-type end-point nil)))))

(defun navi2ch-article-apply-message-filters (alist)
  (let (score)
    (catch 'loop
      (dolist (filter navi2ch-article-message-filter-list)
	(let ((result (funcall filter alist)))
	  (when result
	    (if (numberp result)
		(setq score (+ (or score 0) result))
	      (throw 'loop result)))))
      (when score
	(cond
	 ((and (car navi2ch-article-message-replace-below)
	       navi2ch-article-message-hide-below
	       (< score (min (car navi2ch-article-message-replace-below)
			     navi2ch-article-message-hide-below)))
	  (if (< (car navi2ch-article-message-replace-below)
		 navi2ch-article-message-hide-below)
	      (cdr navi2ch-article-message-replace-below)
	    'hide))
	 ((and (car navi2ch-article-message-replace-below)
	       (< score (car navi2ch-article-message-replace-below)))
	  (cdr navi2ch-article-message-replace-below))
	 ((and navi2ch-article-message-hide-below
	       (< score navi2ch-article-message-hide-below))
	  'hide)
	 ((and navi2ch-article-message-add-important-above
	       (> score navi2ch-article-message-add-important-above))
	  'important))))))

(defun navi2ch-article-message-filter-by-name (alist)
  (when navi2ch-article-message-filter-by-name-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-name-alist
     (cdr (assq 'name alist)))))

(defun navi2ch-article-message-filter-by-message (alist)
  (let ((l (append navi2ch-article-default-message-filter-by-message-alist
		   navi2ch-article-message-filter-by-message-alist)))
    (when l
      (prog1
	  (navi2ch-article-message-filter-subr
	   l (cdr (assq 'data alist)))
	(dolist (elt navi2ch-article-default-message-filter-by-message-alist)
	  (setq l (delq elt l)))
	(setq navi2ch-article-message-filter-by-message-alist l)))))

(defun navi2ch-article-message-filter-by-id (alist)
  (let ((case-fold-search nil))
    (when (and navi2ch-article-message-filter-by-id-alist
	       (string-match " ID:\\([^ ]+\\)"
			     (cdr (assq 'date alist))))
      (navi2ch-article-message-filter-subr
       navi2ch-article-message-filter-by-id-alist
       (match-string 1 (cdr (assq 'date alist)))))))

(defun navi2ch-article-message-filter-by-mail (alist)
  (when navi2ch-article-message-filter-by-mail-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-mail-alist
     (cdr (assq 'mail alist)))))

(defun navi2ch-article-message-filter-by-subject (alist)
  (let ((slot (assq 'subject navi2ch-article-message-filter-cache)))
    (if slot
	(cdr slot)
      (let ((subject (or (cdr (assq 'subject navi2ch-article-current-article))
			 (if (eq (cdr (assq 'number alist)) 1)
			     (cdr (assq 'subject alist))
			   (let ((msg (navi2ch-article-get-message 1)))
			     (when (stringp msg)
			       (setq msg (navi2ch-article-parse-message msg)))
			     (cdr (assq 'subject msg)))))))
	(when subject
	  (let ((result (when navi2ch-article-message-filter-by-subject-alist
			  (navi2ch-article-message-filter-subr
			   navi2ch-article-message-filter-by-subject-alist
			   subject))))
	    (when navi2ch-article-use-message-filter-cache
	      (setq navi2ch-article-message-filter-cache
		    (navi2ch-put-alist 'subject
				       result
				       navi2ch-article-message-filter-cache)))
	    result))))))

(defun navi2ch-article-message-filter-subr (rules string)
  (let (score)
    (catch 'loop
      (dolist (rule rules)
	(let* ((char (and (listp (car rule))
			  (string-to-char (symbol-name (cadar rule)))))
	       (case-fold-search (and char
				      (eq char
					  (setq char (downcase char)))))
	       (regexp (cond
			((null char)
			 (regexp-quote (car rule)))
			((eq char ?r)
			 (caar rule))
			((eq char ?s)
			 (regexp-quote (caar rule)))
			((eq char ?e)
			 (concat "\\`" (regexp-quote (caar rule)) "\\'"))
			((eq char ?f)
			 (regexp-quote
			  (apply #'concat
				 (split-string
				  (japanese-hankaku (caar rule) t))))))))
	  (when (and regexp
		     (string-match regexp
				   (if (eq char ?f)
				       (apply #'concat
					      (split-string
					       (japanese-hankaku string t)))
				     string)))
	    ;; 適用したフィルタ条件を age
	    (when (and navi2ch-article-sort-message-filter-rules
		       (not (eq rule (car rules))))
	      (setcdr rules (cons (car rules) (delq rule (cdr rules))))
	      (setcar rules rule))
	    (if (numberp (cdr rule))
		(setq score (+ (or score 0) (cdr rule)))
	      (throw 'loop
		     (if (and char
			      (stringp (cdr rule)))
			 (navi2ch-expand-newtext (cdr rule) string)
		       (cdr rule)))))))
      score)))

(defun navi2ch-article-default-header-format-function (number name mail date)
  "デフォルトのヘッダをフォーマットする関数
  ヘッダのface を付けるのもここで。"
  (when (string-match (concat "\\`" navi2ch-article-number-number-regexp
			      "\\'")
		      name)
    (navi2ch-article-set-link-property-subr (match-beginning 0)
					    (match-end 0)
					    'number
					    (match-string 0 name)
					    name))
  (let ((from-header (navi2ch-propertize "From: "
					 'face 'navi2ch-article-header-face))
        (from (navi2ch-propertize (concat (format "[%d] " number)
					  name
					  (format " <%s>\n" mail))
				  'face 'navi2ch-article-header-contents-face))
        (date-header (navi2ch-propertize "Date: "
					 'face 'navi2ch-article-header-face))
	(date (navi2ch-propertize (if navi2ch-article-dispweek
				      ;;曜日表示する？
				      (navi2ch-article-appendweek date)
				    date)
				  'face
				  'navi2ch-article-header-contents-face))
	(start 0) next)
    (while start
      (setq next
	    (next-single-property-change start 'navi2ch-fusianasan-flag from))
      (when (get-text-property start 'navi2ch-fusianasan-flag from)
	(add-text-properties start (or next (length from))
			     '(face navi2ch-article-header-fusianasan-face)
			     from))
      (setq start next))
    (concat from-header from date-header date "\n\n")))

(defun navi2ch-article-appendweek (d)
  "YY/MM/DD形式の日付に曜日を足す。"
  (let ((youbi '("日" "月" "火" "水" "木" "金" "土"))
	year month day et dt time date)
    ;; "あぼーん"とかIDとか旧形式の日付にも対応しているはず．
    ;; 正規表現に工夫が必要かも…
    (if (string-match "^\\([0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\) \\([A-Za-z0-9: +/?]+\\)$" d)
	(progn
	  (setq time (match-string 2 d))
	  (setq date (match-string 1 d))
	  (string-match "\\(.+\\)/\\(.*\\)/\\(.*\\)" date)
	  (setq year (+ (string-to-number (match-string 1 date)) 2000))
	  (setq month (string-to-number (match-string 2 date)))
	  (setq day (string-to-number (match-string 3 date)))
	  (setq et (encode-time 0 0 0 day month year))
	  (setq dt (decode-time et))
	  ;; 頭に20を足してYYYY形式にする(2100年問題ボッパツ予定)
	  (concat "20" date "("  (nth (nth 6 dt) youbi) ") " time ))
      d)))

(defun navi2ch-article-expunge-buffers (&optional num)
  "スレのバッファを削除して NUM 個にする。
NUM を指定しない場合は `navi2ch-article-max-buffers' を使用。
NUM が 0 以上のときは sticky バッファは削除しない。
NUM が -1 のときは sticky バッファも含めてすべて削除。"
  (interactive "P")
  (when (not (numberp num)) ; C-u のみの時4個にしたいわけじゃないと思われ
    (setq num navi2ch-article-max-buffers))
  (let ((buffer-num (length (navi2ch-article-buffer-list)))
	buffer-list)
    (when (> buffer-num num)
      (if (< num 0)
	  (setq buffer-list (navi2ch-article-buffer-list))
	(save-excursion
	  (dolist (buf (navi2ch-article-buffer-list))
	    (set-buffer buf)
	    (unless navi2ch-article-sticky-mode
	      (push buf buffer-list)))))
      (catch 'loop
	(dolist (buf buffer-list)
	  (kill-buffer buf)
	  (setq buffer-num (1- buffer-num))
	  (when (<= buffer-num num)
	    (throw 'loop nil)))))))

(defun navi2ch-article-view-article (board
				     article
				     &optional force number max-line dont-display)
  "スレを見る。FORCE で強制読み込み MAX-LINE で読み込む行数を指定。
ただ `navi2ch-article-max-line' とは逆で t で全部読み込み。
DONT-DISPLAY が non-nil のときはスレバッファを表示せずに実行。"
  (let ((buf-name (navi2ch-article-get-buffer-name board article))
	(navi2ch-article-max-line (cond ((numberp max-line) max-line)
					(max-line nil)
					(t navi2ch-article-max-line)))
	(window-configuration (current-window-configuration))
	buffer list)
    (unwind-protect
	(progn
	  (when (and (null (get-buffer buf-name))
		     navi2ch-article-auto-expunge
		     (> navi2ch-article-max-buffers 0))
	    (navi2ch-article-expunge-buffers (1- navi2ch-article-max-buffers)))
	  (setq buffer (get-buffer-create buf-name))
	  (if dont-display
	      (set-buffer buffer)
	    (switch-to-buffer buffer))
	  (if (eq major-mode 'navi2ch-article-mode)
	      (setq list (navi2ch-article-sync force nil))
	    (setq navi2ch-article-current-board board
		  navi2ch-article-current-article article)
	    (when navi2ch-article-auto-range
	      (if (file-exists-p (navi2ch-article-get-file-name board article))
		  (setq navi2ch-article-view-range
			navi2ch-article-exist-message-range)
		(setq navi2ch-article-view-range
		      navi2ch-article-new-message-range)))
	    (when navi2ch-article-auto-activate-message-filter
	      (setq navi2ch-article-message-filter-mode t
		    navi2ch-article-message-filter-cache
		    (navi2ch-article-load-message-filter-cache)))
	    (setq list (navi2ch-article-sync force 'first))
	    (navi2ch-article-mode))
	  (when (and number
		     (not (equal (navi2ch-article-get-current-number) number)))
	    (navi2ch-article-goto-number number t))
	  (navi2ch-history-add navi2ch-article-current-board
			       navi2ch-article-current-article)
	  (navi2ch-bm-update-article navi2ch-article-current-board
				     navi2ch-article-current-article))
      (when (and (buffer-live-p buffer)
		 (or (not (eq (navi2ch-get-major-mode buffer)
			      'navi2ch-article-mode))
		     (null (with-current-buffer buffer
			     navi2ch-article-message-list))))
	(set-window-configuration window-configuration)
	(kill-buffer buffer)
	(setq list nil)))
    list))

(defun navi2ch-article-view-article-from-file (file)
  "FILE からスレを見る。"
  (setq file (expand-file-name file))
  (let* ((board (list (cons 'id "navi2ch")
		      (cons 'uri (navi2ch-filename-to-url
				  (file-name-directory file)))
		      (cons 'name navi2ch-board-name-from-file)))
	 (article (list (cons 'artid (file-name-sans-extension
				      (file-name-nondirectory file)))))
         (buf-name (navi2ch-article-get-buffer-name board article)))
    (if (get-buffer buf-name)
        (progn
          (switch-to-buffer buf-name)
	  nil)
      (if (and navi2ch-article-auto-expunge
	       (> navi2ch-article-max-buffers 0))
	  (navi2ch-article-expunge-buffers (1- navi2ch-article-max-buffers)))
      (switch-to-buffer (get-buffer-create buf-name))
      (setq navi2ch-article-current-board board
            navi2ch-article-current-article article)
      (when navi2ch-article-auto-range
        (setq navi2ch-article-view-range
              navi2ch-article-new-message-range))
      (when navi2ch-article-auto-activate-message-filter
	(setq navi2ch-article-message-filter-mode t
	      navi2ch-article-message-filter-cache
	      (navi2ch-article-load-message-filter-cache)))
      (prog1
	  (navi2ch-article-sync-from-file)
	(navi2ch-article-set-mode-line)
	(navi2ch-article-mode)))))

(easy-menu-define navi2ch-article-mode-menu
  navi2ch-article-mode-map
  "Menu used in navi2ch-article"
  navi2ch-article-mode-menu-spec)

(defun navi2ch-article-setup-menu ()
  (easy-menu-add navi2ch-article-mode-menu))

(defun navi2ch-article-mode ()
  "\\{navi2ch-article-mode-map}"
  (interactive)
  (setq major-mode 'navi2ch-article-mode)
  (setq mode-name "Navi2ch Article")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)
  (use-local-map navi2ch-article-mode-map)
  (navi2ch-article-setup-menu)
  (setq navi2ch-article-point-stack nil)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'navi2ch-article-kill-buffer-hook t t)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'navi2ch-article-display-link-minibuffer nil t)
  (run-hooks 'navi2ch-article-mode-hook))

(defun navi2ch-article-kill-buffer-hook ()
  (navi2ch-bm-update-article navi2ch-article-current-board
			     navi2ch-article-current-article
			     'cache)
  (navi2ch-article-save-info))

(defun navi2ch-article-exit (&optional kill)
  (interactive "P")
  ;; (navi2ch-article-add-number)
  (run-hooks 'navi2ch-article-exit-hook)
  (navi2ch-article-save-info)
  (let ((buf (current-buffer)))
    (if (or kill
	    (null navi2ch-article-message-list))
        (progn
          (delete-windows-on buf)
          (kill-buffer buf))
      (unless (eq (selected-window)
		  (next-window (selected-window) 'never))
	(delete-windows-on buf)))
    ;;  (bury-buffer navi2ch-article-buffer-name)
    (let ((board-win (get-buffer-window navi2ch-board-buffer-name))
	  (board-buf (get-buffer navi2ch-board-buffer-name)))
      (cond (board-win (select-window board-win))
	    (board-buf (switch-to-buffer board-buf))
	    (t (navi2ch-list))))))

(defun navi2ch-article-goto-current-board (&optional kill)
  "スレッドと同じ板へ移動"
  (interactive "P")
  (let ((board navi2ch-article-current-board))
    (navi2ch-article-exit kill)
    (navi2ch-board-select-board board)))

(defun navi2ch-article-fix-range (num)
  "navi2ch-article-view-range を num が含まれる範囲に変更"
  (let ((len (length navi2ch-article-message-list))
	(range navi2ch-article-view-range))
    (unless (navi2ch-article-inside-range-p num range len)
      (let ((first (car range))
	    (last (+ navi2ch-article-fix-range-diff (- len num))))
	(setq navi2ch-article-view-range (cons first last))))))

(defun navi2ch-article-sync (&optional force first number)
  "スレを更新する。force なら強制。
first が nil ならば、ファイルが更新されてなければ何もしない"
  (interactive "P")
  (when (not (navi2ch-board-from-file-p navi2ch-article-current-board))
    (run-hooks 'navi2ch-article-before-sync-hook)
    (let* ((list navi2ch-article-message-list)
           (article navi2ch-article-current-article)
           (board navi2ch-article-current-board)
           (navi2ch-net-force-update (or navi2ch-net-force-update
                                         force))
           (file (navi2ch-article-get-file-name board article))
           (old-size (nth 7 (file-attributes file)))
           header start)
      (when first
        (setq article (navi2ch-article-load-info)))
      (navi2ch-article-set-mode-line)
      (if (and (cdr (assq 'kako article))
	       (file-exists-p file)
	       (not (and force	  ; force が指定されない限りsyncしない
			 (y-or-n-p "re-sync kako article?"))))
	  (setq navi2ch-article-current-article article)
	(let ((ret (navi2ch-article-update-file board article force)))
	  (setq article (nth 0 ret)
		navi2ch-article-current-article article
		header (nth 1 ret))))
      (prog1
	  ;; 更新できたら
	  (when (or (and first (file-exists-p file))
		    (and header
			 (not (navi2ch-net-get-state 'not-updated header))
			 (not (navi2ch-net-get-state 'error header))))
	    (if (or first
		    (navi2ch-net-get-state 'aborn header)
		    (navi2ch-net-get-state 'kako header)
		    (not navi2ch-article-enable-diff))
		(setq list (navi2ch-article-get-message-list file))
	      (setq start (max 1
			       (- (1+ (length list))
				  (or (cdr navi2ch-article-view-range) 0))))
	      (setq list (navi2ch-article-append-message-list
			  list (navi2ch-article-get-message-list
				file old-size))))
	    (setq navi2ch-article-message-list list)
	    (let ((num (or number (cdr (assq 'number article)))))
	      (when (and navi2ch-article-fix-range-when-sync num)
		(navi2ch-article-fix-range num)
		(when (and navi2ch-article-view-range
			   start)
		  (setq start
			(min start
			     (max 1
				  (- (1+ (length list))
				     (cdr navi2ch-article-view-range))))))))
	    (unless first
	      (navi2ch-article-save-number))
	    (when (or (eq start 1)
		      navi2ch-article-hide-mode
		      navi2ch-article-important-mode)
	      (setq start nil))
	    (setq navi2ch-article-hide-mode nil
		  navi2ch-article-important-mode nil)
	    (let ((buffer-read-only nil))
	      (if start
		  (navi2ch-article-reinsert-partial-messages start)
		(erase-buffer)
		(navi2ch-article-insert-messages list
						 navi2ch-article-view-range)))
	    (navi2ch-article-load-number)
	    (navi2ch-article-save-info board article first)
	    (navi2ch-article-set-mode-line)
	    (run-hooks 'navi2ch-article-after-sync-hook)
	    list)
	(when (and navi2ch-article-fix-range-when-sync number)
	  (navi2ch-article-fix-range number)
	  (navi2ch-article-redraw))
	(navi2ch-article-goto-number (or number
					 (navi2ch-article-get-current-number)))
	(navi2ch-article-set-summary-element board article nil)))))

(defun navi2ch-article-fetch-article (board article &optional force)
  (if (get-buffer (navi2ch-article-get-buffer-name board article))
      (save-excursion
	(navi2ch-article-view-article board article force nil nil t))
    (let (ret header file)
      (setq article (navi2ch-article-load-info board article)
	    file (navi2ch-article-get-file-name board article))
      (unless (and (cdr (assq 'kako article))
		   (file-exists-p file)
		   (not (and force ; force が指定されない限り sync しない
			     (y-or-n-p "re-sync kako article?"))))
	(setq ret (navi2ch-article-update-file board article force)
	      article (nth 0 ret)
	      header (nth 1 ret))
	(when (and header
		   (not (navi2ch-net-get-state 'not-updated header))
		   (not (navi2ch-net-get-state 'error header)))
	  (navi2ch-article-save-info board article)
	  (navi2ch-article-set-summary-element board article t)
	  t)))))

(defun navi2ch-article-check-message-suppression (board article start
							&optional end)
  (let ((buffer (get-buffer (navi2ch-article-get-buffer-name board article)))
	suppressed)
    (if buffer
	(with-current-buffer buffer
	  (when navi2ch-article-message-filter-mode
	    (let ((res (length navi2ch-article-message-list)))
	      (when (and (>= res start)
			 (or (null end)
			     (<= res end)))
		(let ((hide (cdr (assq 'hide navi2ch-article-current-article)))
		      (i start))
		  (while (memq i hide)
		    (setq i (1+ i)))
		  (when (> i res)
		    (setq suppressed res)))))))
      (when navi2ch-article-auto-activate-message-filter
	(with-temp-buffer
	  (setq navi2ch-article-current-board board)
	  (setq navi2ch-article-message-list
		(navi2ch-article-get-message-list
		 (navi2ch-article-get-file-name
		  navi2ch-article-current-board
		  article)))
	  (message "filtering current messages...")
	  (let ((res (length navi2ch-article-message-list)))
	    (when (and (>= res start)
		       (or (null end)
			   (<= res end)))
	      (setq navi2ch-article-current-article
		    (navi2ch-article-load-info
		     navi2ch-article-current-board
		     article))
	      (setq navi2ch-article-message-filter-cache
		    (navi2ch-article-load-message-filter-cache
		     navi2ch-article-current-board
		     navi2ch-article-current-article))
	      (let ((hide (cdr (assq 'hide navi2ch-article-current-article)))
		    (f-hide (cdr (assq 'hide navi2ch-article-message-filter-cache))))
		(catch 'loop
		  (dolist (x (nthcdr (1- start) navi2ch-article-message-list))
		    (if (eq (navi2ch-article-apply-message-filters
			     (navi2ch-put-alist
			      'number
			      (car x)
			      (navi2ch-article-parse-message (cdr x))))
			    'hide)
			(progn
			  (setq hide (cons (car x) hide))
			  (setq navi2ch-article-current-article
				(navi2ch-put-alist
				 'hide
				 hide
				 navi2ch-article-current-article))
			  (when navi2ch-article-use-message-filter-cache
			    (setq f-hide (cons (car x) f-hide))
			    (setq navi2ch-article-message-filter-cache
				  (navi2ch-put-alist
				   'hide
				   f-hide
				   navi2ch-article-message-filter-cache))))
		      (throw 'loop nil)))
		  (setq suppressed res)))
	      (navi2ch-article-save-info
	       navi2ch-article-current-board
	       navi2ch-article-current-article)
	      (navi2ch-article-save-message-filter-cache
	       navi2ch-article-current-board
	       navi2ch-article-current-article
	       navi2ch-article-message-filter-cache)
	      (message "Garbage collecting...")
	      (garbage-collect)	; `navi2ch-article-parse-message' のゴミ掃除
	      (message "Garbage collecting...done")))
	  (message "filtering current messages...done"))))
    suppressed))

(defun navi2ch-article-get-last-read-number (board article)
  (let ((buffer (get-buffer (navi2ch-article-get-buffer-name board article)))
	hide num)
    (if buffer
	(with-current-buffer buffer
	  (setq hide (cdr (assq 'hide navi2ch-article-current-article))
		num (if (or navi2ch-article-hide-mode
			    navi2ch-article-important-mode)
			(cdr (assq 'number navi2ch-article-current-article))
		      (navi2ch-article-get-current-number))))
      (setq article (navi2ch-article-load-info board article))
      (setq hide (cdr (assq 'hide article))
	    num (cdr (assq 'number article))))
    (when num
      (while (memq (1+ num) hide)
	(setq num (1+ num)))
      num)))

(defun navi2ch-article-get-readcgi-raw-url (board article &optional start)
  (let ((url (navi2ch-article-to-url board article))
	(file (navi2ch-article-get-file-name board article))
	size)
    (if start
	(setq size (nth 7 (file-attributes file)))
      (setq start 0
	    size 0))
    (format "%s?raw=%s.%s" url start size)))

(defun navi2ch-article-update-file (board article &optional force)
  "BOARD, ARTICLE に対応するファイルを更新する。
返り値は \(article header) のリスト。"
  (let (header)
    (unless navi2ch-offline
      (let ((navi2ch-net-force-update (or navi2ch-net-force-update
					  force))
	    (file (navi2ch-article-get-file-name board article))
	    start)
	(when (and (file-exists-p file)
		   navi2ch-article-enable-diff)
	  (setq start (1+ (navi2ch-count-lines-file file))))
	(setq header (navi2ch-multibbs-article-update board article start))
	(when header
	  (unless (or (navi2ch-net-get-state 'not-updated header)
		      (navi2ch-net-get-state 'error header))
	    (setq article (navi2ch-put-alist 'time
					     (or (cdr (assoc "Last-Modified"
							     header))
						 (cdr (assoc "Date"
							     header)))
					     article)))
	  (when (navi2ch-net-get-state 'kako header)
	    (setq article (navi2ch-put-alist 'kako t article))))))
    (list article header)))

(defun navi2ch-article-sync-from-file ()
  "from-file なスレを更新する。"
  (let ((file (navi2ch-article-get-file-name navi2ch-article-current-board
					     navi2ch-article-current-article)))
    (when (and (navi2ch-board-from-file-p navi2ch-article-current-board)
	       (file-exists-p file))
      (let ((list (navi2ch-article-get-message-list file))
	    (range navi2ch-article-view-range)
	    (buffer-read-only nil))
	(erase-buffer)
	(navi2ch-article-insert-messages list range)
	(prog1
	    (setq navi2ch-article-message-list list)
	  (navi2ch-article-goto-number 1))))))

(defun navi2ch-article-set-mode-line ()
  (let ((article navi2ch-article-current-article)
        (x (cdr (car navi2ch-article-message-list))))
    (unless (assq 'subject article)
      (setq article (navi2ch-put-alist
                     'subject
                     (cdr (assq 'subject
                                (if (stringp x)
                                    (navi2ch-article-parse-message x)
                                  x)))
                     article)
            navi2ch-article-current-article article))
    (setq navi2ch-mode-line-identification
          (format "%s (%s/%s) [%s]"
                  (or (cdr (assq 'subject article))
		      navi2ch-bm-empty-subject)
                  (let ((l (length navi2ch-article-message-list)))
		    (if (= l 0) "-"
		      (number-to-string l)))
                  (or (cdr (assq 'response article)) "-")
                  (cdr (assq 'name navi2ch-article-current-board)))))
  (navi2ch-set-mode-line-identification))

(defun navi2ch-article-sync-disable-diff (&optional force)
  (interactive "P")
  (let ((navi2ch-article-enable-diff nil))
    (navi2ch-article-sync force)))

(defun navi2ch-article-redraw ()
  "現在表示してるスレを表示しなおす"
  (let ((buffer-read-only nil))
    (navi2ch-article-save-number)
    (erase-buffer)
    (navi2ch-article-insert-messages navi2ch-article-message-list
                                     navi2ch-article-view-range)
    (navi2ch-article-load-number)))

(defun navi2ch-article-select-view-range-subr ()
  "表示する範囲をキーボードメニューで選択する"
  (save-window-excursion
    (delete-other-windows)
    (let (buf
          (range navi2ch-article-view-range))
      (unwind-protect
	  (progn
	    (setq buf (get-buffer-create "*select view range*"))
	    (save-excursion
	      (set-buffer buf)
	      (erase-buffer)
	      (insert (format "   %8s %8s\n" "first" "last"))
	      (insert (format "0: %17s\n" "all range"))
	      (let ((i 1))
		(dolist (x navi2ch-article-view-range-list)
		  (insert (format "%d: %8d %8d\n" i (car x) (cdr x)))
		  (setq i (1+ i)))))
	    (display-buffer buf)
	    (let (n)
	      (setq n (navi2ch-read-char "input: "))
	      (when (or (< n ?0) (> n ?9))
		(error "%c is bad key" n))
	      (setq n (- n ?0))
	      (setq range
		    (if (eq n 0) nil
		      (nth (1- n) navi2ch-article-view-range-list)))))
	(if (bufferp buf)
	    (kill-buffer buf)))
      range)))

(defun navi2ch-article-redraw-range ()
  "表示する範囲を指定した後 redraw"
  (interactive)
  (setq navi2ch-article-view-range
        (navi2ch-article-select-view-range-subr))
  (navi2ch-article-redraw))

(defun navi2ch-article-save-number ()
  (unless (or navi2ch-article-hide-mode
              navi2ch-article-important-mode)
    (let ((num (navi2ch-article-get-current-number)))
      (when num
        (setq navi2ch-article-current-article
              (navi2ch-put-alist 'number
                                 num
                                 navi2ch-article-current-article))))))

(defun navi2ch-article-load-number ()
  (unless (or navi2ch-article-hide-mode
              navi2ch-article-important-mode)
    (let ((num (cdr (assq 'number navi2ch-article-current-article))))
      (navi2ch-article-goto-number (or num 1)))))

(defun navi2ch-article-save-info (&optional board article first)
  (let (ignore alist)
    (when (eq major-mode 'navi2ch-article-mode)
      (if (navi2ch-board-from-file-p (or board navi2ch-article-current-board))
	  (setq ignore t)
	(when (and navi2ch-article-message-list (not first))
	  (navi2ch-article-save-number))
	(or board (setq board navi2ch-article-current-board))
	(or article (setq article navi2ch-article-current-article))))
    (when (and (not ignore) board article)
      (let ((article-tmp (if navi2ch-article-save-info-wrapper-func
			     (funcall navi2ch-article-save-info-wrapper-func article)
			   article)))
	(setq alist (mapcar
		     (lambda (x)
		       (assq x article-tmp))
		     navi2ch-article-save-info-keys)))
      (navi2ch-save-info
       (navi2ch-article-get-info-file-name board article)
       alist)
      (navi2ch-article-save-message-filter-cache board article))))

(defun navi2ch-article-load-info (&optional board article)
  (let (ignore alist)
    (if (navi2ch-board-from-file-p (or board navi2ch-article-current-board))
	(setq ignore t)
      (or board (setq board navi2ch-article-current-board))
      (or article (setq article navi2ch-article-current-article)))
    (when (and (not ignore) board article)
      (setq alist (navi2ch-load-info
		   (navi2ch-article-get-info-file-name board article)))
      (dolist (x alist)
        (setq article (navi2ch-put-alist (car x) (cdr x) article)))
      article)))

(defun navi2ch-article-write-message (&optional sage)
  (interactive)
  (when (not (navi2ch-board-from-file-p navi2ch-article-current-board))
    (navi2ch-article-save-number)
    (navi2ch-message-write-message navi2ch-article-current-board
                                   navi2ch-article-current-article
				   nil sage)))

(defun navi2ch-article-write-sage-message ()
  (interactive)
  (navi2ch-article-write-message 'sage))

(defun navi2ch-article-str-to-num (str)
  "レス参照の文字列を数字か数字の list に変換"
  (cond ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" str)
	 (let* ((n1 (string-to-number (match-string 1 str)))
		(n2 (string-to-number (match-string 2 str)))
		(min (max (min n1 n2) 1))
		(i (min (max n1 n2)
			(1+ (length navi2ch-article-message-list))))
		list)
	   (while (>= i min)
	     (push i list)
	     (setq i (1- i)))
	   list))
	((string-match "\\([0-9]+,\\)+[0-9]+" str)
	 (mapcar 'string-to-number (split-string str ",")))
	(t (string-to-number str))))

(defun navi2ch-article-get-number-list (number-property &optional limit)
  (if (string-match "[^ ][^ ][^ ][^ ][^ ][^ ][^ ][^ ]" number-property)
      (let (nums)
	(dolist (msg navi2ch-article-message-list (nreverse nums))
	  (when (listp (cdr msg))
	    (let ((date (cdr (assq 'date (cdr msg))))
		  (name (cdr (assq 'name (cdr msg)))))
	      (when (or (and date
			     (string-match " ID:\\([^ ][^ ][^ ][^ ]+\\)"
					   ;; ID:??? はスルー
					   date)
			     (string-match (regexp-quote
					    (match-string 1 date))
					   number-property))
			(and name
			     (string-match "◆\\([^ ]+\\)" name)
			     (string-match (regexp-quote
					    (match-string 1 name))
					   number-property)))
		(if (and (numberp limit)
			 (>= (car msg) limit)
			 nums)
		    (return (car nums))
		  (push (car msg) nums)))))))
    (navi2ch-article-str-to-num (japanese-hankaku number-property))))

(defun navi2ch-article-select-current-link (&optional browse-p)
  (interactive "P")
  (let (prop)
    (cond ((setq prop (get-text-property (point) 'number))
	   (navi2ch-article-select-current-link-number
	    (navi2ch-article-get-number-list prop)
	    browse-p))
          ((setq prop (get-text-property (point) 'url))
           (navi2ch-article-select-current-link-url prop browse-p nil))
          ((setq prop (get-text-property (point) 'content))
	   (navi2ch-article-save-content)))))

(defun navi2ch-article-number-list-to-url (number-list)
  (navi2ch-article-to-url navi2ch-article-current-board
			  navi2ch-article-current-article
			  (if (numberp number-list)
			      number-list
			    (apply #'min number-list))
			  (if (numberp number-list)
			      number-list
			    (apply #'max number-list))
			  t))

(defun navi2ch-article-select-current-link-number (prop browse-p)
  ;; prop は「数の list」か「数」。
  (cond (browse-p
	 (navi2ch-browse-url-internal
	  (navi2ch-article-number-list-to-url prop)))
	((eq navi2ch-article-select-current-link-number-style 'popup)
	 (navi2ch-popup-article (if (listp prop) prop (list prop))))
	((eq navi2ch-article-select-current-link-number-style 'jump)
	 (navi2ch-article-goto-number (if (listp prop) (car prop) prop)
				      t t))
	;; (eq navi2ch-article-select-current-link-number-style 'auto)
	((numberp prop)
	 (let ((hide (cdr (assq 'hide navi2ch-article-current-article)))
	       (imp (cdr (assq 'important navi2ch-article-current-article)))
	       (range navi2ch-article-view-range)
	       (len (length navi2ch-article-message-list)))
	   (if (cond (navi2ch-article-hide-mode
		      (memq prop hide))
		     (navi2ch-article-important-mode
		      (memq prop imp))
		     (t
		      (and (navi2ch-article-inside-range-p prop range len)
			   (not (memq prop hide)))))
	       (navi2ch-article-goto-number prop t t)
	     (navi2ch-popup-article (list prop)))))
	(t
	 (navi2ch-popup-article prop))))

(defun navi2ch-article-select-current-link-url (url browse-p popup)
  (if (and (not browse-p)
	   (navi2ch-2ch-url-p url)
	   (or (navi2ch-board-url-to-board url)
	       (navi2ch-article-url-to-article url)))
      (progn
	(if popup
	    (navi2ch-popup-article-exit)
	  (and (get-text-property (point) 'help-echo)
	       (let ((buffer-read-only nil))
		 (navi2ch-article-change-help-echo-property
		  (point) (function navi2ch-article-help-echo)))))
	(navi2ch-goto-url url))
    (navi2ch-browse-url-internal url)))

(defun navi2ch-article-mouse-select (e)
  (interactive "e")
  (mouse-set-point e)
  (navi2ch-article-select-current-link))

(defun navi2ch-article-recenter (num)
  "NUM 番目のレスを画面の一番上に"
  (let ((win (if (eq (window-buffer) (current-buffer))
		 (selected-window)
	       (get-buffer-window (current-buffer)))))
    (if (and win (numberp num))
	(set-window-start
	 win (cdr (assq 'point (navi2ch-article-get-message num)))))))

(defun navi2ch-article-goto-number-or-board ()
  "入力された数字の位置に移動するか、入力された板を表示する。
名前が数字ならばデフォルトはその名前の数字。"
  (interactive)
  (let (default alist ret)
    (setq default
	  (let* ((msg (navi2ch-article-get-message
		       (navi2ch-article-get-current-number)))
		 (from (cdr (assq 'name msg)))
		 (data (cdr (assq 'data msg))))
	    (or (and from
		     (string-match "^[^◆0-9０-９]*\\([0-9０-９]+\\)" from)
		     (japanese-hankaku (match-string 1 from)))
		(and data
		     (string-match "[0-9０-９]+" data)
		     (japanese-hankaku (match-string 0 data)))
		nil)))
    (setq alist (mapcar (lambda (x) (cons (cdr (assq 'id x)) x))
			navi2ch-list-board-name-list))
    (setq ret (completing-read
	       (concat "input number or board"
		       (and default (format "(%s)" default))
		       ": ")
	       alist nil nil))
    (setq ret (if (string= ret "") default ret))
    (if ret
	(let ((num (string-to-number ret)))
	  (if (and (> num 0)
		   (equal ret (number-to-string num)))
	      (navi2ch-article-goto-number num t t)
	    (let (board board-id)
	      (setq board-id (try-completion ret alist))
	      (and (eq board-id t) (setq board-id ret))
	      (setq board (cdr (assoc board-id alist)))
	      (if board
		  (progn
		    (when (eq (navi2ch-get-major-mode
			       navi2ch-board-buffer-name)
			      'navi2ch-board-mode)
		      (navi2ch-board-save-info navi2ch-board-current-board))
		    (navi2ch-article-exit)
		    (navi2ch-bm-select-board board))
		(error "don't move")))))
      (error "don't move"))))

(defun navi2ch-article-goto-number (num &optional save pop)
  "NUM 番目のレスに移動"
  (interactive "ninput number: ")
  (when (and num (> num 0)
	     navi2ch-article-message-list)
    (when (or (interactive-p) save)
      (navi2ch-article-push-point))
    (catch 'break
      (let ((len (length navi2ch-article-message-list))
	    (range navi2ch-article-view-range)
	    (first (caar navi2ch-article-message-list))
	    (last (caar (last navi2ch-article-message-list))))
	(setq num (max first (min last num)))
	(unless (navi2ch-article-inside-range-p num range len)
	  (if navi2ch-article-redraw-when-goto-number
	      (progn
		(navi2ch-article-fix-range num)
		(navi2ch-article-redraw))
	    (if (or (interactive-p) pop)
		(progn (when (or (interactive-p) save)
			 (navi2ch-article-pop-point))
		       (navi2ch-popup-article (list num))
		       (throw 'break nil))
	      (setq num (1+ (- len (cdr range))))))))
      (condition-case nil
	  (goto-char (cdr (assq 'point (navi2ch-article-get-message num))))
	(error nil))
      (if navi2ch-article-goto-number-recenter
	  (navi2ch-article-recenter (navi2ch-article-get-current-number))))
    (force-mode-line-update t)))

(defun navi2ch-article-goto-board (&optional board)
  (interactive)
  (navi2ch-list-goto-board (or board
			       navi2ch-article-current-board)))

(defun navi2ch-article-get-point (&optional point)
  (save-window-excursion
    (save-excursion
      (if point (goto-char point) (setq point (point)))
      (let ((num (navi2ch-article-get-current-number)))
	(navi2ch-article-goto-number num)
	(cons num (- point (point)))))))

(defun navi2ch-article-pop-point ()
  "stack から pop した位置に移動する"
  (interactive)
  (let ((point (pop navi2ch-article-point-stack)))
    (if point
        (progn
          (push (navi2ch-article-get-point (point)) navi2ch-article-poped-point-stack)
          (navi2ch-article-goto-number (car point))
	  (forward-char (cdr point)))
      (message "stack is empty"))))

(defun navi2ch-article-push-point (&optional point)
  "現在位置か POINT を stack に push する"
  (interactive)
  (setq navi2ch-article-poped-point-stack nil)
  (push (navi2ch-article-get-point point) navi2ch-article-point-stack)
  (message "push current point"))

(defun navi2ch-article-pop-poped-point () ; 名前だせぇ、ってか何か違う。
  (interactive)
  (let ((point (pop navi2ch-article-poped-point-stack)))
    (if point
        (progn
          (push (navi2ch-article-get-point (point)) navi2ch-article-point-stack)
	  (navi2ch-article-goto-number (car point))
	  (forward-char (cdr point)))
      (message "stack is empty"))))

(defun navi2ch-article-rotate-point ()
  "stack へ push した位置を巡回する。"
  (interactive)
  (let ((cur (navi2ch-article-get-point nil)) ; 現在地
	(top (pop navi2ch-article-point-stack))) ; トップ
    (if top
        (progn
	  (setq navi2ch-article-point-stack
		(append navi2ch-article-point-stack (list cur))) ; 最後尾へ保存
          (navi2ch-article-goto-number (car top)) ; トップの
          (forward-char (cdr top)))	; 以前いた文字へ
      (message "stack is empty"))))

(defun navi2ch-article-goto-last-message ()
  "最後のレスへ"
  (interactive)
  (navi2ch-article-goto-number
   (save-excursion
     (goto-char (point-max))
     (navi2ch-article-get-current-number)) t))

(defun navi2ch-article-goto-first-message ()
  "最初のレスへ"
  (interactive)
  (navi2ch-article-goto-number
   (save-excursion
     (goto-char (point-min))
     (navi2ch-article-get-current-number)) t))

(defun navi2ch-article-few-scroll-up ()
  (interactive)
  (scroll-up 1))

(defun navi2ch-article-few-scroll-down ()
  (interactive)
  (scroll-down 1))

(defun navi2ch-article-scroll-up ()
  (interactive)
  (condition-case nil
      (scroll-up)
    (end-of-buffer
     (funcall navi2ch-article-through-next-function)))
  (force-mode-line-update t))

(defun navi2ch-article-scroll-down ()
  (interactive)
  (condition-case nil
      (scroll-down)
    (beginning-of-buffer
     (funcall navi2ch-article-through-previous-function)))
  (force-mode-line-update t))

(defun navi2ch-article-through-ask-y-or-n-p (num title)
  "次のスレに移動するときに \"y or n\" で確認する。"
  (if title
      (navi2ch-y-or-n-p
       (concat title " --- Through " (if (< num 0) "previous" "next")
	       " article or quit? ")
       'quit)
    (when (navi2ch-y-or-n-p
	   (concat " --- The " (if (< num 0) "first" "last")
		   " article. Quit? ")
	   t)
      'quit)))

(defun navi2ch-article-through-ask-n-or-p-p (num title)
  "次のスレに移動するときに \"n\" か \"p\" で確認する。"
  (let* ((accept-key (if (< num 0) '(?p ?P ?\177) '(?n ?N ?\ )))
	 (accept-value (if title t 'quit))
	 (prompt (if title
		     (format "%s --- Through %s article or quit? (%c or q) "
			     title (if (< num 0) "previous" "next")
			     (car accept-key))
		   (format " --- The %s article. Quit? (%c or q) "
			   (if (< num 0) "first" "last")
			   (car accept-key))))
	 (c (navi2ch-read-char prompt)))
    (if (memq c accept-key)
	accept-value
      (push (navi2ch-ifxemacs (character-to-event c) c)
	    unread-command-events)
      nil)))

(defun navi2ch-article-through-ask-last-command-p (num title)
  "次のスレに移動するときに、直前のコマンドと同じかで確認する。"
  (let* ((accept-value (if title t 'quit))
	 (prompt (if title
		     (format "Type %s for %s "
			     (single-key-description last-command-event)
			     title)
		   (format "The %s article. Type %s for quit "
			   (if (< num 0) "first" "last")
			   (single-key-description last-command-event))))
	 (e (navi2ch-read-event prompt)))
    (if (equal e last-command-event)
	accept-value
      (push e unread-command-events)
      nil)))

(defun navi2ch-article-through-ask (no-ask num)
  "次のスレに移動するか聞く。
次のスレに移動するなら t を返す。
移動しないなら nil を返す。
article buffer から抜けるなら 'quit を返す。"
  (if (or (eq navi2ch-article-enable-through 'ask-always)
	  (and (not no-ask)
	       (eq navi2ch-article-enable-through 'ask)))
      (funcall navi2ch-article-through-ask-function
	       num
	       (save-excursion
		 (set-buffer navi2ch-board-buffer-name)
		 (save-excursion
		   (when (zerop
			  (funcall
			   navi2ch-article-through-forward-line-function num))
		     (cdr (assq 'subject
				(navi2ch-bm-get-article-internal
				 (navi2ch-bm-get-property-internal
				  (point)))))))))
    (or no-ask
	navi2ch-article-enable-through)))

(defun navi2ch-article-through-subr (interactive-flag num)
  "前後のスレに移動する。
NUM が 1 のときは次、-1 のときは前のスレに移動。
呼び出す際はINTERACTIVE-FLAGに(interactive-p)を入れる。"
  (interactive)
  (or num (setq num 1))
  (if (and (not (eq num 1))
	   (not (eq num -1)))
      (error "arg error"))
  (let ((mode (navi2ch-get-major-mode navi2ch-board-buffer-name)))
    (if (and mode
	     (or (not (eq mode 'navi2ch-board-mode))
		 (and (eq mode 'navi2ch-board-mode)
		      (navi2ch-board-equal navi2ch-article-current-board
					   navi2ch-board-current-board))))
	(let ((ret (navi2ch-article-through-ask interactive-flag num)))
	  (cond ((eq ret 'quit)
;;; 		 (goto-char (if (> num 0) (point-max) (point-min)))
		 (navi2ch-article-exit))
		(ret
;;; 		 (goto-char (if (> num 0) (point-max) (point-min)))
		 (let ((window (get-buffer-window navi2ch-board-buffer-name)))
		   (if window
		       (progn
			 (delete-window)
			 (select-window window))
		     (switch-to-buffer navi2ch-board-buffer-name)))
		 (if (zerop
		      (funcall navi2ch-article-through-forward-line-function
			       num))
		     (progn
		       (recenter (/ navi2ch-board-window-height 2))
		       (navi2ch-bm-select-article))
		   (message "Can't through article")))
		(t
		 (message "Don't through article"))))
      (message "Don't through article"))))

(defun navi2ch-article-through-next ()
  "次のスレに移動する。"
  (interactive)
  (navi2ch-article-through-subr (interactive-p) 1))

(defun navi2ch-article-through-previous ()
  "前のスレに移動する。"
  (interactive)
  (navi2ch-article-through-subr (interactive-p) -1))

(defun navi2ch-article-get-message (num)
  "NUM 番目のレスを得る"
  (cdr (assq num navi2ch-article-message-list)))

(defun navi2ch-article-get-current-number ()
  "今の位置のレスの番号を得る"
  (condition-case nil
      (or (get-text-property (point) 'current-number)
          (get-text-property
           (navi2ch-previous-property (point) 'current-number)
           'current-number))
    (error nil)))

(defun navi2ch-article-get-current-name ()
  (cdr (assq 'name (cdr (assq (navi2ch-article-get-current-number)
			      navi2ch-article-message-list)))))

(defun navi2ch-article-get-current-mail ()
  (cdr (assq 'mail (cdr (assq (navi2ch-article-get-current-number)
			      navi2ch-article-message-list)))))

(defun navi2ch-article-get-current-date ()
  (let ((date (cdr (assq 'date (cdr (assq (navi2ch-article-get-current-number)
					  navi2ch-article-message-list))))))
    (if (string-match " ID:.*" date)
	(replace-match "" nil t date)
      date)))

(defun navi2ch-article-get-current-id ()
  (let ((date (cdr (assq 'date (cdr (assq (navi2ch-article-get-current-number)
					  navi2ch-article-message-list))))))
    (if (string-match " ID:\\([^ ]+\\)" date)
	(match-string 1 date)
      nil)))

(defun navi2ch-article-get-current-word-in-body ()
  (let ((case-fold-search nil)
	(word (if (get-text-property (point) 'url)
		  (buffer-substring-no-properties
		   (if (get-text-property (1- (point)) 'url)
		       (previous-single-property-change (point) 'url)
		     (point))
		   (next-single-property-change (point) 'url))
		(current-word))))
    (when (string-match
	   (regexp-quote word)
	   (navi2ch-article-get-message-string
	    (navi2ch-article-get-current-number)))
      word)))

(defun navi2ch-article-get-current-subject ()
  (or (cdr (assq 'subject navi2ch-article-current-article))
      (cdr (assq 'subject
		 (let ((msg (navi2ch-article-get-message 1)))
		   (if (stringp msg)
		       (navi2ch-article-parse-message msg)
		     msg))))))

(defun navi2ch-article-get-visible-numbers ()
  "表示中のレスの番号のリストを得る。"
  (let (list prev)
    (save-excursion
      (goto-char (point-max))
      (unless (bobp)
	(while (setq prev (navi2ch-previous-property (point) 'current-number))
	  (goto-char prev)
	  (setq list (cons (get-text-property (point) 'current-number) list)))))
    list))

(defun navi2ch-article-show-url ()
  "url を表示して、その url を見るか kill ring にコピーする"
  (interactive)
  (let ((url (navi2ch-article-to-url navi2ch-article-current-board
				     navi2ch-article-current-article)))
    (let ((char (navi2ch-read-char-with-retry
		 (format "c)opy v)iew t)itle? URL: %s: " url)
		 nil '(?c ?v ?t))))
      (if (eq char ?t)
	  (navi2ch-article-copy-title navi2ch-article-current-board
				      navi2ch-article-current-article)
	(funcall (cond ((eq char ?c)
			(lambda (x)
			  (kill-new x)
			  (message "copy: %s" x)))
		       ((eq char ?v)
			(lambda (x)
			  (navi2ch-browse-url-internal x)
			  (message "view: %s" x))))
		 (navi2ch-article-show-url-subr))))))

(defun navi2ch-article-show-url-subr ()
  "メニューを表示して、url を得る"
  (let* ((prompt (format "a)ll c)urrent r)egion b)oard l)ast%d: "
			 navi2ch-article-show-url-number))
	 (char (navi2ch-read-char-with-retry prompt
					     nil '(?a ?c ?r ?b ?l))))
    (if (eq char ?b)
	(navi2ch-board-to-url navi2ch-article-current-board)
      (apply 'navi2ch-article-to-url
	     navi2ch-article-current-board navi2ch-article-current-article
	     (cond ((eq char ?a) nil)
		   ((eq char ?l)
		    (let ((l (format "l%d"
				     navi2ch-article-show-url-number)))
		      (list l l nil)))
		   ((eq char ?c) (list (navi2ch-article-get-current-number)
				       (navi2ch-article-get-current-number)
				       t))
		   ((eq char ?r)
		    (let ((rb (region-beginning)) (re (region-end)))
		      (save-excursion
			(list (progn (goto-char rb)
				     (navi2ch-article-get-current-number))
			      (progn (goto-char re)
				     (navi2ch-article-get-current-number))
			      t)))))))))

(defun navi2ch-article-copy-title (board article)
  "メニューを表示して、タイトルを得る"
  (let* ((char (navi2ch-read-char-with-retry
		"b)oard a)rticle B)oard&url A)rtile&url: "
		nil '(?b ?a ?B ?A)))
	 (title (cond ((eq char ?b)
		       (cdr (assq 'name board)))
		      ((eq char ?a)
		       (when article
			 (cdr (assq 'subject article))))
		      ((eq char ?B)
		       (concat (cdr (assq 'name board))
			       "\n"
			       (navi2ch-board-to-url board)))
		      ((eq char ?A)
		       (when article
			 (concat (cdr (assq 'subject article))
				 "\n"
				 (navi2ch-article-to-url board article)))))))
    (if (not title)
	(message "Can't select this line!")
      (kill-new title)
      (message "copy: %s" title))))

(defun navi2ch-article-redisplay-current-message ()
  "今いるレスを画面の中心(上？)に"
  (interactive)
  (navi2ch-article-recenter
   (navi2ch-article-get-current-number)))

(defun navi2ch-article-next-message ()
  "次のメッセージへ"
  (interactive)
  (run-hooks 'navi2ch-article-next-message-hook)
  (condition-case nil
      (progn
        (goto-char (navi2ch-next-property (point) 'current-number))
        (navi2ch-article-goto-number
         (navi2ch-article-get-current-number)))
    (error
     (funcall navi2ch-article-through-next-function))))

(defun navi2ch-article-previous-message ()
  "前のメッセージへ"
  (interactive)
  (run-hooks 'navi2ch-article-previous-message-hook)
  (condition-case nil
      (progn
        (goto-char (navi2ch-previous-property (point) 'current-number))
        (navi2ch-article-goto-number
         (navi2ch-article-get-current-number)))
    (error
     (funcall navi2ch-article-through-previous-function))))

(defun navi2ch-article-get-message-string (num)
  "num 番目のレスの文章を得る。"
  (let ((msg (navi2ch-article-get-message num)))
    (when (stringp msg)
      (setq msg (navi2ch-article-parse-message msg)))
    (cdr (assq 'data msg))))

(defun navi2ch-article-cached-subject-minimum-size (file)
  "スレタイトルを得るのに十分なファイルサイズを求める。"
  (with-temp-buffer
    (let ((beg 0) (end 0) (n 1))
      (while (and (= (point) (point-max))
		  (> n 0))
	(setq beg end)
	(setq end (+ end 1024))
	(setq n (car (cdr (navi2ch-insert-file-contents file beg end))))
	(forward-line))
      end)))

(defun navi2ch-article-cached-subject (board article)
  "キャッシュされている dat ファイルからスレタイトルを得る。"
  ;; "キャッシュされている dat ファイルやスレ一覧からスレタイトルを得る。"
  (let ((state (navi2ch-article-check-cached board article))
	subject)
    (if (eq state 'view)
	(save-excursion
	  (set-buffer (navi2ch-article-get-buffer-name board article))
	  (setq subject			; nil になることがある
		(cdr (assq 'subject
			   navi2ch-article-current-article)))))
    (when (not subject)
      (if (eq state 'cache)
	  (let* ((file (navi2ch-article-get-file-name board article))
		 (msg-list (navi2ch-article-get-message-list
			    file
			    0
			    (navi2ch-article-cached-subject-minimum-size file))))
	    (setq subject
		  (cdr (assq 'subject
			     (navi2ch-article-parse-message (cdar msg-list))))))))
    (when (not subject)
      (let ((subject-list
	     (if (equal (cdr (assq 'name board))
			(cdr (assq 'name navi2ch-board-current-board)))
		 navi2ch-board-subject-list
;;;	       (navi2ch-board-get-subject-list
;;;		(navi2ch-board-get-file-name board))
	       )))
	(setq subject
	      (catch 'subject
		(dolist (s subject-list)
		  (if (equal (cdr (assq 'artid article))
			     (cdr (assq 'artid s)))
		      (throw 'subject (cdr (assq 'subject s)))))))))
    (when (not subject)
      (setq subject "navi2ch: ???")) ; 変数にして navi2ch-vars.el に入れるべき?
    subject))

(eval-when-compile
  (defvar mark-active)
  (defvar deactivate-mark))

(defun navi2ch-article-get-link-text-subr (&optional point)
  "POINT (省略時はカレントポイント) のリンク先を得る。"
  (setq point (or point (point)))
  (let (mark-active deactivate-mark) ; transient-mark-mode が切れないよう
    (catch 'ret
      (when (or (eq major-mode 'navi2ch-article-mode)
		(eq major-mode 'navi2ch-popup-article-mode))
	(let ((num-prop (get-text-property point 'number))
	      (url-prop (get-text-property point 'url))
	      num-list num)
	  (cond
	   (num-prop
	    (setq num-list (navi2ch-article-get-number-list
			    num-prop (navi2ch-article-get-current-number)))
	    (cond ((numberp num-list)
		   (setq num num-list))
		  (t
		   (setq num (car num-list))))
	    (let ((msg (navi2ch-article-get-message-string num)))
	      (when msg
		(setq msg (navi2ch-replace-string
			   navi2ch-article-citation-regexp "" msg t))
		(setq msg (navi2ch-replace-string
			   "\\(\\cj\\)\n+\\(\\cj\\)" "\\1\\2" msg t))
		(setq msg (navi2ch-replace-string "\n+" " " msg t))
		(throw
		 'ret
		 (format "%s" (truncate-string-to-width
			       (format "[%d]: %s" num msg)
			       (eval navi2ch-article-display-link-width)))))))
	   ((and navi2ch-article-get-url-text
		 url-prop)
	    (if (navi2ch-2ch-url-p url-prop)
		(let ((board (navi2ch-board-url-to-board url-prop))
		      (article (navi2ch-article-url-to-article url-prop)))
		  (throw
		   'ret
		   (format "%s"
			   (truncate-string-to-width
			    (if article
				(format "[%s]: %s"
					(cdr (assq 'name board))
					(navi2ch-article-cached-subject board article))
			      (format "[%s]" (cdr (assq 'name board))))
			    (eval navi2ch-article-display-link-width))))))))))
      nil)))

(defun navi2ch-article-get-link-text (&optional point)
  "POINT (省略時はカレントポイント) のリンク先を得る。
結果を help-echo プロパティに設定してキャッシュする。"
  (setq point (or point (point)))
  (let ((help-echo-prop (get-text-property point 'help-echo))
	mark-active deactivate-mark) ; transient-mark-mode が切れないよう
    (unless (or (null help-echo-prop)
		(stringp help-echo-prop))
      (setq help-echo-prop (navi2ch-article-get-link-text-subr point))
      (let ((buffer-read-only nil))
	(navi2ch-article-change-help-echo-property point help-echo-prop)))
    help-echo-prop))

(defun navi2ch-article-change-help-echo-property (point value)
  (unless (get-text-property point 'help-echo)
    (error "POINT (%d) does not have property help-echo" point))
  (let* ((end (or (min (next-single-property-change point 'help-echo)
		       (or (navi2ch-next-property point 'link-head)
			   (point-max)))
		  point))
	 (start (or (max (previous-single-property-change end 'help-echo)
			 (or (navi2ch-previous-property end 'link-head)
			     (point-min)))
		    point)))
    (put-text-property start end 'help-echo value)))

(defvar navi2ch-article-disable-display-link-commands
  '(navi2ch-show-url-at-point eval-expression)
  "このコマンドの後では minibuffer にリンク先を表示しない。")

(defun navi2ch-article-display-link-minibuffer (&optional point)
  "POINT (省略時はカレントポイント) のリンク先を minibuffer に表示。"
  (unless (or isearch-mode
	      (memq this-command
		    navi2ch-article-disable-display-link-commands))
    (save-match-data
      (save-excursion
	(let ((text (navi2ch-article-get-link-text point)))
	  (if (stringp text)
	      (message "%s" text)))))))

(defun navi2ch-article-help-echo (window-or-extent &optional object position)
  (save-match-data
    (save-excursion
      (navi2ch-ifxemacs
	  (when (extentp window-or-extent)
	    (setq object (extent-object window-or-extent))
	    (setq position (extent-start-position window-or-extent))))
      (when (buffer-live-p object)
	(with-current-buffer object
	  (navi2ch-article-get-link-text position))))))

(defun navi2ch-article-next-link ()
  "次のリンクへ"
  (interactive)
  (let ((point (navi2ch-next-property (point) 'link-head)))
    (if point
	(goto-char point))))

(defun navi2ch-article-previous-link ()
  "前のリンクへ"
  (interactive)
  (let ((point (navi2ch-previous-property (point) 'link-head)))
    (if point
	(goto-char point))))

(defun navi2ch-article-fetch-link (&optional force)
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (and url
	 (navi2ch-2ch-url-p url)
	 (let ((article (navi2ch-article-url-to-article url))
	       (board (navi2ch-board-url-to-board url)))
	   (when article
	     (and (get-text-property (point) 'help-echo)
		  (let ((buffer-read-only nil))
		    (navi2ch-article-change-help-echo-property (point)
							       (function navi2ch-article-help-echo))))
	     (let (summary artid element seen)
	       (when (and navi2ch-board-check-article-update-suppression-length
			  (not (navi2ch-bm-fetched-article-p board article)))
		 (setq summary (navi2ch-article-load-article-summary board))
		 (setq artid (cdr (assq 'artid article)))
		 (setq element (cdr (assoc artid summary)))
		 (setq seen (or (navi2ch-article-summary-element-seen element)
				(cdr (assoc artid navi2ch-board-last-seen-alist))
				0)))
	       (and (navi2ch-article-fetch-article board article force)
		    (if (and seen
			     (setq seen
				   (navi2ch-article-check-message-suppression
				    board
				    article
				    (1+ seen)
				    (+ seen navi2ch-board-check-article-update-suppression-length))))
			(progn
			  (navi2ch-article-summary-element-set-seen element seen)
			  (navi2ch-article-save-article-summary board summary))
		      (navi2ch-bm-remember-fetched-article board article)))))))))

(defun navi2ch-article-detect-encoded-regions (&optional sort)
  "バッファから uuencode または base64 エンコードされた領域を探す。
 (list (list type fname start end)) を返す。
SORT が non-nil のときは start でソートした結果を返す。
ただし、
 type: 'uuencode か 'base64
fname: デコードするときのデフォルトのファイル名
start: エンコードされた領域の先頭(デリミタを含む)のポイント
  end: エンコードされた領域の末尾(デリミタの次の行の行頭)のポイント
である。
end が nil の場合末尾のデリミタが無く先頭のデリミタのみあることを意味する。"
  ;; start が nil の場合先頭のデリミタが無く末尾のデリミタのみあることを意味し、
  ;; ↑ すると、誤判定が多そうなのでやめ。
  (let ((dels (list (cons 'base64
			  (cons navi2ch-base64-begin-delimiter-regexp
				navi2ch-base64-end-delimiter-regexp))
		    (cons 'base64
			  (cons navi2ch-base64-susv3-begin-delimiter-regexp
				navi2ch-base64-susv3-end-delimiter-regexp))
		    (cons 'uudecode
			  (cons navi2ch-uuencode-begin-delimiter-regexp
				navi2ch-uuencode-end-delimiter-regexp))))
	regions type fname start end)
    (save-excursion
      (dolist (d dels)
	(goto-char (point-min))
	(while (re-search-forward (cadr d) nil t)
	  (setq type (car d)
		start (match-beginning 0)
		fname (navi2ch-match-string-no-properties 2)
		end (and (re-search-forward (cddr d) nil t)
			 (navi2ch-line-beginning-position 2))
		regions (cons (list type fname start end) regions)))))
    (when sort
      (setq regions (sort regions (lambda (r1 r2) (< (nth 2 r1) (nth 2 r2)))))
      ;; end が nil は、最後のみ許す。
      (let ((r regions))
	(while (> (length r) 1)
	  (when (null (nth 3 (car r)))
	    (setq regions (delete (car r) regions)))
	  (setq r (cdr r)))))
    regions))

(defun navi2ch-article-decode-message (prefix)
  "現在のレスをデコードする。
PREFIXを指定した場合は、markのあるレスと現在のレスの間の範囲が対象になる。

複数レスに分割されたエンコードセクションをデコードしたい場合は、
エンコードセクションの先頭のレスで、PREFIXを指定せずに実行すること。
現在のレス内に始めのデリミタのみがある場合、対応する末尾のデリミタが
現れるレスまでデコードする領域を拡張する。

デリミタとみなすのは、
`navi2ch-base64-begin-delimiter-regexp'
`navi2ch-base64-end-delimiter-regexp'

`navi2ch-base64-susv3-begin-delimiter-regexp'
`navi2ch-base64-susv3-end-delimiter-regexp'

`navi2ch-uuencode-begin-delimiter-regexp'
`navi2ch-uuencode-end-delimiter-regexp'

の3組のいずれかにマッチする行とする。"
  (interactive "P")
  (let* ((num (navi2ch-article-get-current-number))
	 (num2 (or (and prefix
			(car (navi2ch-article-get-point (mark))))
		   num))
	 (abuf (current-buffer))
	 (nmax (caar (last navi2ch-article-message-list)))
	 end regions)
    (when (> num num2)
      (setq num (prog1 num2
		  (setq num2 num))))
    (with-temp-buffer
      (while (<= num num2)
	(insert (or (with-current-buffer abuf
		      (navi2ch-article-get-message-string num))
		    "")
		"\n")
	(setq num (1+ num)))
      (setq end (point))
      (setq regions (navi2ch-article-detect-encoded-regions 'sort))
      ;; 複数レスに分割されたものを探す。
      (when (and (not prefix) regions)
	(while (and (null (nth 3 (car (last regions))))
		    (< (nth 2 (car (last regions))) end)
		    (<= num nmax))
	  (insert (or (with-current-buffer abuf
			(navi2ch-article-get-message-string num))
		      "")
		  "\n")
	  (setq num (1+ num))
	  (setq regions (navi2ch-article-detect-encoded-regions 'sort)))
	(while (and regions
		    (>= (nth 2 (car (last regions))) end))
	  (setq regions (delete (car (last regions)) regions))))
      (unless regions
	(let ((c (navi2ch-read-char-with-retry
		  "(u)udecode or (b)ase64: "
		  "Please answer u, or b.  (u)udecode or (b)ase64: "
		  '(?u ?U ?b ?B))))
	  (cond
	   ((memq c '(?u ?U))
	    (setq regions (list (list 'uudecode nil nil nil))))
	   ((memq c '(?b ?B))
	    (setq regions (list (list 'base64 nil nil nil)))))))
      (dolist (r regions)
	(condition-case err
	    (cond
	     ((eq (car r) 'uudecode)
	      (navi2ch-uudecode-write-region (or (nth 2 r) (point-min))
					     (or (nth 3 r) end)))
	     ((eq (car r) 'base64)
	      (navi2ch-base64-write-region (or (nth 2 r) (point-min))
					   (or (nth 3 r) end))))
	  (error (ding)
		 (message "%s" (error-message-string err))
		 (sit-for 1)))))))

(defun navi2ch-article-auto-decode-encoded-section ()
  "エンコードされたセクションをデコードしたものへのアンカーにする。"
  (let ((regions (delq nil
		       (mapcar (lambda (r)
				 (when (and (nth 2 r) (nth 3 r))
				   (list (nth 0 r)
					 (nth 1 r)
					 (copy-marker (nth 2 r))
					 ;; line-end
					 (copy-marker (1- (nth 3 r))))))
			       (navi2ch-article-detect-encoded-regions))))
	type filename begin end encoded decoded)
    (dolist (r regions)
      (setq type (nth 0 r)
	    filename (nth 1 r)
	    begin (nth 2 r)
	    end (nth 3 r)
	    encoded (cond
		     ((eq type 'uudecode)
		      (buffer-substring-no-properties
		       begin
		       (progn (goto-char end)
			      (navi2ch-line-beginning-position 2))))
		     ((eq type 'base64)
		      (buffer-substring-no-properties
		       (progn (goto-char begin)
			      (navi2ch-line-beginning-position 2))
		       (progn (goto-char end)
			      (navi2ch-line-end-position 0)))))
	    decoded nil)
      (with-temp-buffer
	(insert encoded)
	(condition-case nil
	    (progn
	      (cond
	       ((eq type 'uudecode)
		(goto-char (point-min))
		(while (search-forward "？" nil t) ;for 2ch
		  (replace-match "&#" nil t))
		(navi2ch-uudecode-region (point-min) (point-max)))
	       ((eq type 'base64)
		(base64-decode-region (point-min) (point-max))))
	      (setq decoded (buffer-string)))
	  (error nil)))
      (when decoded
	(let ((fname (unless (or (null filename) (equal filename ""))
		       filename))
	      part-begin)
	  (delete-region begin end)
	  (goto-char begin)
	  (insert (navi2ch-propertize
		   "> " 'face 'navi2ch-article-auto-decode-face)
		  (navi2ch-propertize
		   (format "%s" (or fname "名無しファイルさん"))
		   'face '(navi2ch-article-url-face
			   navi2ch-article-auto-decode-face)
		   'link t
		   'mouse-face navi2ch-article-mouse-face
		   'file-name fname
		   'content decoded))
	  (put-text-property begin (1+ begin) 'auto-decode-text 'off)
	  (put-text-property (+ 2 begin) (+ 3 begin) 'link-head t)
	  (setq part-begin (point))
	  (insert (format " (%.1fKB)\n" (/ (length decoded) 1024.0)))
	  (put-text-property part-begin (point)
			     'face 'navi2ch-article-auto-decode-face)
	  (add-text-properties begin (point) (list 'auto-decode t
						   'hard t))
	  (when navi2ch-article-auto-decode-insert-text
	    (forward-line -1)
	    (navi2ch-article-auto-decode-text-on))))
      (set-marker begin nil)
      (set-marker end nil))))

(defun navi2ch-article-auto-decode-text-on (&optional coding-system compress)
  (save-excursion
    (beginning-of-line)
    (let* ((point (next-single-property-change
		   (point) 'link-head nil (navi2ch-line-end-position)))
	   (content (get-text-property point 'content))
	   (filename (get-text-property point 'file-name))
	   ret)
      (when (and (eq (get-text-property (point) 'auto-decode-text) 'off)
		 content)
	(with-temp-buffer
	  (let ((buffer-file-coding-system 'binary)
		(coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		exit-status)
	    (insert content)
	    ;; extract
	    (cond
	     ((or (eq compress 'gzip)
		  (and (null compress)
		       filename
		       (string-match "\\.t?gz$" filename)))
	      (setq exit-status
		    (apply 'call-process-region (point-min) (point-max)
			   navi2ch-net-gunzip-program t t nil
			   navi2ch-net-gunzip-args))
	      (unless (= exit-status 0)
		(erase-buffer)
		(insert content)))
	     ((or (eq compress 'bzip2)
		  (and (null compress)
		       filename
		       (string-match "\\.bz2$" filename)
		       (navi2ch-which navi2ch-bzip2-program)))
	      (setq exit-status
		    (apply 'call-process-region (point-min) (point-max)
			   navi2ch-bzip2-program t t nil
			   navi2ch-bzip2-args))
	      (unless (= exit-status 0)
		(erase-buffer)
		(insert content))))
	    ;; decode
	    (unless coding-system
	      (let ((detect (detect-coding-region (point-min) (point-max)))
		    name)
		(setq coding-system (or (car-safe detect) detect)
		      name (navi2ch-ifxemacs
			       (coding-system-name
				(coding-system-base coding-system))
			     (coding-system-base coding-system)))
		(when (memq name '(raw-text binary)) ;適当
		  (setq ret 'binary
			coding-system nil
			content nil))))
	    (when coding-system
	      (decode-coding-region (point-min) (point-max) coding-system)
	      (setq content (buffer-string)))))
	(when content
	  (setq ret t)
	  (let ((buffer-read-only nil))
	    (put-text-property (point) (1+ (point)) 'auto-decode-text 'on)
	    (forward-line)
	    (setq point (point))
	    (insert content)
	    (add-text-properties point (point)
				 (list 'auto-decode t
				       'hard t
				       'face 'navi2ch-article-auto-decode-face))
	    (set-buffer-modified-p nil))))
      ret)))

(defun navi2ch-article-auto-decode-text-off ()
  (when (get-text-property (point) 'auto-decode)
    (let ((buffer-read-only nil)
	  (start (or (and (get-text-property (point) 'auto-decode-text)
			  (point))
		     (navi2ch-previous-property (point) 'auto-decode-text)))
	  bs be)
      (when (eq (get-text-property start 'auto-decode-text) 'on)
	(put-text-property start (1+ start) 'auto-decode-text 'off)
	(save-excursion
	  (goto-char start)
	  (forward-line)
	  (setq bs (point)
		be (min (next-single-property-change bs 'auto-decode
						     nil (point-max))
			(next-single-property-change bs 'auto-decode-text
						     nil (point-max)))))
	(delete-region bs be)
	(when (>= (point) bs)
	  (forward-line -1))
	(set-buffer-modified-p nil)))))

(defun navi2ch-article-auto-decode-toggle-text (&optional ask)
  "エンコードされたセクションをデコードしたものの表示を切り換える。
ASK が non-nil だと、デコードしたものの文字コードと圧縮形式を聞いてくる。"
  (interactive "P")
  (if (not (get-text-property (point) 'auto-decode))
      (error "Decoded text is not here")
    (if (eq (or (get-text-property (point) 'auto-decode-text)
		(get-text-property (navi2ch-previous-property
				    (point) 'auto-decode-text)
				   'auto-decode-text))
	    'on)
	(navi2ch-article-auto-decode-text-off)
      (let* ((cs (and ask (read-coding-system "Coding-system (guess): ")))
	     (ch (and ask (navi2ch-read-char
			   "Compression type (guess): g)zip b)zip2 n)one: ")))
	     (cmp (cond ((eq ch ?g) 'gzip)
			((eq ch ?b) 'bzip2)
			((eq ch ?n) t)))
	     ret)
	(message "Inserting decoded content...")
	(setq ret (navi2ch-article-auto-decode-text-on cs cmp))
	(cond ((eq ret 'binary)
	       (message "Content may be a binary"))
	      (ret
	       (message "Inserting decoded content...done"))
	      (t
	       (message "Not inserted")))))))

(defun navi2ch-article-save-content ()
  (interactive)
  (let ((prop (get-text-property (point) 'content))
	(default-filename (get-text-property (point) 'file-name))
	filename)
    (when default-filename
      (setq default-filename (file-name-nondirectory default-filename)))
    (setq filename (read-file-name
		    (if default-filename
			(format "Save file (default `%s'): "
				default-filename)
		      "Save file: ")
		    nil default-filename))
    (when (file-directory-p filename)
      (if default-filename
	  (setq filename (expand-file-name default-filename filename))
	(error "%s is a directory" filename)))
    (if (not (file-writable-p filename))
	(error "File not writable: %s" filename)
      (with-temp-buffer
	(let ((buffer-file-coding-system 'binary)
	      (coding-system-for-write 'binary)
	      ;; auto-compress-mode を disable にする
	      (inhibit-file-name-operation 'write-region)
	      (inhibit-file-name-handlers (cons 'jka-compr-handler
						inhibit-file-name-handlers)))
	  (insert prop)
	  (if (or (not (file-exists-p filename))
		  (y-or-n-p (format "File `%s' exists; overwrite? "
				    filename)))
	      (write-region (point-min) (point-max) filename)))))))

(defun navi2ch-article-textize-article (&optional dir-or-file buffer)
  (interactive)
  (let* ((article navi2ch-article-current-article)
	 (board navi2ch-article-current-board)
	 (id (cdr (assq 'id board)))
	 (subject (cdr (assq 'subject article)))
	 (basename (format "%s_%s.txt" id (cdr (assq 'artid article))))
	 dir file)
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
    (when navi2ch-article-view-range
      (setq navi2ch-article-view-range nil)
      (navi2ch-article-redraw))
    (let ((coding-system-for-write navi2ch-coding-system))
      (navi2ch-write-region (point-min) (point-max)
			    file))
    (message "Wrote %s" file)))

;; shut up XEmacs warnings
(eval-when-compile
  (defvar w32-start-process-show-window))

(defun navi2ch-article-call-aadisplay (str)
  (let* ((coding-system-for-write navi2ch-article-aadisplay-coding-system)
	 (file (expand-file-name (make-temp-name (navi2ch-temp-directory)))))
    (unwind-protect
	(progn
	  (with-temp-file file
	    (insert str))
	  (let ((w32-start-process-show-window t)) ; for meadow
	    (call-process navi2ch-article-aadisplay-program
			  nil nil nil file)))
      (ignore-errors (delete-file file)))))

(defun navi2ch-article-popup-dialog (str)
  (navi2ch-ifxemacs
      (ignore str)			; とりあえず何もしない
    (x-popup-dialog
     t (cons "navi2ch"
	     (mapcar (lambda (x)
		       (cons (if (string= x "") " " x) t))
		     (split-string str "\n"))))))

(defun navi2ch-article-view-aa ()
  (interactive)
  (funcall navi2ch-article-view-aa-function
           (cdr (assq 'data
                      (navi2ch-article-get-message
                       (navi2ch-article-get-current-number))))))

(defun navi2ch-article-load-article-summary (board)
  (navi2ch-load-info (navi2ch-board-get-file-name
		      board
		      navi2ch-article-summary-file-name)))

(defun navi2ch-article-save-article-summary (board summary)
  (navi2ch-save-info (navi2ch-board-get-file-name
		      board
		      navi2ch-article-summary-file-name)
		     summary))

(defun navi2ch-article-set-summary-element (board article remove-seen)
  "BOARD, ARTICTLE に対応した 情報を article-summary に保存する"
  (let* ((summary (navi2ch-article-load-article-summary board))
	 (artid (cdr (assq 'artid article)))
	 (element (cdr (assoc artid summary))))
    (navi2ch-article-summary-element-set-seen
     element
     (unless remove-seen
       (save-excursion
	 (set-buffer (navi2ch-article-get-buffer-name board article))
	 (length navi2ch-article-message-list))))
    (navi2ch-article-summary-element-set-access-time element (current-time))
    (setq summary (navi2ch-put-alist artid element summary))
    (navi2ch-article-save-article-summary board summary)))

(defun navi2ch-article-add-board-bookmark ()
  (interactive)
  (navi2ch-board-add-bookmark-subr navi2ch-article-current-board
				   navi2ch-article-current-article))

(defun navi2ch-article-add-global-bookmark (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "bookmark id: ")))
  (navi2ch-bookmark-add
   bookmark-id
   navi2ch-article-current-board
   navi2ch-article-current-article))

(defun navi2ch-article-buffer-list ()
  "`navi2ch-article-mode' の buffer の list を返す"
  (let (list)
    (dolist (x (buffer-list))
      (when (save-excursion
              (set-buffer x)
              (eq major-mode 'navi2ch-article-mode))
        (setq list (cons x list))))
    (nreverse list)))

(defun navi2ch-article-toggle-sticky ()
  "現在のバッファの sticky モードを toggle する。"
  (interactive)
  (setq navi2ch-article-sticky-mode
	(not navi2ch-article-sticky-mode))
  (force-mode-line-update)
  (if navi2ch-article-sticky-mode
      (message "Marked as sticky")
    (message "Marked as non-sticky")))

(defun navi2ch-article-current-buffer (&optional sticky)
  "BUFFER-LIST の一番最初の `navi2ch-article-mode' の buffer を返す。
STICKY が non-nil のときは一番最初の sticky article buffer を返す。"
  (let ((list (buffer-list)))
    (catch 'loop
      (while list
        (when (save-excursion
                (set-buffer (car list))
		(and (eq major-mode 'navi2ch-article-mode)
		     (or (not sticky)
			 navi2ch-article-sticky-mode)))
	  (throw 'loop (car list)))
        (setq list (cdr list)))
      nil)))

(defun navi2ch-article-forward-buffer (&optional sticky)
  "次の article buffer に切り替える。
STICKY が non-nil のときは次の sticky article buffer に切り替える。"
  (interactive "P")
  (let (buf)
    (dolist (x (buffer-list))
      (when (save-excursion
              (set-buffer x)
              (and (eq major-mode 'navi2ch-article-mode)
		   (or (not sticky)
		       navi2ch-article-sticky-mode)))
	(setq buf x)))
    (if buf
	(progn
	  (navi2ch-split-window 'article)
	  (switch-to-buffer buf))
      (if sticky
	  (message "No sticky aritcle buffer")
	(message "No aritcle buffer"))
      nil)))

(defun navi2ch-article-backward-buffer (&optional sticky)
  "前の article buffer に切り替える。
STICKY が non-nil のときは前の sticky article buffer に切り替える。"
  (interactive "P")
  (let ((orig (current-buffer))
	buf)
    (when (setq buf (navi2ch-article-current-buffer))
      (bury-buffer buf))
    (setq buf (navi2ch-article-current-buffer sticky))
    (if buf
	(progn
	  (navi2ch-split-window 'article)
	  (switch-to-buffer buf))
      (switch-to-buffer orig)
      (if sticky
	  (message "No sticky aritcle buffer")
	(message "No aritcle buffer"))
      nil)))

(defun navi2ch-article-forward-sticky-buffer (&optional no-sync)
  "次の sticky article buffer に切り替え、sync する。
NO-SYNC が non-nil のときは sync しない。"
  (interactive "P")
  (and (navi2ch-article-forward-buffer t)
       (not no-sync)
       (navi2ch-article-sync)))

(defun navi2ch-article-backward-sticky-buffer (&optional no-sync)
  "前の sticky article buffer に切り替え、sync する。
NO-SYNC が non-nil のときは sync しない。"
  (interactive "P")
  (and (navi2ch-article-backward-buffer t)
       (not no-sync)
       (navi2ch-article-sync)))

(defun navi2ch-article-delete-message (sym func msg &optional perm)
  (let* ((article navi2ch-article-current-article)
         (list (cdr (assq sym article)))
         (num (navi2ch-article-get-current-number)))
    (setq list (funcall func num list))
    (setq article (navi2ch-put-alist sym list article))
    (unless (memq num list)
      (let* ((cache navi2ch-article-message-filter-cache)
	     (f-list (cdr (assq sym cache)))
	     (unfilter (cdr (assq 'unfilter article))))
	(setq f-list (delq num f-list))
	(setq cache (navi2ch-put-alist sym f-list cache))
	(setq navi2ch-article-message-filter-cache
	      (navi2ch-put-alist 'cache
				 (delq num (cdr (assq 'cache cache)))
				 cache))
	(when (and perm
		   (not (memq num unfilter)))
	  (setq article (navi2ch-put-alist 'unfilter
					   (cons num unfilter)
					   article)))))
    (setq navi2ch-article-current-article article)
    (when num
      (save-excursion
	(let ((buffer-read-only nil))
	  (delete-region
	   (if (get-text-property (point) 'current-number)
	       (point)
	     (navi2ch-previous-property (point) 'current-number))
	   (or (navi2ch-next-property (point) 'current-number)
	       (point-max)))))
      (message msg))))

;;; hide mode
(navi2ch-set-minor-mode 'navi2ch-article-hide-mode
                        " Hide"
                        navi2ch-article-hide-mode-map)

(defun navi2ch-article-hide-message ()
  (interactive)
  (run-hooks 'navi2ch-article-hide-message-hook)
  (navi2ch-article-delete-message
   'hide
   (lambda (num list)
     (if (memq num list)
         list
       (cons num list)))
   "Hide message"))

(defun navi2ch-article-cancel-hide-message (&optional prefix)
  (interactive "P")
  (run-hooks 'navi2ch-article-cancel-hide-message-hook)
  (navi2ch-article-delete-message 'hide 'delq
                                  "Cancel hide message"
				  prefix))

(defun navi2ch-article-toggle-hide ()
  (interactive)
  (setq navi2ch-article-hide-mode
        (if navi2ch-article-hide-mode
            nil
          (navi2ch-article-save-number)
          t))
  (setq navi2ch-article-important-mode nil)
  (force-mode-line-update)
  (let ((buffer-read-only nil))
    (save-excursion
      (erase-buffer)
      (navi2ch-article-insert-messages
       navi2ch-article-message-list
       navi2ch-article-view-range)))
  (unless navi2ch-article-hide-mode
    (navi2ch-article-load-number)))

;;; important mode
(navi2ch-set-minor-mode 'navi2ch-article-important-mode
                        " Important"
                        navi2ch-article-important-mode-map)

(defun navi2ch-article-add-important-message (&optional prefix)
  (interactive "P")
  (run-hooks 'navi2ch-article-add-important-message-hook)
  (if prefix
      (navi2ch-article-add-board-bookmark)
    (let* ((article navi2ch-article-current-article)
	   (list (cdr (assq 'important article)))
	   (num (navi2ch-article-get-current-number)))
      (unless (memq num list)
	(setq list (cons num list))
	(setq navi2ch-article-current-article
	      (navi2ch-put-alist 'important list article))
	(message "Add important message")))))

(defun navi2ch-article-delete-important-message (&optional prefix)
  (interactive "P")
  (run-hooks 'navi2ch-article-delete-important-message-hook)
  (navi2ch-article-delete-message 'important 'delq
                                  "Delete important message"
				  prefix))

(defun navi2ch-article-toggle-important ()
  (interactive)
  (setq navi2ch-article-important-mode
        (if navi2ch-article-important-mode
            nil
          (navi2ch-article-save-number)
          t))
  (setq navi2ch-article-hide-mode nil)
  (force-mode-line-update)
  (let ((buffer-read-only nil))
    (save-excursion
      (erase-buffer)
      (navi2ch-article-insert-messages
       navi2ch-article-message-list
       navi2ch-article-view-range)))
  (unless navi2ch-article-important-mode
    (navi2ch-article-load-number)))

(defun navi2ch-article-search ()
  "メッセージを検索する。
名前 (name)、メール (mail)、日付 (date)、ID (id)、本文 (body) から
検索条件を選ぶことができます。

パーズ済みのメッセージのみを検索対象とするので、あらかじめ
`navi2ch-article-redraw-range' を使うなどして検索したいメッセージを
表示しておくこと。"
  (interactive)
  (let* ((has-id (navi2ch-article-get-current-id))
	 (ch (navi2ch-read-char-with-retry
	      (if has-id
		  "Search for: n)ame m)ail d)ate i)d b)ody: "
		"Search for: n)ame m)ail d)ate b)ody: ")
	      nil
	      (if has-id
		  '(?n ?m ?d ?i ?b)
		'(?n ?m ?d ?b)))))
    (cond
     ((eq ch ?n) (navi2ch-article-search-name))
     ((eq ch ?m) (navi2ch-article-search-mail))
     ((eq ch ?d) (navi2ch-article-search-date))
     ((eq ch ?i) (navi2ch-article-search-id))
     ((eq ch ?b) (navi2ch-article-search-body)))))

(defun navi2ch-article-search-name (&optional name)
  (interactive)
  (unless name
    (setq name (navi2ch-read-string "Name: "
				    (navi2ch-article-get-current-name)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'name (regexp-quote name)))

(defun navi2ch-article-search-mail (&optional mail)
  (interactive)
  (unless mail
    (setq mail (navi2ch-read-string "Mail: "
				    (navi2ch-article-get-current-mail)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'mail (regexp-quote mail)))

(defun navi2ch-article-search-date (&optional date)
  (interactive)
  (unless date
    (setq date (navi2ch-read-string "Date: "
				    (navi2ch-article-get-current-date)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'date
			       (concat (regexp-quote date)
				       (if (navi2ch-article-get-current-id)
					   ".* ID:" ""))))

(defun navi2ch-article-search-id (&optional id)
  (interactive)
  (unless id
    (setq id (navi2ch-read-string "ID: "
				  (navi2ch-article-get-current-id)
				  'navi2ch-search-history)))
  (navi2ch-article-search-subr 'date
			       (concat " ID:[^ ]*" (regexp-quote id))))

(defun navi2ch-article-search-body (&optional body)
  (interactive)
  (unless body
    (setq body (navi2ch-read-string "Body: "
				    (navi2ch-article-get-current-word-in-body)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'data (regexp-quote body)))

(defun navi2ch-article-search-subr (field regexp)
  (let (num-list len)
    (dolist (msg navi2ch-article-message-list)
      (when (and (listp (cdr msg))
		 (string-match regexp (or (cdr (assq field (cdr msg))) "")))
	(setq num-list (cons (car msg) num-list))))
    (setq len (length num-list))
    (if (= len 0)
	(message "No message found.")
      (navi2ch-popup-article (nreverse num-list))
      (message (format "%d message%s found."
		       len
		       (if (= len 1) "" "s"))))))

(defun navi2ch-article-save-dat-file (board article)
  (interactive (list navi2ch-article-current-board
		     navi2ch-article-current-article))
  (let ((file (navi2ch-article-get-file-name board article)))
    (cond ((not (file-exists-p file))
	   (message ".dat ファイルがありません。まずスレを取得してください。")
	   (ding))
	  ((not (file-readable-p file))
	   (message ".dat ファイルを読めません。")
	   (ding))
	  (t
	   (let ((newname (read-file-name
			   (format "Save .dat file to (default `%s'): "
				   (file-name-nondirectory file))
			   nil
			   (file-name-nondirectory file))))
	     (if (file-directory-p newname)
		 (setq newname (expand-file-name (file-name-nondirectory file)
						 newname)))
	     (when (or (not (file-exists-p newname))
		       (y-or-n-p "すでに存在します。上書きしますか? "))
	       (copy-file file newname t)
	       (message "`%s' に保存しました。" newname)))))))

;;; filter mode
(navi2ch-set-minor-mode 'navi2ch-article-message-filter-mode
                        " Filter"
                        navi2ch-article-message-filter-mode-map)

(defun navi2ch-article-get-message-filter-cache-file-name (board article)
  (concat (navi2ch-article-get-info-file-name board article) ".filter"))

(defun navi2ch-article-save-message-filter-cache (&optional board article cache)
  (unless (and board article cache)
    (let ((buffer (get-buffer (navi2ch-article-get-buffer-name board article))))
      (when buffer
	(with-current-buffer buffer
	  (or board (setq board navi2ch-article-current-board))
	  (or article (setq article navi2ch-article-current-article))
	  (or cache (setq cache navi2ch-article-message-filter-cache))))))
  (when cache
    (let ((file (navi2ch-article-get-message-filter-cache-file-name board article))
	  (alist (delq nil
		       (mapcar
			(lambda (key)
			  (let ((slot (assq key cache)))
			    (if (eq key 'replace)
				(let ((reps (delq nil
						  (mapcar
						   (lambda (reps-slot)
						     (let ((rep (delq nil
								      (mapcar
								       (lambda (rep-slot)
									 (and (cdr rep-slot)
									      (cons (car rep-slot)
										    (cadr rep-slot))))
								       (cdr reps-slot)))))
						       (and rep
							    (cons (car reps-slot) rep))))
						   (cdr slot)))))
				  (and reps
				       (cons key reps)))
			      (and (cdr slot)
				   slot))))
			navi2ch-article-save-message-filter-cache-keys))))
      (if (and (null alist)
	       (file-exists-p file))
	  (condition-case nil
	      (delete-file file)
	    (error nil))
	(navi2ch-save-info file alist)))))

(defun navi2ch-article-load-message-filter-cache (&optional board article)
  (or board (setq board navi2ch-article-current-board))
  (or article (setq article navi2ch-article-current-article))
  (let ((alist (navi2ch-load-info
		(navi2ch-article-get-message-filter-cache-file-name board article))))
    (dolist (reps-slot (cdr (assq 'replace alist)) alist)
      (dolist (rep-slot (cdr reps-slot))
	(setcdr rep-slot (list (cdr rep-slot)))))))

(defun navi2ch-article-toggle-replace-message (&optional prefix)
  "現在のレスの置換の有効・無効を切り替える。"
  (interactive "P")
  (let* ((unfilter (cdr (assq 'unfilter navi2ch-article-current-article)))
	 (num (navi2ch-article-get-current-number))
	 (rep (cdr (assq num
			 (cdr (assq 'replace navi2ch-article-message-filter-cache)))))
	 (msg "No replacement cached"))
    (when (catch 'loop
	    (dolist (slot rep)
	      (when (cdr slot)
		(throw 'loop t))))
      (if (memq num unfilter)
	  (setq unfilter (delq num unfilter)
		msg "Replace message")
	(setq unfilter (cons num unfilter)
	      msg "Undo replace message"))
      (setq navi2ch-article-current-article
	    (navi2ch-put-alist 'unfilter
			       unfilter
			       navi2ch-article-current-article))
      (let ((buffer-read-only nil))
	(navi2ch-article-save-view
	  (navi2ch-article-reinsert-partial-messages num num)))
      (when (and prefix
		 (memq num unfilter))
	(navi2ch-put-alist num
			   nil
			   (cdr (assq 'replace navi2ch-article-message-filter-cache)))
	(setq navi2ch-article-message-filter-cache
	      (navi2ch-put-alist
	       'cache
	       (delq num
		     (cdr (assq 'cache navi2ch-article-message-filter-cache)))
	       navi2ch-article-message-filter-cache))))
    (message msg)))

(defun navi2ch-article-toggle-message-filter (&optional prefix)
  "フィルタ機能の on/off を切り替える。
PREFIX が与えられた場合は、
キャッシュをクリアし、フィルタ機能を on にしてスレを表示しなおす。"
  (interactive "P")
  (if (null prefix)
      (progn
	(setq navi2ch-article-message-filter-mode
	      (not navi2ch-article-message-filter-mode))
	(when (and navi2ch-article-message-filter-mode
		   (null navi2ch-article-message-filter-cache))
	  (setq navi2ch-article-message-filter-cache
		(navi2ch-article-load-message-filter-cache))))
    (when navi2ch-article-message-filter-cache
      ;; フィルタ前のレスの状態をキャッシュから復元
      (dolist (reps-slot (cdr (assq 'replace navi2ch-article-message-filter-cache)))
	(let ((alist (navi2ch-article-get-message (car reps-slot))))
	  (dolist (rep-slot (cdr reps-slot))
	    (let ((org (cddr rep-slot)))
	      (when org
		(navi2ch-put-alist (car rep-slot) (car org) alist))))))
      (setq navi2ch-article-current-article
	    (navi2ch-put-alist 'hide
			       (navi2ch-set-difference
				(cdr (assq 'hide navi2ch-article-current-article))
				(cdr (assq 'hide navi2ch-article-message-filter-cache)))
			       navi2ch-article-current-article))
      (setq navi2ch-article-current-article
	    (navi2ch-put-alist 'important
			       (navi2ch-set-difference
				(cdr (assq 'important navi2ch-article-current-article))
				(cdr (assq 'important navi2ch-article-message-filter-cache)))
			       navi2ch-article-current-article))
      ;; キャッシュをクリア
      (setq navi2ch-article-message-filter-cache nil))
    (setq navi2ch-article-message-filter-mode t))
  (force-mode-line-update)
  (let ((buffer-read-only nil)
	(navi2ch-article-sort-message-filter-rules nil))
    (navi2ch-article-save-view
      (erase-buffer)
      (navi2ch-article-insert-messages
       navi2ch-article-message-list
       navi2ch-article-view-range)))
  navi2ch-article-message-filter-mode)

(defun navi2ch-article-add-message-filter-rule (&optional prefix)
  "レスのフィルタ条件を対話的に追加する。"
  (interactive "P")
  (let* ((has-id (navi2ch-article-get-current-id))
	 (char (navi2ch-read-char-with-retry
		(if has-id
		    "Filter by: n)ame m)ail i)d b)ody s)ubject: "
		  "Filter by: n)ame m)ail b)ody s)ubject: ")
		nil
		(if has-id
		    '(?n ?m ?i ?b ?s)
		  '(?n ?m ?b ?s)))))
    (cond
     ((eq char ?n) (navi2ch-article-add-message-filter-by-name prefix))
     ((eq char ?m) (navi2ch-article-add-message-filter-by-mail prefix))
     ((eq char ?i) (navi2ch-article-add-message-filter-by-id prefix))
     ((eq char ?b) (navi2ch-article-add-message-filter-by-message prefix))
     ((eq char ?s) (navi2ch-article-add-message-filter-by-subject prefix)))))

(defun navi2ch-article-add-message-filter-by-name (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-name-alist
   "Name: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-name))))

(defun navi2ch-article-add-message-filter-by-mail (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-mail-alist
   "Mail: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-mail))))

(defun navi2ch-article-add-message-filter-by-id (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-id-alist
   "ID: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-id))))

(defun navi2ch-article-add-message-filter-by-message (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-message-alist
   "Body: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-word-in-body))))

(defun navi2ch-article-add-message-filter-by-subject (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-subject-alist
   "Subject: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-subject))))

(defun navi2ch-article-add-message-filter-rule-subr (variable
						     prompt
						     &optional initial-input)
  (let* ((match (navi2ch-article-read-message-filter-match prompt
							   initial-input))
	 (current (assoc match (symbol-value variable)))
	 (result (navi2ch-article-read-message-filter-result (cdr current))))
    (set variable (cons (cons match result)
			(delq current (symbol-value variable))))
    (navi2ch-auto-modify-variables (list variable))
    (when (y-or-n-p "Apply new rules to current messages now? ")
      (navi2ch-article-toggle-message-filter t))))

(defun navi2ch-article-read-message-filter-match (prompt
						  &optional initial-input)
  (let (char str)
    (when (y-or-n-p "Use extensional match? ")
      (setq char (navi2ch-read-char-with-retry
		  "Match for: s)ubstring f)uzzy-substring e)xact-string r)egxp"
		  nil
		  '(?s ?f ?e ?r)))
      (when (and (eq char ?r)
		 initial-input)
	(setq initial-input (regexp-quote initial-input)))
      (unless (y-or-n-p "Ignore case? ")
	(setq char (upcase char))))
    (setq str (navi2ch-read-string prompt
				   initial-input
				   'navi2ch-search-history))
    (if char
	(list str (intern (char-to-string char)))
      str)))

(defun navi2ch-article-read-message-filter-result (&optional initial-input)
  (let ((char (navi2ch-read-char-with-retry
	       "Result: r)eplace h)ide i)mportant s)core"
	       nil
	       '(?r ?h ?i ?s))))
    (cond
     ((eq char ?r)
      (navi2ch-read-string "Relace with: "
			   (if (stringp initial-input)
			       initial-input
			     "あぼぼーん")))
     ((eq char ?h)
      'hide)
     ((eq char ?i)
      'important)
     ((eq char ?s)
      (string-to-number
       (navi2ch-read-string "Score: "
			    (if (numberp initial-input)
				(number-to-string initial-input)
			      "0")))))))

(defun navi2ch-article-remove-article ()
  (interactive)
  (let ((board navi2ch-article-current-board)
	(article navi2ch-article-current-article))
    (when (and board article)
      (navi2ch-article-exit)
      (navi2ch-bm-remove-article-subr board article))))

(defun navi2ch-article-orphan-p (board article)
  "BOARD と ARTICLE で指定されるスレがオルファンな場合、non-nil を返す。"
  (cond ((navi2ch-bookmark-exist-all board article) nil)
	((let ((subject-list (navi2ch-board-get-subject-list
			      (navi2ch-board-get-file-name board)))
	       (artid (cdr (assq 'artid article))))
	   (catch 'break
	     (dolist (s subject-list)
	       (if (equal (cdr (assq 'artid s)) artid)
		   (throw 'break t)))))
	 nil)
	(t t)))

(defun navi2ch-article-url-at-point (point)
  "POINT の下のリンクを指す URL を得る。"
  (let ((number-property (get-text-property point 'number))
	(url-property (get-text-property point 'url)))
    (cond (number-property
	   (navi2ch-article-number-list-to-url
	    (navi2ch-article-get-number-list number-property)))
	  (url-property))))

(run-hooks 'navi2ch-article-load-hook)
;;; navi2ch-article.el ends here
