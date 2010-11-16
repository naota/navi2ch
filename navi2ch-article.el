;;; navi2ch-article.el --- article view module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
;; 2009 by Navi2ch Project

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

(eval-when-compile 
  (require 'cl)
  (defvar navi2ch-board-buffer-name)
  (defvar navi2ch-board-current-board)
  (defvar navi2ch-board-subject-list)
  (defvar navi2ch-board-last-seen-alist)
  (defvar navi2ch-popup-article-current-board)
  (defvar navi2ch-popup-article-current-article)
  (require 'wid-edit))
  
(require 'base64)
(require 'widget)

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
    (unless (featurep 'xemacs)
      (define-key map [follow-link] 'mouse-face))
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
    (define-key map [(iso-left-tab)] 'navi2ch-article-previous-link)
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
    (define-key map "!" 'navi2ch-article-add-message-filter-cus)
    (define-key map "\C-c\C-r" 'navi2ch-article-remove-article)
    (navi2ch-ifxemacs
	(define-key map  "\C-c\C- " 'navi2ch-article-toggle-sticky)
      (define-key map [(control c) (control ? )] 'navi2ch-article-toggle-sticky))
    (define-key map "u" 'navi2ch-show-url-at-point)
    (define-key map "\C-c\C-y" 'navi2ch-article-write-cite-message)
    (define-key map "," 'navi2ch-thumbnail-select-current-link)
    (define-key map "V" 'navi2ch-thumbnail-save-content)
    (define-key map "\e\r" 'navi2ch-article-select-current-link)
    (define-key map "D" 'navi2ch-thumbnail-image-delete-cache)
    (define-key map "T" 'navi2ch-thumbnail-all-show)
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
  "$BI=<($9$k%9%l%C%I$NHO0O!#(B
$B=q<0$O(B '(first . last) $B$G!"(B
first $B$,:G=i$+$i$$$/$DI=<($9$k$+!"(B
last $B$,:G8e$+$i$$$/$DI=<($9$k$+!#(B
$BNc$($P!"(B(10 . 50) $B$G!":G=i$N(B10$B$H:G8e$N(B50$B$rI=<((B")

(defvar navi2ch-article-buffer-name-prefix "*navi2ch article ")
(defvar navi2ch-article-current-article nil)
(defvar navi2ch-article-current-board nil)
(defvar navi2ch-article-message-list nil)
(defvar navi2ch-article-point-stack nil "$B0LCV$r3P$($H$/(B stack")
(defvar navi2ch-article-poped-point-stack nil)
(defvar navi2ch-article-hide-mode nil)
(defvar navi2ch-article-through-next-function 'navi2ch-article-through-next)
(defvar navi2ch-article-through-previous-function 'navi2ch-article-through-previous)
(defvar navi2ch-article-through-forward-line-function 'navi2ch-bm-forward-line
  "$BA08e$N%9%l$K0\F0$9$k$?$a$K<B9T$5$l$k4X?t!#(B
$B0l$D$N@0?t$r0z?t$K$H$j!"$=$N?t$@$18e$m(B($B0z?t$,Ii$N$H$-$OA0(B)$B$N%9%l$K0\F0$7!"(B
$B0\F0$G$-$?$i(B 0$B!"$G$-$J$1$l$P(B 0 $B0J30$N@0?t$rJV$94X?t$G$"$k$3$H!#(B")

(defvar navi2ch-article-save-info-keys
  '(number name time hide important unfilter mail kako response down compressed))

(defvar navi2ch-article-insert-message-separator-function
  (if (and window-system
	   (eq emacs-major-version 20)
	   (not (featurep 'xemacs)))
      'navi2ch-article-insert-message-separator-by-face
    'navi2ch-article-insert-message-separator-by-char)
  "$B%;%Q%l!<%?$rA^F~$9$k4X?t!#(B")

(defvar navi2ch-article-summary-file-name "article-summary")
(defvar navi2ch-article-local-dat-regexp "[0-9]+\\.dat\\(?:.gz\\)?\\'"
  "$B%m!<%+%k$K$"$k(B dat $B%U%!%$%k$rI=$o$9@55,I=8=!#(B
$B%5!<%P$K$"$k(B dat $B%U%!%$%k$K$O$3$NJQ?t$r;H$C$F$O$$$1$J$$!#(B")

(defvar navi2ch-article-link-internal nil)

(defconst navi2ch-article-separator " *<> *")

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
  '(cache replace hide important))

;; sticky mode
(defvar navi2ch-article-sticky-mode nil)
(make-variable-buffer-local 'navi2ch-article-sticky-mode)
(add-to-list 'minor-mode-alist '(navi2ch-article-sticky-mode " Sticky"))

(defvar navi2ch-article-message-filter-default-rule-alist
  '((?n
     :var navi2ch-article-message-filter-by-name-alist
     :string navi2ch-article-get-current-name)
    (?m 
     :var navi2ch-article-message-filter-by-mail-alist
     :string navi2ch-article-get-current-mail)
    (?i 
     :var navi2ch-article-message-filter-by-id-alist
     :string navi2ch-article-get-current-id
     :scope board-local
     :date t)
    (?h 
     :var navi2ch-article-message-filter-by-hostname-alist
     :string navi2ch-article-get-current-hostname)
    (?b 
     :var navi2ch-article-message-filter-by-message-alist
     :string (lambda () 
	       (or (navi2ch-article-get-current-word-in-body)
		   "")))
    (?s 
     :var navi2ch-article-message-filter-by-subject-alist
     :string navi2ch-article-get-current-subject)
    (t :match-method "s")))
     
(defvar navi2ch-article-message-filter-wid-string)
(defvar navi2ch-article-message-filter-wid-rule)
(defvar navi2ch-article-message-filter-wid-method)
(defvar navi2ch-article-message-filter-wid-case)
(defvar navi2ch-article-message-filter-wid-invert)
(defvar navi2ch-article-message-filter-wid-scope)
(defvar navi2ch-article-message-filter-wid-float)
(defvar navi2ch-article-message-filter-wid-var)
(defvar navi2ch-article-message-filter-wid-date)
(defvar navi2ch-article-message-filter-wid-window-configuration)

;; JIT
(defvar navi2ch-article-jit-interval 0.1)
(defvar navi2ch-article-jit-timer nil)
(defvar navi2ch-article-use-jit nil)
(defvar navi2ch-article-jit-buffers nil)
(defvar navi2ch-article-jit-need-insert nil)

;; local variables
(make-variable-buffer-local 'navi2ch-article-current-article)
(make-variable-buffer-local 'navi2ch-article-current-board)
(make-variable-buffer-local 'navi2ch-article-message-list)
(make-variable-buffer-local 'navi2ch-article-message-filter-cache)
(make-variable-buffer-local 'navi2ch-article-point-stack)
(make-variable-buffer-local 'navi2ch-article-poped-point-stack)
(make-variable-buffer-local 'navi2ch-article-view-range)
(make-variable-buffer-local 'navi2ch-article-through-next-function)
(make-variable-buffer-local 'navi2ch-article-through-previous-function)

(make-variable-buffer-local 'navi2ch-article-message-filter-wid-string)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-rule)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-method)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-case)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-invert)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-scope)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-float)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-var)
(make-variable-buffer-local 'navi2ch-article-message-filter-wid-date)

(make-variable-buffer-local 'navi2ch-article-jit-need-insert)

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

(defsubst navi2ch-article-get-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat (cdr (assq 'artid article)) 
				       (if (cdr (or (assq 'compressed article)
						    (assq 'compressed
							  (navi2ch-article-load-info
							   board
							   article))))
					   ".dat.gz"
					 ".dat"))))

(defsubst navi2ch-article-get-info-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat "info/" (cdr (assq 'artid article)))))

(defsubst navi2ch-article-file-name-to-artid (filename)
  "*FILENAME $B$r%9%l(BID$B$KJQ49$9$k!#(B"
  (file-name-sans-extension (file-name-sans-extension 
			     (file-name-nondirectory filename))))


(defsubst navi2ch-article-inside-range-p (num range len)
  "NUM $B$,(B RANGE $B$G<($9HO0O$KF~$C$F$k$+!#(B
LEN $B$O(B RANGE $B$GHO0O$r;XDj$5$l$k(B list $B$ND9$5!#(B"
  (or (not range)
      (<= num (car range))
      (> num (- len (cdr range)))))

(defsubst navi2ch-article-get-buffer-name (board article)
  (concat navi2ch-article-buffer-name-prefix
	  (navi2ch-article-get-url board article 'no-kako)))

(defsubst navi2ch-article-check-cached (board article)
  "BOARD $B$H(B ARTICLE $B$G;XDj$5$l$k%9%l%C%I$,%-%c%C%7%e$5$l$F$k$+!#(B"
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
  "BODY $BFb$G8=:_I=<($7$F$$$k%9%l$rI=<($7$J$*$9$H$-$K!"(B
$B%&%#%s%I%&Fb$N%+!<%=%k$N0LCV$r$G$-$k$@$10];}$9$k!#(B"
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
  "URL $B$+$i(B article $B$KJQ49!#(B"
  (navi2ch-multibbs-url-to-article url))

(defun navi2ch-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE $B$+$i(B url $B$KJQ49!#(B
START, END, NOFIRST $B$GHO0O$r;XDj$9$k(B"
  (navi2ch-multibbs-article-to-url board article start end nofirst))

(defun navi2ch-article-cleanup-message ()
  (let (re)
    (when navi2ch-article-cleanup-trailing-newline ; $B%l%9KvHx$N6uGr$r<h$j=|$/(B
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
	(replace-match "<br>")))))	; "\n" $B$G$b$$$$$+$b!#(B

(defun navi2ch-article-parse-message (str)
  (unless (string= str "")
    (let ((board navi2ch-article-current-board)
	  (article navi2ch-article-current-article))
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
				     (if (re-search-forward navi2ch-article-separator nil t)
					 (copy-marker (match-beginning 0))
				       (goto-char max)
				       max))))
		       syms))
	  (let ((start (car (cdr (assq 'name alist))))
		(end (cdr (cdr (assq 'name alist)))))
	    (when (and start end)
	      (goto-char start)
	      (while (re-search-forward "\\(</b>[^<]+<b>\\)\\|\\(<font[^>]+>[^<]+</font>\\)" end t)
		;; fusianasan $B$d%H%j%C%W$J$I(B
		(replace-match (navi2ch-propertize (match-string 0)
						   'navi2ch-fusianasan-flag t)
			       t t))))
	  (navi2ch-replace-html-tag-with-buffer)
	  (dolist (x alist)
	    (setcdr x (buffer-substring (cadr x) (cddr x))))
	  alist)))))

(defun navi2ch-article-get-separator ()
  (save-excursion
    (let ((string (buffer-substring (navi2ch-line-beginning-position)
				    (navi2ch-line-end-position))))
      (if (or (string-match "<>.*<>.*<>" string)
	      (not (string-match ",.*,.*," string)))
	  " *<> *"
	" *, *"))))

(defun navi2ch-article-get-first-message ()
  "current-buffer $B$N(B article $B$N:G=i$N(B message $B$rJV$9!#(B"
  (goto-char (point-min))
  (navi2ch-article-parse-message
   (buffer-substring-no-properties (point)
				   (progn (forward-line 1)
					  (1- (point))))))

(defun navi2ch-article-get-first-message-from-file (file &optional board)
  "FILE $B$G;XDj$5$l$?(B article $B$N:G=i$N(B message $B$rJV$9!#(B
BOARD non-nil $B$J$i$P!"$=$NHD$N(B coding-system $B$r;H$&!#(B"
  (with-temp-buffer
    (navi2ch-board-insert-file-contents board file)
    (navi2ch-apply-filters board navi2ch-article-filter-list)
    (navi2ch-article-get-first-message)))

(defun navi2ch-article-get-message-list (file &optional begin end)
  "FILE $B$N(B BEGIN $B$+$i(B END $B$^$G$NHO0O$+$i%9%l$N(B list $B$r:n$k!#(B
$B6u9T$O(B nil$B!#(B"
  (when (file-exists-p file)
    (let ((board navi2ch-article-current-board)
	  message-list)
      (with-temp-buffer
        (navi2ch-board-insert-file-contents board file begin end)
	(run-hooks 'navi2ch-article-get-message-list-hook)
        (let ((i 1))
	  (navi2ch-apply-filters board navi2ch-article-filter-list)
          (message "Splitting current messages...")
          (goto-char (point-min))
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
          (message "Splitting current messages...done")))
      (nreverse message-list))))

(defun navi2ch-article-append-message-list (list1 list2)
  (let ((num (length list1)))
    (append list1
	    (mapcar
	     (lambda (x)
	       (setq num (1+ num))
	       (cons num (cdr x)))
	     list2))))

(defun navi2ch-article-update-previous-message-separator ()
  "$B8=:_0LCV$ND>A0$N%l%96h@Z$r99?7$9$k!#(B"
  (let ((old-pos (point-marker)))
    (set-marker-insertion-type old-pos t)
    (save-excursion
      (let ((buffer-read-only nil)
	    end beg)
	(if (= (point) (point-max))
	    (setq end (point))
	  (setq end (previous-single-property-change (point) 'message-separator)))
	(when end
	  (if (get-text-property (max (1- end) (point-min)) 'message-separator)
	      (setq beg (previous-single-property-change end 'message-separator))
	    (setq beg end)
	    (setq end (next-single-property-change beg 'message-separator))))
	(when (and beg end)
	  (let ((number (get-text-property beg 'message-separator)))
	    (goto-char beg)
	    (delete-region beg end)
	    (navi2ch-article-insert-message-separator number)))))
    (goto-char old-pos)))

(defun navi2ch-article-insert-message-separator (number)
  "$B%l%96h@Z$rA^F~$9$k!#(B"
  (let ((p (point)))
    (funcall navi2ch-article-insert-message-separator-function)
    (when (and navi2ch-article-message-separator-insert-hide-number-p
	       (navi2ch-article-insert-hide-number-following number))
      (funcall navi2ch-article-insert-message-separator-function))
    (when navi2ch-article-message-separator-insert-trailing-newline-p
      (insert "\n"))
    (put-text-property p (point) 'message-separator number)))

(defun navi2ch-article-insert-hide-number-following (number)
  "$B%l%9HV9f(B NUMBER $B$N8e$KB3$/(B hide $B$5$l$?%l%9?t$rA^F~$9$k!#(B
$BA^F~$7$?>l9g$O(B non-nil $B$rJV$9!#(B"
  (unless (or navi2ch-article-hide-mode navi2ch-article-important-mode)
    (let (hide beg end cnt)
      ;; hide $B>pJs$O(B filter mode $B$+$I$&$+$GJQ$o$C$F$/$k(B
      (setq hide
	    (funcall
	     (if navi2ch-article-message-filter-mode
		 'navi2ch-union
	       'navi2ch-set-difference)
	     (cdr (assq 'hide navi2ch-article-current-article))
	     (cdr (assq 'hide navi2ch-article-message-filter-cache))))
      (setq beg (car (memq (1+ number) hide)))
      (when beg
	(setq end beg)
	(while (memq (1+ end) hide)
	  (setq end (1+ end)))
	(setq cnt (1+ (- end beg)))
	(let ((number-str (if (= cnt 1)
			      (format "%d" beg)
			    (format "%d-%d" beg end))))
	  (insert (format "[%d hidden message(s) (" cnt))
	  (let ((pos (point)))
	    (insert ">>" number-str)
	    (navi2ch-article-set-link-property-subr pos (point)
						    'number number-str)
	    (insert ")]")))
	'found))))

(defun navi2ch-article-insert-message-separator-by-face ()
  (let ((p (point)))
    (insert "\n")
    (put-text-property p (point) 'face 'underline)))

(defun navi2ch-article-insert-message-separator-by-char ()
  (let ((pos (point)))
    (insert (make-string (max 0
			      (- (eval navi2ch-article-message-separator-width)
				 (current-column)))
			 navi2ch-article-message-separator))
    (put-text-property pos (point) 'face 'navi2ch-article-message-separator-face)
    (insert "\n")))

(defun navi2ch-article-set-link-property-subr (start end type value
							&optional object)
  (let ((face (cond ((eq type 'number) 'navi2ch-article-link-face)
		    ((eq type 'url) 'navi2ch-article-url-face))))
        (add-text-properties start end
			 (list 'face face
			       'help-echo #'navi2ch-article-help-echo
			       'navi2ch-link-type type
			       'navi2ch-link value
			       'mouse-face navi2ch-article-mouse-face)
			 object)))

(defun navi2ch-article-link-regexp-alist-to-internal ()
  (navi2ch-regexp-alist-to-internal
   (append
    navi2ch-article-link-regexp-alist
    (list (cons (concat navi2ch-article-number-prefix-regexp
			navi2ch-article-number-number-regexp)
		(lambda (match)
		  (navi2ch-article-set-link-property-subr
		   (match-beginning 0) (match-end 0)
		   'number (navi2ch-match-string-no-properties 1))
		  (while (looking-at (concat
				      navi2ch-article-number-separator-regexp
				      navi2ch-article-number-number-regexp))
		    (navi2ch-article-set-link-property-subr
		     (match-beginning 1) (match-end 1)
		     'number (navi2ch-match-string-no-properties 1))
		    (goto-char (max (1+ (match-beginning 0))
				    (match-end 0))))))
	  (cons navi2ch-article-url-regexp
		(lambda (url)
		  (if (string-match "\\`\\(h?t?tp\\)\\(s?:\\)" url)
		      (replace-match "http\\2" nil nil url)
		    url)))))))  

(defun navi2ch-article-set-link-property ()
  ">>1 $B$H$+(B http:// $B$K(B property $B$rIU$1$k!#(B"
  (goto-char (point-min))
  (let* ((reg-internal (or navi2ch-article-link-internal
			   (navi2ch-article-link-regexp-alist-to-internal)))
	 match rep literal)
    (while (setq match (navi2ch-re-search-forward-regexp-alist reg-internal nil t))
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

(defun navi2ch-article-put-cite-face ()
  (goto-char (point-min))
  (while (re-search-forward navi2ch-article-citation-regexp nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0)
		       'face 'navi2ch-article-citation-face)))

(defun navi2ch-article-arrange-message ()
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

(defun navi2ch-article-insert-message (num alist)
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
	;; (navi2ch-article-cleanup-message) ; $B$d$C$QCY$$(B
	(put-text-property (point-min) (point-max) 'face
			   'navi2ch-article-face)
	(navi2ch-article-put-cite-face)
	(navi2ch-article-set-link-property)
        (if navi2ch-article-auto-decode-p
            (navi2ch-article-auto-decode-encoded-section))
	(navi2ch-article-arrange-message)
        ;; $B%5%`%M%$%k2hA|$N%-%c%C%7%e$,$"$C$?$iI=<((B
	(navi2ch-thumbnail-insert-image-reload))))
  (navi2ch-article-insert-message-separator num))

(defun navi2ch-article-insert-messages (list range)
  "LIST $B$r@07A$7$FA^F~$9$k!#(B"
  (let ((msg (if navi2ch-article-message-filter-mode
		 "Filtering and inserting current messages..."
	       "Inserting current messages..."))
	(len (length list))
	(hide (cdr (assq 'hide navi2ch-article-current-article)))
	(imp (cdr (assq 'important navi2ch-article-current-article)))
	(unfilter (cdr (assq 'unfilter navi2ch-article-current-article)))
	(cache (cdr (assq 'cache navi2ch-article-message-filter-cache)))
	(rep (cdr (assq 'replace navi2ch-article-message-filter-cache)))
	(orig (cdr (assq 'original navi2ch-article-message-filter-cache)))
	(progress 0)
	(percent 0)
	(navi2ch-article-link-internal (navi2ch-article-link-regexp-alist-to-internal)))
    (unless navi2ch-article-use-jit (message msg))
    (let ((func (if navi2ch-article-message-filter-mode
		    #'navi2ch-union
		  #'navi2ch-set-difference)))
      (setq hide (funcall func
			  hide
			  (cdr (assq 'hide
				     navi2ch-article-message-filter-cache))))
      (setq imp (funcall func
			 imp
			 (cdr (assq 'important
				    navi2ch-article-message-filter-cache)))))
    (setq navi2ch-article-current-article
	  (navi2ch-put-alist 'hide hide navi2ch-article-current-article))
    (setq navi2ch-article-current-article
	  (navi2ch-put-alist 'important imp navi2ch-article-current-article))
    (dolist (x list)
      (let* ((num (car x))
	     (alist (cdr x))
	     (rep-alist (cdr (assq num rep)))
	     (orig-alist (cdr (assq num orig)))
	     suppress)
        (when (and alist
		   (cond (navi2ch-article-hide-mode
			  (memq num hide))
			 (navi2ch-article-important-mode
			  (memq num imp))
			 (t
			  (and (navi2ch-article-inside-range-p num range len)
			       (not (memq num hide))))))
          (if (stringp alist)
	      (progn
		(setq alist (navi2ch-article-parse-message alist))
		(cond
		 ((and (string= "$B$"$\!<$s(B" (cdr (assq 'date alist)))
		       (memq num cache))
		  ;; $B?7$7$/!V$"$\!<$s!W$5$l$?%l%9$O%-%c%C%7%e$r%/%j%"(B
		  (setq unfilter (delq num unfilter))
		  (setq navi2ch-article-current-article
			(navi2ch-put-alist
			 'unfilter
			 unfilter
			 navi2ch-article-current-article))
		  (setq cache (delq num cache))
		  (setq navi2ch-article-message-filter-cache
			(navi2ch-put-alist
			 'cache
			 cache
			 navi2ch-article-message-filter-cache)))
		 (rep-alist
		  ;; $BCV498e$N%-%c%C%7%e$,$"$k>l9g$OCV49A0$N%l%9$rB`Hr(B
		  (setq orig-alist (mapcar
				    (lambda (x)
				      (cons (car x)
					    (cdr (assq (car x) alist))))
				    rep-alist))
		  (setq orig (navi2ch-put-alist num orig-alist orig))
		  (setq navi2ch-article-message-filter-cache
			(navi2ch-put-alist
			 'original
			 orig
			 navi2ch-article-message-filter-cache)))))
	    (dolist (slot alist)
	      (when (stringp (cdr slot))
		(set-text-properties 0 (length (cdr slot)) nil (cdr slot)))))
	  (if (and navi2ch-article-message-filter-mode
		   (not (memq num unfilter)))
	      (if (and navi2ch-article-use-message-filter-cache
		       (memq num cache))
		  ;; $BCV498e$N%l%9$r%-%c%C%7%e$+$iCj=P(B
		  (dolist (slot rep-alist)
		    (when (cdr slot)
		      (navi2ch-put-alist (car slot) (cdr slot) alist)))
		;; $B%U%#%k%?=hM}$NK\BN(B
		(let ((result (navi2ch-article-apply-message-filters
			       (navi2ch-put-alist 'number num alist))))
		  (when (and (eq result 'hide)
			     (not navi2ch-article-hide-mode))
		    (setq suppress t))))
	    ;; $BCV49A0$N%l%9$r%-%c%C%7%e$+$iI|85(B
	    (dolist (slot orig-alist)
	      (when (cdr slot)
		(navi2ch-put-alist (car slot) (cdr slot) alist))))
	  (setq alist (navi2ch-put-alist 'point (point-marker) alist))
	  (setcdr x alist)
	  ;; (setcdr x (navi2ch-put-alist 'point (point) alist))
	  (if suppress
	      (navi2ch-article-update-previous-message-separator)
	    (navi2ch-article-insert-message num alist)
	    (set-marker-insertion-type (cdr (assq 'point alist)) t)))
	;; $B?JD=I=<((B
	(unless navi2ch-article-use-jit
	  (and (> (setq progress (+ progress 100)) 10000)
	       (/= (/ progress len) percent)
	       (navi2ch-no-logging-message
		"%s%d%%" msg (setq percent (/ progress len)))))))
    (unless navi2ch-article-use-jit (message "%sdone" msg))))

(defsubst navi2ch-article-get-message (num)
  "NUM $BHVL\$N%l%9$rF@$k!#(B"
  (cdr (assq num navi2ch-article-message-list)))

(defun navi2ch-article-reinsert-partial-messages (start &optional end)
  "START $BHVL\$+$i!":G8e$^$?$O(B END $BHVL\$^$G$N%l%9$rA^F~$7$J$*$9!#(B"
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
	    (cdr (assq 'point (navi2ch-article-get-message (car visible))))))
    (if (null start-point)
	(goto-char (point-max))
      (goto-char start-point)
      (delete-region start-point (or end-point (point-max))))
    (navi2ch-article-insert-messages list nil)))

(defun navi2ch-article-apply-message-filters (alist)
  (let* ((num (cdr (assq 'number alist)))
	 (orig (cdr (assq 'original navi2ch-article-message-filter-cache))))
    ;; $BCV49A0$N%l%9$r%-%c%C%7%e$+$iI|85(B
    (dolist (slot (cdr (assq num orig)))
      (when (cdr slot)
	(navi2ch-put-alist (car slot) (cdr slot) alist)))
    (let ((old-alist (copy-alist alist))
	  score result)
      (catch 'loop
	(dolist (filter navi2ch-article-message-filter-list)
	  (setq result (funcall filter alist))
	  (cond ((numberp result)
		 (setq score (+ (or score 0) result)))
		(result
		 (throw 'loop nil)))))
      (when score
	(cond
	 ((and navi2ch-article-message-add-important-above
	       (> score navi2ch-article-message-add-important-above))
	  (setq result 'important))
	 ((and navi2ch-article-message-replace-below
	       navi2ch-article-message-hide-below
	       (< score (car navi2ch-article-message-replace-below))
	       (< score navi2ch-article-message-hide-below))
	  ;; `navi2ch-article-message-replace-below' $B$H(B
	  ;; `navi2ch-article-message-hide-below' $B$,N>J}$H$b(B
	  ;; $BE,MQ$5$l$&$k>l9g!"$7$-$$CM$NDc$$J}$rM%@h(B
	  (if (< navi2ch-article-message-hide-below
		 (car navi2ch-article-message-replace-below))
	      (setq result 'hide)
	    (setq result (cdr navi2ch-article-message-replace-below))))
	 ((and navi2ch-article-message-replace-below
	       (< score (car navi2ch-article-message-replace-below)))
	  (setq result (cdr navi2ch-article-message-replace-below)))
	 ((and navi2ch-article-message-hide-below
	       (< score navi2ch-article-message-hide-below))
	  (setq result 'hide))))
      (cond
       ((stringp result)
	(let ((case-fold-search nil))
	  (navi2ch-put-alist 'name result alist)
	  (navi2ch-put-alist 'data result alist)
	  (navi2ch-put-alist 'mail
			     (if (string-match "sage"
					       (cdr (assq 'mail alist)))
				 "sage"
			       "")
			     alist)
	  (navi2ch-put-alist 'date
			     (navi2ch-replace-string " ID:.*"
						     ""
						     (cdr (assq 'date alist)))
			     alist)))
       ((memq result '(hide important unfilter))
	(let ((nums (cdr (assq result navi2ch-article-current-article))))
	  (unless (memq num nums)
	    (setq navi2ch-article-current-article
		  (navi2ch-put-alist result
				     (cons num nums)
				     navi2ch-article-current-article))))))
      (unless (equal old-alist alist)
	(setq navi2ch-article-message-filter-cache
	      (navi2ch-put-alist
	       'original
	       (navi2ch-put-alist
		num
		(navi2ch-set-difference old-alist alist)
		orig)
	       navi2ch-article-message-filter-cache)))
      (when navi2ch-article-use-message-filter-cache
	(unless (equal alist old-alist)
	  (setq navi2ch-article-message-filter-cache
		(navi2ch-put-alist
		 'replace
		 (navi2ch-put-alist
		  num
		  (copy-alist (navi2ch-set-difference alist old-alist))
		  (cdr (assq 'replace navi2ch-article-message-filter-cache)))
		 navi2ch-article-message-filter-cache)))
	(unless (or (memq result '(nil cache replace original unfilter))
		    (stringp result))
	  (setq navi2ch-article-message-filter-cache
		(navi2ch-put-alist
		 result
		 (cons num
		       (cdr (assq result navi2ch-article-message-filter-cache)))
		 navi2ch-article-message-filter-cache)))
	(setq navi2ch-article-message-filter-cache
	      (navi2ch-put-alist
	       'cache
	       (cons num
		     (cdr (assq 'cache navi2ch-article-message-filter-cache)))
	       navi2ch-article-message-filter-cache)))
      result)))

(defun navi2ch-article-extract-date (str)
  (cond
   ((not (stringp str))
    nil)
   ((string-match "^[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]" str)
    (match-string 0 str))
   ((string-match "^[0-9][0-9]/[0-9][0-9]/[0-9][0-9]" str)
    (concat "20" (match-string 0 str)))
   (t nil)))

(defun navi2ch-article-message-filter-by-name (alist)
  (when navi2ch-article-message-filter-by-name-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-name-alist
     (cdr (assq 'name alist))
     (navi2ch-article-extract-date (cdr (assq 'date alist))))))

(defun navi2ch-article-message-filter-by-message (alist)
  (when navi2ch-article-message-filter-by-message-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-message-alist
     (cdr (assq 'data alist))
     (navi2ch-article-extract-date (cdr (assq 'date alist))))))

(defun navi2ch-article-message-filter-by-id (alist)
  (let ((case-fold-search nil))
    (when (and navi2ch-article-message-filter-by-id-alist
	       (string-match " ID:\\([^ ]+\\)"
			     (cdr (assq 'date alist))))
      (navi2ch-article-message-filter-subr
       navi2ch-article-message-filter-by-id-alist
       (match-string 1 (cdr (assq 'date alist)))
       (navi2ch-article-extract-date (cdr (assq 'date alist)))))))

(defun navi2ch-article-message-filter-by-mail (alist)
  (when navi2ch-article-message-filter-by-mail-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-mail-alist
     (cdr (assq 'mail alist))
     (navi2ch-article-extract-date (cdr (assq 'date alist))))))

(defun navi2ch-article-message-filter-by-subject (alist)
  (when navi2ch-article-message-filter-by-subject-alist
    (navi2ch-article-message-filter-subr
     navi2ch-article-message-filter-by-subject-alist
     (if (equal (or (cdr (assq 'subject alist)) "") "")
	 (navi2ch-article-get-current-subject)
       (cdr (assq 'subject alist)))
     (navi2ch-article-extract-date (cdr (assq 'date alist))))))

(defun navi2ch-article-message-filter-by-hostname (alist)
  (let ((case-fold-search nil)
	(date (cdr (assq 'date alist))))
    (when (and navi2ch-article-message-filter-by-hostname-alist
	       (or (string-match "\\[ \\([^ ]+\\) \\]" date)
		   (string-match "$BH/?.85(B:\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" date)))
      (navi2ch-article-message-filter-subr
       navi2ch-article-message-filter-by-hostname-alist
       (match-string 1 date)
       (navi2ch-article-extract-date (cdr (assq 'date alist)))))))

(defun navi2ch-article-message-filter-subr (rules string date)
  (let ((board-id (cdr (assq 'id navi2ch-article-current-board)))
	(artid (cdr (assq 'artid navi2ch-article-current-article)))
	score)
    (catch 'loop
      (dolist (rule rules)
	(when (or (not (consp (car rule)))
		  (and (equal (or (plist-get (car rule) :board-id) board-id)
			      (and (equal (or (plist-get (car rule) :artid) artid)
					  artid)
				   board-id))
		       (or (null (and date (plist-get (car rule) :date)))
			   (equal date (plist-get (car rule) :date)))))
	  (let* ((char (and (consp (car rule))
			    (stringp (car (car rule)))
			    (string-to-char (symbol-name (cadr (car rule))))))
		 (case-fold-search (and char
					(eq char
					    (setq char (downcase char)))))
		 (regexp (cond
			  ((null char)
			   (regexp-quote (car rule)))
			  ((eq char ?r)
			   (car (car rule)))
			  ((eq char ?s)
			   (regexp-quote (car (car rule))))
			  ((eq char ?e)
			   (concat "\\`" (regexp-quote (car (car rule))) "\\'"))
			  ((eq char ?f)
			   (navi2ch-fuzzy-regexp (car (car rule))
						 case-fold-search
						 "[$B!!(B \f\t\n\r\v]*"))
			  (t
			   ""))))
	    (when (if (and char
			   (plist-get (car rule) :invert))
		      (not (string-match regexp string))
		    (string-match regexp string))
	      ;; $BE,MQ$7$?%U%#%k%?>r7o$r(B age
	      (when (and (not (eq rule (car rules)))
			 (> (or (and char
				     (plist-get (car rule) :float))
				(if navi2ch-article-sort-message-filter-rules
				    1
				  0))
			    0))
		(setcdr rules (cons (car rules) (delq rule (cdr rules))))
		(setcar rules rule))
	      (if (numberp (cdr rule))
		  (setq score (+ (or score 0) (cdr rule)))
		(throw 'loop
		       (if (and char
				(stringp (cdr rule))
				(not (plist-get (car rule) :invert)))
			   (navi2ch-expand-newtext (cdr rule) string)
			 (cdr rule))))))))
      score)))

(defun navi2ch-article-separator-filter ()
  "$B5l7A<0$N%;%Q%l!<%?$r;HMQ$7$F$$$k%9%l$r?77A<0$KJQ49$9$k%U%#%k%?!#(B"
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^.*,.*,.*,")
      (while (re-search-forward ",\\|$B!w!.(B" nil t)
	(if (string= (match-string 0) ",")
	    (replace-match "<>")
	  (replace-match ","))))))

(defun navi2ch-article-date-format-be2ch (date)
  "BE $B$K%j%s%/$rIU$1$k!#(B"
  (when (string-match "BE:\\([0-9]*\\)-[^(]*([0-9]*)" date)
    (set-text-properties (match-beginning 0) (match-end 0)
			 (list 'mouse-face navi2ch-article-mouse-face
			       'navi2ch-link (concat "http://be.2ch.net/test/p.php?i="
						     (match-string 1 date))
			       'navi2ch-link-type 'url
			       'help-echo #'navi2ch-article-help-echo)
			 date))
  date)
    

(defun navi2ch-article-default-header-format-function (number name mail date)
  "$B%G%U%)%k%H$N%X%C%@$r%U%)!<%^%C%H$9$k4X?t!#(B
$B%X%C%@$N(B face $B$rIU$1$k$N$b$3$3$G!#(B"
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
	(date (navi2ch-propertize (funcall navi2ch-article-date-format-function date)
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
  "YY/MM/DD$B7A<0$NF|IU$KMKF|$rB-$9!#(B"
  (let ((youbi '("$BF|(B" "$B7n(B" "$B2P(B" "$B?e(B" "$BLZ(B" "$B6b(B" "$BEZ(B"))
	year month day et dt time date)
    ;; "$B$"$\!<$s(B"$B$H$+(BID$B$H$+5l7A<0$NF|IU$K$bBP1~$7$F$$$k$O$:!%(B
    ;; $B@55,I=8=$K9)IW$,I,MW$+$b!D(B
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
	  ;; $BF,$K(B20$B$rB-$7$F(BYYYY$B7A<0$K$9$k(B(2100$BG/LdBj%\%C%Q%DM=Dj(B)
	  (concat "20" date "("  (nth (nth 6 dt) youbi) ") " time ))
      d)))

(defun navi2ch-article-expunge-buffers (&optional num)
  "$B%9%l$N%P%C%U%!$r:o=|$7$F(B NUM $B8D$K$9$k!#(B
NUM $B$r;XDj$7$J$$>l9g$O(B `navi2ch-article-max-buffers' $B$r;HMQ!#(B
NUM $B$,(B 0 $B0J>e$N$H$-$O(B sticky $B%P%C%U%!$O:o=|$7$J$$!#(B
NUM $B$,(B -1 $B$N$H$-$O(B sticky $B%P%C%U%!$b4^$a$F$9$Y$F:o=|!#(B"
  (interactive "P")
  (when (not (numberp num)) ; C-u $B$N$_$N;~(B4$B8D$K$7$?$$$o$1$8$c$J$$$H;W$o$l(B
    (setq num navi2ch-article-max-buffers))
  (let* ((buffer-list (navi2ch-article-buffer-list))
	 (buffer-num (length buffer-list)))
    (when (> buffer-num num)
      (unless (< num 0)
	(save-excursion
	  (setq buffer-list
		(nreverse
		 (apply 'append
			(mapcar (lambda (buf)
				  (set-buffer buf)
				  (and (not navi2ch-article-sticky-mode)
				       (list buf)))
				buffer-list))))))
      (catch 'loop
	(dolist (buf buffer-list)
	  (kill-buffer buf)
	  (setq buffer-num (1- buffer-num))
	  (when (<= buffer-num num)
	    (throw 'loop nil)))))))

(defun navi2ch-article-view-article (board
				     article
				     &optional force number max-line dont-display)
  "$B%9%l$r8+$k!#(BFORCE $B$G6/@)FI$_9~$_(B MAX-LINE $B$GFI$_9~$`9T?t$r;XDj!#(B
$B$?$@(B `navi2ch-article-max-line' $B$H$O5U$G(B t $B$GA4ItFI$_9~$_!#(B
DONT-DISPLAY $B$,(B non-nil $B$N$H$-$O%9%l%P%C%U%!$rI=<($;$:$K<B9T!#(B"
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
  "FILE $B$+$i%9%l$r8+$k!#(B"
  (setq file (expand-file-name file))
  (let* ((board (list (cons 'id "navi2ch")
		      (cons 'uri (navi2ch-filename-to-url
				  (file-name-directory file)))
		      (cons 'name navi2ch-board-name-from-file)))
	 (article (list (cons 'artid
			      (navi2ch-article-file-name-to-artid file))))
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
  (navi2ch-make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'navi2ch-article-kill-buffer-hook t t)
  (navi2ch-make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'navi2ch-article-display-link-minibuffer nil t)
  (run-hooks 'navi2ch-article-mode-hook))

(defun navi2ch-article-kill-buffer-hook ()
  ;; update $B$G$"$l$P(B cache $B$K$7$J$$(B
  ;; view $B$G$"$C$?$b$N$,(B update $B$K$J$C$?8e$K(B kill $B$5$l$?;~$NBP:v(B
  (setq navi2ch-article-jit-buffers (delq (current-buffer) navi2ch-article-jit-buffers))
  (unless navi2ch-article-jit-buffers
    (when navi2ch-article-jit-timer
      (cancel-timer navi2ch-article-jit-timer)
      (setq navi2ch-article-jit-timer nil)))
  (let ((status (navi2ch-bm-get-state-from-article navi2ch-article-current-board
						   navi2ch-article-current-article)))
    (cond
     ((eq status 'update))
     ((eq status 'down)
      (navi2ch-bm-update-article navi2ch-article-current-board
				 navi2ch-article-current-article
				 'down))
     (t
      (navi2ch-bm-update-article navi2ch-article-current-board
				 navi2ch-article-current-article
				 'cache))))
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
  "$B%9%l%C%I$HF1$8HD$X0\F0$9$k!#(B"
  (interactive "P")
  (let ((board navi2ch-article-current-board))
    (navi2ch-article-exit kill)
    (navi2ch-board-select-board board)))

(defun navi2ch-article-fix-range (num)
  "navi2ch-article-view-range $B$r(B NUM $B$,4^$^$l$kHO0O$KJQ99!#(B"
  (let ((len (length navi2ch-article-message-list))
	(range navi2ch-article-view-range))
    (unless (navi2ch-article-inside-range-p num range len)
      (let ((first (car range))
	    (last (+ navi2ch-article-fix-range-diff (- len num))))
	(setq navi2ch-article-view-range (cons first last))))))

(defun navi2ch-article-sync (&optional force first number)
  "$B%9%l$r99?7$9$k!#(BFORCE $B$,(B non-nil $B$J$i6/@)!#(B
FIRST $B$,(B nil $B$J$i$P!"%U%!%$%k$,99?7$5$l$F$J$1$l$P2?$b$7$J$$!#(B"
  (interactive "P")
  (when (not (navi2ch-board-from-file-p navi2ch-article-current-board))
    (run-hooks 'navi2ch-article-before-sync-hook)
    (let* ((list navi2ch-article-message-list)
           (article navi2ch-article-current-article)
           (board navi2ch-article-current-board)
           (navi2ch-net-force-update (or navi2ch-net-force-update
                                         force))
           (file (navi2ch-article-get-file-name board article))
           (old-size (navi2ch-file-size file))
           header start)
      (when first
        (setq article (navi2ch-article-load-info)))
      (navi2ch-article-set-mode-line)
      (if (and (or (cdr (assq 'kako article))
		   (cdr (assq 'down article)))
	       (file-exists-p file)
	       (not (and force	  ; force $B$,;XDj$5$l$J$$8B$j(Bsync$B$7$J$$(B
			 (y-or-n-p "Re-sync kako/down article? "))))
	  (setq navi2ch-article-current-article article)
	(let ((ret (navi2ch-article-update-file board article force)))
	  (setq article (nth 0 ret)
		navi2ch-article-current-article article
		header (nth 1 ret))))
      (prog1
	  ;; $B99?7$G$-$?$i(B
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
		  (funcall
		   (if navi2ch-article-use-jit
		       'navi2ch-article-jit-reinsert-partial-messages
		     'navi2ch-article-reinsert-partial-messages)
		   start)
		(erase-buffer)
		(if navi2ch-article-use-jit
		    (navi2ch-article-jit-insert-messages
		     list
		     navi2ch-article-view-range
		     (or number
			 (cdr (assq 'number navi2ch-article-current-article))
			 1))
		  (navi2ch-article-insert-messages list
						   navi2ch-article-view-range))))
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
      (unless (and (or (cdr (assq 'kako article))
		       (cdr (assq 'down article)))
		   (file-exists-p file)
		   (not (and force ; force $B$,;XDj$5$l$J$$8B$j(B sync $B$7$J$$(B
			     (y-or-n-p "Re-sync kako/down article? "))))
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
	  (message "Filtering current messages...")
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
	      (catch 'loop
		(dolist (x (nthcdr (1- start) navi2ch-article-message-list))
		  (unless (eq (navi2ch-article-apply-message-filters
			       (navi2ch-put-alist
				'number
				(car x)
				(navi2ch-article-parse-message (cdr x))))
			      'hide)
		    (throw 'loop nil)))
		(setq suppressed res))
	      (navi2ch-article-save-info
	       navi2ch-article-current-board
	       navi2ch-article-current-article)
	      (navi2ch-article-save-message-filter-cache
	       navi2ch-article-current-board
	       navi2ch-article-current-article
	       navi2ch-article-message-filter-cache)))
	  (message "Filtering current messages...done"))))
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
      (setq article (navi2ch-article-load-info board (copy-alist article)))
      (setq hide (cdr (assq 'hide article))
	    num (cdr (assq 'number article))))
    (when num
      (while (memq (1+ num) hide)
	(setq num (1+ num)))
      num)))

(defun navi2ch-article-update-file (board article &optional force)
  "BOARD, ARTICLE $B$KBP1~$9$k%U%!%$%k$r99?7$9$k!#(B
$BJV$jCM$O(B \(article header) $B$N%j%9%H!#(B"
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
					     (or (cdr (assq 'last-modified
							     header))
						 (cdr (assq 'date
							     header)))
					     article)))
	  (when (navi2ch-net-get-state 'kako header)
	    (setq article (navi2ch-put-alist 'kako t article)))
	  (when (and (cdr (assq 'down article))
		     (or (navi2ch-net-get-state 'not-updated header)
			 (not (navi2ch-net-get-state 'error header))))
	    (setq article (navi2ch-put-alist 'down nil article))
	    (navi2ch-article-uncompress board article)
	    (navi2ch-article-save-info board article))
	  (when (and (file-exists-p file)
		     (file-readable-p file))
	    (with-temp-buffer
	      (navi2ch-insert-file-contents file)
	      (setq article 
		    (navi2ch-put-alist
		     'response
		     (number-to-string (count-lines (point-min) (point-max)))
		     article)))))))
    (list article header)))

(defun navi2ch-article-sync-from-file ()
  "from-file $B$J%9%l$r99?7$9$k!#(B"
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
          (navi2ch-article-make-mode-line-identification article)))
  (navi2ch-set-mode-line-identification))

(defvar navi2ch-article-mode-line-format "%a (%n/%N) [%b]"
  "$B%9%lI=<(%P%C%U%!$N(B mode-line $B$N%U%)!<%^%C%H(B

$B0J2<$N$b$N$,CV$-49$($i$l$k!#(B
%a - $B%9%l%C%IL>(B
%b - $BHDL>(B
%n - $B<B:]$N%l%9?t(B
%N - $B%9%l0lMw$+$i<hF@$7$?%l%9?t(B
%% - %")
(defun navi2ch-article-make-mode-line-identification (article)
  (navi2ch-replace-string
   "%."
   (lambda (str)
     (let ((char (aref str 1)))
       (cond
	((eq char ?a)
	 (or (cdr (assq 'subject article))
	      navi2ch-bm-empty-subject))
	((eq char ?b)
	 (or (cdr (assq 'name navi2ch-article-current-board))
	     "-"))
	((eq char ?n)
	 (let ((l (length navi2ch-article-message-list)))
	    (if (zerop l )
		"-"
	      (number-to-string l))))
	((eq char ?N)
	 (or (cdr (assq 'response article)) "-"))
	((eq char ?%) "%")
	(t ""))))
   navi2ch-article-mode-line-format t t t))

(defun navi2ch-article-sync-disable-diff (&optional force)
  (interactive "P")
  (let ((navi2ch-article-enable-diff nil))
    (navi2ch-article-sync force)))

(defun navi2ch-article-redraw ()
  "$B8=:_I=<($7$F$k%9%l$rI=<($7$J$*$9!#(B"
  (let ((buffer-read-only nil))
    (navi2ch-article-save-number)
    (erase-buffer)
    (navi2ch-article-insert-messages navi2ch-article-message-list
                                     navi2ch-article-view-range)
    (navi2ch-article-load-number)))

(defun navi2ch-article-select-view-range-subr ()
  "$BI=<($9$kHO0O$r%-!<%\!<%I%a%K%e!<$GA*Br$9$k!#(B"
  (save-window-excursion
    (delete-other-windows)
    (let (buf
          (range navi2ch-article-view-range))
      (unwind-protect
	  (progn
	    (setq buf (get-buffer-create "*select view range*"))
	    (with-current-buffer buf
	      (erase-buffer)
	      (insert (format "   %8s %8s\n" "first" "last"))
	      (insert (format "0: %17s\n" "all range"))
	      (let ((i 1))
		(dolist (x navi2ch-article-view-range-list)
		  (insert (format "%d: %8d %8d\n" i (car x) (cdr x)))
		  (setq i (1+ i)))))
	    (display-buffer buf)
	    (let (n)
	      (setq n (navi2ch-read-char "Input: "))
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
  "$BI=<($9$kHO0O$r;XDj$7$?8e(B redraw $B$9$k!#(B"
  (interactive)
  (setq navi2ch-article-view-range
        (navi2ch-article-select-view-range-subr))
  (sit-for 0)
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
  (let ((num (cdr (assq 'number navi2ch-article-current-article)))
	(list (cond 
	       (navi2ch-article-hide-mode
		(cdr (assq 'hide navi2ch-article-current-article)))
	       (navi2ch-article-important-mode
		(cdr (assq 'important navi2ch-article-current-article)))
	       (t nil))))
    (when list
      (let (nearest_len nearest_n)
	(dolist (n list)
	  (let ((len (abs (- n num))))
	    (when (or (null nearest_len)
		      (< len nearest_len))
	      (setq nearest_len len
		    nearest_n n))))
	(setq num nearest_n)))
    (when (or list
	      (not (or navi2ch-article-hide-mode
		       navi2ch-article-important-mode)))
      (navi2ch-article-goto-number (or num 1)))))

(defun navi2ch-article-save-info (&optional board article first)
  (let (ignore)
    (when (eq major-mode 'navi2ch-article-mode)
      (if (navi2ch-board-from-file-p (or board navi2ch-article-current-board))
	  (setq ignore t)
	(when (and navi2ch-article-message-list (not first))
	  (navi2ch-article-save-number))
	(or board (setq board navi2ch-article-current-board))
	(or article (setq article navi2ch-article-current-article))))
    (when (and (not ignore) board article)
      (let* ((article-tmp (if navi2ch-article-save-info-wrapper-func
			     (funcall navi2ch-article-save-info-wrapper-func article)
			   article))
	     (alist (mapcar
		     (lambda (x)
		       (assq x article-tmp))
		     navi2ch-article-save-info-keys)))
	(navi2ch-save-info (navi2ch-article-get-info-file-name board article) alist)
	(navi2ch-article-save-message-filter-cache board article)))))

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

(defun navi2ch-article-write-message (&optional sage cite)
  (interactive)
  (when (not (navi2ch-board-from-file-p navi2ch-article-current-board))
    (navi2ch-article-save-number)
    (navi2ch-message-write-message navi2ch-article-current-board
                                   navi2ch-article-current-article
				   nil sage cite)))

(defun navi2ch-article-write-sage-message ()
  (interactive)
  (navi2ch-article-write-message 'sage))

(defun navi2ch-article-write-cite-message ()
  (interactive)
  (navi2ch-article-write-message 'sage 'cite))

(defun navi2ch-article-str-to-num (str)
  "$B%l%9;2>H$NJ8;zNs$r?t;z$+?t;z$N(B list $B$KJQ49!#(B"
  (cond ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" str)
	 (let* ((n1 (string-to-number (match-string 1 str)))
		(n2 (string-to-number (match-string 2 str)))
		(min (max (min n1 n2) 1))
		(i (min (max n1 n2)
			(1+ (navi2ch-article-get-article-length))))
		list)
	   (while (>= i min)
	     (push i list)
	     (setq i (1- i)))
	   list))
	((string-match "\\([0-9]+,\\)+[0-9]+" str)
	 (mapcar 'string-to-number (split-string str ",")))
	(t (string-to-number str))))

(defun navi2ch-article-get-article-length ()
  (let* ((board (or navi2ch-article-current-board
		    navi2ch-popup-article-current-board))
	 (article (or navi2ch-article-current-article
		      navi2ch-popup-article-current-article))
	 (buffer-name (navi2ch-article-get-buffer-name board article)))
    (with-current-buffer (or (get-buffer buffer-name)
			     (progn
			       (navi2ch-article-view-article board article nil nil nil t)
			       buffer-name))
      (length navi2ch-article-message-list))))

(defun navi2ch-article-get-number-list (number-property &optional limit)
  (or (and (string-match "[^ ][^ ][^ ][^ ][^ ][^ ][^ ][^ ]" number-property)
	   (let (nums)
	     (dolist (msg navi2ch-article-message-list (nreverse nums))
	       (when (listp (cdr msg))
		 (let ((date (cdr (assq 'date (cdr msg))))
		       (name (cdr (assq 'name (cdr msg)))))
		   (when (or (and date
				  (string-match " ID:\\([^ ][^ ][^ ][^ ]+\\)"
						;; ID:??? $B$O%9%k!<(B
						date)
				  (string-match (regexp-quote
						 (match-string 1 date))
						number-property))
			     (and name
				  (string-match "$B"!(B\\([^ ]+\\)" name)
				  (string-match (regexp-quote
						 (match-string 1 name))
						number-property)))
		     (if (and (numberp limit)
			      (>= (car msg) limit)
			      nums)
			 (return (car nums))
		       (push (car msg) nums))))))))
      (navi2ch-article-str-to-num (japanese-hankaku number-property))))

(defun navi2ch-article-select-current-link (&optional browse-p)
  (interactive "P")
  (let ((type (get-text-property (point) 'navi2ch-link-type))
	(prop (get-text-property (point) 'navi2ch-link)))
    (cond ((eq type 'number)
	   (navi2ch-article-select-current-link-number 
	    (navi2ch-article-get-number-list prop)
	    browse-p))
	  ((eq type 'url)
           (navi2ch-article-select-current-link-url prop browse-p nil))
          ((eq type 'content)
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
  ;; prop $B$O!V?t$N(B list$B!W$+!V?t!W!#(B
  (cond (browse-p
	 (navi2ch-browse-url-internal
	  (navi2ch-article-number-list-to-url prop)))
	((eq navi2ch-article-select-current-link-number-style 'popup)
	 (navi2ch-popup-article (if (listp prop) prop (list prop))))
	((eq navi2ch-article-select-current-link-number-style 'jump)
	 (let ((navi2ch-article-redraw-when-goto-number t))
	   (navi2ch-article-goto-number (if (listp prop) (car prop) prop)
					t t)))
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
	   (navi2ch-2ch-url-p url))
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
  "NUM $BHVL\$N%l%9$r2hLL$N0lHV>e$K!#(B"
  (let ((win (if (eq (window-buffer) (current-buffer))
		 (selected-window)
	       (get-buffer-window (current-buffer)))))
    (if (and win (numberp num))
	(set-window-start
	 win (cdr (assq 'point (navi2ch-article-get-message num)))))))

(defun navi2ch-article-goto-number-or-board ()
  "$BF~NO$5$l$??t;z$N0LCV$K0\F0$9$k$+!"F~NO$5$l$?HD$rI=<($9$k!#(B
$BL>A0$,?t;z$J$i$P%G%U%)%k%H$O$=$NL>A0$N?t;z!#(B"
  (interactive)
  (let (default alist ret)
    (setq default
	  (let* ((msg (navi2ch-article-get-message
		       (navi2ch-article-get-current-number)))
		 (from (cdr (assq 'name msg)))
		 (data (cdr (assq 'data msg))))
	    (or (and from
		     (string-match "^[^$B"!(B0-9$B#0(B-$B#9(B]*\\([0-9$B#0(B-$B#9(B]+\\)" from)
		     (japanese-hankaku (match-string 1 from)))
		(and data
		     (string-match "[0-9$B#0(B-$B#9(B]+" data)
		     (japanese-hankaku (match-string 0 data)))
		nil)))
    (setq alist (mapcar (lambda (x) (cons (cdr (assq 'id x)) x))
			navi2ch-list-board-name-list))
    (setq ret (completing-read
	       (concat "Input number or board"
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
		(error "No such board"))))))))

(defun navi2ch-article-goto-number (num &optional save pop)
  "NUM $BHVL\$N%l%9$K0\F0!#(B"
  (interactive "nInput number: ")
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
	(unless (or navi2ch-article-hide-mode 
		    navi2ch-article-important-mode
		    (if navi2ch-article-use-jit
			(condition-case nil
			    (cdr (assq 'point (navi2ch-article-get-message num)))
			  (error nil))
		      (navi2ch-article-inside-range-p num range len)))
	  (if navi2ch-article-redraw-when-goto-number
	      (if navi2ch-article-use-jit
		  (let (buffer-read-only)
		    (navi2ch-article-reinsert-partial-messages num num))
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
  "BOARD $B$G;XDj$5$l$?HD$K0\F0!#(B
BOARD $B$,(B nil $B$J$i$P!"8=:_3+$$$F$$$k%9%l$NHD$K0\F0!#(B"
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
  "stack $B$+$i(B pop $B$7$?0LCV$K0\F0$9$k!#(B"
  (interactive)
  (let ((point (pop navi2ch-article-point-stack)))
    (if point
        (progn
          (push (navi2ch-article-get-point (point)) navi2ch-article-poped-point-stack)
          (navi2ch-article-goto-number (car point))
	  (forward-char (cdr point)))
      (message "Stack is empty"))))

(defun navi2ch-article-push-point (&optional point)
  "$B8=:_0LCV$+(B POINT $B$r(B stack $B$K(B push $B$9$k!#(B"
  (interactive)
  (setq navi2ch-article-poped-point-stack nil)
  (push (navi2ch-article-get-point point) navi2ch-article-point-stack)
  (message "Push current point"))

(defun navi2ch-article-pop-poped-point () ; $BL>A0$@$;$'!"$C$F$+2?$+0c$&!#(B
  (interactive)
  (let ((point (pop navi2ch-article-poped-point-stack)))
    (if point
        (progn
          (push (navi2ch-article-get-point (point)) navi2ch-article-point-stack)
	  (navi2ch-article-goto-number (car point))
	  (forward-char (cdr point)))
      (message "Stack is empty"))))

(defun navi2ch-article-rotate-point ()
  "stack $B$X(B push $B$7$?0LCV$r=d2s$9$k!#(B"
  (interactive)
  (let ((cur (navi2ch-article-get-point nil)) ; $B8=:_CO(B
	(top (pop navi2ch-article-point-stack))) ; $B%H%C%W(B
    (if top
        (progn
	  (setq navi2ch-article-point-stack
		(append navi2ch-article-point-stack (list cur))) ; $B:G8eHx$XJ]B8(B
          (navi2ch-article-goto-number (car top)) ; $B%H%C%W$N(B
          (forward-char (cdr top)))	; $B0JA0$$$?J8;z$X(B
      (message "Stack is empty"))))

(defun navi2ch-article-goto-last-message ()
  "$B:G8e$N%l%9$X0\F0!#(B"
  (interactive)
  (navi2ch-article-goto-number
   (save-excursion
     (goto-char (point-max))
     (navi2ch-article-get-current-number)) t))

(defun navi2ch-article-goto-first-message ()
  "$B:G=i$N%l%9$X0\F0!#(B"
  (interactive)
  (navi2ch-article-goto-number
   (save-excursion
     (goto-char (point-min))
     (navi2ch-article-get-current-number)) t))

(defun navi2ch-article-few-scroll-up (n)
  (interactive "P")
  (scroll-up (or n 1)))

(defun navi2ch-article-few-scroll-down (n)
  (interactive "P")
  (scroll-down (or n 1)))

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
  "$B<!$N%9%l$K0\F0$9$k$H$-$K(B \"y or n\" $B$G3NG'$9$k!#(B"
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
  "$B<!$N%9%l$K0\F0$9$k$H$-$K(B \"n\" $B$+(B \"p\" $B$G3NG'$9$k!#(B"
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
  "$B<!$N%9%l$K0\F0$9$k$H$-$K!"D>A0$N%3%^%s%I$HF1$8$+$G3NG'$9$k!#(B"
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
  "$B<!$N%9%l$K0\F0$9$k$+J9$/!#(B
$B<!$N%9%l$K0\F0$9$k$J$i(B t $B$rJV$9!#(B
$B0\F0$7$J$$$J$i(B nil $B$rJV$9!#(B
article buffer $B$+$iH4$1$k$J$i(B 'quit $B$rJV$9!#(B"
  (if (or (eq navi2ch-article-enable-through 'ask-always)
	  (and (not no-ask)
	       (eq navi2ch-article-enable-through 'ask)))
      (funcall navi2ch-article-through-ask-function
	       num
	       (with-current-buffer navi2ch-board-buffer-name
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
  "$BA08e$N%9%l$K0\F0$9$k!#(B
NUM $B$,(B 1 $B$N$H$-$O<!!"(B-1 $B$N$H$-$OA0$N%9%l$K0\F0!#(B
$B8F$S=P$9:]$O(B INTERACTIVE-FLAG $B$K(B (interactive-p) $B$rF~$l$k!#(B"
  (interactive)
  (or num (setq num 1))
  (if (and (not (eq num 1))
	   (not (eq num -1)))
      (error "Arg error"))
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
  "$B<!$N%9%l$K0\F0$9$k!#(B"
  (interactive)
  (navi2ch-article-through-subr (interactive-p) 1))

(defun navi2ch-article-through-previous ()
  "$BA0$N%9%l$K0\F0$9$k!#(B"
  (interactive)
  (navi2ch-article-through-subr (interactive-p) -1))

(defun navi2ch-article-get-current-number ()
  "$B:#$N0LCV$N%l%9$NHV9f$rF@$k!#(B"
  (condition-case nil
      (or (get-text-property (point) 'current-number)
          (get-text-property
           (navi2ch-previous-property (point) 'current-number)
           'current-number))
    (error nil)))

(defsubst navi2ch-article-get-current-name ()
  (cdr (assq 'name (cdr (assq (navi2ch-article-get-current-number)
			      navi2ch-article-message-list)))))

(defsubst navi2ch-article-get-current-mail ()
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

(defun navi2ch-article-get-current-hostname ()
  (let ((date (cdr (assq 'date (cdr (assq (navi2ch-article-get-current-number)
					  navi2ch-article-message-list))))))
    (if (or (string-match "\\[ \\([^ ]+\\) \\]" date)
	    (string-match "$BH/?.85(B:\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" date))
	(match-string 1 date)
      nil)))

(defun navi2ch-article-get-current-word-in-body ()
  (let ((case-fold-search nil)
	(word (if (eq (get-text-property (point) 'navi2ch-link-type) 'url)
		  (buffer-substring-no-properties
		   (if (eq (get-text-property (1- (point)) 'navi2ch-link-type) 'url)
		       (previous-single-property-change (point) 'navi2ch-link)
		     (point))
		   (next-single-property-change (point) 'navi2ch-link))
		(current-word))))
    (when (and word
	       (string-match
		(regexp-quote word)
		(navi2ch-article-get-message-string
		 (navi2ch-article-get-current-number))))
      word)))

(defun navi2ch-article-get-current-subject ()
  (or (cdr (assq 'subject navi2ch-article-current-article))
      (cdr (assq 'subject
		 (let ((msg (navi2ch-article-get-message 1)))
		   (if (stringp msg)
		       (navi2ch-article-parse-message msg)
		     msg))))
      ""))

(defun navi2ch-article-get-visible-numbers ()
  "$BI=<(Cf$N%l%9$NHV9f$N%j%9%H$rF@$k!#(B"
  (let (list prev)
    (save-excursion
      (goto-char (point-max))
      (unless (bobp)
	(while (setq prev (navi2ch-previous-property (point) 'current-number))
	  (goto-char prev)
	  (setq list (cons (get-text-property (point) 'current-number) list)))))
    list))

(defun navi2ch-article-show-url ()
  "url $B$rI=<($7$F!"$=$N(B url $B$r8+$k$+(B kill ring $B$K%3%T!<$9$k!#(B"
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
			  (message "Copy: %s" x)))
		       ((eq char ?v)
			(lambda (x)
			  (navi2ch-browse-url-internal x)
			  (message "View: %s" x))))
		 (navi2ch-article-show-url-subr))))))

(defun navi2ch-article-show-url-subr ()
  "$B%a%K%e!<$rI=<($7$F!"(Burl $B$rF@$k!#(B"
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
  "$B%a%K%e!<$rI=<($7$F!"%?%$%H%k$rF@$k!#(B"
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
      (message "Copy: %s" title))))

(defun navi2ch-article-redisplay-current-message ()
  "$B:#$$$k%l%9$r2hLL$NCf?4(B ($B>e(B?) $B$K!#(B"
  (interactive)
  (navi2ch-article-recenter
   (navi2ch-article-get-current-number)))

(defun navi2ch-article-next-message ()
  "$B<!$N%a%C%;!<%8$X0\F0!#(B"
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
  "$BA0$N%a%C%;!<%8$X0\F0!#(B"
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
  "NUM $BHVL\$N%l%9$NJ8>O$rF@$k!#(B"
  (let ((msg (navi2ch-article-get-message num)))
    (when (stringp msg)
      (setq msg (navi2ch-article-parse-message msg)))
    (cdr (assq 'data msg))))

(defun navi2ch-article-cached-subject-minimum-size (board article)
  "$B%9%l%?%$%H%k$rF@$k$N$K==J,$J%U%!%$%k%5%$%:$r5a$a$k!#(B"
  (with-temp-buffer
    (let ((beg 0) (end 0) (n 1))
      (while (and (= (point) (point-max))
		  (> n 0))
	(setq beg end)
	(setq end (+ end 1024))
	(setq n (car (cdr (navi2ch-board-insert-file-contents
			   board
			   (navi2ch-article-get-file-name board article)
			   beg end))))
	(forward-line))
      end)))

(defun navi2ch-article-cached-subject (board article)
  "$B%-%c%C%7%e$5$l$F$$$k(B dat $B%U%!%$%k$+$i%9%l%?%$%H%k$rF@$k!#(B"
  ;; "$B%-%c%C%7%e$5$l$F$$$k(B dat $B%U%!%$%k$d%9%l0lMw$+$i%9%l%?%$%H%k$rF@$k!#(B"
  (let ((state (navi2ch-article-check-cached board article))
	subject)
    (if (eq state 'view)
	(with-current-buffer (navi2ch-article-get-buffer-name board article)
	  (setq subject			; nil $B$K$J$k$3$H$,$"$k(B
		(cdr (assq 'subject
			   navi2ch-article-current-article)))))
    (when (not subject)
      (if (eq state 'cache)
	  (let* ((file (navi2ch-article-get-file-name board article))
		 (msg-list (navi2ch-article-get-message-list
			    file
			    0
			    (navi2ch-article-cached-subject-minimum-size board article))))
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
      (setq subject "navi2ch: ???")) ; $BJQ?t$K$7$F(B navi2ch-vars.el $B$KF~$l$k$Y$-(B?
    subject))

(eval-when-compile
  (defvar mark-active)
  (defvar deactivate-mark))

(defun navi2ch-article-get-link-text-subr (&optional point)
  "POINT ($B>JN,;~$O%+%l%s%H%]%$%s%H(B) $B$N%j%s%/@h$rF@$k!#(B"
  (setq point (or point (point)))
  (let (mark-active deactivate-mark) ; transient-mark-mode $B$,@Z$l$J$$$h$&(B
    (catch 'ret
      (when (or (eq major-mode 'navi2ch-article-mode)
		(eq major-mode 'navi2ch-popup-article-mode))
	(let ((type (get-text-property point 'navi2ch-link-type))
	      (prop (get-text-property point 'navi2ch-link))
	      num-list num)
	  (cond
	   ((eq type 'number)
	    (setq num-list (navi2ch-article-get-number-list
			    prop (navi2ch-article-get-current-number)))
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
		 (eq type 'url))
	    (if (navi2ch-2ch-url-p prop)
		(let ((board (navi2ch-board-url-to-board prop))
		      (article (navi2ch-article-url-to-article prop)))
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
  "POINT ($B>JN,;~$O%+%l%s%H%]%$%s%H(B) $B$N%j%s%/@h$rF@$k!#(B
$B7k2L$r(B help-echo $B%W%m%Q%F%#$K@_Dj$7$F%-%c%C%7%e$9$k!#(B"
  (setq point (or point (point)))
  (let ((help-echo-prop (get-text-property point 'help-echo))
	mark-active deactivate-mark) ; transient-mark-mode $B$,@Z$l$J$$$h$&(B
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
		       (or (navi2ch-next-property point 'navi2ch-link)
			   (point-max)))
		  point))
	 (start (or (max (previous-single-property-change end 'help-echo)
			 (or (navi2ch-previous-property end 'navi2ch-link)
			     (point-min)))
		    point)))
    (put-text-property start end 'help-echo value)))

(defvar navi2ch-article-disable-display-link-commands
  '(navi2ch-show-url-at-point
    navi2ch-article-select-current-link
    eval-expression)
  "$B$3$N%3%^%s%I$N8e$G$O(B minibuffer $B$K%j%s%/@h$rI=<($7$J$$!#(B")

(defvar navi2ch-article-info-cache nil)
(defvar navi2ch-article-info-cache-limit 100)

(defun navi2ch-article-display-link-minibuffer (&optional point)
  "POINT ($B>JN,;~$O%+%l%s%H%]%$%s%H(B) $B$N%j%s%/@h$r(B minibuffer $B$KI=<(!#(B"
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

(defsubst navi2ch-article-load-article-summary (board)
  (navi2ch-load-info (navi2ch-board-get-file-name
		      board
		      navi2ch-article-summary-file-name)))

(defsubst navi2ch-article-save-article-summary (board summary)
  (navi2ch-save-info (navi2ch-board-get-file-name
		      board
		      navi2ch-article-summary-file-name)
		     summary))

(defun navi2ch-article-next-link ()
  "$B<!$N%j%s%/$X0\F0!#(B"
  (interactive)
  (let ((point (navi2ch-next-property (point) 'navi2ch-link)))
    (if point
	(goto-char point))))

(defun navi2ch-article-previous-link ()
  "$BA0$N%j%s%/$X0\F0!#(B"
  (interactive)
  (let ((point (navi2ch-previous-property (point) 'navi2ch-link)))
    (if point
	(goto-char point))))

(defun navi2ch-article-fetch-link (&optional force)
  (interactive)
  (let ((type (get-text-property (point) 'navi2ch-link-type))
	(url  (get-text-property (point) 'navi2ch-link)))
    (and (eq type 'url)
	 url
	 (navi2ch-2ch-url-p url)
	 (let ((article (navi2ch-article-url-to-article url))
	       (board (navi2ch-board-url-to-board url)))
	   (when article
	     (and (get-text-property (point) 'help-echo)
		  (let ((buffer-read-only nil))
		    (navi2ch-article-change-help-echo-property 
		     (point)
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
				    (+ seen 
				       navi2ch-board-check-article-update-suppression-length))))
			(progn
			  (navi2ch-article-summary-element-set-seen element seen)
			  (navi2ch-article-save-article-summary board summary))
		      (navi2ch-bm-remember-fetched-article board article)))))))))

(defun navi2ch-article-detect-encoded-regions (&optional sort)
  "$B%P%C%U%!$+$i(B uuencode $B$^$?$O(B base64 $B%(%s%3!<%I$5$l$?NN0h$rC5$9!#(B
\(list (list type fname start end)) $B$rJV$9!#(B
SORT $B$,(B non-nil $B$N$H$-$O(B start $B$G%=!<%H$7$?7k2L$rJV$9!#(B
$B$?$@$7!"(B
 type: 'uuencode $B$+(B 'base64
fname: $B%G%3!<%I$9$k$H$-$N%G%U%)%k%H$N%U%!%$%kL>(B
start: $B%(%s%3!<%I$5$l$?NN0h$N@hF,(B($B%G%j%_%?$r4^$`(B)$B$N%]%$%s%H(B
  end: $B%(%s%3!<%I$5$l$?NN0h$NKvHx(B($B%G%j%_%?$N<!$N9T$N9TF,(B)$B$N%]%$%s%H(B
$B$G$"$k!#(B
end $B$,(B nil $B$N>l9gKvHx$N%G%j%_%?$,L5$/@hF,$N%G%j%_%?$N$_$"$k$3$H$r0UL#$9$k!#(B"
  ;; start $B$,(B nil $B$N>l9g@hF,$N%G%j%_%?$,L5$/KvHx$N%G%j%_%?$N$_$"$k$3$H$r0UL#$7!"(B
  ;; $B",(B $B$9$k$H!"8mH=Dj$,B?$=$&$J$N$G$d$a!#(B
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
      ;; end $B$,(B nil $B$O!":G8e$N$_5v$9!#(B
      (let ((r regions))
	(while (> (length r) 1)
	  (when (null (nth 3 (car r)))
	    (setq regions (delete (car r) regions)))
	  (setq r (cdr r)))))
    regions))

(defun navi2ch-article-decode-message (prefix)
  "$B8=:_$N%l%9$r%G%3!<%I$9$k!#(B
PREFIX $B$r;XDj$7$?>l9g$O!"(Bmark $B$N$"$k%l%9$H8=:_$N%l%9$N4V$NHO0O$,BP>]$K$J$k!#(B

$BJ#?t%l%9$KJ,3d$5$l$?%(%s%3!<%I%;%/%7%g%s$r%G%3!<%I$7$?$$>l9g$O!"(B
$B%(%s%3!<%I%;%/%7%g%s$N@hF,$N%l%9$G!"(BPREFIX $B$r;XDj$;$:$K<B9T$9$k$3$H!#(B
$B8=:_$N%l%9Fb$K;O$a$N%G%j%_%?$N$_$,$"$k>l9g!"BP1~$9$kKvHx$N%G%j%_%?$,(B
$B8=$l$k%l%9$^$G%G%3!<%I$9$kNN0h$r3HD%$9$k!#(B

$B%G%j%_%?$H$_$J$9$N$O!"(B
`navi2ch-base64-begin-delimiter-regexp'
`navi2ch-base64-end-delimiter-regexp'

`navi2ch-base64-susv3-begin-delimiter-regexp'
`navi2ch-base64-susv3-end-delimiter-regexp'

`navi2ch-uuencode-begin-delimiter-regexp'
`navi2ch-uuencode-end-delimiter-regexp'

$B$N(B3$BAH$N$$$:$l$+$K%^%C%A$9$k9T$H$9$k!#(B"
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
      ;; $BJ#?t%l%9$KJ,3d$5$l$?$b$N$rC5$9!#(B
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
  "$B%(%s%3!<%I$5$l$?%;%/%7%g%s$r%G%3!<%I$7$?$b$N$X$N%"%s%+!<$K$9$k!#(B"
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
		(while (search-forward "$B!)(B" nil t) ;for 2ch
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
		   (format "%s" (or fname "$BL>L5$7%U%!%$%k$5$s(B"))
		   'face '(navi2ch-article-url-face
			   navi2ch-article-auto-decode-face)
		   'link t
		   'mouse-face navi2ch-article-mouse-face
		   'file-name fname
		   'navi2ch-link-type 'content
		   'navi2ch-link decoded))
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
		   (point) 'navi2ch-link nil (navi2ch-line-end-position)))
	   (content (get-text-property point 'navi2ch-link))
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
		    (let ((default-directory (navi2ch-default-directory)))
		      (apply 'call-process-region (point-min) (point-max)
			     navi2ch-net-gunzip-program t t nil
			     navi2ch-net-gunzip-args)))
	      (unless (= exit-status 0)
		(erase-buffer)
		(insert content)))
	     ((or (eq compress 'bzip2)
		  (and (null compress)
		       filename
		       (string-match "\\.bz2$" filename)
		       (navi2ch-which navi2ch-bzip2-program)))
	      (setq exit-status
		    (let ((default-directory (navi2ch-default-directory)))
		      (apply 'call-process-region (point-min) (point-max)
			     navi2ch-bzip2-program t t nil
			     navi2ch-bzip2-args)))
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
		(when (memq name '(raw-text binary)) ;$BE,Ev(B
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
  "$B%(%s%3!<%I$5$l$?%;%/%7%g%s$r%G%3!<%I$7$?$b$N$NI=<($r@Z$j49$($k!#(B
ASK $B$,(B non-nil $B$@$H!"%G%3!<%I$7$?$b$N$NJ8;z%3!<%I$H05=L7A<0$rJ9$$$F$/$k!#(B"
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
  (let ((prop (get-text-property (point) 'navi2ch-link))
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
	      ;; auto-compress-mode $B$r(B disable $B$K$9$k(B
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
	 (with-current-buffer buffer
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
	  (let ((w32-start-process-show-window t) ; for meadow
		(default-directory (navi2ch-default-directory)))
	    (call-process navi2ch-article-aadisplay-program
			  nil nil nil file)))
      (ignore-errors (delete-file file)))))

(defun navi2ch-article-popup-dialog (str)
  (navi2ch-ifxemacs
      (ignore str)			; $B$H$j$"$($:2?$b$7$J$$(B
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

(defun navi2ch-article-set-summary-element (board article remove-seen)
  "BOARD, ARTICLE $B$KBP1~$7$?>pJs$r(B article-summary $B$KJ]B8$9$k!#(B"
  (let* ((summary (navi2ch-article-load-article-summary board))
	 (artid (cdr (assq 'artid article)))
	 (element (cdr (assoc artid summary))))
    (navi2ch-article-summary-element-set-seen
     element
     (unless remove-seen
       (with-current-buffer (navi2ch-article-get-buffer-name board article)
	 (length navi2ch-article-message-list))))
    (navi2ch-article-summary-element-set-access-time element (current-time))
    (setq summary (navi2ch-put-alist artid element summary))
    (navi2ch-article-save-article-summary board summary)))

(defun navi2ch-article-add-board-bookmark ()
  (interactive)
  (navi2ch-board-add-bookmark-subr navi2ch-article-current-board
				   navi2ch-article-current-article))

(defun navi2ch-article-add-global-bookmark (bookmark-id)
  (interactive (list (navi2ch-bookmark-read-id "Bookmark ID: ")))
  (navi2ch-bookmark-add
   bookmark-id
   navi2ch-article-current-board
   navi2ch-article-current-article))

(defun navi2ch-article-buffer-list ()
  "`navi2ch-article-mode' $B$N(B buffer $B$N(B list $B$rJV$9!#(B"
  (let (list)
    (dolist (x (buffer-list))
      (when (with-current-buffer x
              (eq major-mode 'navi2ch-article-mode))
        (setq list (cons x list))))
    (nreverse list)))

(defun navi2ch-article-toggle-sticky ()
  "$B8=:_$N%P%C%U%!$N(B sticky $B%b!<%I$r(B toggle $B$9$k!#(B"
  (interactive)
  (setq navi2ch-article-sticky-mode
	(not navi2ch-article-sticky-mode))
  (force-mode-line-update)
  (if navi2ch-article-sticky-mode
      (message "Marked as sticky")
    (message "Marked as non-sticky")))

(defun navi2ch-article-current-buffer (&optional sticky)
  "BUFFER-LIST $B$N0lHV:G=i$N(B `navi2ch-article-mode' $B$N(B buffer $B$rJV$9!#(B
STICKY $B$,(B non-nil $B$N$H$-$O0lHV:G=i$N(B sticky article buffer $B$rJV$9!#(B"
  (let ((list (buffer-list)))
    (catch 'loop
      (while list
        (when (with-current-buffer (car list)
		(and (eq major-mode 'navi2ch-article-mode)
		     (or (not sticky)
			 navi2ch-article-sticky-mode)))
	  (throw 'loop (car list)))
        (setq list (cdr list)))
      nil)))

(defun navi2ch-article-forward-buffer (&optional sticky)
  "$B<!$N(B article buffer $B$K@Z$jBX$($k!#(B
STICKY $B$,(B non-nil $B$N$H$-$O<!$N(B sticky article buffer $B$K@Z$jBX$($k!#(B"
  (interactive "P")
  (let (buf)
    (dolist (x (buffer-list))
      (when (with-current-buffer x
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
  "$BA0$N(B article buffer $B$K@Z$jBX$($k!#(B
STICKY $B$,(B non-nil $B$N$H$-$OA0$N(B sticky article buffer $B$K@Z$jBX$($k!#(B"
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
  "$B<!$N(B sticky article buffer $B$K@Z$jBX$(!"(Bsync $B$9$k!#(B
NO-SYNC $B$,(B non-nil $B$N$H$-$O(B sync $B$7$J$$!#(B"
  (interactive "P")
  (and (navi2ch-article-forward-buffer t)
       (not no-sync)
       (navi2ch-article-sync)))

(defun navi2ch-article-backward-sticky-buffer (&optional no-sync)
  "$BA0$N(B sticky article buffer $B$K@Z$jBX$(!"(Bsync $B$9$k!#(B
NO-SYNC $B$,(B non-nil $B$N$H$-$O(B sync $B$7$J$$!#(B"
  (interactive "P")
  (and (navi2ch-article-backward-buffer t)
       (not no-sync)
       (navi2ch-article-sync)))

(defun navi2ch-article-delete-message (sym func msg &optional perm)
  "$B3:Ev0LCV$N%l%9$r(B buffer $B$+$i:o=|$9$k!#(B
`navi2ch-article-current-article' $B$N(B SYM $B$G;XDj$5$l$k(B list $B$KBP$7$F!"(BFUNC $B$r8F$S$@$9!#(B
PERM $B$,(B non-nil $B$N>l9g$O!"$=$N%l%9$r(B unfilter $B$H$7$F5-O?$9$k!#(B
FUNC $B$O(B (NUMBER, LIST) $B$r0z?t$K<h$k4X?t$G$"$k;v!#(B"
  (let* ((article navi2ch-article-current-article)
         (list (cdr (assq sym article)))
         (num (navi2ch-article-get-current-number)))
    (setq list (funcall func num list))
    (setq article (navi2ch-put-alist sym list article))
    (unless (memq num list)
      (let ((cache navi2ch-article-message-filter-cache)
	    (unfilter (cdr (assq 'unfilter article))))
	(setq cache (navi2ch-put-alist sym
				       (delq num (cdr (assq sym cache)))
				       cache))
	(setq cache (navi2ch-put-alist 'cache
				       (delq num (cdr (assq 'cache cache)))
				       cache))
	(setq navi2ch-article-message-filter-cache cache)
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
   "Hide message")
  (navi2ch-article-update-previous-message-separator))

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
  (navi2ch-article-load-number))

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
  (navi2ch-article-load-number))

(defun navi2ch-article-search ()
  "$B%a%C%;!<%8$r8!:w$9$k!#(B
$BL>A0(B (name)$B!"%a!<%k(B (mail)$B!"F|IU(B (date)$B!"(BID (id)$B!"%[%9%H(B
$BL>(B(hostname)$B!"K\J8(B (body)$B!";2>H(B(reference) $B$+$i8!:w>r7o$rA*$V$3$H(B
$B$,$G$-$^$9!#(B

$B%Q!<%::Q$_$N%a%C%;!<%8$N$_$r8!:wBP>]$H$9$k$N$G!"$"$i$+$8$a(B
`navi2ch-article-redraw-range' $B$r;H$&$J$I$7$F8!:w$7$?$$%a%C%;!<%8$r(B
$BI=<($7$F$*$/$3$H!#(B"
  (interactive)
  (let* ((has-id (navi2ch-article-get-current-id))
	 (has-hostname (navi2ch-article-get-current-hostname))
	 (ch (navi2ch-read-char-with-retry
	      (concat "Search for: n)ame m)ail d)ate "
		      (and has-id "i)d ")
		      (and has-hostname "h)ostname ")
		      "b)ody s)ubject r)eference: ")
	      nil
	      (append '(?n ?m ?b ?s ?d ?r)
		      (and has-id (list ?i))
		      (and has-hostname (list ?h))))))
    (cond
     ((eq ch ?n) (navi2ch-article-search-name))
     ((eq ch ?m) (navi2ch-article-search-mail))
     ((eq ch ?d) (navi2ch-article-search-date))
     ((eq ch ?i) (navi2ch-article-search-id))
     ((eq ch ?h) (navi2ch-article-search-hostname))
     ((eq ch ?b) (navi2ch-article-search-body))
     ((eq ch ?r) (navi2ch-article-search-reference)))))

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

(defun navi2ch-article-search-hostname (&optional host)
  (interactive)
  (unless host
    (setq host (navi2ch-read-string "Host: "
				    (navi2ch-article-get-current-hostname)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'date
			       (concat "\\(?:\\[ \\|$BH/?.85(B:\\)" 
				       (regexp-quote host))))

(defun navi2ch-article-search-body (&optional body)
  (interactive)
  (unless body
    (setq body (navi2ch-read-string "Body: "
				    (navi2ch-article-get-current-word-in-body)
				    'navi2ch-search-history)))
  (navi2ch-article-search-subr 'data (regexp-quote body)))

(defun navi2ch-article-search-reference (&optional num)
  (interactive)
  (unless (and num (numberp num))
    (setq num (read-number "Reference: "
			   (navi2ch-article-get-current-number))))
  (let ((num-regexp (navi2ch-fuzzy-regexp (number-to-string num)))
	(board navi2ch-article-current-board)
	(article navi2ch-article-current-article)
	num-list len)
    (dolist (msg navi2ch-article-message-list)
      (when (and (listp (cdr msg))
		 (or (string-match num-regexp (or (cdr (assq 'name (cdr msg))) ""))
		     (catch 'result
		       (with-temp-buffer
			 (setq navi2ch-article-current-board board
			       navi2ch-article-current-article article)
			 (insert (or (cdr (assq 'data (cdr msg))) ""))
			 (goto-char (point-min))
			 (while (re-search-forward 
				 (concat navi2ch-article-number-prefix-regexp
					 navi2ch-article-number-number-regexp)
				 nil t)
			   (when (navi2ch-eq-or-memq
				  num 
				  (navi2ch-article-str-to-num
				   (japanese-hankaku (match-string 1))))
			     (throw 'result t))
			   (while (looking-at (concat
					       navi2ch-article-number-separator-regexp
					       navi2ch-article-number-number-regexp))
			     (when (navi2ch-eq-or-memq
				    num 
				    (navi2ch-article-str-to-num 
				     (japanese-hankaku (match-string 1))))
			       (throw 'result t))
			     (goto-char (max (1+ (match-beginning 0))
					     (match-end 0))))))
		       nil)))
	(setq num-list (cons (car msg) num-list))))
    (setq len (length num-list))
    (if (= len 0)
	(message "No message found")
      (navi2ch-popup-article (nreverse num-list))
      (message (format "%d message%s found"
		       len
		       (if (= len 1) "" "s"))))))

(defun navi2ch-article-search-subr (field regexp)
  (let (num-list len)
    (dolist (msg navi2ch-article-message-list)
      (when (and (listp (cdr msg))
		 (string-match regexp (or (cdr (assq field (cdr msg))) "")))
	(setq num-list (cons (car msg) num-list))))
    (setq len (length num-list))
    (if (= len 0)
	(message "No message found")
      (navi2ch-popup-article (nreverse num-list))
      (message (format "%d message%s found"
		       len
		       (if (= len 1) "" "s"))))))

(defun navi2ch-article-save-dat-file (board article)
  (interactive (list navi2ch-article-current-board
		     navi2ch-article-current-article))
  (let ((file (navi2ch-article-get-file-name board article)))
    (cond ((not (file-exists-p file))
	   (message ".dat $B%U%!%$%k$,$"$j$^$;$s!#$^$:%9%l$r<hF@$7$F$/$@$5$$!#(B")
	   (ding))
	  ((not (file-readable-p file))
	   (message ".dat $B%U%!%$%k$rFI$a$^$;$s!#(B")
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
		       (y-or-n-p "$B$9$G$KB8:_$7$^$9!#>e=q$-$7$^$9$+(B? "))
	       (copy-file file newname t)
	       (message "`%s' $B$KJ]B8$7$^$7$?!#(B" newname)))))))

;;; filter mode
(navi2ch-set-minor-mode 'navi2ch-article-message-filter-mode
                        " Filter"
                        navi2ch-article-message-filter-mode-map)

(defsubst navi2ch-article-get-message-filter-cache-file-name (board article)
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
    (let ((file (navi2ch-article-get-message-filter-cache-file-name board
								    article))
	  (alist (delq nil
		       (mapcar
			(lambda (key)
			  (let ((slot (assq key cache)))
			    (and (cdr slot)
				 slot)))
			navi2ch-article-save-message-filter-cache-keys))))
      (if (and (null alist)
	       (file-exists-p file))
	  (condition-case nil
	      (delete-file file)
	    (error nil))
	(navi2ch-save-info file alist)))))

(defun navi2ch-article-load-message-filter-cache (&optional board article)
  (navi2ch-load-info
   (navi2ch-article-get-message-filter-cache-file-name
    (or board navi2ch-article-current-board)
    (or article navi2ch-article-current-article))))

(defun navi2ch-article-toggle-replace-message (&optional prefix)
  "$B8=:_$N%l%9$NCV49$NM-8z!&L58z$r@Z$jBX$($k!#(B"
  (interactive "P")
  (let ((unfilter (cdr (assq 'unfilter navi2ch-article-current-article)))
	(rep (cdr (assq 'replace navi2ch-article-message-filter-cache)))
	(num (navi2ch-article-get-current-number))
	(msg "No replacement cached"))
    (when (cdr (assq num rep))
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
	(setq navi2ch-article-message-filter-cache
	      (navi2ch-put-alist 'replace
				 (delq (assq num rep) rep)
				 navi2ch-article-message-filter-cache))
	(let ((orig (cdr (assq 'original navi2ch-article-message-filter-cache)))
	      (cache (cdr (assq 'cache navi2ch-article-message-filter-cache))))
	  (setq navi2ch-article-message-filter-cache
		(navi2ch-put-alist 'original
				   (delq (assq num orig) orig)
				   navi2ch-article-message-filter-cache))
	  (setq navi2ch-article-message-filter-cache
		(navi2ch-put-alist 'cache
				   (delq (assq num cache) cache)
				   navi2ch-article-message-filter-cache)))))
    (message msg)))

(defun navi2ch-article-toggle-message-filter (&optional prefix)
  "$B%U%#%k%?5!G=$N(B on/off $B$r@Z$jBX$($k!#(B
PREFIX $B$,M?$($i$l$?>l9g$O!"(B
$B%-%c%C%7%e$r%/%j%"$7!"%U%#%k%?5!G=$r(B on $B$K$7$F%9%l$rI=<($7$J$*$9!#(B"
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
      ;; $B%U%#%k%?A0$N%l%9$N>uBV$r%-%c%C%7%e$+$iI|85(B
      (when navi2ch-article-message-filter-mode
	(dolist (slots (cdr (assq 'original
				  navi2ch-article-message-filter-cache)))
	  (let ((alist (navi2ch-article-get-message (car slots))))
	    (dolist (slot (cdr slots))
	      (when (cdr slot)
		(navi2ch-put-alist (car slot) (cdr slot) alist))))))
      (setq navi2ch-article-current-article
	    (navi2ch-put-alist
	     'hide
	     (navi2ch-set-difference
	      (cdr (assq 'hide navi2ch-article-current-article))
	      (cdr (assq 'hide navi2ch-article-message-filter-cache)))
	     navi2ch-article-current-article))
      (setq navi2ch-article-current-article
	    (navi2ch-put-alist
	     'important
	     (navi2ch-set-difference
	      (cdr (assq 'important navi2ch-article-current-article))
	      (cdr (assq 'important navi2ch-article-message-filter-cache)))
	     navi2ch-article-current-article))
      ;; $B%-%c%C%7%e$r%/%j%"(B
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
  "$B%l%9$N%U%#%k%?>r7o$rBPOCE*$KDI2C$9$k!#(B"
  (interactive "P")
  (let* ((has-id (navi2ch-article-get-current-id))
	 (has-hostname (navi2ch-article-get-current-hostname))
	 (char (navi2ch-read-char-with-retry
		(concat "Filter by: n)ame m)ail "
			(and has-id "i)d ")
			(and has-hostname "h)ostname ")
			"b)ody s)ubject: ")
		nil
		(append '(?n ?m ?b ?s)
			(and has-id (list ?i))
			(and has-hostname (list ?h))))))
    (cond
     ((eq char ?n) (navi2ch-article-add-message-filter-by-name prefix))
     ((eq char ?m) (navi2ch-article-add-message-filter-by-mail prefix))
     ((eq char ?i) (navi2ch-article-add-message-filter-by-id prefix))
     ((eq char ?h) (navi2ch-article-add-message-filter-by-hostname prefix))
     ((eq char ?b) (navi2ch-article-add-message-filter-by-message prefix))
     ((eq char ?s) (navi2ch-article-add-message-filter-by-subject prefix)))))

(defun navi2ch-article-add-message-filter-cus (&optional prefix)
  "$B%l%9$N%U%#%k%?>r7o$r%+%9%?%`%(%G%#%?7A<0$GDI2C$9$k!#(B"
  (interactive "P")
  (let* ((has-id (navi2ch-article-get-current-id))
	 (has-hostname (navi2ch-article-get-current-hostname))
	 (char (navi2ch-read-char-with-retry
		(concat "Filter by: n)ame m)ail "
			(and has-id "i)d ")
			(and has-hostname "h)ostname ")
			"b)ody s)ubject: ")
		nil
		(append '(?n ?m ?b ?s)
			(and has-id (list ?i))
			(and has-hostname (list ?h)))))
	 (rule (cdr (assq char navi2ch-article-message-filter-default-rule-alist)))
	 (default-rule (cdr (assq t navi2ch-article-message-filter-default-rule-alist)))
	 (str (if prefix
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(or (plist-get rule :string)
		    (plist-get default-rule :string))))
	 (date (navi2ch-article-extract-date (navi2ch-article-get-current-date)))
	 (article navi2ch-article-current-article)
	 (board navi2ch-article-current-board))
    (setq str (cond
	       ((stringp str) str)
	       ((functionp str) (funcall str))
	       (t (error "rule type missmatch: string")))
	  navi2ch-article-message-filter-wid-window-configuration
	  (current-window-configuration))
    (kill-buffer (get-buffer-create "*navi2ch Add filter*"))
    (pop-to-buffer (get-buffer-create "*navi2ch Add filter*"))
    (kill-all-local-variables)
    (buffer-disable-undo)
    (setq navi2ch-article-message-filter-wid-var (or (plist-get rule :var)
						     (plist-get default-rule :var))
	  navi2ch-article-current-article article
	  navi2ch-article-current-board board)
    (insert "navi2ch Filter Editor\n\nString: ")
    (setq navi2ch-article-message-filter-wid-string 
	  (widget-create 'editable-field str))
    (insert "\nRule:\n")
    (setq navi2ch-article-message-filter-wid-rule
	  (widget-create 'radio-button-choice
			 :value (or (plist-get rule :rule)
				    (plist-get default-rule :rule))
			 '(editable-field :tag "replace" :format "%t: %v" "$B$"$\$\!<$s(B")
			 '(item :tag "hide"      :value hide)
			 '(item :tag "important" :value important)
			 '(editable-field :tag "score" :format "%t: %v" "0")))
    (insert "\n")
    (widget-create 'push-button
		   :notify 'navi2ch-article-add-message-filter-cus-done
		   "Done")
    (insert "\n\nMatch method:\n")
    (setq navi2ch-article-message-filter-wid-method
	  (widget-create 'radio-button-choice
			 :value (or (plist-get rule :match-method)
				    (plist-get default-rule :match-method))
			 '(item :tag "substring" :value "s")
			 '(item :tag "fuzzy"     :value "f")
			 '(item :tag "exact"     :value "e")
			 '(item :tag "regexp"    :value "r")))
    (insert "\nIgnore case: ")
    (setq navi2ch-article-message-filter-wid-case 
 	  (widget-create 'toggle
			 :value (or (plist-get rule :ignore-case)
				    (plist-get default-rule :ignore-case))))
;;; 	  (widget-create 'radio-button-choice
;;; 			 :value nil
;;; 			 '(item :tag "On" :value t)
;;; 			 '(item :tag "Off" :value nil)))
    (insert "Invert match: ")
    (setq navi2ch-article-message-filter-wid-invert 
;;; 	  (widget-create 'radio-button-choice
;;; 			 :value nil
;;; 			 '(item :tag "On" :value t)
;;; 			 '(item :tag "Off" :value nil)))
	  (widget-create 'toggle
			 :value (or (plist-get rule :invert-match)
				    (plist-get default-rule :invert-match))))
    (insert "\nScope:\n")
    (setq navi2ch-article-message-filter-wid-scope
	  (widget-create 'radio-button-choice
			 :value (or (plist-get rule :scope)
				    (plist-get default-rule :scope))
			 '(item :tag "board local"   :value board-local)
			 '(item :tag "article local" :value article-local)
			 '(item :tag "global"        :value nil)))
    (insert "\nFloating:\n")
    (setq navi2ch-article-message-filter-wid-float
	  (widget-create 'radio-button-choice
			 :value (or (plist-get rule :floating)
				    (plist-get default-rule :floating))
			 '(item :tag "never"   :value 0)
			 '(item :tag "always"  :value 1)
			 '(item :tag "default" :value nil)))
    (insert "\nDate:\n")
    (setq navi2ch-article-message-filter-wid-date
	  (widget-create 'radio-button-choice
			 :value (and (or (plist-get rule :date)
					 (plist-get default-rule :date))
				     date)
			 `(editable-field :tag "date local" :format "%t: %v" ,date)
			 '(item :tag "none specified"     :value nil)))
    (insert "\n")
    (widget-create 'push-button
		   :notify 'navi2ch-article-add-message-filter-cus-done
		   "Done")
    (insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (when (or (plist-get rule :auto)
	      (plist-get default-rule :auto))
      (navi2ch-article-add-message-filter-cus-done))))

(defun navi2ch-article-add-message-filter-cus-done (&rest ignore)
  (let* ((variable navi2ch-article-message-filter-wid-var)
	 (char (widget-value navi2ch-article-message-filter-wid-method))
	 (scope (widget-value navi2ch-article-message-filter-wid-scope))
	 (match (list 
		 (widget-value navi2ch-article-message-filter-wid-string)
		 (intern
		  (if (widget-value navi2ch-article-message-filter-wid-case)
		      char
		    (upcase char)))
		 :invert (widget-value navi2ch-article-message-filter-wid-invert)
		 :board-id (and (or (eq scope 'board-local)
				    (eq scope 'article-local))
				(cdr (assq 'id navi2ch-article-current-board)))
		 :artid (and (eq scope 'article-local)
			     (cdr (assq 'artid
					navi2ch-article-current-article)))
		 :float (widget-value navi2ch-article-message-filter-wid-float)
		 :date  (widget-value navi2ch-article-message-filter-wid-date)))
	 (res (widget-value navi2ch-article-message-filter-wid-rule))
	 (rule-children (widget-get 
			 navi2ch-article-message-filter-wid-rule
			 :children))
	 (result (cond ((or (eq res 'hide)
			    (eq res 'important)
			    (widget-apply
			     (nth 0 rule-children)
			     :active))
			res)
		       ((widget-apply (nth 3 rule-children) :active)
			(string-to-number res))
		       (t (error "You should select rule."))))
	 (current (assoc match (symbol-value variable))))
     (set variable (cons (cons match result)
			 (delq current (symbol-value variable))))
     (navi2ch-auto-modify-variables (list variable))
     (bury-buffer)
     (set-window-configuration 
      navi2ch-article-message-filter-wid-window-configuration)
     (if (y-or-n-p "Apply new rules to current messages now? ")
	 (navi2ch-article-toggle-message-filter t)
       (message "Don't apply now"))))
  
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

(defun navi2ch-article-add-message-filter-by-hostname (&optional prefix)
  (interactive "P")
  (navi2ch-article-add-message-filter-rule-subr
   'navi2ch-article-message-filter-by-hostname-alist
   "Hostname: "
   (if prefix
       (buffer-substring-no-properties (region-beginning) (region-end))
     (navi2ch-article-get-current-hostname))))

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
    (if (y-or-n-p "Apply new rules to current messages now? ")
	(navi2ch-article-toggle-message-filter t)
      (message "Don't apply now"))))

(defun navi2ch-article-read-message-filter-match (prompt
						  &optional initial-input)
  (let (str list)
    (when (y-or-n-p "Use extended matching? ")
      (let ((char (navi2ch-read-char-with-retry
		   "Type: s)ubstring f)uzzy e)xact r)egxp: "
		   nil
		   '(?s ?f ?e ?r))))
	(when (and (eq char ?r)
		   initial-input)
	  (setq initial-input (regexp-quote initial-input)))
	(unless (y-or-n-p "Ignore case? ")
	  (setq char (upcase char)))
	(setq list (list initial-input (intern (char-to-string char))))
	(when (y-or-n-p "Invert match? ")
	  (setq list (plist-put list :invert t)))))
    (setq str (navi2ch-read-string prompt
				   initial-input
				   'navi2ch-search-history))
    (if (null list)
	str
      (setcar list str)
      (let ((options '("s)cope" "f)loating" "d)ate")))
	(while (and options
		    (y-or-n-p "Set other options? "))
	  (let ((char (navi2ch-read-char-with-retry
		       (concat "Options: "
			       (mapconcat #'identity options " ")
			       ": ")
		       nil
		       (mapcar
			(lambda (x) (aref x (1- (string-match ")" x))))
			options))))
	    (cond
	     ((eq char ?s)
	      (let ((char (navi2ch-read-char-with-retry
			   "Scope: b)oard-local a)rticle-local d)efault: "
			   nil
			   '(?b ?a ?d))))
		(when (memq char '(?b ?a))
		  (setq list
			(plist-put
			 list
			 :board-id
			 (cdr (assq 'id navi2ch-article-current-board))))
		  (when (eq char ?a)
		    (setq list
			  (plist-put
			   list
			   :artid
			   (cdr (assq 'artid
				      navi2ch-article-current-article))))))
		(setq options (delete "s)cope" options))))
	     ((eq char ?f)
	      (let ((char (navi2ch-read-char-with-retry
			   "Floating: n)ever a)lways d)efault: "
			   nil
			   '(?n ?a ?d))))
		(cond
		 ((eq char ?n)
		  (setq list (plist-put list :float 0)))
		 ((eq char ?a)
		  (setq list (plist-put list :float 1))))
		(setq options (delete "f)loating" options))))
	     ((eq char ?d)
	      (when (y-or-n-p "Set the rule date local? ")
		(let ((date (navi2ch-read-string "Date local: "
						 (navi2ch-article-extract-date
						  (navi2ch-article-get-current-date)))))
		  (when (not (string= date ""))
		    (setq list
			  (plist-put list :date date))))))))))
      list)))

(defun navi2ch-article-read-message-filter-result (&optional initial-input)
  (let ((char (navi2ch-read-char-with-retry
	       "Result: r)eplace h)ide i)mportant s)core: "
	       nil
	       '(?r ?h ?i ?s))))
    (cond
     ((eq char ?r)
      (navi2ch-read-string "Relace with: "
			   (if (stringp initial-input)
			       initial-input
			     "$B$"$\$\!<$s(B")))
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
  "BOARD $B$H(B ARTICLE $B$G;XDj$5$l$k%9%l$,%*%k%U%!%s$J>l9g!"(Bnon-nil $B$rJV$9!#(B"
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
  "POINT $B$N2<$N%j%s%/$r;X$9(B URL $B$rF@$k!#(B"
  (let ((type (get-text-property point 'navi2ch-link-type))
	(prop (get-text-property point 'navi2ch-link)))
    (cond ((eq type 'number)
	   (navi2ch-article-number-list-to-url
	    (navi2ch-article-get-number-list prop)))
	  ((eq type 'url)
	   prop))))

(defun navi2ch-article-show-sssp-icon ()
  "sssp://$B$N%"%$%3%s$rI=<(!#(B"
  (interactive)
  (if (display-images-p)
      (let ((buffer-read-only nil)
	    (sssp_dir (expand-file-name "sssp_icon/" navi2ch-directory))
	    url image file)
	(unless (file-directory-p sssp_dir)
	  (make-directory sssp_dir))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward 
		  (concat "sssp://\\([^ \t\n\r]+\\.\\("
			  (regexp-opt navi2ch-browse-url-image-extentions)
			  "\\)\\)") 
		  nil t)
	    (setq url (concat "http://" (match-string 1)))
	    (when (string-match "/\\([^/]+\\)$" url)
	      (setq file (expand-file-name (match-string 1 url) sssp_dir))
	      ;; $B%U%!%$%k$,4{$K%m!<%+%k$KB8:_$7$F$k$N$J$i%"%C%W%G!<%H$7$J$$(B
	      ;; ($B99?7$OL5$5$=$&$@$7(B)
	      (unless (file-exists-p file)
		(save-excursion
		  (navi2ch-net-update-file url file)))
	      (forward-line)
	      (if (featurep 'xemacs)
		  (set-extent-end-glyph
		   (make-overlay (point) (point))
		   (make-glyph (vector 'gif :file file)))
		(insert-image (create-image file)))
	      (put-text-property (1- (point)) (point) 
				 'help-echo 
				 (navi2ch-propertize "[image]" 'display image))
	      (insert "\n")))))))

(defsubst navi2ch-article-jit-insert-1 (n cur diffpos wintop-pos start end)
  (when (and (<= start n) (<= n end))
    (let* ((alist (cdr (assq n navi2ch-article-message-list)))
	   (p (and (listp alist)
		   (cdr (assq 'point (assq n navi2ch-article-message-list))))))
      (when (or (null p) (= p (point-max)))
	(navi2ch-article-reinsert-partial-messages n n)
	(when (and cur diffpos)
	  (let ((navi2ch-article-goto-number-recenter t))
	    (navi2ch-article-goto-number cur))
	  (goto-char (+ (point) diffpos 1))
	  (when wintop-pos
	    (set-window-start (selected-window) 
			      (- (point) wintop-pos))))
	(redisplay)))))

(defun navi2ch-article-jit-insert ()
  (let ((buffer
	 (if (memq (current-buffer)
		   navi2ch-article-jit-buffers)
	     (current-buffer)
	   (car navi2ch-article-jit-buffers))))
    (if (buffer-live-p buffer)
	(let ((wintop-pos (and (eq (window-buffer) buffer)
			       (- (point) (window-start)))))
	  (with-current-buffer buffer
	    (let ((n (navi2ch-article-get-current-number))
		  diffpos)
	      (if (get-text-property (point) 'current-number)
		  (setq diffpos -1)
		(setq diffpos 
		      (previous-single-property-change (point) 'current-number))
		(when diffpos
		  (setq diffpos (- (point) diffpos))))
	      (let ((i 1)
		    (start (car navi2ch-article-jit-need-insert))
		    (end (cdr navi2ch-article-jit-need-insert))
		    (buffer-read-only nil)
		    (repeat t))
		(while (and repeat
			    (or (and (<= start (- n i)) (<= (- n i) end))
				(and (<= start (+ n i)) (<= (+ n i) end))))
		  (navi2ch-article-jit-insert-1 (+ n i) n diffpos wintop-pos start end)
		  (navi2ch-article-jit-insert-1 (- n i) n diffpos wintop-pos start end)
		  (setq i (1+ i))
		  (setq repeat (not (input-pending-p))))
		(when repeat
		  (setq navi2ch-article-jit-buffers
			(delq (current-buffer) navi2ch-article-jit-buffers))
		    (unless navi2ch-article-jit-buffers
		      (when navi2ch-article-jit-timer
			(cancel-timer navi2ch-article-jit-timer)
			(setq navi2ch-article-jit-timer nil))))))))
      (setq navi2ch-article-jit-buffers
	    (delq buffer navi2ch-article-jit-buffers)))))

(defun navi2ch-article-jit-reinsert-partial-messages (start &optional end)
  (let* ((nums (mapcar #'car navi2ch-article-message-list))
	 (len (length nums))
	 (last (car (last nums)))
	 (cur-res (navi2ch-article-get-current-number))
	 range-list)
    (when (minusp start)
      (setq start (+ start last 1)))
    (if (null end)
	(setq end last)
      (when (minusp end)
	(setq end (+ end last 1)))
      (when (> start end)
	(setq start (prog1 end
		      (setq end start)))))
    (navi2ch-article-reinsert-partial-messages start start)
    (navi2ch-article-reinsert-partial-messages end end)
    (setq navi2ch-article-jit-need-insert (if navi2ch-article-jit-need-insert
					      (cons (min (car navi2ch-article-jit-need-insert)
							 start)
						    (max (cdr navi2ch-article-jit-need-insert)
							 end))
					    (cons start end)))
    (push (current-buffer) navi2ch-article-jit-buffers)
    (unless navi2ch-article-jit-timer
      (setq navi2ch-article-jit-timer
	    (run-with-idle-timer navi2ch-article-jit-interval
				 t
				 'navi2ch-article-jit-insert)))))

(defun navi2ch-article-jit-insert-messages (list range number)
  (let ((len (length list)))
    (if (navi2ch-article-inside-range-p number range len)
	(progn
	  (navi2ch-article-reinsert-partial-messages 1 1)
	  (navi2ch-article-reinsert-partial-messages number number)
	  (navi2ch-article-reinsert-partial-messages len len)
	  (setq navi2ch-article-jit-need-insert (cons 1 len))
	  (push (current-buffer) navi2ch-article-jit-buffers)
	  (unless navi2ch-article-jit-timer
	    (setq navi2ch-article-jit-timer
		  (run-with-idle-timer navi2ch-article-jit-interval
				       t
				       'navi2ch-article-jit-insert))))
      ;; $BI=<(3+;O>l=j$,I=<(HO0O$K$J$$;~$O$^$k$J$2(B
      (navi2ch-article-insert-messages list range))))

(defun navi2ch-article-compress (&optional board article)
  (let (ignore)
    (when (eq major-mode 'navi2ch-article-mode)
      (if (navi2ch-board-from-file-p (or board navi2ch-article-current-board))
	  (setq ignore t)
	(or board (setq board navi2ch-article-current-board))
	(or article (setq article navi2ch-article-current-article))))
    (when (and (not ignore) board article (not (cdr (assq 'compressed article))))
      (let* ((file (navi2ch-article-get-file-name board article))
	     (gzfile (concat file ".gz")))
	(when (file-exists-p file)
	  (with-temp-file gzfile
	    (navi2ch-set-buffer-multibyte nil)
	    (insert-file-contents file))
	  (delete-file file))
	(setq article (navi2ch-put-alist 'compressed t article))
	(navi2ch-article-save-info board article)))))

(defun navi2ch-article-uncompress (&optional board article)
  (let (ignore)
    (when (eq major-mode 'navi2ch-article-mode)
      (if (navi2ch-board-from-file-p (or board navi2ch-article-current-board))
	  (setq ignore t)
	(or board (setq board navi2ch-article-current-board))
	(or article (setq article navi2ch-article-current-article))))
    (when (and (not ignore) board article (cdr (assq 'compressed article)))
      (let* ((gzfile (navi2ch-article-get-file-name board article))
	     (file (file-name-sans-extension gzfile)))
	(when (file-exists-p gzfile)
	  (with-temp-file file
	    (navi2ch-set-buffer-multibyte nil)
	    (insert-file-contents gzfile))
	  (delete-file gzfile))
	(setq article (navi2ch-put-alist 'compressed nil article))
	(navi2ch-article-save-info board article)))))

(run-hooks 'navi2ch-article-load-hook)
;;; navi2ch-article.el ends here
