;;; navi2ch-article.el --- article view module for navi2ch

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
(require 'navi2ch-net)
(require 'browse-url)
(require 'navi2ch-vars)
(require 'navi2ch-face)
(require 'navi2ch-popup-article)

(eval-when-compile
  (provide 'navi2ch-article)
  (require 'navi2ch))

(defvar navi2ch-article-mode-map nil)
(unless navi2ch-article-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'navi2ch-article-exit)
    (define-key map "Q" 'navi2ch-article-goto-current-board)
    (define-key map "s" 'navi2ch-article-sync)
    (define-key map "S" 'navi2ch-article-sync-disable-diff)
    (define-key map "r" 'navi2ch-article-redraw-range)
    (define-key map "j" 'navi2ch-article-few-scroll-up)
    (define-key map "k" 'navi2ch-article-few-scroll-down)
    (define-key map " " 'navi2ch-article-scroll-up)
    (define-key map [del] 'navi2ch-article-scroll-down)
    (define-key map [backspace] 'navi2ch-article-scroll-down)
    (define-key map "\177" 'navi2ch-article-scroll-down)
    (define-key map "w" 'navi2ch-article-write-message)
    (define-key map "W" 'navi2ch-article-write-sage-message)
    (define-key map "t" 'navi2ch-toggle-offline)
    (define-key map "\r" 'navi2ch-article-select-current-link)
    (navi2ch-define-mouse-key map 2 'navi2ch-article-mouse-select)
    (define-key map "g" 'navi2ch-article-goto-number-or-board)
    ;; (define-key map "g" 'navi2ch-article-goto-number)
    (define-key map "G" 'navi2ch-list-goto-board)
    (define-key map "l" 'navi2ch-article-pop-point)
    (define-key map "L" 'navi2ch-article-pop-poped-point)
    (define-key map "m" 'navi2ch-article-push-point)
    (define-key map "U" 'navi2ch-article-show-url)
  
    (define-key map "." 'navi2ch-article-redisplay-current-message)
    (define-key map "p" 'navi2ch-article-previous-message)
    (define-key map "n" 'navi2ch-article-next-message)
    (define-key map "P" 'navi2ch-article-through-previous)
    (define-key map "N" 'navi2ch-article-through-next)
    (define-key map [(shift tab)] 'navi2ch-article-previous-link)
    (define-key map "\e\C-i" 'navi2ch-article-previous-link)
    (define-key map "\C-\i" 'navi2ch-article-next-link)
    (define-key map ">" 'navi2ch-article-goto-last-message)
    (define-key map "<" 'navi2ch-article-goto-first-message)
    (define-key map "\eu" 'navi2ch-article-uudecode-message)
    (define-key map "\eb" 'navi2ch-article-base64-decode-message)
    (define-key map "v" 'navi2ch-article-view-aa)
    (define-key map "f" 'navi2ch-article-forward-buffer)
    (define-key map "b" 'navi2ch-article-backward-buffer)
    (define-key map "d" 'navi2ch-article-hide-message)
    (define-key map "a" 'navi2ch-article-add-important-message)
    (define-key map "h" 'navi2ch-article-toggle-hide)
    (define-key map "$" 'navi2ch-article-toggle-important)
    (define-key map "1" 'navi2ch-one-pain)
    (define-key map "2" 'navi2ch-article-two-pain)
    (define-key map "3" 'navi2ch-three-pain)
    (define-key map "A" 'navi2ch-article-add-global-bookmark)
    (define-key map "B" 'navi2ch-bookmark-goto-bookmark)
    (define-key map "\C-c\C-f" 'navi2ch-article-find-file)
    (define-key map "\C-c\C-u" 'navi2ch-goto-url)
    (define-key map "\C-c\C-m" 'navi2ch-message-pop-message-buffer)
    (define-key map "\C-xk" 'navi2ch-article-kill-buffer)
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
(defvar navi2ch-article-from-file-p nil
  "ファイルから読み込んでるか")
(defvar navi2ch-article-window-configuretion nil)
(defvar navi2ch-article-through-next-function 'navi2ch-article-through-next)
(defvar navi2ch-article-through-previous-function 'navi2ch-article-through-previous)

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

(defsubst navi2ch-article-inside-range-p (num range len)
  "NUM が RANGE で示す範囲に入ってるか
LEN は RANGE で範囲を指定される list の長さ"
  (or (not range)
      (<= num (car range))
      (> num (- len (cdr range)))))

(defsubst navi2ch-article-get-buffer-name (board article)
  (concat navi2ch-article-buffer-name-prefix
          (cdr (assq 'id board))
          "/"
          (cdr (assq 'artid article))))
                    
(defsubst navi2ch-article-get-url (board article)
  (concat (navi2ch-board-get-uri board)
          (if (cdr (assq 'kako board)) "" "dat/")
          (cdr (assq 'artid article))
          ".dat"))

(defsubst navi2ch-article-get-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat (cdr (assq 'artid article)) ".dat")))

(defsubst navi2ch-article-get-info-file-name (board article)
  (navi2ch-board-get-file-name board
                               (concat "info/" (cdr (assq 'artid article)))))

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

(defun navi2ch-article-url-to-article (url)
  "URL から article に変換。"
  (let (list)
    (cond ((string-match "http://.+/test/read\\.cgi.*&key=\\([0-9]+\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))
           (when (string-match "&st=\\([0-9]+\\)" url)
             (setq list (cons (cons 'number
                                    (string-to-number (match-string 1 url)))
                              list))))
	  ((string-match "http://.+/test/read\\.cgi/[^/]+/\\([^/]+\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))
           (when (string-match "http://.+/test/read\\.cgi/.+/[ni.]?\\([0-9]+\\)[^/]*$" url)
             (setq list (cons (cons 'number
                                    (string-to-number (match-string 1 url)))
                              list))))
          ((string-match "http://.+/\\([0-9]+\\)\\.\\(dat\\|html\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))))
    list))

(defun navi2ch-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE から url に変換。
START, END, NOFIRST で範囲を指定する"
  (let ((url (navi2ch-board-get-readcgi-url board)))
    (setq url (concat url (cdr (assq 'artid article)) "/"))
    (if (eq start end)
	(concat url start)
      (concat url
	      start (and (or start end) "-") end
	      (and nofirst "n")))))

(defsubst navi2ch-article-parse-message (str &optional sep)
  (or sep (setq sep navi2ch-article-separator))
  (unless (string= str "")
    (let ((strs (split-string str sep))
	  (syms '(name mail date data subject))
	  s)
      (mapcar (lambda (sym)
		(setq s (or (car strs) "")
		      strs (cdr strs))
		(cons sym
		      (if (eq sym 'data)
			  (navi2ch-replace-html-tag-with-temp-buffer s)
			(navi2ch-replace-html-tag s))))
	      syms))))

(defun navi2ch-article-get-separator ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[^\n]+<>[^\n]*<>")
        "<> *"
      ", *")))

(defun navi2ch-article-apply-filters (board)
  (dolist (filter navi2ch-article-filter-list)
    (if (stringp (car-safe filter))
	(apply 'navi2ch-call-process-buffer
	       (mapcar (lambda (x)
			 (if (eq x 'board)
			     (cdr (assq 'id board))
			   x))
		       filter))
      (funcall filter))))

(defun navi2ch-article-get-message-list (file &optional begin end)
  "FILE の BEGIN から END までの範囲からスレの list を作る
空行は nil"
  (when (file-exists-p file)
    (let ((board navi2ch-article-current-board)
	  sep message-list)
      (with-temp-buffer
        (navi2ch-insert-file-contents file begin end)
        (let ((i 1))
	  (navi2ch-article-apply-filters board)
          (message "splitting current messages...")
          (goto-char (point-min))
          (setq sep (navi2ch-article-get-separator))
          (while (not (eobp))
            (setq message-list
                  (cons (cons i
			      (let ((str (buffer-substring
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

(defsubst navi2ch-article-insert-message (num alist)
  (let ((p (point)))
    (insert (funcall navi2ch-article-header-format-function
                     num
                     (cdr (assq 'name alist))
                     (cdr (assq 'mail alist))
                     (cdr (assq 'date alist))))
    (put-text-property p (1+ p) 'current-number num))
  (let ((p (point)))
    (insert (cdr (assq 'data alist)) "\n")
    (save-excursion
      (save-restriction
	(narrow-to-region p (point))
	(navi2ch-article-set-link-property)
	(navi2ch-article-put-cite-face)
	(navi2ch-article-arrange-message))))
  (funcall navi2ch-article-insert-message-separator-function)
  (insert "\n"))

(defun navi2ch-article-insert-messages (list range)
  "LIST を整形して挿入する"
  (message "inserting current messages...")
  (let ((len (length list))
        (hide (cdr (assq 'hide navi2ch-article-current-article)))
        (imp (cdr (assq 'important navi2ch-article-current-article))))
    (dolist (x list)
      (let ((num (car x))
            (alist (cdr x)))
        (when (and alist
		   (cond (navi2ch-article-hide-mode
			  (memq num hide))
			 (navi2ch-article-important-mode
			  (memq num imp))
			 (t
			  (and (navi2ch-article-inside-range-p num range len)
			       (not (memq num hide))))))
          (when (stringp alist)
            (setq alist (navi2ch-article-parse-message alist)))
          (setcdr x (navi2ch-put-alist 'point (point-marker) alist))
          (navi2ch-article-insert-message num alist))))
    (garbage-collect) ; navi2ch-parse-message は大量にゴミを残す
    (message "inserting current messages...done")))

(defun navi2ch-article-default-header-format-function (number name mail date)
  "デフォルトのヘッダをフォーマットする関数
  ヘッダのface を付けるのもここで。"
  (let ((from-header "From: ")
        (from (format "[%d] %s <%s>\n" number name mail))
        (date-header "Date: ")
        str p)
    (setq str (concat from-header from date-header date "\n\n"))
 
    (setq p (length from-header))
    (put-text-property 0 p
		       'face 'navi2ch-article-header-face str)
    (put-text-property p (1- (setq p (+ p (length from))))
 		       'face 'navi2ch-article-header-contents-face str)
    (put-text-property p (setq p (+ p (length date-header)))
		       'face 'navi2ch-article-header-face str)
    (put-text-property p (setq p (+ p (length date)))
		       'face 'navi2ch-article-header-contents-face str)
    str))

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

(defun navi2ch-article-set-link-property ()
  ">>1 とか http:// に property を付ける"
  (goto-char (point-min))
  (while (re-search-forward
	  navi2ch-article-number-regexp nil t)
    (add-text-properties
     (match-beginning 0)
     (match-end 0)
     (list 'face 'navi2ch-article-link-face
	   'link t
	   'mouse-face 'highlight
	   'number (match-string 1))))
  (goto-char (point-min))
  (while (re-search-forward
	  navi2ch-article-url-regexp nil t)
    (add-text-properties
     (match-beginning 0)
     (match-end 0)
     (list 'face 'navi2ch-article-url-face
	   'link t
	   'mouse-face 'highlight
	   'url (concat "http://" (match-string 1))))))
  
(defun navi2ch-article-put-cite-face ()
  (goto-char (point-min))
  (while (re-search-forward navi2ch-article-citation-regexp nil t)
    (put-text-property (match-beginning 0)
		       (match-end 0)
		       'face 'navi2ch-article-citation-face)))

(defun navi2ch-article-expunge-buffers (&optional num)
  "スレのバッファの数を NUM に制限する。
NUM を指定しない場合は `navi2ch-article-max-buffers' を使用。"
  (interactive "P")
  (if (or (not (numberp num)) ; C-uのみの時4個にしたいわけじゃないと思われ
	  (= num 0))
      (setq num navi2ch-article-max-buffers))
  (if (> num 0)
      (save-excursion
	(dolist (killed-buf (nthcdr num (navi2ch-article-buffer-list)))
	  (set-buffer killed-buf)
	  (navi2ch-article-kill-buffer)))))

(defun navi2ch-article-view-article (board article
				     &optional force number max-line)
  "スレを見る。FORCE で強制読み込み MAX-LINE で読み込む行数を指定。
だた `navi2ch-article-max-line' とは逆で t で全部読み込み"
  (let ((buf-name (navi2ch-article-get-buffer-name board article))
	(navi2ch-article-max-line (cond ((numberp max-line) max-line)
					(max-line nil)
					(t navi2ch-article-max-line))))
    (if (get-buffer buf-name)
        (progn
          (switch-to-buffer buf-name)
          (navi2ch-article-sync force nil number))
      (switch-to-buffer (get-buffer-create buf-name))
      (navi2ch-article-mode)
      (navi2ch-article-expunge-buffers)
      (setq navi2ch-article-current-board board
            navi2ch-article-current-article article
            navi2ch-article-from-file-p nil)
      (when navi2ch-article-auto-range
        (if (file-exists-p (navi2ch-article-get-file-name board article))
            (setq navi2ch-article-view-range
                  navi2ch-article-exist-message-range)
          (setq navi2ch-article-view-range
                navi2ch-article-new-message-range)))
      (prog1 (navi2ch-article-sync force 'first number)
	(navi2ch-history-add navi2ch-article-current-board
			     navi2ch-article-current-article)))))

(defun navi2ch-article-view-article-from-file (file)
  "FILE からスレを見る。"
  (let* ((board (list (cons 'id "navi2ch")
                      (cons 'name "From File")))
         (article (list (cons 'artid file)))
         (buf-name (navi2ch-article-get-buffer-name board article)))
    (if (get-buffer buf-name)
        (progn
          (switch-to-buffer buf-name)
          (navi2ch-article-sync))
      (switch-to-buffer (get-buffer-create buf-name))
      (navi2ch-article-mode)
      (navi2ch-article-expunge-buffers)
      (setq navi2ch-article-current-board board
            navi2ch-article-current-article article
            navi2ch-article-from-file-p t)
      (when navi2ch-article-auto-range
        (setq navi2ch-article-view-range
              navi2ch-article-new-message-range))
      (save-excursion
	(setq navi2ch-article-message-list
	      (navi2ch-article-sync-from-file file))
	(navi2ch-article-set-mode-line)))))
  
(defun navi2ch-article-setup-menu ()
  (easy-menu-define navi2ch-article-mode-menu
		    navi2ch-article-mode-map
		    "Menu used in navi2ch-article"
		    navi2ch-article-mode-menu-spec)
  (easy-menu-add navi2ch-article-mode-menu))

(defun navi2ch-article-mode ()
  "\\{navi2ch-article-mode-map}"
  (interactive)
  (setq major-mode 'navi2ch-article-mode)
  (setq mode-name "Navi2ch Article")
  (setq buffer-read-only t)
  (make-local-variable 'navi2ch-article-current-article)
  (make-local-variable 'navi2ch-article-current-board)
  (make-local-variable 'navi2ch-article-message-list)
  (make-local-variable 'navi2ch-article-point-stack)
  (make-local-variable 'navi2ch-article-poped-point-stack)
  (make-local-variable 'truncate-partial-width-windows)
  (make-local-variable 'navi2ch-article-view-range)
  (make-local-variable 'navi2ch-article-from-file-p)
  (make-local-variable 'navi2ch-article-separator)
  (setq truncate-partial-width-windows nil)
  (use-local-map navi2ch-article-mode-map)
  (navi2ch-article-setup-menu)
  (setq navi2ch-article-point-stack nil)
  (run-hooks 'navi2ch-article-mode-hook))

(defun navi2ch-article-exit ()
  (interactive)
  ;; (navi2ch-article-add-number)
  (let ((buf (current-buffer)))
    (if (null navi2ch-article-message-list)
        (progn
          (delete-windows-on buf)
          (kill-buffer buf))
      (delete-windows-on buf))
    ;;  (bury-buffer navi2ch-article-buffer-name)
    (let ((board-win (get-buffer-window navi2ch-board-buffer-name))
	  (board-buf (get-buffer navi2ch-board-buffer-name)))
      (cond (board-win (select-window board-win))
	    (board-buf (switch-to-buffer board-buf))
	    (t (navi2ch-list))))))

(defun navi2ch-article-goto-current-board ()
  "スレッドと同じ板へ移動"
  (interactive)
  (let ((board navi2ch-article-current-board))
    (navi2ch-article-exit)
    (navi2ch-board-select-board board)))

(defun navi2ch-article-kill-buffer ()
  "save-info してから kill-buffer する"
  (interactive)
  (navi2ch-article-save-info)
  (kill-buffer (current-buffer)))

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
  (when (not navi2ch-article-from-file-p)
    (run-hooks 'navi2ch-article-before-sync-hook)
    (let* ((list navi2ch-article-message-list)
           (article navi2ch-article-current-article)
           (board navi2ch-article-current-board)
           (navi2ch-net-force-update (or navi2ch-net-force-update
                                         force))
           (file (navi2ch-article-get-file-name board article))
           (old-size (nth 7 (file-attributes file)))
           state)
      (when first
        (setq article (navi2ch-article-load-info)))
      (let ((ret (navi2ch-article-update-file board article force)))
        (setq article (nth 0 ret)
              navi2ch-article-current-article article
              state (nth 1 ret)))
      (prog1
	  ;; 更新できたら
	  (when (or (and first (file-exists-p file))
		    state)
	    (setq list
		  (if (or first (nth 1 state)) ; first or あぼーん
		      (navi2ch-article-get-message-list file)
		    (navi2ch-article-append-message-list
		     list (navi2ch-article-get-message-list
			   file old-size))))
	    (setq navi2ch-article-message-list list)
	    (let ((num (or number (cdr (assq 'number article)))))
	      (when (and navi2ch-article-fix-range-when-sync num)
		(navi2ch-article-fix-range num)))
	    (unless first
	      (navi2ch-article-save-number))
	    (setq navi2ch-article-hide-mode nil
		  navi2ch-article-important-mode nil)
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (navi2ch-article-insert-messages list
					       navi2ch-article-view-range))
	    (navi2ch-article-load-number)
	    (navi2ch-article-save-info board article first)
	    (navi2ch-article-set-mode-line)
	    (run-hooks 'navi2ch-article-after-sync-hook)
	    list)
	(when (and navi2ch-article-fix-range-when-sync number)
	  (navi2ch-article-fix-range number)
	  (navi2ch-article-redraw)
	  (navi2ch-article-goto-number number))
	(navi2ch-article-set-summary-element board article nil)))))

(defun navi2ch-article-fetch-article (board article &optional force)
  (if (get-buffer (navi2ch-article-get-buffer-name
                   board article))
      (let ((buf (current-buffer))
            state)
        (setq state (navi2ch-article-view-article board article force))
        (switch-to-buffer buf)
        state)
    (let (ret state)
      (setq article (navi2ch-article-load-info board article)
            ret (navi2ch-article-update-file board article force)
            article (nth 0 ret)
            state (nth 1 ret))
      (when state
	(navi2ch-article-save-info board article)
	(navi2ch-article-set-summary-element board article t)
	t))))

(defun navi2ch-article-get-readcgi-raw-url (board article)
  (let ((url (navi2ch-article-to-url board article))
	(file (navi2ch-article-get-file-name board article))
	(raw 0)
	(size 0))
    (when (file-exists-p file)
      (setq raw (with-temp-buffer (navi2ch-insert-file-contents file)
				  (count-lines (point-min) (point-max))))
      (setq size (nth 7 (file-attributes file))))
    (format "%s?raw=%s.%s" url raw size)))
    
(defun navi2ch-article-update-file (board article &optional force)
  "BOARD, ARTICLE に対応するファイルを更新する。
返り値は (article (header aborn-p)) のリスト"
  (let (state)
    (unless navi2ch-offline
      (let ((file (navi2ch-article-get-file-name board article))
	    (time (cdr (assq 'time article)))
	    full-size url)
        (setq state
	      (if (and (navi2ch-enable-readcgi-p
			(navi2ch-board-get-host board)) (file-exists-p file))
		  (progn
		    (setq url (navi2ch-article-get-readcgi-raw-url board article))
		    (navi2ch-net-update-file-with-readcgi url file time
							  (file-exists-p file)))
		(setq url (navi2ch-article-get-url board article))
		(if (and (file-exists-p file) navi2ch-article-enable-diff)
		    (navi2ch-net-update-file-diff
		     url file (cdr (assq 'time article)))
		  (let ((st (navi2ch-net-update-file
			     url file (cdr (assq 'time article)) nil)))
		    (when st (list st nil))))))
        (when state
	  (setq article (navi2ch-put-alist 'time
					   (cdr (assoc "Last-Modified"
						       (nth 0 state)))
					   article)))))
    (list article state)))


(defun navi2ch-article-sync-from-file (file)
  "スレを FILE から更新する。"
  (when navi2ch-article-from-file-p
    (let ((list (navi2ch-article-get-message-list file))
	  (range navi2ch-article-view-range))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (navi2ch-article-insert-messages list range))
      list)))

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
          (format "%s (%s/%d) [%s]"
                  (cdr (assq 'subject article))
                  (or "" (cdr (assq 'response article)))
                  (length navi2ch-article-message-list)
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
    (let ((buf (get-buffer-create "*select view range*"))
          (range navi2ch-article-view-range))
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
        (message "input: ")
        (setq n (read-char))
        (when (or (< n ?0) (> n ?9))
          (error "%c is bad key" n))
        (setq n (- n ?0))
        (setq range
              (if (eq n 0) nil
                (nth (1- n) navi2ch-article-view-range-list))))
      (kill-buffer buf)
      range)))

(defun navi2ch-article-redraw-range ()
  "表示する範囲を指定した後 redraw"
  (interactive)
  (setq navi2ch-article-view-range
        (navi2ch-article-select-view-range-subr))
  (navi2ch-article-redraw))

(defun navi2ch-article-find-file (file)
  "FILE からスレを選ぶ"
  (interactive "fArtilce File: ")
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer)))))
    (cond (art-win (select-window art-win))
          (board-win
           (select-window board-win)
	   (when navi2ch-bm-stay-board-window
	     (split-window-vertically navi2ch-board-window-height)
	     (other-window 1)))
          (list-win
           (select-window list-win)
	   (when navi2ch-list-stay-list-window
	     (split-window-horizontally navi2ch-list-window-width)
	     (other-window 1))))
    (navi2ch-article-view-article-from-file file)))

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
  (when (not navi2ch-article-from-file-p)
    (when (and navi2ch-article-message-list (not first))
      (navi2ch-article-save-number))
    (or board (setq board navi2ch-article-current-board))
    (or article (setq article navi2ch-article-current-article))
    (let ((alist (list
                  (assq 'number article)
                  (assq 'name article)
                  (assq 'time article)
                  (assq 'hide article)
                  (assq 'important article)
		  (assq 'mail article)
		  (assq 'access-time article))))
      (navi2ch-save-info
       (navi2ch-article-get-info-file-name board article)
       alist))))

(defun navi2ch-article-load-info (&optional board article)
  (when (not navi2ch-article-from-file-p)
    (or board (setq board navi2ch-article-current-board))
    (or article (setq article navi2ch-article-current-article))
    (let ((alist (navi2ch-load-info
                  (navi2ch-article-get-info-file-name board article))))
      (dolist (x alist)
        (setq article (navi2ch-put-alist (car x) (cdr x) article)))
      article)))

(defun navi2ch-article-write-message (&optional sage)
  (interactive)
  (when (not navi2ch-article-from-file-p)
    (navi2ch-message-write-message navi2ch-article-current-board
                                   navi2ch-article-current-article
				   nil sage)))

(defun navi2ch-article-write-sage-message ()
  (interactive)
  (navi2ch-article-write-message 'sage))

(defun navi2ch-article-str-to-num (str)
  "レス参照の文字列を数字か数字の list に変換"
  (cond ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" str)
	 (let ((min (string-to-number (match-string 1 str)))
	       (i (string-to-number (match-string 2 str)))
	       list)
	   (while (>= i min)
	     (push i list)
	     (setq i (1- i)))
	   list))
	((string-match "\\([0-9]+,\\)+[0-9]+" str)
	 (mapcar 'string-to-number (split-string str ",")))
	(t (string-to-number str))))

(defun navi2ch-article-select-current-link (&optional browse-p)
  (interactive "P")
  (let ((prop (get-text-property (point) 'number)))
    (if prop
	(progn
	  (setq prop (navi2ch-article-str-to-num (japanese-hankaku prop)))
	  (if (numberp prop)
	      (navi2ch-article-goto-number prop t t)
	    (navi2ch-popup-article prop)))
      (setq prop (get-text-property (point) 'url))
      (when prop
	(let ((2ch-url-p (navi2ch-2ch-url-p prop)))
	  (if (and 2ch-url-p
		   (or (navi2ch-board-url-to-board prop)
		       (navi2ch-article-url-to-article prop))
		   (not browse-p))
	      (navi2ch-goto-url prop)
	    (navi2ch-browse-url prop)))))))

(defun navi2ch-article-mouse-select (e)
  (interactive "e")
  (mouse-set-point e)
  (navi2ch-article-select-current-link))

(defun navi2ch-article-recenter (num)
  "NUM 番目のレスを画面の一番上に"
  (save-excursion
    (goto-char (cdr (assq 'point (navi2ch-article-get-message num))))
    (recenter 0)))

(defun navi2ch-article-goto-number-or-board ()
  "入力された数字の位置に移動するか、入力された板を表示する。
名前が数字ならばデフォルトはその名前の数字。"
  (interactive)
  (let (default alist ret)
    (setq default 
	  (let ((from (cdr (assq 'name 
				 (navi2ch-article-get-message 
				  (navi2ch-article-get-current-number))))))
	    (or (and (string-match "[0-9０-９]+" from)
		     (japanese-hankaku (match-string 0 from)))
		nil)))
    (setq alist (mapcar
		 (lambda (x) (cons (cdr (assq 'id x)) x))
		 (navi2ch-list-get-board-name-list
		  navi2ch-list-category-list)))
    (setq ret (completing-read
	       (concat "input number or board"
		       (and default (format "(%s)" default))
		       ": ")
	       alist nil nil))
    (setq ret (if (string= ret "") default ret))

    (if ret
	(let ((num (string-to-number ret)))
	  (if (> num 0)
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
  (when (> num 0)
    (when (or (interactive-p) save)
      (navi2ch-article-push-point))
    (catch 'break
      (let ((len (length navi2ch-article-message-list))
	    (range navi2ch-article-view-range))
	(and (> num len) (setq num len))
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
  (condition-case error
      (scroll-up)
    (end-of-buffer
     (funcall navi2ch-article-through-next-function)))
  (force-mode-line-update t))

(defun navi2ch-article-scroll-down ()
  (interactive)
  (condition-case error
      (scroll-down)
    (beginning-of-buffer
     (funcall navi2ch-article-through-previous-function)))
  (force-mode-line-update t))

(defun navi2ch-article-through-ask (no-ask)
  "次のスレに移動するか聞く。
次のスレに移動するなら t を返す。
移動しないなら nil を返す。
article buffer から抜けるなら 'quit を返す。"
  (if (or (eq navi2ch-article-enable-through 'ask-always)
	  (and (not no-ask)
	       (eq navi2ch-article-enable-through 'ask)))
      (navi2ch-y-or-n-p "Through next article or quit?" 'quit)
    navi2ch-article-enable-through))
  
(defun navi2ch-article-through-next ()
  (interactive)
  (let ((mode (navi2ch-get-major-mode navi2ch-board-buffer-name)))
    (if (and mode
	     (or (not (eq mode 'navi2ch-board-mode))
		 (and (eq mode 'navi2ch-board-mode)
		      (navi2ch-board-equal navi2ch-article-current-board
					   navi2ch-board-current-board))))
	(let ((ret (navi2ch-article-through-ask (interactive-p))))
	  (cond ((eq ret 'quit)
		 (navi2ch-article-exit))
		(ret
		 (if (get-buffer-window navi2ch-board-buffer-name)
		     (select-window (get-buffer-window navi2ch-board-buffer-name))
		   (switch-to-buffer navi2ch-board-buffer-name))
		 (navi2ch-bm-next-line)
		 (navi2ch-bm-select-article))
		(t
		 (message "Don't through next article"))))
      (message "Don't through next article"))))

(defun navi2ch-article-through-previous ()
  (interactive)
  (let ((mode (navi2ch-get-major-mode navi2ch-board-buffer-name)))
    (if (and mode
             (or (not (eq mode 'navi2ch-board-mode))
                 (and (eq mode 'navi2ch-board-mode)
		      (navi2ch-board-equal navi2ch-article-current-board
					   navi2ch-board-current-board))))
	(let ((ret (navi2ch-article-through-ask (interactive-p))))
	  (cond ((eq ret 'quit)
		 (navi2ch-article-exit))
		(ret
		 (if (get-buffer-window navi2ch-board-buffer-name)
		     (select-window (get-buffer-window navi2ch-board-buffer-name))
		   (switch-to-buffer navi2ch-board-buffer-name))
		 (navi2ch-bm-previous-line)
		 (navi2ch-bm-select-article))
      		(t
		 (message "Don't through previous article"))))
      (message "Don't through previous article"))))

(defun navi2ch-article-two-pain ()
  (interactive)
  (let* ((list-buf (get-buffer navi2ch-list-buffer-name))
         (board-buf (get-buffer navi2ch-board-buffer-name))
         (art-buf (navi2ch-article-current-buffer))
         (list-win (get-buffer-window (or list-buf "")))
         (board-win (get-buffer-window (or board-buf "")))
         buf)
    (when art-buf
      (delete-other-windows)
      (switch-to-buffer art-buf)
      (setq buf
            (cond ((and list-buf board-buf)
                   (cond ((and list-win board-win) board-buf)
                         (list-win board-buf)
                         (board-win list-buf)
                         (t board-buf)))
                  (list-buf list-buf)
                  (board-buf board-buf)))
      (when buf
        (if (eq buf list-buf)
            (split-window-horizontally navi2ch-list-window-width)
          (split-window-vertically navi2ch-board-window-height))
        (switch-to-buffer buf))
      (other-window 1))))

(defun navi2ch-article-get-message (num)
  "NUM 番目のレスを得る"
  (cdr (assq num navi2ch-article-message-list)))

(defun navi2ch-article-get-current-number ()
  "今の位置のレスの番号を得る"
  (condition-case error
      (or (get-text-property (point) 'current-number)
          (get-text-property
           (navi2ch-previous-property (point) 'current-number)
           'current-number))
    (error nil)))

(defun navi2ch-article-show-url ()
  "url を表示して、その url を見るか kill ring にコピーする"
  (interactive)
  (let ((url (navi2ch-article-to-url navi2ch-article-current-board
				     navi2ch-article-current-article)))
    (message "c)opy v)iew t)itle? URL: %s" url)
    (let ((char (read-char)))
      (if (eq char ?t) (navi2ch-article-copy-title)
	(funcall (cond ((eq char ?c) '(lambda (x) (message "copy: %s" (kill-new x))))
		       ((eq char ?v) 'navi2ch-browse-url))
		 (navi2ch-article-show-url-subr))))))

(defun navi2ch-article-show-url-subr ()
  "メニューを表示して、url を得る"
  (message "a)ll c)rrent r)egion b)oard")
  (let ((char (read-char)))
    (if (eq char ?b)
	(navi2ch-board-to-url navi2ch-article-current-board)
      (apply 'navi2ch-article-to-url
	     navi2ch-article-current-board navi2ch-article-current-article
	     (cond ((eq char ?a) nil)
		   ((eq char ?c) (list (navi2ch-article-get-current-number)
				       (navi2ch-article-get-current-number)
				       t))
		   ((eq char ?r) (list (save-excursion
					 (goto-char (region-beginning))
					 (navi2ch-article-get-current-number))
				       (save-excursion
					 (goto-char (region-end))
					 (navi2ch-article-get-current-number)
					 t))))))))

(defun navi2ch-article-copy-title ()
  "メニューを表示して、タイトルを得る"
  (message "b)oard a)rticle")
  (let ((char (read-char)))
    (message "copy: %s"
	     (kill-new
	      (cond ((eq char ?b) (cdr (assq 'name navi2ch-article-current-board)))
		    ((eq char ?a) (cdr (assq 'subject navi2ch-article-current-article))))))))

(defun navi2ch-article-redisplay-current-message ()
  "今いるレスを画面の中心(上？)に"
  (interactive)
  (navi2ch-article-recenter
   (navi2ch-article-get-current-number)))

(defun navi2ch-article-next-message ()
  "次のメッセージへ"
  (interactive)
  (condition-case error
      (progn
        (goto-char (navi2ch-next-property (point) 'current-number))
        (navi2ch-article-goto-number
         (navi2ch-article-get-current-number)))
    (error
     (funcall navi2ch-article-through-next-function))))

(defun navi2ch-article-previous-message ()
  "前のメッセージへ"
  (interactive)
  (condition-case error
      (progn
        (goto-char (navi2ch-previous-property (point) 'current-number))
        (navi2ch-article-goto-number
         (navi2ch-article-get-current-number)))
    (error
     (funcall navi2ch-article-through-previous-function))))

(defun navi2ch-article-next-link ()
  "次のリンクへ"
  (interactive)
  (let ((point (navi2ch-next-property (point) 'link)))
    (and point (goto-char point))))

(defun navi2ch-article-previous-link ()
  "前のリンクへ"
  (interactive)
  (let ((point (navi2ch-previous-property (point) 'link)))
    (and point (goto-char point))))

(defun navi2ch-article-uudecode-message ()
  (interactive)
  (with-temp-buffer
    (insert (cdr
             (assq 'data
                   (save-excursion
                     (set-buffer (navi2ch-article-current-buffer))
                     (navi2ch-article-get-message
                      (navi2ch-article-get-current-number))))))
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at "end\\([ \t]*\\)")
      (delete-region (match-beginning 1) (match-end 1))
      (end-of-line)
      (insert "\n"))
    (navi2ch-uudecode-region (point-min) (point-max))))

(defun navi2ch-article-base64-decode-message (prefix &optional filename)
  "現在のレスをbase64デコードし、FILENAMEに書き出す
PREFIXを指定した場合は、markのあるレスと現在のレスの間の範囲が対象になる"
  (interactive "P")
  (save-excursion
    (let* ((num (navi2ch-article-get-current-number))
	   (num2 (or (and prefix
			  (car (navi2ch-article-get-point (mark))))
		     num))
	   (begin (or (cdr (assq 'point (navi2ch-article-get-message
					 (min num num2))))
		      (point-min-marker)))
	   (end (or (cdr (assq 'point (navi2ch-article-get-message
				       (1+ (max num num2)))))
		    (point-max-marker))))
      (navi2ch-base64-write-region (marker-position begin)
				   (marker-position end) filename))))

(defun navi2ch-article-call-aadisplay (str)
  (let* ((coding-system-for-write navi2ch-article-aadisplay-coding-system)
	 (file (make-temp-name (concat temporary-file-directory "navi2ch"))))
    (with-temp-file file
      (insert str))
    (let ((w32-start-process-show-window t)) ; for meadow
      (call-process navi2ch-article-aadisplay-program
                    nil nil nil file))
    (delete-file file)))

(defun navi2ch-article-popup-dialog (str)
  (x-popup-dialog
   t (cons "navi2ch"
           (mapcar (lambda (x)
                     (cons x t))
                   (split-string str "\n")))))

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

(defun navi2ch-article-current-buffer ()
  "BUFFER-LIST の一番最初の `navi2ch-article-mode' の buffer を返す"
  (let ((list (buffer-list)))
    (catch 'loop
      (while list
        (when (save-excursion
                (set-buffer (car list))
                (eq major-mode 'navi2ch-article-mode))
          (throw 'loop (car list)))
        (setq list (cdr list)))
      nil)))

(defun navi2ch-article-forward-buffer ()
  "次の article buffer へ"
  (interactive)
  (let (buf)
    (dolist (x (buffer-list))
      (when (save-excursion
              (set-buffer x)
              (eq major-mode 'navi2ch-article-mode))
        (setq buf x)))
    (switch-to-buffer buf)))
  
(defun navi2ch-article-backward-buffer ()
  "前の article buffer へ"
  (interactive)
  (bury-buffer)
  (switch-to-buffer (navi2ch-article-current-buffer)))

(defun navi2ch-article-delete-message (sym func msg)
  (let* ((article navi2ch-article-current-article)
         (list (cdr (assq sym article)))
         (num (navi2ch-article-get-current-number)))
    (setq list (funcall func num list))
    (setq navi2ch-article-current-article
          (navi2ch-put-alist sym list article))
    (save-excursion
      (let ((buffer-read-only nil))
        (delete-region
         (if (get-text-property (point) 'current-number)
             (point)
           (navi2ch-previous-property (point) 'current-number))
         (or (navi2ch-next-property (point) 'current-number)
             (point-max))))))
  (message msg))


;;; hide mode
(navi2ch-set-minor-mode 'navi2ch-article-hide-mode
                        " Hide"
                        navi2ch-article-hide-mode-map)

(defun navi2ch-article-hide-message ()
  (interactive)
  (navi2ch-article-delete-message
   'hide
   (lambda (num list)
     (if (memq num list)
         list
       (cons num list)))
   "Hide message"))

(defun navi2ch-article-cancel-hide-message ()
  (interactive)
  (navi2ch-article-delete-message 'hide 'delq
                                  "Cansel hide message"))


  
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

(defun navi2ch-article-add-important-message ()
  (interactive)
  (let* ((article navi2ch-article-current-article)
         (list (cdr (assq 'important article)))
         (num (navi2ch-article-get-current-number)))
    (unless (memq num list)
      (setq list (cons num list))
      (setq navi2ch-article-current-article
            (navi2ch-put-alist 'important list article))
      (message "Add important message"))))

(defun navi2ch-article-delete-important-message ()
  (interactive)
  (navi2ch-article-delete-message 'important 'delq
                                  "Delete important message"))
  
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

(provide 'navi2ch-article)

;;; navi2ch-article.el ends here
