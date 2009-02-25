;;; navi2ch.el --- Navigator for 2ch for Emacsen -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008 by
;; Navi2ch Project

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
(provide 'navi2ch)
(defconst navi2ch-ident
  "$Id$")

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
(require 'navi2ch-directory)
(require 'navi2ch-message)
(navi2ch-ifxemacs
    (require 'navi2ch-xmas)
  (when navi2ch-on-emacs21
    (require 'navi2ch-e21)))
(require 'navi2ch-splash)
(require 'navi2ch-version)
(require 'navi2ch-jbbs-net)
(require 'navi2ch-jbbs-shitaraba)
(require 'navi2ch-machibbs)
(require 'navi2ch-futaba)
(require 'navi2ch-be2ch)
(require 'navi2ch-multibbs)
(require 'navi2ch-mona)
(require 'navi2ch-oyster)
(require 'navi2ch-localfile)
(require 'navi2ch-auto-modify)

(defgroup navi2ch nil
  "Navigator for 2ch."
  :group 'hypermedia)

(defvar navi2ch-ask-when-exit t)

(defvar navi2ch-init nil)

;; hook 用関数。
(defun navi2ch-kill-emacs-hook ()
  (run-hooks 'navi2ch-kill-emacs-hook)
  (navi2ch-save-status)
  (navi2ch-unlock))

(defvar navi2ch-mona-enable)

;;;###autoload
(defun navi2ch (&optional arg)
  "Navigator for 2ch for Emacs."
  (interactive "P")
  (run-hooks 'navi2ch-before-startup-hook)
  (unless navi2ch-init
    (if arg (setq navi2ch-offline (not navi2ch-offline)))
    (setq navi2ch-info-cache
	  (navi2ch-make-cache navi2ch-info-cache-limit 'equal))
    (load (expand-file-name navi2ch-update-file navi2ch-directory) t)
    (load (expand-file-name navi2ch-init-file navi2ch-directory) t)
    (navi2ch-lock)
    (when navi2ch-auto-update
      (let ((done nil))
	(unwind-protect
	    (progn
	      (navi2ch-update)
	      (setq done t))
	  (unless done
	    (navi2ch-unlock)))))
    (add-hook 'kill-emacs-hook 'navi2ch-kill-emacs-hook)
    (run-hooks 'navi2ch-load-status-hook)
    (run-hooks 'navi2ch-hook)
    (let ((splash-buffer (and navi2ch-display-splash-screen
			      (navi2ch-splash))))
      (unwind-protect
	  (navi2ch-list)
	(when (buffer-live-p splash-buffer)
	  (kill-buffer splash-buffer))))
    (setq navi2ch-init t))
  (when navi2ch-mona-enable
    (navi2ch-mona-setup))
  (navi2ch-be2ch-login-p)
  (navi2ch-list)
  (run-hooks 'navi2ch-after-startup-hook))

(defun navi2ch-version ()
  (interactive)
  (message "Navigator for 2ch %s" navi2ch-version))

(defun navi2ch-save-status ()
  "list, board, article の状態を保存する。"
  (interactive)
  (message "Save status...")
  (run-hooks 'navi2ch-save-status-hook)
  (message "Save status...done"))

(defun navi2ch-exit (&optional suspend)
  "navi2ch を終了する。
SUSPEND が non-nil なら buffer を消さない。"
  (interactive)
  (when (or suspend
            (not navi2ch-ask-when-exit)
            (if (functionp navi2ch-ask-when-exit)
		(funcall navi2ch-ask-when-exit "Really exit navi2ch? ")
	      (y-or-n-p "Really exit navi2ch? ")))
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
      (navi2ch-unlock)
      (remove-hook 'kill-emacs-hook 'navi2ch-kill-emacs-hook))))

(defun navi2ch-suspend ()
  "navi2ch を一時的に終了する。"
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

(defsubst navi2ch-make-backup-file-name (file)
  "FILE で指定されるファイルからバックアップファイルの名前を返す。"
  ;; とりあえずは、OS ごとのバックアップ名の違いは Emacs にまかせる。
  ;; 後々変えたくなった時に変更し忘れるのを防ぐため。
  (make-backup-file-name file))

;; make-temp-file の方が安全だけど、存在しない環境では make-temp-name を使う。
(defsubst navi2ch-make-temp-file (file)
  "テンポラリファイルを作る。"
  (funcall (if (fboundp 'make-temp-file)
	       'make-temp-file
	     'make-temp-name) file))

(defvar navi2ch-info-cache nil)
(defvar navi2ch-info-cache-limit 1000)

(defun navi2ch-save-info (file info &optional backup)
  "lisp-object INFO を FILE に保存する。
BACKUP が non-nil の場合は元のファイルをバックアップする。"
  (setq file (expand-file-name file navi2ch-directory))	; 絶対パスにしておく
  ;; FIXME:とりあえず、全て保存するようにしてある
  ;; できれば内容が変わらない時には保存したくない
  (navi2ch-cache-put file info navi2ch-info-cache)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (when (or (file-regular-p file)
	    (not (file-exists-p file)))
    (let ((coding-system-for-write navi2ch-coding-system)
	  (backup-file (navi2ch-make-backup-file-name file))
	  temp-file)
      (unwind-protect
	  (progn
	    ;; ファイルが確実に消えるよう、下の setq は上の let に移動
	    ;; してはダメ
	    (setq temp-file (navi2ch-make-temp-file
			     (file-name-directory file)))
	    (with-temp-file temp-file
	      (let ((standard-output (current-buffer))
		    print-length print-level)
		(princ ";;; -*- mode: emacs-lisp; -*-\n")
		(prin1 info)))
	    (if (and backup (file-exists-p file))
		(rename-file file backup-file t))
	    ;; 上の rename が成功して下が失敗しても、 navi2ch-load-info
	    ;; がバックアップファイルから読んでくれる。
	    (condition-case err
		(progn
		  (unless (file-exists-p file)
		    (with-temp-file file)) ; ファイルが作成可能かチェック
		  (rename-file temp-file file t))
	      (file-error
	       (message "%s: %s"
			(cadr err)
			(mapconcat #'identity (cddr err) ", ")))))
	(if (and temp-file (file-exists-p temp-file))
	    (delete-file temp-file))))))

(defun navi2ch-load-info (file)
  "FILE から lisp-object を読み込み、それを返す。"
  (setq file (expand-file-name file navi2ch-directory))	; 絶対パスにしておく
  (navi2ch-cache-get
   file
   (progn
     (let ((backup-file (navi2ch-make-backup-file-name file)))
       (when (and (file-exists-p backup-file)
		  (file-regular-p backup-file)
		  (or (not (file-exists-p file))
		      (not (file-regular-p file))
		      (file-newer-than-file-p backup-file file))
		  (yes-or-no-p
		   (format
		    "%s の読み込みで問題発生。バックアップファイルから読み込みますか? "
		    file)))
	 (setq file backup-file)))
     (when (file-regular-p file)
       (let ((coding-system-for-read navi2ch-coding-system))
	 (condition-case nil
	     (navi2ch-ifemacsce
		 (with-temp-buffer
		   (insert-file-contents file)
		   (goto-char (point-min))
		   (while (search-forward "..." nil t)
		     (replace-match ""))
		   (car (read-from-string (buffer-string))))
	       (with-temp-buffer
		 (insert-file-contents file)
		 (let ((standard-input (current-buffer)))
		   (read))))
	   (error
	    (when (yes-or-no-p 
		   (format "%s は正しく保存されていないようなのです。削除しますか? "
			   file))
	      (delete-file file))
	    (navi2ch-load-info file))))))
   navi2ch-info-cache))

(defun navi2ch-split-window (display)
  "window を分割する。
DISPLAY が `board' のときは board を表示する用に分割する。
DISPLAY が `article' のときは article を表示する用に分割する。"
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer)))))
    (cond (art-win
	   (select-window art-win)
	   (when (eq display 'board)
	     (navi2ch-article-exit)))
          (board-win
	   (select-window board-win)
	   (when (and (eq display 'article)
		      navi2ch-bm-stay-board-window)
	     (condition-case nil
		 (enlarge-window (frame-height))
	       (error nil))
	     (split-window-vertically navi2ch-board-window-height)
	     (other-window 1)))
          (list-win
           (select-window list-win)
	   (when navi2ch-list-stay-list-window
	     (split-window-horizontally navi2ch-list-window-width)
	     (other-window 1))))))

(defun navi2ch-goto-url (url &optional force)
  "URL からスレまたは板を選ぶ。"
  (interactive (list (navi2ch-read-string 
		      "URL: "
		      (when (eq (get-text-property (point) 'navi2ch-link-type)
				'url)
			(get-text-property (point) 'navi2ch-link))
		      current-prefix-arg)))
  (let ((article (navi2ch-article-url-to-article url))
	(board (navi2ch-board-url-to-board url)))
    (cond (article
	   (navi2ch-split-window 'article)
	   (navi2ch-article-view-article board
					 article
					 force
					 (cdr (assq 'number article))))
	  (board
	   (navi2ch-split-window 'board)
	   (navi2ch-list-select-board board force)))))

(defun navi2ch-find-file (file)
  "FILE からスレまたは板を選ぶ。"
  (interactive "fFind article file or board directory: ")
  (let ((article-p (file-regular-p file))
	(board-p (file-directory-p file)))
    (cond (article-p
	   (navi2ch-split-window 'article)
	   (navi2ch-article-view-article-from-file file))
	  (board-p
	   (navi2ch-split-window 'board)
	   (navi2ch-directory-find-directory file)))))

(defun navi2ch-2ch-url-p (url)
  "URL が 2ch 内の url であれば non-nil を返す。"
  (let ((host (navi2ch-url-to-host url)))
    (and host
	 (or (member host navi2ch-2ch-host-list)
	     (let (list)
	       (setq list
		     (mapcar (lambda (x)
			       (navi2ch-url-to-host (cdr (assq 'uri x))))
			     navi2ch-list-board-name-list))
	       (member host list)))
	 (or (navi2ch-article-url-to-article url)
	     (navi2ch-board-url-to-board url))
	 t)))

(defun navi2ch-change-log-directory (changed-list)
  "変更された板のログを保存するディレクトリを修正する。
CHANGED-LIST については `navi2ch-list-get-changed-status' を参照。"
  (dolist (node changed-list)
    (let ((old-dir (navi2ch-board-get-file-name (cadr node) ""))
	  (new-dir (navi2ch-board-get-file-name (caddr node) ""))
	  tmp-dir)
      (when (file-exists-p old-dir)
	(when (file-exists-p new-dir)
	  (catch 'loop
	    (while t
	      (setq tmp-dir (expand-file-name
			     (make-temp-name (concat "navi2ch-" (car node)))
			     (navi2ch-temp-directory)))
	      (unless (file-exists-p tmp-dir)
		(throw 'loop nil))))
	  (navi2ch-rename-file new-dir tmp-dir))
	(make-directory (expand-file-name ".." new-dir) t)
	(navi2ch-rename-file old-dir new-dir)))))

(defun navi2ch-update ()
  "navi2ch-update.el をダウンロードして実行する。"
  (interactive)
  (let* ((navi2ch-update-file (expand-file-name navi2ch-update-file
						navi2ch-directory))
	 (new (concat navi2ch-update-file ".new"))
	 (time (and (file-exists-p navi2ch-update-file)
		    (navi2ch-file-mtime navi2ch-update-file)))
	 (asc (concat navi2ch-update-file ".asc"))
	 (asc-url (concat navi2ch-update-url ".asc"))
	 verified)
    (when (and navi2ch-update-url
	       (not (string= navi2ch-update-url ""))
	       (not navi2ch-offline)
	       (navi2ch-net-update-file navi2ch-update-url new time)
	       (file-exists-p new)
	       (if navi2ch-pgp-verify-command-line
		   (and (navi2ch-net-update-file asc-url asc 'file)
			(file-exists-p asc)
			(navi2ch-verify-signature-file asc new)
			(setq verified t))
		 t)
	       (or (not (file-exists-p navi2ch-update-file))
		   (not (= (navi2ch-file-size navi2ch-update-file)
			   (navi2ch-file-size new)))
		   (not (string=
			 (with-temp-buffer
			   (insert-file-contents-literally navi2ch-update-file)
			   (buffer-string))
			 (with-temp-buffer
			   (insert-file-contents-literally new)
			   (buffer-string)))))
	       (yes-or-no-p
		(concat "navi2ch-update.el が更新されました"
			(unless verified " (未検証)")
			"。保存して実行しますか? ")))
      (navi2ch-rename-file new navi2ch-update-file t)
      (load navi2ch-update-file))
    (if (file-exists-p new)
	(delete-file new))))

(defun navi2ch-toggle-offline ()
  (interactive)
  (navi2ch-net-cleanup)
  (setq navi2ch-offline (not navi2ch-offline))
  (message (if navi2ch-offline
               "Now offline"
             "Now online"))
  (navi2ch-set-mode-line-identification))

(defun navi2ch-unload ()
  "Unload all navi2ch features."
  (interactive)
  (if (and (symbolp 'navi2ch-init)
	   navi2ch-init)
      (navi2ch-exit))
  (dolist (feature features)
    (if (or (save-match-data (string-match "\\`navi2ch-"
					   (symbol-name feature)))
	    (equal feature 'navi2ch))
	(unload-feature feature 'force))))

(defun navi2ch-lock ()
  "`navi2ch-directory' をロックする。"
  (if (and navi2ch-use-lock
	   (not (navi2ch-lock-directory navi2ch-directory navi2ch-lock-name))
	   (not (yes-or-no-p "ディレクトリのロックに失敗しました。危険を承知で続けますか? ")))
      (error "Lock failed")))

(defun navi2ch-unlock ()
  "`navi2ch-directory' のロックを解除する。"
  (if navi2ch-use-lock
      (navi2ch-unlock-directory navi2ch-directory navi2ch-lock-name)))

(defun navi2ch-ident-list ()
  "ロードしている Navi2ch の各モジュールの Id を表示する。"
  (interactive)
  (let (ident-list)
    (mapatoms (lambda (symbol)
		(if (and (boundp symbol)
			 (string-match "\\`navi2ch\\(-.+\\)?-ident\\'"
				       (symbol-name symbol)))
		    (setq ident-list (cons symbol ident-list)))))
    (when ident-list
      (setq ident-list (sort ident-list
			     (lambda (a b) (string< (symbol-name a)
						    (symbol-name b)))))
      (with-output-to-temp-buffer "*Navi2ch Ident List*"
	(princ (mapconcat 'symbol-value ident-list "\n"))))))


(eval-when-compile
  (autoload 'browse-url-interactive-arg "browse-url"))

;;;###autoload
(defun navi2ch-browse-url (url &rest args)
  "Navi2ch interface function for browse-url.el."
  (interactive
   (browse-url-interactive-arg "Navi2ch URL: "))
  (unless navi2ch-init
    (save-window-excursion
      (navi2ch)))
  (if (navi2ch-2ch-url-p url)
      (navi2ch-goto-url url)
    (message "Falling back...")
    (apply 'navi2ch-browse-url-internal url args)))

(defun navi2ch-url-at-point (point)
  "POINT の下のリンクを指す URL を得る。
\(defadvice browse-url-url-at-point
  (around my-browse-url-url-at-point-navi2ch activate compile)
  (let ((url (navi2ch-url-at-point (point))))
    (if url
	(setq ad-return-value url)
      ad-do-it)))
のようにすると、リンクに対して browse-url をインタラクティブに
実行できる。"
  (let ((alist `((navi2ch-list-mode . navi2ch-list-url-at-point)
		 (navi2ch-article-mode . navi2ch-article-url-at-point)
		 (navi2ch-popup-article-mode
		  . navi2ch-popup-article-url-at-point)
		 ,@(mapcar (lambda (mode)
			     (cons (intern (format "navi2ch-%s-mode"
						   (car mode)))
				   #'navi2ch-bm-url-at-point))
			   navi2ch-bm-board-type-alist))))
    (funcall (or (cdr (assq major-mode alist)) #'ignore) point)))

(defun navi2ch-show-url-at-point (point)
  "POINT の下のリンクを指す URL を表示し、kill-ring にコピーする。"
  (interactive "d")
  (let ((url (navi2ch-article-url-at-point point)))
    (when url
      (kill-new url)
      (message "%s" url))))

(eval-when-compile
  (mapatoms (lambda (symbol)
	      (if (and (fboundp symbol)
		       (string-match "\\`navi2ch-" (symbol-name symbol))
		       (eq (get symbol 'byte-optimizer)
			   'byte-compile-inline-expand))
		  (byte-compile symbol)))))

(run-hooks 'navi2ch-load-hook)
;;; navi2ch.el ends here
