;;; navi2ch.el --- Navigator for 2ch for Emacsen

;; Copyright (C) 2000 by Navi2ch Project

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
(provide 'navi2ch)

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
(and navi2ch-on-emacs21
     (require 'navi2ch-e21))
(require 'navi2ch-splash)
(require 'navi2ch-version)
(require 'navi2ch-jbbs-net)
(require 'navi2ch-jbbs-shitaraba)
(require 'navi2ch-machibbs)
(require 'navi2ch-multibbs)

(defgroup navi2ch nil
  "Navigator for 2ch."
  :group 'hypermedia)

(defvar navi2ch-ask-when-exit t)

(defvar navi2ch-init nil)

;; hook 用関数。
(defun navi2ch-kill-emacs-hook ()
  (run-hooks 'navi2ch-kill-emacs-hook)
  (navi2ch-save-status)
  (if navi2ch-use-lock
      (navi2ch-unlock)))

;;;###autoload
(defun navi2ch (&optional arg)
  "Navigator for 2ch for Emacs"
  (interactive "P")
  (run-hooks 'navi2ch-before-startup-hook)
  (unless navi2ch-init
    (if arg (setq navi2ch-offline (not navi2ch-offline)))
    (when (file-exists-p navi2ch-update-file)
      (load-file navi2ch-update-file))
    (load navi2ch-init-file t)
    (if navi2ch-use-lock
	(navi2ch-lock))
    (if navi2ch-auto-update
	(navi2ch-update))
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
    (require 'navi2ch-mona))
  (navi2ch-list)
  (run-hooks 'navi2ch-after-startup-hook))

(defun navi2ch-version ()
  (interactive)
  (message "Navigator for 2ch %s" navi2ch-version))

(defun navi2ch-save-status ()
  "list, board, article の状態を保存する"
  (interactive)
  (message "save status...")
  (run-hooks 'navi2ch-save-status-hook)
  (message "save status...done"))

(defun navi2ch-exit (&optional suspend)
  "navi2ch を終了する
SUSPEND が non-nil なら buffer を消さない"
  (interactive)
  (when (or suspend
            (not navi2ch-ask-when-exit)
            (if (functionp navi2ch-ask-when-exit)
		(funcall navi2ch-ask-when-exit "really exit navi2ch?")
	      (y-or-n-p "really exit navi2ch?")))
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
      (if navi2ch-use-lock
	  (navi2ch-unlock))
      (remove-hook 'kill-emacs-hook 'navi2ch-kill-emacs-hook))))

(defun navi2ch-suspend ()
  "navi2ch を一時的に終了する"
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
	 (list-win (get-buffer-window (or list-buf "")))
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

(defun navi2ch-make-backup-file-name (file)
  "FILE で指定されるファイルからバックアップファイルの名前を返す。"
  ;; とりあえずは、OS ごとのバックアップ名の違いは Emacs にまかせる。
  ;; 後々変えたくなった時に変更し忘れるのを防ぐため。
  (make-backup-file-name file))

;; make-temp-file の方が安全だけど、存在しない環境では make-temp-name を使う。
(defun navi2ch-make-temp-file (file)
  "テンポラリファイルを作る。"
  (funcall (if (fboundp 'make-temp-file)
	       'make-temp-file
	     'make-temp-name) file))

(defun navi2ch-save-info (file info &optional backup)
  "lisp-object INFO を FILE に保存する。
BACKUP が non-nil の場合は元のファイルをバックアップする。"
  (setq info (navi2ch-strip-properties info)
	file (expand-file-name file))	; 絶対パスにしておく
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
	      (let ((standard-output (current-buffer)))
		(prin1 info)))
	    (if (and backup (file-exists-p file))
		(rename-file file backup-file t))
	    ;; 上の rename が成功して下が失敗しても、navi2ch-load-info
	    ;; がバックアップファイルから読んでくれる。
	    (rename-file temp-file file t))
	(if (and temp-file (file-exists-p temp-file))
	    (delete-file temp-file))))))

(defun navi2ch-load-info (file)
  "FILE から lisp-object を読み込み、それを返す。"
  (setq file (expand-file-name file))	; 絶対パスにしておく
  (let ((backup-file (navi2ch-make-backup-file-name file)))
    (when (and (file-exists-p backup-file)
	       (file-regular-p backup-file)
	       (or (not (file-exists-p file))
		   (not (file-regular-p file))
		   (file-newer-than-file-p backup-file file))
	       (yes-or-no-p
		"問題発生。バックアップファイルから読み込みますか? "))
      (setq file backup-file)))
  (when (file-regular-p file)
    (let ((coding-system-for-read navi2ch-coding-system))
      (with-temp-buffer
	(insert-file-contents file)
	(let ((standard-input (current-buffer)))
	  (read))))))

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
  "URL からスレまたは板を選ぶ"
  (interactive "sURL: ")
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer))))
	(article (navi2ch-article-url-to-article url))
	(board (navi2ch-board-url-to-board url)))
    (cond (article
	   (navi2ch-split-window 'article)
	   (navi2ch-article-view-article board
					 article
					 force
					 (cdr (assq 'number article))))
	  (board
	   (navi2ch-split-window 'board)
	   (navi2ch-board-select-board board force)))))

(defun navi2ch-find-file (file)
  "FILE からスレまたは板を選ぶ"
  (interactive "fFind article file or board directory: ")
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer))))
	(article-p (file-regular-p file))
	(board-p (file-directory-p file)))
    (cond (article-p
	   (navi2ch-split-window 'article)
	   (navi2ch-article-view-article-from-file file))
	  (board-p
	   (navi2ch-split-window 'board)
	   (navi2ch-directory-find-directory file)))))

(defun navi2ch-2ch-url-p (url)
  "URL が 2ch 内の url かを返す。"
  (let ((host (navi2ch-url-to-host url)))
    (or (member host navi2ch-2ch-host-list)
	(let (list)
	  (setq list
		(mapcar
		 (lambda (x)
		   (navi2ch-url-to-host (cdr (assq 'uri x))))
		 (navi2ch-list-get-board-name-list
		  navi2ch-list-category-list)))
	  (member host list)))))

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
  (let ((new (concat navi2ch-update-file ".new")))
    (when (and navi2ch-update-url
	       (not (string= navi2ch-update-url ""))
	       (not navi2ch-offline)
	       (navi2ch-net-update-file navi2ch-update-url new)
	       (file-exists-p new)
	       (or (not (file-exists-p navi2ch-update-file))
		   (not (= (nth 7 (file-attributes navi2ch-update-file))
			   (nth 7 (file-attributes new))))
		   (not (string=
			 (with-temp-buffer
			   (insert-file-contents-literally navi2ch-update-file)
			   (buffer-string))
			 (with-temp-buffer
			   (insert-file-contents-literally new)
			   (buffer-string)))))
	       (yes-or-no-p
		"navi2ch-update.elが更新されました。保存して実行しますか? "))
      (navi2ch-rename-file new navi2ch-update-file t)
      (load navi2ch-update-file))
    (if (file-exists-p new)
	(delete-file new))))

(defun navi2ch-toggle-offline ()
  (interactive)
  (navi2ch-net-cleanup)
  (setq navi2ch-offline (not navi2ch-offline))
  (message (if navi2ch-offline
               "offline"
             "online"))
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

;;; ロック
;; 最も汎用的な mkdir ロックを実装してみた。
;; ~/.navi2ch/lockdir というディレクトリがある場合はそのディレクトリは
;; ロックされているということになる。
;; シェルスクリプトで同じ手法を使い、cron で wget でも動かせば、
;; ~/.navi2ch/ 以下を常に新鮮に保てるかも。(w

(defun navi2ch-lock ()
  "`navi2ch-directory' をロックする。"
  (let* ((lockdir (navi2ch-chop-/ (expand-file-name navi2ch-lock-directory)))
	 (basedir (file-name-directory navi2ch-lock-directory))
	 ;; make-directory-internal は mkdir(2) を呼び出すので、アトミッ
	 ;; クなロックが期待できる。
	 (make-directory-function (if (fboundp 'make-directory-internal)
				      'make-directory-internal
				    'make-directory))
	 (redo t)
	 error-message)
    ;; まず、下でエラーが起きないよう、親ディレクトリを作っておく
    (unless (file-exists-p basedir)
      (ignore-errors
	(make-directory basedir t)))
    (while redo
      (setq redo nil)
      (if (file-exists-p lockdir)	; lockdir がすでにあると失敗
	  (setq error-message "ロックディレクトリがすでにあります。")
	;; file-name-handler-alist があると mkdir が直接呼ばれな
	;; い可能性がある。
	(condition-case error
	    (let ((file-name-handler-alist nil))
	      (funcall make-directory-function lockdir))
	  (error
	   (message "%s" (error-message-string error))
	   (sit-for 3)
	   (discard-input)
	   (setq error-message "ロックディレクトリの作成に失敗しました。"))))
      (unless (file-exists-p lockdir)	; 念のため、確認しておく
	(setq error-message "ロックディレクトリを作成できませんでした。"))
      (if (and error-message
	       (y-or-n-p (format "%sもう一度試しますか? "
				 error-message)))
	  (setq redo t)))
    (if (and error-message
	     (not (yes-or-no-p (format "%s危険を承知で続けますか? "
				       error-message))))
	(error "lock failed: %s" lockdir))))

(defun navi2ch-unlock ()
  "`navi2ch-directory' のロックを解除する。"
  (ignore-errors
    (delete-directory navi2ch-lock-directory)))

(run-hooks 'navi2ch-load-hook)
;;; navi2ch.el ends here
