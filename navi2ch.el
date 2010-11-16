;;; navi2ch.el --- Navigator for 2ch for Emacsen -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008, 2009
;; by Navi2ch Project

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
(require 'navi2ch-p2)
(require 'navi2ch-thumbnail)

(defgroup navi2ch nil
  "Navigator for 2ch."
  :group 'hypermedia)

(defvar navi2ch-ask-when-exit t)

(defvar navi2ch-init nil)

;; hook $BMQ4X?t!#(B
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
    (setq navi2ch-init t)
    (when navi2ch-mona-enable
      (navi2ch-mona-setup))
    (navi2ch-be2ch-login-p))
  (navi2ch-list)
  (unless navi2ch-init
    (run-hooks 'navi2ch-after-startup-hook)))

(defun navi2ch-version ()
  (interactive)
  (message "Navigator for 2ch %s" navi2ch-version))

(defun navi2ch-save-status ()
  "list, board, article $B$N>uBV$rJ]B8$9$k!#(B"
  (interactive)
  (message "Save status...")
  (run-hooks 'navi2ch-save-status-hook)
  (message "Save status...done"))

(defun navi2ch-exit (&optional suspend)
  "navi2ch $B$r=*N;$9$k!#(B
SUSPEND $B$,(B non-nil $B$J$i(B buffer $B$r>C$5$J$$!#(B"
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
  "navi2ch $B$r0l;~E*$K=*N;$9$k!#(B"
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
  "$B2hLL$r:81&$KJ,3d$7$F(B BUF-LEFT$B!"(BBUF-RIGHT $B$r3d$jEv$F$k!#(B
\(win-left win-right) $B$N%j%9%H$rJV$9(B"
  (delete-other-windows)
  (let ((win-left (selected-window))
	(win-right (split-window-horizontally navi2ch-list-window-width)))
    (set-window-buffer win-left buf-left)
    (set-window-buffer win-right buf-right)
    (list win-left win-right)))

(defun navi2ch-two-pane-vertically (buf-top buf-bottom)
  "$B2hLL$r>e2<$KJ,3d$7$F(B BUF-TOP$B!"(BBUF-BOTTOM $B$r3d$jEv$F$k!#(B
\(win-top win-bottom) $B$N%j%9%H$rJV$9(B"
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
  "FILE $B$G;XDj$5$l$k%U%!%$%k$+$i%P%C%/%"%C%W%U%!%$%k$NL>A0$rJV$9!#(B"
  ;; $B$H$j$"$($:$O!"(BOS $B$4$H$N%P%C%/%"%C%WL>$N0c$$$O(B Emacs $B$K$^$+$;$k!#(B
  ;; $B8e!9JQ$($?$/$J$C$?;~$KJQ99$7K:$l$k$N$rKI$0$?$a!#(B
  (make-backup-file-name file))

;; make-temp-file $B$NJ}$,0BA4$@$1$I!"B8:_$7$J$$4D6-$G$O(B make-temp-name $B$r;H$&!#(B
(defsubst navi2ch-make-temp-file (file)
  "$B%F%s%]%i%j%U%!%$%k$r:n$k!#(B"
  (funcall (if (fboundp 'make-temp-file)
	       'make-temp-file
	     'make-temp-name) file))

(defvar navi2ch-info-cache nil)
(defvar navi2ch-info-cache-limit 1000)

(defun navi2ch-save-info (file info &optional backup)
  "lisp-object INFO $B$r(B FILE $B$KJ]B8$9$k!#(B
BACKUP $B$,(B non-nil $B$N>l9g$O85$N%U%!%$%k$r%P%C%/%"%C%W$9$k!#(B"
  (setq file (expand-file-name file navi2ch-directory))	; $B@dBP%Q%9$K$7$F$*$/(B
  ;; FIXME:$B$H$j$"$($:!"A4$FJ]B8$9$k$h$&$K$7$F$"$k(B
  ;; $B$G$-$l$PFbMF$,JQ$o$i$J$$;~$K$OJ]B8$7$?$/$J$$(B
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
	    ;; $B%U%!%$%k$,3N<B$K>C$($k$h$&!"2<$N(B setq $B$O>e$N(B let $B$K0\F0(B
	    ;; $B$7$F$O%@%a(B
	    (setq temp-file (navi2ch-make-temp-file
			     (file-name-directory file)))
	    (with-temp-file temp-file
	      (let ((standard-output (current-buffer))
		    print-length print-level)
		(princ ";;; -*- mode: emacs-lisp; -*-\n")
		(prin1 info)))
	    (if (and backup (file-exists-p file))
		(rename-file file backup-file t))
	    ;; $B>e$N(B rename $B$,@.8y$7$F2<$,<:GT$7$F$b!"(B navi2ch-load-info
	    ;; $B$,%P%C%/%"%C%W%U%!%$%k$+$iFI$s$G$/$l$k!#(B
	    (condition-case err
		(progn
		  (unless (file-exists-p file)
		    (with-temp-file file)) ; $B%U%!%$%k$,:n@.2DG=$+%A%'%C%/(B
		  (rename-file temp-file file t))
	      (file-error
	       (message "%s: %s"
			(cadr err)
			(mapconcat #'identity (cddr err) ", ")))))
	(if (and temp-file (file-exists-p temp-file))
	    (delete-file temp-file))))))

(defun navi2ch-load-info (file)
  "FILE $B$+$i(B lisp-object $B$rFI$_9~$_!"$=$l$rJV$9!#(B"
  (setq file (expand-file-name file navi2ch-directory))	; $B@dBP%Q%9$K$7$F$*$/(B
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
		    "%s $B$NFI$_9~$_$GLdBjH/@8!#%P%C%/%"%C%W%U%!%$%k$+$iFI$_9~$_$^$9$+(B? "
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
		   (format "%s $B$O@5$7$/J]B8$5$l$F$$$J$$$h$&$J$N$G$9!#:o=|$7$^$9$+(B? "
			   file))
	      (delete-file file))
	    (navi2ch-load-info file))))))
   navi2ch-info-cache))

(defun navi2ch-split-window (display)
  "window $B$rJ,3d$9$k!#(B
DISPLAY $B$,(B `board' $B$N$H$-$O(B board $B$rI=<($9$kMQ$KJ,3d$9$k!#(B
DISPLAY $B$,(B `article' $B$N$H$-$O(B article $B$rI=<($9$kMQ$KJ,3d$9$k!#(B"
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
  "URL $B$+$i%9%l$^$?$OHD$rA*$V!#(B"
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
  "FILE $B$+$i%9%l$^$?$OHD$rA*$V!#(B"
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
  "URL $B$,(B 2ch $BFb$N(B url $B$G$"$l$P(B non-nil $B$rJV$9!#(B"
  (let ((host (navi2ch-url-to-host url)))
    (and host
	 (not (string-match navi2ch-list-invalid-host-regexp host))
	 (or (member host navi2ch-2ch-host-list)
	     ;; (string-match "^[a-z]+[0-9]*\\.2ch\\.net$" host)
	     (string-match navi2ch-list-valid-host-regexp host)
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
  "$BJQ99$5$l$?HD$N%m%0$rJ]B8$9$k%G%#%l%/%H%j$r=$@5$9$k!#(B
CHANGED-LIST $B$K$D$$$F$O(B `navi2ch-list-get-changed-status' $B$r;2>H!#(B"
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
  "navi2ch-update.el $B$r%@%&%s%m!<%I$7$F<B9T$9$k!#(B"
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
		(concat "navi2ch-update.el $B$,99?7$5$l$^$7$?(B"
			(unless verified " ($BL$8!>Z(B)")
			"$B!#J]B8$7$F<B9T$7$^$9$+(B? ")))
      (navi2ch-rename-file new navi2ch-update-file t)
      (load navi2ch-update-file))
    (if (file-exists-p new)
	(delete-file new))))

(defun navi2ch-toggle-offline ()
  (interactive)
  (navi2ch-net-cleanup)
  (setq navi2ch-offline (not navi2ch-offline))
  (when (and (eq last-command 'navi2ch-toggle-offline)
	     (not navi2ch-offline))
    (setq navi2ch-net-down-host-alist nil))
  (message (if navi2ch-offline
               "Now offline"
	     (if (eq last-command 'navi2ch-toggle-offline)
		 "Now online (reset down host)"
	       "Now online")))
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
  "`navi2ch-directory' $B$r%m%C%/$9$k!#(B"
  (if (and navi2ch-use-lock
	   (not (navi2ch-lock-directory navi2ch-directory navi2ch-lock-name))
	   (not (yes-or-no-p "$B%G%#%l%/%H%j$N%m%C%/$K<:GT$7$^$7$?!#4m81$r>5CN$GB3$1$^$9$+(B? ")))
      (error "Lock failed")))

(defun navi2ch-unlock ()
  "`navi2ch-directory' $B$N%m%C%/$r2r=|$9$k!#(B"
  (if navi2ch-use-lock
      (navi2ch-unlock-directory navi2ch-directory navi2ch-lock-name)))

(defun navi2ch-ident-list ()
  "$B%m!<%I$7$F$$$k(B Navi2ch $B$N3F%b%8%e!<%k$N(B Id $B$rI=<($9$k!#(B"
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
  "POINT $B$N2<$N%j%s%/$r;X$9(B URL $B$rF@$k!#(B
\(defadvice browse-url-url-at-point
  (around my-browse-url-url-at-point-navi2ch activate compile)
  (let ((url (navi2ch-url-at-point (point))))
    (if url
	(setq ad-return-value url)
      ad-do-it)))
$B$N$h$&$K$9$k$H!"%j%s%/$KBP$7$F(B browse-url $B$r%$%s%?%i%/%F%#%V$K(B
$B<B9T$G$-$k!#(B"
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
  "POINT $B$N2<$N%j%s%/$r;X$9(B URL $B$rI=<($7!"(Bkill-ring $B$K%3%T!<$9$k!#(B"
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
