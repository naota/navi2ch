;;; navi2ch-js-livedoor-move.el --- Supports moving JBBS-shitaraba.

;; Copyright (C) 2004 by Navi2ch Project

;; Author: mami <mami@users.sourceforge.net>
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

;; jbbs.livedoor.com から jbbs.livedoor.jp への移転のサポート。

;;; Code:

(provide 'navi2ch-js-livedoor-move)

(eval-when-compile (require 'cl))
(require 'navi2ch)

(defun navi2ch-js-livedoor-move ()
  (interactive)
  (let* ((old-host "jbbs.livedoor.com")
	 (new-host "jbbs.livedoor.jp")
	 (etc-category (navi2ch-list-get-etc-category))
	 changed-list)

    ;; ログ移動
    (dolist (board (cdr (assq 'child etc-category)))
      (let ((uri (cdr (assq 'uri board))))
	(when (string-match (format "^http://%s/"
				    (regexp-quote old-host))
			    uri)
	  (let ((new-board (copy-tree board)))
	    (setcdr (assq 'uri new-board)
		    (navi2ch-replace-string (format "^http://%s/"
						    (regexp-quote old-host))
					    (format "http://%s/" new-host)
					    uri))
	    (push (list (cdr (assq 'id board))
			board
			new-board)
		  changed-list)))))
    (navi2ch-list-apply-changed-status (list (cons 'add nil)
					     (cons 'change changed-list)))

    ;; etc.txt を変更
    (save-current-buffer
      (let ((default-major-mode 'fundamental-mode))
	(set-buffer (find-file-noselect
		     (navi2ch-list-get-file-name navi2ch-list-etc-file-name))))
      (while (re-search-forward "\\(.+\\)\n\\(.+\\)\n\\(.+\\)" nil t)
	(let ((name (match-string 1))
	      (uri (match-string 2))
	      (id (match-string 3)))
	  (dolist (item changed-list)
	    (when (and (string= id (car item))
		       (string= name (cdr (assq 'name (cadr item))))
		       (string= uri (cdr (assq 'uri (cadr item)))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert (cdr (assq 'name (caddr item))) "\n"
		      (cdr (assq 'uri (caddr item))) "\n"
		      (cdr (assq 'id (caddr item))))
	      (return)))))
      (save-buffer))
    (navi2ch-list)
    (navi2ch-list-sync t)))

;;; navi2ch-js-livedoor-move.el ends here
