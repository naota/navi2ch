;;; batch-texi2info.el --- run texi2info with emacs -batch

;; Copyright (C) 2002, 2008 by Navi2ch Project
;; Copyright (C) 1985, 1986, 1988, 1990, 1991, 1992, 1993,
;;               1994, 1995, 1996, 1997, 1998, 2000, 2001
;;    Free Software Foundation, Inc.
;; Copyright (C) 1999 Yoshiki Hayashi <yoshiki@xemacs.org>
;; Copyright (C) 2000, 2001, 2002 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: 名無しさん＠お腹いっぱい
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

;;; Code:

(require 'texinfmt)

(set-language-environment "Japanese")

;; workaround for emacs20
(unless (symbol-plist 'ifnottex)
  (put 'ifnottex 'texinfo-format 'texinfo-discard-line)
  (put 'ifnottex 'texinfo-end 'texinfo-discard-command))

;; From ptexinfmt.el
;; @copying ... @end copying
;; that Emacs 21.4 and lesser and XEmacs don't support.
(if (fboundp 'texinfo-copying)
    nil
  (defvar texinfo-copying-text ""
    "Text of the copyright notice and copying permissions.")

  (defun texinfo-copying ()
    "Copy the copyright notice and copying permissions from the Texinfo file,
as indicated by the @copying ... @end copying command;
insert the text with the @insertcopying command."
    (let ((beg (progn (beginning-of-line) (point)))
	  (end  (progn (re-search-forward "^@end copying[ \t]*\n") (point))))
      (setq texinfo-copying-text
	    (buffer-substring-no-properties
	     (save-excursion (goto-char beg) (forward-line 1) (point))
	     (save-excursion (goto-char end) (forward-line -1) (point))))
      (delete-region beg end)))

  (defun texinfo-insertcopying ()
    "Insert the copyright notice and copying permissions from the Texinfo file,
which are indicated by the @copying ... @end copying command."
    (insert (concat "\n" texinfo-copying-text)))

  (defadvice texinfo-format-scan (before expand-@copying-section activate)
    "Extract @copying and replace @insertcopying with it."
    (goto-char (point-min))
    (when (search-forward "@copying" nil t)
      (texinfo-copying))
    (while (search-forward "@insertcopying" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (texinfo-insertcopying))))

;; batch-texinfo-format からほぼコピペ
(defun batch-texi2info ()
  "Runs  texi2info  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texi2nfo $docs/ ~/*.texinfo\"."
  (if (not noninteractive)
      (error "batch-tex2info may only be used -batch"))
  (let ((version-control t)
        (auto-save-default nil)
        (find-file-run-dired nil)
        (kept-old-versions 259259)
        (kept-new-versions 259259)
	(coding-system-for-read 'undecided)
	(coding-system-for-write 'iso-2022-7bit-unix))
    (let ((error 0)
          file
          (files ()))
      (while command-line-args-left
        (setq file (expand-file-name (car command-line-args-left)))
        (cond ((not (file-exists-p file))
               (message ">> %s does not exist!" file)
               (setq error 1
                     command-line-args-left (cdr command-line-args-left)))
              ((file-directory-p file)
               (setq command-line-args-left
                     (nconc (directory-files file)
                            (cdr command-line-args-left))))
              (t
               (setq files (cons file files)
                     command-line-args-left (cdr command-line-args-left)))))
      (while files
        (setq file (car files)
              files (cdr files))
        (condition-case err
            (progn
              (if buffer-file-name (kill-buffer (current-buffer)))
              (find-file file)
              (buffer-disable-undo (current-buffer))
              (set-buffer-modified-p nil)
              (texinfo-mode)
	      (texinfo-every-node-update)
              (message "texinfo formatting %s..." file)
              (texi2info nil)
              (if (buffer-modified-p)
                  (progn (message "Saving modified %s" (buffer-file-name))
                         (save-buffer))))
          (error
           (message ">> Error: %s" (prin1-to-string err))
           (message ">>  point at")
           (let ((s (buffer-substring (point)
                                      (min (+ (point) 100)
                                           (point-max))))
                 (tem 0))
             (while (setq tem (string-match "\n+" s tem))
               (setq s (concat (substring s 0 (match-beginning 0))
                               "\n>>  "
                               (substring s (match-end 0)))
                     tem (1+ tem)))
             (message ">>  %s" s))
           (setq error 1))))
      (kill-emacs error))))
