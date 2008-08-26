;;; navi2ch-auto-modify.el --- auto file modification module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2005, 2006 by Navi2ch Project

;; Author: extra <ekisutora@users.sourceforge.net>
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
(provide 'navi2ch-auto-modify)
(defconst navi2ch-auto-modify-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch-vars)
(require 'navi2ch-util)

(defvar navi2ch-auto-modify-variable-list nil
  "設定を自動的に変更して保存する変数名のリスト。")

(add-hook 'navi2ch-exit-hook 'navi2ch-auto-modify-save)

(defun navi2ch-auto-modify-subr (body)
  (prog2 (setq navi2ch-auto-modify-variable-list nil)
      (eval (cons 'progn body))
    (let (added)
      (dolist (sexp body)
	(when (memq (car-safe sexp) '(setq setq-default))
	  (setq sexp (cdr sexp))
	  (while sexp
	    (unless (or (memq (car sexp) navi2ch-auto-modify-variable-list)
			(memq (car sexp) added))
	      (setq added (cons (car sexp) added)))
	    (setq sexp (cddr sexp)))))
      (when added
	(setq navi2ch-auto-modify-variable-list
	      (append navi2ch-auto-modify-variable-list
		      (nreverse added)))))))

(defmacro navi2ch-auto-modify (&rest body)
  "`navi2ch-auto-modify-file' で指定されたファイルに記述すると、
その中に含まれる変数の設定を自動的に変更して保存する。

例えば下記のように記述すると、
変数 `navi2ch-article-message-filter-by-id-alist' と
`navi2ch-article-message-filter-by-message-alist' の設定値は、
Navi2ch 終了時に自動的に変更・保存される。

\(navi2ch-auto-modify
  (setq navi2ch-article-message-filter-by-id-alist
	...)
  (setq navi2ch-article-message-filter-by-message-alist
	...))"
  `(navi2ch-auto-modify-subr ',body))

(put 'navi2ch-auto-modify 'lisp-indent-function 0)

(defun navi2ch-auto-modify-variables (variables)
  (let (added)
    (dolist (var variables)
      (unless (or (memq var navi2ch-auto-modify-variable-list)
		  (memq var added))
	(setq added (cons var added))))
    (when added
      (setq navi2ch-auto-modify-variable-list
	    (append navi2ch-auto-modify-variable-list (nreverse added)))))
  (navi2ch-auto-modify-save))

(defun navi2ch-auto-modify-save ()
  (run-hooks 'navi2ch-auto-modify-save-hook)
  (navi2ch-auto-modify-truncate-lists)
  (when navi2ch-auto-modify-variable-list
    (let ((navi2ch-auto-modify-file
	   (if (eq navi2ch-auto-modify-file t)
	       (locate-library (expand-file-name navi2ch-init-file
						 navi2ch-directory))
	     navi2ch-auto-modify-file)))
      (when navi2ch-auto-modify-file
	(let ((inhibit-read-only t)
	      (require-final-newline (eq require-final-newline t))
	      (value-buffer (current-buffer))
	      (exist-buffer (get-file-buffer navi2ch-auto-modify-file)))
	  (save-current-buffer
	    (let ((default-major-mode 'fundamental-mode))
	      (set-buffer (find-file-noselect navi2ch-auto-modify-file)))
	    (save-excursion
	      (save-restriction
		(widen)
		(navi2ch-auto-modify-narrow)
		(navi2ch-auto-modify-save-variables value-buffer)))
	    (unless exist-buffer
	      (basic-save-buffer)
	      (kill-buffer (current-buffer))))))
      (navi2ch-auto-modify-customize-variables))))

(defun navi2ch-auto-modify-skip-comments ()
  (while (and (not (eobp))
	      (forward-comment 1))))

(defun navi2ch-auto-modify-narrow ()
  (goto-char (point-min))
  (navi2ch-auto-modify-skip-comments)
  ;; Test for scan errors.
  (save-excursion
    (while (not (eobp))
      (forward-sexp)))
  (catch 'loop
    (let ((standard-input (current-buffer)))
      (while (not (eobp))
	(condition-case nil
	    (let ((beg (point))
		  (sexp (read)))
	      (when (consp sexp)
		(if (eq (car sexp) 'navi2ch-auto-modify)
		    (progn
		      (narrow-to-region beg (point))
		      (throw 'loop nil))
		  (when (re-search-backward "\\<navi2ch-auto-modify\\>"
					    (1+ beg) t)
		    (goto-char (1+ beg))))))
	  (invalid-read-syntax nil))
	(navi2ch-auto-modify-skip-comments)))
    (unless (bobp)
      (skip-chars-backward "\n" (1- (point)))
      (let ((count (save-excursion (skip-chars-backward "\n"))))
	(when (> count -2)
	  (insert-char ?\n (+ count 2))))
      (narrow-to-region (point) (point)))
    (insert "(navi2ch-auto-modify)")))

(defun navi2ch-auto-modify-save-variables (&optional buffer)
  (goto-char (1+ (point-min)))		; "\\`("
  (forward-sexp)			; "navi2ch-auto-modify"
  (navi2ch-auto-modify-skip-comments)
  (let ((standard-input (current-buffer))
	(standard-output (current-buffer))
	(print-length nil)
	(print-level nil)
	modified)
    (condition-case nil
	(while (not (eobp))
	  (let ((beg (point))
		(sexp (read)))
	    (when (memq (car-safe sexp) '(setq setq-default))
	      (save-excursion
		(goto-char (1+ beg))	; "("
		(forward-sexp)		; "setq\\(-default\\)?"
		(navi2ch-auto-modify-skip-comments)
		(condition-case nil
		    (while (not (eobp))
		      (let ((var (read))
			    start end)
			(navi2ch-auto-modify-skip-comments)
			(setq start (point))
			(forward-sexp)
			(delete-region start (point))
			(pp (navi2ch-quote-maybe
			     (if (and buffer
				      (local-variable-p var buffer))
				 (with-current-buffer buffer
				   (symbol-value var))
			       (symbol-value var))))
			(setq end (point-marker))
			(goto-char start)
			(indent-sexp)
			(forward-sexp)
			(delete-region (point) end)
			(unless (memq var modified)
			  (setq modified (cons var modified))))
		      (navi2ch-auto-modify-skip-comments))
		  (invalid-read-syntax nil)))))	; ")"
	  (navi2ch-auto-modify-skip-comments))
      (invalid-read-syntax nil))	; ")\\'"
    (backward-char)
    (dolist (var navi2ch-auto-modify-variable-list)
      (unless (memq var modified)
	(unless (navi2ch-auto-modify-customize-variable-p var)
	  (insert ?\n)
	  (lisp-indent-line)
	  (let ((start (point))
		end)
	    (pp (list (if (local-variable-if-set-p var (current-buffer))
			  'setq-default
			'setq)
		      var
		      (navi2ch-quote-maybe
		       (if (and buffer
				(local-variable-p var buffer))
			   (with-current-buffer buffer
			     (symbol-value var))
			 (symbol-value var)))))
	    (setq end (point-marker))
	    (goto-char start)
	    (indent-sexp)
	    (forward-sexp)
	    (delete-region (point) end)))
	(setq modified (cons var modified))))
    (setq navi2ch-auto-modify-variable-list (nreverse modified))))

(defun navi2ch-auto-modify-customize-variable-p (variable)
  (or (null navi2ch-auto-modify-file)
      (get variable 'saved-value)	; From `customize-saved'
      (get variable 'saved-variable-comment))) ; For XEmacs

(defun navi2ch-auto-modify-customize-variables ()
  (let (customized)
    (dolist (var navi2ch-auto-modify-variable-list)
      (when (navi2ch-auto-modify-customize-variable-p var)
	(customize-set-variable var (symbol-value var))
	(setq customized t)))
    (when customized
      (customize-save-customized))))

(defun navi2ch-auto-modify-truncate-lists ()
  (when navi2ch-auto-modify-truncate-list-alist
    (let (added)
      (dolist (slot navi2ch-auto-modify-truncate-list-alist)
	(when (> (length (symbol-value (car slot))) (cdr slot))
	  (if (zerop (cdr slot))
	      (set (car slot) nil)
	    (setcdr (nthcdr (1- (cdr slot)) (symbol-value (car slot))) nil))
	  (unless (or (memq (car slot) navi2ch-auto-modify-variable-list)
		      (memq (car slot) added))
	    (setq added (cons (car slot) added)))))
      (when added
	(setq navi2ch-auto-modify-variable-list
	      (append navi2ch-auto-modify-variable-list (nreverse added)))))))

;;; navi2ch-auto-modify.el ends here
