;;; navi2ch-be2ch.el --- View be@2ch module for Navi2ch.

;; Copyright (C) 2005 by Navi2ch Project

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
(provide 'navi2ch-be2ch)

(eval-when-compile (require 'cl))
(require 'navi2ch-net)
(require 'navi2ch-util)

(defconst navi2ch-be2ch-ident
  "$Id$")

(defvar navi2ch-be2ch-login-url "http://be.2ch.net/test/login.php")

(defvar navi2ch-be2ch-mail-address nil)
(defvar navi2ch-be2ch-password nil)
(defvar navi2ch-be2ch-mdmd nil)
(defvar navi2ch-be2ch-dmdm nil)

(defun navi2ch-be2ch-login-p ()
  (and navi2ch-be2ch-mdmd
       navi2ch-be2ch-dmdm))

(defun navi2ch-be2ch-login (mail password)
  (interactive
   (list
    (or navi2ch-be2ch-mail-address
	(read-string "mail address: "))
    (or navi2ch-be2ch-password
	(read-passwd "password: "))))
  (navi2ch-be2ch-logout)
  (let ((proc (navi2ch-net-send-request
	       navi2ch-be2ch-login-url
	       "POST"
	       (list
		(cons "Referer" navi2ch-be2ch-login-url)
		(cons "User-Agent" navi2ch-net-user-agent)
		(cons "Content-Type" "application/x-www-form-urlencoded"))
	       (navi2ch-net-get-param-string 
		(list 
		 (cons "m" mail)
		 (cons "p" password)
		 (cons "submit" "登録"))))))
    (with-temp-buffer
      (navi2ch-set-buffer-multibyte nil)
      (insert (navi2ch-net-get-content proc))
      (goto-char (point-min))
      ;; document.cookie = 'MDMD=... ; expires=Thu,05-Jan-30 00:00:01 GMT; domain=2ch.net; path=/;';
      ;; document.cookie = 'DMDM=' + '...' + ' ; expires=Thu,05-Jan-30 00:00:01 GMT;path=/; domain=2ch.net;';
      (while (re-search-forward "document.cookie *= *\\([^;]+\\);" nil t)
	(let ((cookie (match-string 1)))
	  (setq cookie (split-string (navi2ch-replace-string "[' +]" "" cookie t) "="))
	  (cond ((string= (car cookie) "MDMD")
		 (setq navi2ch-be2ch-mdmd (car (cdr cookie))))
		((string= (car cookie) "DMDM")
		 (setq navi2ch-be2ch-dmdm (car (cdr cookie)))))))
      (when (navi2ch-be2ch-login-p)
	(message "Be@2ch にログインしました。")))))

(defun navi2ch-be2ch-logout ()
  (interactive)
  (setq navi2ch-be2ch-mdmd nil)
  (setq navi2ch-be2ch-dmdm nil)
  (message "Be@2ch からログアウトしました。"))

;;; navi2ch-be2ch.el ends here
