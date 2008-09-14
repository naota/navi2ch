;;; navi2ch-be2ch.el --- View be@2ch module for Navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2005, 2008 by Navi2ch Project

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
(defconst navi2ch-be2ch-coding-system 'euc-jp)
(defconst navi2ch-be2ch-cookie-names '("MDMD" "DMDM"))
(defconst navi2ch-be2ch-cookie-domain '2ch.net)
(defconst navi2ch-be2ch-cookie-path '/)

(defvar navi2ch-be2ch-login-url "http://be.2ch.net/test/login.php")

(defvar navi2ch-be2ch-mail-address nil)
(defvar navi2ch-be2ch-password nil)

(defvar navi2ch-be2ch-login-flag nil)

(defun navi2ch-be2ch-login-p ()
  (let ((cookies (navi2ch-net-match-cookies navi2ch-be2ch-login-url)))
    (setq navi2ch-be2ch-login-flag
	  (null (memq nil
		      (mapcar (lambda (name) (assoc name cookies))
			      navi2ch-be2ch-cookie-names))))))

(defun navi2ch-be2ch-login (mail password)
  (interactive
   (list
    (or navi2ch-be2ch-mail-address
	(read-string "mail address: "))
    (or navi2ch-be2ch-password
	(read-passwd "password: "))))
  (navi2ch-be2ch-logout t)
  (let ((proc (navi2ch-net-send-request
	       navi2ch-be2ch-login-url
	       "POST"
	       (list
		(cons "Referer" navi2ch-be2ch-login-url)
		(cons "Content-Type" "application/x-www-form-urlencoded"))
	       (navi2ch-net-get-param-string 
		(list 
		 (cons "m" mail)
		 (cons "p" password)
		 (cons "submit" "登録"))
		navi2ch-be2ch-coding-system))))
    (navi2ch-net-update-cookies navi2ch-be2ch-login-url
				proc
				navi2ch-be2ch-coding-system)
    (navi2ch-net-save-cookies)
    (when (navi2ch-be2ch-login-p)
      (message "Be@2ch にログインしました。"))))

(defun navi2ch-be2ch-logout (&optional no-msg)
  (interactive)
  (dolist (name navi2ch-be2ch-cookie-names)
    (navi2ch-net-store-cookie (list name "" 0 0)
			      navi2ch-be2ch-cookie-domain
			      navi2ch-be2ch-cookie-path))
  (navi2ch-net-save-cookies)
  (setq navi2ch-be2ch-login-flag nil)
  (unless no-msg
    (message "Be@2ch からログアウトしました。")))

(defun navi2ch-be2ch-toggle-login ()
  "Be@2ch へのログイン状態を切り替える。"
  (interactive)
  (if navi2ch-be2ch-login-flag
      (navi2ch-be2ch-logout)
    (call-interactively 'navi2ch-be2ch-login)))

;;; navi2ch-be2ch.el ends here
