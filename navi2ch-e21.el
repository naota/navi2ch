;;; navi2ch-e21.el --- GNU Emacs 21 module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2004 by Navi2ch Project
;; Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>
;; Copyright (C) 2000,2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: UEYAMA Rui <rui314159@users.sourceforge.net>
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
(provide 'navi2ch-e21)
(defconst navi2ch-e21-ident
  "$Id$")
(require 'navi2ch)

;;; 以下、Wanderlust (wl-e21.el) からほとんどコピペ。これ最強。
(add-hook 'navi2ch-hook 'navi2ch-offline-init-icons)

(eval-when-compile
  (navi2ch-defalias-maybe 'display-mouse-p 'ignore)
  (navi2ch-defalias-maybe 'make-mode-line-mouse-map 'ignore)
  (navi2ch-defalias-maybe 'display-images-p 'ignore)
  (navi2ch-defalias-maybe 'image-type-available-p 'ignore)
  (navi2ch-defalias-maybe 'find-image 'ignore)
  (defmacro navi2ch-e21-display-image-p ()
    '(and (display-images-p)
	  (image-type-available-p 'xpm))))

(defvar navi2ch-online-image nil)
(defvar navi2ch-offline-image nil)

(defun navi2ch-offline-init-icons ()
  (let ((props (when (display-mouse-p)
		 (list 'local-map (purecopy (make-mode-line-mouse-map
					     'mouse-2 #'navi2ch-toggle-offline))
		       'help-echo "mouse-2 toggles offline mode"))))
    (if (navi2ch-e21-display-image-p)
	(progn
	  (unless navi2ch-online-image
	    (let ((load-path (cons navi2ch-icon-directory load-path)))
	      (setq navi2ch-online-image (find-image
					  `((:type xpm
						   :file ,navi2ch-online-icon
						   :ascent center)))
		    navi2ch-offline-image (find-image
					   `((:type xpm
						    :file ,navi2ch-offline-icon
						    :ascent center))))))
	  (setq navi2ch-modeline-online
		(apply 'navi2ch-propertize navi2ch-online-indicator
		       `(display ,navi2ch-online-image ,@props))
                navi2ch-modeline-offline
		(apply 'navi2ch-propertize navi2ch-offline-indicator
		       `(display ,navi2ch-offline-image ,@props))))
      (when props
        (setq navi2ch-modeline-online
              (apply 'navi2ch-propertize navi2ch-online-indicator props)
              navi2ch-modeline-offline
              (apply 'navi2ch-propertize navi2ch-offline-indicator props))))))

;;; navi2ch-e21.el ends here
