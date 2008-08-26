;;; navi2ch-splash.el --- Navigator for 2ch for Emacsen -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 by Navi2ch
;; Project

;; Author: UEYAMA Rui <rui314159@users.sourceforge.net>
;; 110 の名無しさん http://pc.2ch.net/test/read.cgi/unix/1013457056/110
;;
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

;; コードは、Wanderlust の wl-demo.el からもらいました。ほとんど
;; 変わってないです。
;;
;;; wl-demo.el --- Opening demo on Wanderlust
;;
;; Copyright (C) 1998,1999,2000,2001 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>

;;; Code:
(provide 'navi2ch-splash)
(defconst navi2ch-splash-ident
  "$Id$")

(eval-when-compile
  (require 'cl))
(require 'navi2ch-vars)
(require 'navi2ch-face)
(require 'navi2ch-util)
(require 'navi2ch-version)

(defconst navi2ch-splash-copyright-notice
  (concat "Copyright (C) 2000-2008  Navi2ch Project.
This software includes some fragments from other softwares;
Copyright (C) 1993-2000 Free Software Foundation, Inc.
Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>
Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>\n"
	  (substitute-command-keys "
Navi2ch comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details."))
  "A declaration of the copyright on Navi2ch.")

(defconst navi2ch-splash-logo-image-name
  "navi2ch-logo"
  "Basename of the logo file.")

;; これはさすがに差しかえないと……
(defvar navi2ch-splash-logo-ascii "\
　　■　　　　■■　　　　　　　　　　　　　■■　　　　■　　
　■　　　　■　　　　　　　■■■■　　　　　　■　　　　■　
■　　　　　　　　　　　　　■　　■　　　　　　　　　　　　■
■　　　　　　　　　　　　　■　　■　　　　　　　　　　　　■
■　　　　　　　　　　　　　■　　■　　　　　　　　　　　　■
■　　　　　　　　　　　　　■　　■　　　　　　　　　　　　■
■　　　　　　　　　　　　■　　　■　　　　　　　　　　　　■
　■　　　　　　　　　　■■■■■■■　　　　　　　　　　■　
　　■　　　　　　　　　■　　　　　■　　　　　　　　　■　　

                            Navi2ch"
  "Ascii picture used to splash the startup screen.")

(eval-and-compile
  (when navi2ch-on-emacs21
    ;; `display-images-p' has not been available in Emacs versions
    ;; prior to Emacs 21.0.105.
    (navi2ch-defalias-maybe 'display-images-p 'display-graphic-p)))

;; Avoid byte compile warnings.
(eval-when-compile
  (autoload 'bitmap-insert-xbm-file "bitmap" nil t)
  (autoload 'create-image "image")
  (autoload 'device-on-window-system-p "device")
  (autoload 'image-type-available-p "image")
  (autoload 'insert-image "image")
  (autoload 'make-glyph "glyphs")
  (autoload 'set-glyph-face "glyphs")
  (autoload 'set-specifier "specifier")
  (navi2ch-defalias-maybe 'frame-char-height 'ignore)
  (navi2ch-defalias-maybe 'frame-char-width 'ignore)
  (navi2ch-defalias-maybe 'glyph-height 'ignore)
  (navi2ch-defalias-maybe 'glyph-width 'ignore)
  (navi2ch-defalias-maybe 'image-size 'ignore)
  (navi2ch-defalias-maybe 'make-extent 'ignore)
  (navi2ch-defalias-maybe 'set-extent-end-glyph 'ignore)
  (navi2ch-defalias-maybe 'window-pixel-height 'ignore)
  (navi2ch-defalias-maybe 'window-pixel-width 'ignore))

(defvar navi2ch-splash-bitmap-mule-available-p 'unknown
  "Internal variable to say whether the BITMAP-MULE package is available.")

(defun navi2ch-splash-image-type-alist ()
  "Return an alist of available logo image types on the current frame."
  (if (or (and (featurep 'xemacs)
	       (device-on-window-system-p))
	  window-system)
      (let ((xpm
             (when (or (and (featurep 'xemacs)
                            (featurep 'xpm))
                       (and navi2ch-on-emacs21
                            (display-images-p)
                            (image-type-available-p 'xpm)))
               '("xpm" . xpm)))
	    (xbm
             (when (or (featurep 'xemacs)
                       (and navi2ch-on-emacs21
                            (display-images-p)
                            (image-type-available-p 'xbm))
                       (eq t navi2ch-splash-bitmap-mule-available-p)
                       (and (eq 'unknown navi2ch-splash-bitmap-mule-available-p)
                            (or (featurep 'bitmap)
                                (locate-library "bitmap"))
                            (setq navi2ch-splash-bitmap-mule-available-p t)))
               '("xbm" . xbm)))
	    (bitmap
             (when (and (not (featurep 'xemacs))
                        (or (eq t navi2ch-splash-bitmap-mule-available-p)
                            (and (eq 'unknown navi2ch-splash-bitmap-mule-available-p)
                                 (or (featurep 'bitmap)
                                     (locate-library "bitmap"))
                                 (setq navi2ch-splash-bitmap-mule-available-p t))))
               '("bitmap" . bitmap))))
	(if (and navi2ch-on-emacs21
		 (image-type-available-p 'xbm))
	    ;; Prefer xbm rather than bitmap on Emacs 21.
	    (delq nil (list xbm bitmap xpm '("ascii")))
	  (delq nil (list bitmap xbm xpm '("ascii")))))
    '(("ascii"))))

(defun navi2ch-splash-insert-image (image-type)
  "Insert a logo image at the point and position it to be centered.
IMAGE-TYPE specifies what a type of image should be displayed.
Return a number of lines that an image occupies in the buffer."
  (let ((file (cond ((eq 'xpm image-type)
		     (concat navi2ch-splash-logo-image-name ".xpm"))
		    ((eq 'bitmap image-type)
		     (concat navi2ch-splash-logo-image-name ".img"))
		    ((eq 'xbm image-type)
		     (concat navi2ch-splash-logo-image-name ".xbm"))))
	image width height)
    (when (featurep 'xemacs)
      (when (boundp 'default-gutter-visible-p)
	(set-specifier (symbol-value 'default-gutter-visible-p)
		       nil (current-buffer)))
      (set-specifier (symbol-value 'scrollbar-height) 0 (current-buffer))
      (set-specifier (symbol-value 'scrollbar-width) 0 (current-buffer)))
    (if (and file
	     (if (and navi2ch-icon-directory
		      (file-directory-p navi2ch-icon-directory))
		 (setq file (expand-file-name file navi2ch-icon-directory))
	       (message "You have to specify the value of `navi2ch-icon-directory'")
	       nil)
	     (if (file-exists-p file)
		 (if (file-readable-p file)
		     t
		   (message "Permission denied: %s" file)
		   nil)
	       (message "File not found: %s" file)
	       nil))
	(progn
	  (cond ((featurep 'xemacs)
		 (setq width (window-pixel-width)
		       height (window-pixel-height)
		       image (make-glyph (vector image-type ':file file)))
		 (when (eq 'xbm image-type)
		   (set-glyph-face image 'navi2ch-splash-screen-face))
		 (insert-char ?\  (max 0 (/ (+ (* (- width (glyph-width image))
						  (window-width)) width)
					    (* 2 width))))
		 (set-extent-end-glyph (make-extent (point) (point)) image)
		 (insert "\n")
		 (/ (+ (* 2 (glyph-height image) (window-height)) height)
		    (* 2 height)))
		((and navi2ch-on-emacs21
		      (or (eq 'xpm image-type)
			  (and (eq 'xbm image-type)
			       (image-type-available-p 'xbm))))
		 ;; Use the new redisplay engine on Emacs 21.
		 (setq image (create-image file image-type)
		       width (image-size image)
		       height (cdr width)
		       width (car width))
		 (when (eq 'xbm image-type)
		   (let ((bg (face-background 'navi2ch-splash-screen-face))
			 (fg (face-foreground 'navi2ch-splash-screen-face)))
		     (when (stringp bg)
		       (plist-put (cdr image) ':background bg))
		     (when (stringp fg)
		       (plist-put (cdr image) ':foreground fg))))
		 (insert (navi2ch-propertize " " 'display
					     (list 'space ':align-to
						   (max 0 (round (- (window-width)
								    width)
								 2)))))
		 (insert-image image)
		 (insert "\n")
		 (round height))
		((eq 'bitmap image-type)
		 ;; Use ready-composed bitmap image.
		 (require 'bitmap)
		 (let ((coding-system-for-read 'iso-2022-7bit))
		   (insert-file-contents file))
		 (goto-char (point-max))
		 (unless (bolp)
		   (insert "\n"))
		 (setq width 0)
		 (while (progn
			  (end-of-line 0)
			  (not (bobp)))
		   (setq width (max width (current-column))))
		 ;; Emacs 21.1 would fail to decode composite chars
		 ;; if it has been built without fixing coding.c.
		 (when (and navi2ch-on-emacs21
			    (>= width 80))
		   (erase-buffer)
		   (let ((coding-system-for-read 'raw-text))
		     (insert-file-contents file))
		   (goto-char (point-max))
		   (unless (bolp)
		     (insert "\n"))
		   (setq width 0)
		   (while (progn
			    (end-of-line 0)
			    (not (bobp)))
		     ;; Decode bitmap data line by line.
		     (decode-coding-region (navi2ch-line-beginning-position)
					   (point)
					   'iso-2022-7bit)
		     (setq width (max width (current-column)))))
		 (indent-rigidly (point-min) (point-max)
				 (max 0 (/ (1+ (- (window-width) width)) 2)))
		 (put-text-property (point-min) (point-max)
				    'face 'navi2ch-splash-screen-face)
		 (count-lines (point-min) (goto-char (point-max))))
		((eq 'xbm image-type)
		 (message "Composing a bitmap image...")
		 (require 'bitmap)
		 (bitmap-insert-xbm-file file)
		 (backward-char)
		 (indent-rigidly (point-min) (point-max)
				 (max 0 (/ (1+ (- (window-width)
						  (current-column)))
					   2)))
		 (put-text-property (point-min) (point-max)
				    'face 'navi2ch-splash-screen-face)
		 (message "Composing a bitmap image...done")
		 (count-lines (point-min) (goto-char (point-max))))))
      (insert navi2ch-splash-logo-ascii)
      (put-text-property (point-min) (point) 'face 'navi2ch-splash-screen-face)
      (unless (bolp)
	(insert "\n"))
      (setq width 0)
      (while (progn
	       (end-of-line 0)
	       (not (bobp)))
	(setq width (max width (current-column))))
      (indent-rigidly (point-min) (point-max)
		      (max 0 (/ (1+ (- (window-width) width)) 2)))
      (count-lines (point-min) (goto-char (point-max))))))

(defun navi2ch-splash-insert-text (height)
  "Insert a version and the copyright message after a logo image.
HEIGHT should be a number of lines that an image occupies in the buffer."
  (let* ((height (- (window-height) height 1))
	 (notice-height (length (split-string navi2ch-splash-copyright-notice
					      "\n")))
	 (text (format (cond ((<= (- height notice-height) 1)
			      "version %s - \"%s\"\n%s")
			     ((eq (- height notice-height) 2)
			      "version %s - \"%s\"\n\n%s")
			     (t
			      "\nversion %s - \"%s\"\n\n%s"))
                       navi2ch-version
                       "オマエモナー"
		       navi2ch-splash-copyright-notice))
	 (text-height (length (split-string text "\n")))
	 start)
    (goto-char (point-min))
    (insert-char ?\n (max 0 (/ (- height text-height) 2)))
    (setq start (goto-char (point-max)))
    (if navi2ch-on-emacs21
	(let ((bg (face-background 'navi2ch-splash-screen-face))
	      (fg (face-foreground 'navi2ch-splash-screen-face)))
	  (insert (navi2ch-propertize text
				      'face (nconc '(variable-pitch :slant oblique)
						   (when (stringp bg)
						     (list ':background bg))
						   (when (stringp fg)
						     (list ':foreground fg))))))
      (insert text)
      (put-text-property start (point) 'face 'navi2ch-splash-screen-face))
    (let ((fill-column (window-width)))
      (center-region start (point)))))

;; shut up XEmacs warnings
(eval-when-compile
  (defvar default-enable-multibyte-characters)
  (defvar default-mc-flag)
  (defvar default-line-spacing))

(defun navi2ch-splash (&optional image-type)
  "Demo on the startup screen.
IMAGE-TYPE should be a symbol which overrides the variable
`navi2ch-splash-display-logo'.  It will prompt user for the type
of image when it is called interactively with a prefix argument."
  (interactive "P")
  (let ((selection (navi2ch-splash-image-type-alist))
	type)
    (if (and image-type (interactive-p))
	(setq type (completing-read "Image type: " selection nil t)
	      image-type (when (assoc type selection)
			   (cdr (assoc type selection))))
      (if (setq type (assoc (format "%s" (or image-type navi2ch-splash-display-logo))
			    selection))
	  (setq image-type (cdr type))
	(setq image-type (when navi2ch-splash-display-logo
			   (cdr (car selection)))))))
  (let ((buffer (let ((default-enable-multibyte-characters t)
		      (default-mc-flag t)
		      (default-line-spacing 0))
		  (get-buffer-create "*navi2ch splash*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (setq truncate-lines t
	  tab-width 8)
    (set (make-local-variable 'tab-stop-list)
	 '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (navi2ch-splash-insert-text (navi2ch-splash-insert-image image-type))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (sit-for (if (featurep 'lisp-float-type)
		 (/ (float 5) (float 10))
	       1))
    buffer))

;;; navi2ch-splash.el ends here
