;;; navi2ch-xmas.el --- XEmacs module for navi2ch -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003 by Navi2ch Project

;; Author: SAITO Takuya  <tabmore@users.sourceforge.net>
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
(provide 'navi2ch-xmas)
(defconst navi2ch-xmas-ident
  "$Id$")
(require 'navi2ch)

(add-hook 'navi2ch-hook 'navi2ch-offline-init-icons)

(eval-when-compile
  (navi2ch-defalias-maybe 'make-extent 'ignore)
  (navi2ch-defalias-maybe 'make-glyph 'ignore)
  (navi2ch-defalias-maybe 'make-modeline-command-wrapper 'ignore)
  (navi2ch-defalias-maybe 'set-extent-keymap 'ignore)
  (navi2ch-defalias-maybe 'set-extent-property 'ignore)
  (navi2ch-defalias-maybe 'set-glyph-image 'ignore))

(defun navi2ch-offline-init-icons ()
  (let ((extent (make-extent nil nil))
	(keymap (make-sparse-keymap))
	(online (make-glyph
		 (vector 'string :data navi2ch-online-indicator)))
	(offline (make-glyph
		  (vector 'string :data navi2ch-offline-indicator))))
    (define-key keymap 'button2
      (make-modeline-command-wrapper 'navi2ch-toggle-offline))
    (set-extent-keymap extent keymap)
    (set-extent-property extent 'help-echo "button2 toggles offline mode")
    (set-glyph-image online
		     (vector 'xpm
			     :file (expand-file-name navi2ch-online-icon
						     navi2ch-icon-directory)))
    (set-glyph-image offline
		     (vector 'xpm
			     :file (expand-file-name navi2ch-offline-icon
						     navi2ch-icon-directory)))
    (setq navi2ch-modeline-online (cons extent online)
	  navi2ch-modeline-offline (cons extent offline))))

;;; navi2ch-xmas.el ends here
