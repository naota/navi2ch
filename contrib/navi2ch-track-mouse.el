;;; navi2ch-track-mouse.el --- Emulate help-echo functionality on Emacs 20

;; Copyright (C) 2002 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Adapted-By: Nanashi San <nanashi@users.sourceforge.net>
;; Keywords: emulations, convenience

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

;; Emacs 20 でも、Emacs 21 以降で使える help-echo プロパティを表示する。
;; でも、track-mouse を t にするんで重いかもよ。

;;; Code:
(defun navi2ch-track-mouse (event)
  "Emulate help-echo functionality on Emacs 20."
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (condition-case nil
	  (progn
	    (mouse-set-point event)
	    (let ((help-echo (get-text-property (point) 'help-echo))
		  (message-log-max nil))
	      (cond ((functionp help-echo)
		     (princ (funcall help-echo
				     (selected-window) (current-buffer)
				     (point))
			    t))
		    ((stringp help-echo)
		     (princ help-echo)))))))))

(when (and (not (featurep 'xemacs))
	   (= emacs-major-version 20))
  (define-key global-map [mouse-movement] 'navi2ch-track-mouse)
  (setq track-mouse t))

(provide 'navi2ch-track-mouse)
;;; navi2ch-track-mouse.el ends here
