;;; -*- Emacs-Lisp -*-
(require 'cl)

(when (featurep 'xemacs)
  (setq log-warning-minimum-level 'info)
  (setenv "XEMACSDEBUG" nil))

(setq bcomp-files
      '(
	"navi2ch-version.el" "navi2ch-vars.el" "navi2ch-face.el" "navi2ch-util.el" "navi2ch-net.el" "navi2ch-list.el" "navi2ch-article.el" "navi2ch-popup-article.el" "navi2ch-board-misc.el" "navi2ch-board.el" "navi2ch-articles.el" "navi2ch-bookmark.el" "navi2ch-history.el" "navi2ch-search.el" "navi2ch-message.el" "navi2ch.el" "navi2ch-head.el" "navi2ch-mona.el" "navi2ch-e21.el" "navi2ch-xmas.el" "navi2ch-splash.el" "navi2ch-directory.el" "navi2ch-be2ch.el" "navi2ch-multibbs.el" "navi2ch-jbbs-net.el" "navi2ch-jbbs-shitaraba.el" "navi2ch-machibbs.el" "navi2ch-futaba.el" "navi2ch-megabbs.el" "navi2ch-http-date.el" "navi2ch-localfile.el" "navi2ch-oyster.el" "navi2ch-auto-modify.el" 
	))

(let* ((dir (expand-file-name default-directory))
       (load-path (cons dir load-path))
       file)
  (message "deleting old .elc files...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt "c"))
    (when (file-exists-p file)
      (delete-file file)))
  (message "compiling...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt))
    (let ((coding-system-for-read 'iso-2022-7bit))
      (byte-compile-file file t)))
  (message "done"))
