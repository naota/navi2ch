;;; -*- Emacs-Lisp -*-
(require 'cl)

(setq bcomp-files
      '(
	"navi2ch-version.el" "navi2ch-vars.el" "navi2ch-face.el" "navi2ch-util.el" "navi2ch-net.el" "navi2ch-list.el" "navi2ch-article.el" "navi2ch-popup-article.el" "navi2ch-board-misc.el" "navi2ch-board.el" "navi2ch-articles.el" "navi2ch-bookmark.el" "navi2ch-history.el" "navi2ch-search.el" "navi2ch-message.el" "navi2ch.el" "navi2ch-head.el" "navi2ch-mona.el" "navi2ch-e21.el" "navi2ch-splash.el" "navi2ch-directory.el" "navi2ch-jbbs-net.el" "navi2ch-jbbs-shitaraba.el" "navi2ch-machibbs.el" "navi2ch-multibbs.el" 
	))

(let* ((dir (expand-file-name default-directory))
       (load-path (cons dir load-path))
       file)
  (message "deleting old .elc files...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt "c"))
    (if (file-exists-p file)
	(delete-file file)))

  (message "compiling...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt))
    (byte-compile-file file t))

  (message "recompiling...")
  (dolist (elt bcomp-files)
    (setq file (concat dir elt))
    (byte-compile-file file t))

  (message "done"))
