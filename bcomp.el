(require 'cl)

(setq load-path (cons default-directory load-path))

(setq bcomp-files
      '("navi2ch.el"
        "navi2ch-net.el"
        "navi2ch-util.el"
        "navi2ch-message.el"
        "navi2ch-article.el"
        "navi2ch-board-misc.el"
        "navi2ch-board.el"
        "navi2ch-bookmark.el"
        "navi2ch-articles.el"
        "navi2ch-list.el"
        "navi2ch-mona.el"))

(dolist (elt bcomp-files)
  (byte-compile-file elt))