;;; navi2ch-vars.el --- User variables for navi2ch. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 by
;; Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: www 2ch

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
(provide 'navi2ch-vars)

(eval-when-compile
  (unless (fboundp 'coding-system-list)
    (defalias 'coding-system-list 'ignore)))

(defconst navi2ch-vars-ident
  "$Id$")

(defconst navi2ch-on-xemacs (featurep 'xemacs))
(defconst navi2ch-on-emacs21 (and (not navi2ch-on-xemacs)
                                  (>= emacs-major-version 21)))

(defvar navi2ch-coding-system
  (or (car (memq 'cp932 (coding-system-list)))
      (car (memq 'shift_jis-2004 (coding-system-list)))
      'shift_jis))

(defgroup navi2ch nil
  "*Navigator for 2ch."
  :prefix "navi2ch-"
  :link '(url-link :tag "Navi2ch Projectホームページ" "http://navi2ch.sourceforge.net/")
  :link '(custom-manual :tag "マニュアル (Info)" "(navi2ch)top")
  :group 'hypermedia
  :group '2ch)

(defgroup navi2ch-list nil
  "*Navi2ch, list buffer."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-board nil
  "*Navi2ch, board buffer."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-article nil
  "*Navi2ch, article buffer."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-message nil
  "*Navi2ch, message buffer."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-net nil
  "*Navi2ch, networking."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-localfile nil
  "*Navi2ch, localbbs."
  :prefix "navi2ch-"
  :group 'navi2ch)

;;; navi2ch variables
(defcustom navi2ch-ask-when-exit 'y-or-n-p
  "*non-nil なら、navi2ch 終了の確認メッセージを表示する。"
  :type '(choice (const :tag "yes-or-no-p で確認" yes-or-no-p)
                 (const :tag "y-or-n-p で確認" y-or-n-p)
                 (const :tag "聞かずに終了" nil))
  :group 'navi2ch)

(defcustom navi2ch-directory "~/.navi2ch"
  "*キャッシュファイルなどを保存するディレクトリ。

このディレクトリは、キャッシュの量によって 100MB 以上に膨らむ
こともある。キャッシュの制限については `navi2ch-board-expire-date'
を参照。"
  :type 'directory
  :group 'navi2ch)

(defcustom navi2ch-uudecode-program "uudecode"
  "*uudecode するのに使うプログラム。"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-uudecode-args nil
  "*uudecode を実行するときの引数。"
  :type '(repeat :tag "引数" string)
  :group 'navi2ch)

(defcustom navi2ch-bzip2-program "bzip2"
  "*bzip2 に使うプログラム。"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-bzip2-args '("-d" "-c" "-q")
  "*bzip2 を呼び出すときの引数。"
  :type '(repeat :tag "引数" string)
  :group 'navi2ch)

(defcustom navi2ch-init-file "init"
  "*navi2ch の初期化ファイル。"
  :type 'file
  :group 'navi2ch)

(defcustom navi2ch-browse-url-browser-function nil
  "*Navi2ch から使用するブラウザ関数。
nil の場合は `browse-url-browser-function' を使う。
\(autoload 'navi2ch-browse-url \"navi2ch\" nil t)
\(setq navi2ch-browse-url-browser-function 'w3m-browse-url)
\(setq browse-url-browser-function 'navi2ch-browse-url)
のように設定しておくと、他のパッケージから Navi2ch を呼び出す事ができる。"
  :type '(choice (const :tag "browsw-url にまかせる" nil)
		 (function :tag "関数を指定する"))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-program nil
  "*`navi2ch-browse-url-image' に使うプログラム。"
  :type '(choice string (const :tag "None" nil))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-args nil
  "*`navi2ch-browse-url-image-program' に与える引数。"
  :type '(repeat (string :tag "Argument"))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-extentions '("jpg" "jpeg" "gif" "png")
  "*`navi2ch-browse-url-image' で表示する画像の拡張子。"
  :type '(repeat (string :tag "拡張子"))
  :group 'navi2ch)

(defcustom navi2ch-base64-fill-column 64
  "*base64 でエンコードされた文字列を fill する文字数。"
  :type 'integer
  :group 'navi2ch)

(defcustom navi2ch-2ch-host-list
  '("cocoa.2ch.net" "pc.2ch.net" "pc2.2ch.net")
  "*2ちゃんねるとみなす host のリスト。"
  :type '(repeat (string :tag "ホスト"))
  :group 'navi2ch)

(defcustom navi2ch-use-lock t
  "*non-nil なら、Navi2ch が起動する際に `navi2ch-directory' をロックする。"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-lock-name "lockdir"
  "*ロックディレクトリの名前。
絶対パスにすれば `navi2ch-directory' 以外の場所にもロックディレクトリ
を作れるが、素人にはお勧めしない。"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-file-name-reserved-char-list '(?:)
  "*navi2ch-expand-file-name でエスケープする文字のリスト。
デフォルトは '(?:) で、URL 中の \":\" がディレクトリ名では \"%3A\" にエスケープされる。
\"~\" もエスケープしたいときは '(?: ?~) を指定する。"
  :type 'regexp
  :group 'navi2ch)

(defcustom navi2ch-decode-character-references t
  "*non-nil なら、数値文字参照、文字実体参照の表示を試みる。
GNU Emacs 21, XEmacs 21.5 以降であればデフォルトで表示できますが、
それ以前の Emacsen では Mule-UCS が必要です。(require 'un-define) してね。"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-pgp-verify-command-line nil
  "PGP 署名を検証するコマンドライン。
署名ファイル名、署名元ファイル名と共に `format' の引数となる。"
  :type '(choice (const :tag "無効" nil)
		 (const :tag "PGP" "pgp %s %s")
		 (const :tag "GPG" "gpg --verify %s %s")
		 (string :tag "指定"))
  :group 'navi2ch)

(defcustom navi2ch-enable-status-check nil
  "non-nil ならばブラウザを開く前に HEAD で接続先を確認する。"
  :type 'boolean
  :group 'navi2ch)

;;; list variables
(defcustom navi2ch-list-window-width 20
  "*板一覧ウィンドウの横幅。"
  :type 'integer
  :group 'navi2ch-list)

(defcustom navi2ch-list-etc-file-name "etc.txt"
  "*「その他」カテゴリに入れる板を書いておくファイル。"
  :type 'file
  :group 'navi2ch-list)

(defcustom navi2ch-list-stay-list-window nil
  "* non-nil なら、板を選んだあと板一覧バッファを表示したままにする。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-bbstable-url nil
  "*bbstable の URL。"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-init-open-category nil
  "*non-nil なら、板一覧のカテゴリをデフォルトですべて開いて表示する。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-indent-width 2
  "*板一覧バッファでの板名のインデント幅。"
  :type 'integer
  :group 'navi2ch-list)

(defcustom navi2ch-list-etc-category-name "その他"
  "*「その他」カテゴリの名前。"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-global-bookmark-category-name "ブックマーク"
  "*「ブックマーク」カテゴリの名前。"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-sync-update-on-boot t
  "*non-nil なら、navi2ch 起動時に常に板一覧を取りにいく。
nil ならば手動で更新しないかぎり取りにいかない。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-load-category-list t
  "*non-nil なら、前回の終了時に開いていたカテゴリを起動時に再び開く。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-valid-host-regexp
  (concat "\\("
	  (regexp-opt '(".2ch.net" ".bbspink.com" ".machibbs.com" ".machi.to"))
	  "\\)\\'")
  "*２ちゃんねるの板とみなすホストの正規表現。"
  :type 'regexp
  :group 'navi2ch-list)

(defcustom navi2ch-list-invalid-host-regexp
  (concat "\\`\\("
	  (regexp-opt '("find.2ch.net" "info.2ch.net"))
	  "\\)\\'")
  "*２ちゃんねるの板とみなさないホストの正規表現。
`navi2ch-list-valid-host-regexp' より優先される。"
  :type 'regexp
  :group 'navi2ch-list)

(defcustom navi2ch-list-board-id-alist nil
  "*板 URL から board-id への alist。"
  :type '(repeat (cons (string :tag "URL") (string :tag "id")))
  :group 'navi2ch-list)

(defcustom navi2ch-list-mouse-face 'highlight
  "リストモードで板をポイントした時に使用するフェイス。"
  :type '(choice (face :tag "フェイスを指定")
		 (const :tag "フェイスを使用しない" nil))
  :group 'navi2ch-list)

(defcustom navi2ch-list-filter-list nil
  "*スレッドの分類一覧をいじるフィルターのリスト。
それぞれのフィルターは elisp の関数ならば その symbol、
外部プログラムを呼ぶなら
'(\"perl\" \"2ch.pl\")
といった感じの list を設定する。
例えばこんな感じ。
\(setq navi2ch-list-filter-list
      '(navi2ch-filter
        (\"perl\" \"2ch.pl\")
        (\"perl\" \"filter-with-board.pl\" \"-b\" board)
        ))"
  :type '(repeat sexp)
  :group 'navi2ch-list)

(defcustom navi2ch-list-moved-board-alist nil
  "*移転した板の新旧 URL の alist。
http://pc.2ch.net/unix/ が http://pc3.2ch.net/unix/ に移転した場合、
\((\"http://pc.2ch.net/unix/\" . \"http://pc3.2ch.net/unix/\"))
のように指定する。"
  :type '(alist :key-type (string :tag "移転前の URL")
		:value-type (string :tag "移転後の URL"))
  :group 'navi2ch-list)

(defcustom navi2ch-list-display-board-id-p t
  "*板一覧に板IDを表示するかどうか。
non-nil ならば板IDを表示する。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-board-id-column 20
  "*板IDを表示する位置。"
  :type 'integer
  :group 'navi2ch-list)

;;; board variables
(defcustom navi2ch-board-max-line nil
  "*ダウンロードする subject.txt の行数。nil なら全部ダウンロードする。"
  :type '(choice (integer :tag "行数を指定")
		 (const :tag "全て" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-board-expire-date 30
  "*最後に変更されてからこの日数以上たったファイルは expire (削除)される。
nil なら expire しない。"
  :type '(choice (integer :tag "日数を指定")
		 (const :tag "expire しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-board-expire-bookmark-p nil
  "*expire するときに bookmark されているスレも expire するかどうか。
non-nil ならば expire する。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-expire-orphan-only nil
  "*non-nil ならオルファンなスレのみを expire する。
オルファンなスレとは、板の subject.txt にもグローバルブックマークにも
登録されてないスレのこと。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-window-height 10
  "*スレの一覧を表示する board window の高さ。"
  :type 'integer
  :group 'navi2ch-board)

(defcustom navi2ch-board-check-updated-article-p t
  "*non-nil なら、新しいレスがあったかチェックする。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-view-logo-program
  (if (eq window-system 'w32)
      "fiber"
    "xv")
  "*ロゴを見るのに使うプログラム。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-board-view-logo-args nil
  "*ロゴを見るのに使うプログラムの引数。"
  :type '(repeat (string :tag "引数"))
  :group 'navi2ch-board)

(defcustom navi2ch-board-delete-old-logo t
  "*non-nil なら、新しいロゴをダウンロードしたときに古いロゴを消す。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-hide-updated-article nil
  "*non-nil なら、navi2ch-board-updated-mode で hide されたスレッドを表示しない。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-bm-subject-width 50
  "*各スレの題名の幅。"
  :type 'integer
  :group 'navi2ch-board)

(defcustom navi2ch-bm-number-width 3
  "*各スレのスレ番号欄の幅。"
  :type 'integer
  :group 'navi2ch-board)

(defcustom navi2ch-bm-mark-and-move t
  "*マークしたあとのポインタの動作。
nil なら移動しない
non-nil なら下に移動する
'follow なら以前移動した方向に移動する"
  :type '(choice (const :tag "移動しない" nil)
		 (const :tag "下に移動" t)
		 (const :tag "以前移動した方向に移動" follow))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-empty-subject "navi2ch: no subject"
  "*subject が無いときに代り表示する subject。"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-history-max-line 100
  "*ヒストリの行数の制限。nil ならば制限しない。"
  :type '(choice (integer :tag "最大の行数を指定")
		 (const :tag "制限しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-stay-board-window t
  "*non-nil なら、スレを選んだときにスレ一覧を表示したままにする。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-bm-fetched-info-file "fetched.txt"
  "*既読スレのリストを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-bookmark-file "bookmark2.txt"
  "*グローバルブックマークを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-bookmark-remember-order-after-sort nil
  "*bookmark モードで sort 後のスレ並び順を記憶するかどうか。
non-nil ならば記憶する。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-history-file "history.txt"
  "*ヒストリを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-board-name-from-file "From File"
  "*ファイルから読み込んだスレを表す板名。"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-bm-mouse-face 'highlight
  "*板でスレをポイントした時に使用するフェイス。"
  :type '(choice (face :tag "フェイスを指定")
		 (const :tag "フェイスを使用しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-sort-by-state-order
  '(("%U" . 0)
    ("+U" . 1)
    ("%V" . 2)
    ("+V" . 3)
    ("%C" . 4)
    ("+C" . 5)
    ("% " . 6)
    ("+ " . 7)
    (" U" . 8)
    (" V" . 9)
    (" C" . 10)
    ("  " . 11)
    ("=U" . 12)
    ("=V" . 13)
    ("=C" . 14)
    ("= " . 15))
  "*状態でソートするときの順序を決めるリスト。"
  :type '(list (cons (const :tag "状態 %U" "%U") (number :tag "順番"))
	       (cons (const :tag "状態 %V" "%V") (number :tag "順番"))
	       (cons (const :tag "状態 %C" "%C") (number :tag "順番"))
	       (cons (const :tag "状態 % " "% ") (number :tag "順番"))
	       (cons (const :tag "状態 +U" "+U") (number :tag "順番"))
	       (cons (const :tag "状態 +V" "+V") (number :tag "順番"))
	       (cons (const :tag "状態 +C" "+C") (number :tag "順番"))
	       (cons (const :tag "状態 + " "+ ") (number :tag "順番"))
	       (cons (const :tag "状態  U" " U") (number :tag "順番"))
	       (cons (const :tag "状態  V" " V") (number :tag "順番"))
	       (cons (const :tag "状態  C" " C") (number :tag "順番"))
	       (cons (const :tag "状態   " "  ") (number :tag "順番"))
	       (cons (const :tag "状態 =U" "=U") (number :tag "順番"))
	       (cons (const :tag "状態 =V" "=V") (number :tag "順番"))
	       (cons (const :tag "状態 =C" "=C") (number :tag "順番"))
	       (cons (const :tag "状態 = " "= ") (number :tag "順番"))
	       (cons (const :tag "状態   " "  ") (number :tag "順番")))
  :group 'navi2ch-board)

(defcustom navi2ch-board-filter-list nil
  "*スレッドの一覧をいじるフィルターのリスト。
それぞれのフィルターは elisp の関数ならば その symbol、
外部プログラムを呼ぶなら
'(\"perl\" \"2ch.pl\")
といった感じの list を設定する。
例えばこんな感じ。
\(setq navi2ch-board-filter-list
      '(navi2ch-filter
        (\"perl\" \"2ch.pl\")
        (\"perl\" \"filter-with-board.pl\" \"-b\" board)
        ))"
  :type '(repeat sexp)
  :group 'navi2ch-board)

(defcustom navi2ch-board-check-article-update-suppression-length nil
  "*スレを更新する際、フィルター処理をチェックする新着レス数。

たとえば 10 を指定すると、
スレの新着レスが10個以下でそのすべてが非表示になるときは、
新着なしと見なされる。

nil を指定すると、新着レスへのフィルター処理をチェックしない。"
  :type '(choice (integer :tag "新着レス数")
		 (const :tag "チェックしない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-board-insert-subject-with-diff nil
  "*non-nil なら、Board モードのレス数欄にレスの増加数を表示する。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-insert-subject-with-unread nil
  "*non-nil なら、Board モードのレス数欄にレスの未読数を表示する。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-coding-system-alist
  nil
  "*板に対して強制的に coding-system を指定する為の alist。
各要素は、(BOARD-ID . CODING-SYSTEM)。
BOARD-ID は板ID。
CODING-SYSTEM は BOARD-ID で指定される板に指定する coding-system。"
  :type `(repeat
	  (cons
	   (string :tag "板ID")
	   (choice :tag "文字コード"
		   :value ,navi2ch-coding-system
		   ,@(mapcar (lambda (x)
			       (list 'const x))
			     (coding-system-list)))))
  :group 'navi2ch-board)

;;; article variables
(defcustom navi2ch-article-aadisplay-program
  (if (eq window-system 'w32)
      "notepad"
    "aadisplay")
  "*AA を表示するために使うプログラム。"
  :type 'string
  :group 'navi2ch-article)

(defcustom navi2ch-article-aadisplay-coding-system
  (if (eq window-system 'w32)
      'shift_jis-dos
    'euc-jp-unix)
  "*AA を表示するプログラムにわたす一時ファイルの `coding-system'。"
  :type 'coding-system
  :group 'navi2ch-article)

(defcustom navi2ch-article-view-aa-function
  (if (eq window-system 'w32)
      'navi2ch-article-popup-dialog
    'navi2ch-article-call-aadisplay)
  "*AA を表示するために使う関数。"
  :type 'function
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-diff t
  "*non-nil ならファイルの差分取得が有効になる。
nil にすると常にファイル全体を転送する。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-max-line nil
  "*ダウンロードする記事の行数。nil なら残りをすべてダウンロードする。"
  :type '(choice (integer :tag "件数を指定")
		 (const :tag "全て" nil))
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-fill nil
  "*non-nil なら、スレのメッセージを fill-region する。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-fill-list nil
  "*fill-region する板のリスト。"
  :type '(repeat string)
  :group 'navi2ch-article)

(defcustom navi2ch-article-disable-fill-list nil
  "*fill-region しない板のリスト。"
  :type '(repeat string)
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-through 'ask-always
  "*スレッドの最後でスペースを押したときに次のスレッドに移動するか。
nil なら移動しない
ask-always なら移動する前に必ず質問する
ask なら明示的に移動する時以外なら質問する
それ以外の non-nil な値なら何も聞かずに移動する。"
  :type '(choice (const :tag "いつでも質問する" ask-always)
		 (const :tag "明示的に移動するとき以外は質問する" ask)
		 (const :tag "聞かずに移動" t)
		 (const :tag "移動しない" nil))
  :group 'navi2ch-article)

(defcustom navi2ch-article-through-ask-function
  #'navi2ch-article-through-ask-y-or-n-p
  "*次のスレッドに移動するときの確認に使用する関数。"
  :type '(choice (const :tag "y or n で確認"
			navi2ch-article-through-ask-y-or-n-p)
		 (const :tag "n または p で確認"
			navi2ch-article-through-ask-n-or-p-p)
		 (const :tag "直前のコマンドと同じかで確認"
			navi2ch-article-through-ask-last-command-p))
  :group 'navi2ch-article)

(defcustom navi2ch-article-parse-field-list '(data name mail)
  "*メッセージのフィールドのうち、パーズ対象にするもののリスト。
遅くてもいいんなら '(data mail name) とかするといいかも"
  :type '(set (const :tag "記事" data)
              (const :tag "メール" mail)
              (const :tag "名前" name))
  :group 'navi2ch-article)

(defcustom navi2ch-article-goto-number-recenter t
  "*non-nil なら、goto-number したあと recenter する。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-new-message-range '(100 . 1)
  "*スレのデフォルトの表示範囲。初めて読むスレに適用する。

たとえば '(100 5) を指定すると、navi2ch はスレの先頭から100個、
末尾から5個のメッセージだけをバッファに挿入し、そのあいだの
メッセージについては処理を飛ばす。"
  :type '(cons integer integer)
  :group 'navi2ch-article)

(defcustom navi2ch-article-exist-message-range '(1 . 100)
  "*スレのデフォルトの表示範囲。既読スレに適用する。"
  :type '(cons integer integer)
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-range t
  "*non-nil なら、まだ表示してないスレッドの表示範囲を勝手に狭める。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-view-range-list
  '((1 . 50)
    (50 . 50)
    (1 . 100)
    (100 . 100))
  "*スレの表示範囲を変えるとき選択候補として使うリスト。"
  :type '(repeat (cons integer integer))
  :group 'navi2ch-article)

(defcustom navi2ch-article-header-format-function
  'navi2ch-article-default-header-format-function
  "*NUMBER NAME MAIL DATE を引数に取り、レスのヘッダを返す関数。"
  :type 'function
  :group 'navi2ch-article)

(defcustom navi2ch-article-citation-regexp
  "^[>＞]\\($\\|[^$>＞0-9０-９].*\\)"
  "*レスの引用部分の正規表現。"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-number-prefix-regexp "[>＞≫<＜][>＞≫<＜]* *"
  "*同じスレ内へのリンクを表す正規表現。"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-number-separator-regexp " *[,、=＝] *"
  "*同じスレ内へのリンクの数字を区切る文字列を表す正規表現。"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-number-number-regexp
  "\\([0-9０-９]+\\(-[0-9０-９]+\\)?\\)"
  "*同じスレ内へのリンクの数字を表す正規表現。"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-select-current-link-number-style 'auto
  "*スレ内リンク (>>3 とか) をたどるときの表示方法。
'popup ならつねに別ウィンドウを popup する。
'jump なら popup せずに移動する。
'auto なら自動で切り替える。"
  :type '(choice (const :tag "Popup" popup)
                 (const :tag "Jump" jump)
		 (const :tag "Auto" auto))
  :group 'navi2ch-article)

;; <http://www.ietf.org/rfc/rfc2396.txt>
;;       URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
;;       uric          = reserved | unreserved | escaped
;;       reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
;;                       "$" | ","
;;       unreserved    = alphanum | mark
;;       mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
;;                       "(" | ")"
(defcustom navi2ch-article-url-regexp
  "\\(h?t?tps?\\|x-localbbs\\|ftp\\|sssp\\)\\(://[-a-zA-Z0-9_.!~*';/?:@&=+$,%#]+\\)"
  "*レスのテキストのうち URL とみなす部分の正規表現。"
  ;; "(" ")" は URL を囲む意味で使われる場合が多いようなので含めない
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-link-regexp-alist
  (if (= (regexp-opt-depth "\\(\\(\\)\\)") 1)
      nil ;; バグ持ち regexp-opt-depth()
    '(("<\\(UR[IL]:\\)?\\([^:>]+\\)>" . nil)
      ("\\<h?t?tp:\\(//www.amazon.co.jp/exec/obidos/ASIN/[0-9A-Z]+\\)/?"
       . "http:\\1/")
      ("\\<h?t?tp://\\(ime\\.nu/\\)+\\([-a-zA-Z0-9_.!~*';/?:@&=+$,%#]+\\)"
       . "http://\\2")
      ("(h?t?tp\\(s?://[-a-zA-Z0-9_.!~*'();/?:@&=+$,%#]+\\))"
       . "http\\1")
      ("h?t?tp\\(s?://[-a-zA-Z0-9_.!~*'();/?:@&=+$,%#]+\\)"
       . "http\\1")
      ("<\\(UR[IL]:\\)?\\([a-z][-+.0-9a-z]*:[^>]*\\)>" . "\\2")))
  "*各要素の car を正規表現とし、マッチしたテキストに cdr へのリンクを張る。
置換先に使える特殊文字については `replace-match' 等を参照。
cdr が nil の場合はリンクを貼らない。
cdr が関数の場合はマッチした文字列を引数として呼び出し、返却値が文字列
の場合、それをリンクとする。その際、特殊文字は使えない。
リストの先頭を優先し、同じ文字列には一度だけマッチする。
`navi2ch-article-url-regexp' より優先される。

URL じゃない物にリンクを貼る:
'((\"\\\\=\\[\\\\(FreeBSD-[a-z]+-jp\\\\) \\\\([0-9]+\\\\)\\\\]\" .
   \"http://home.jp.freebsd.org/cgi-bin/showmail/\\\\1/\\\\2\"))

置換先に関数を使用:
'(\"\\\\=\\[postfix-jp: *\\\\([0-9]+\\\\)\\\\]\" .
  (lambda (str)
    (format \"http://www.kobitosan.net/postfix/ML/arc-2.1/msg%05d.html\"
            (1- (string-to-number (match-string 1))))))"
  :type '(repeat (cons (regexp :tag "マッチする正規表現")
		       (choice (const :tag "リンクを貼らない"
				      :value nil)
			       (string :tag "置換する文字列")
			       (function :tag "置換に利用する関数"))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-filter-list nil
  "*スレッドの記事をいじるフィルターのリスト。
それぞれのフィルターは elisp の関数ならば その symbol、
外部プログラムを呼ぶなら
'(\"perl\" \"2ch.pl\")
といった感じの list を設定する。
板IDを引数で指定するなら board というシンボルを板名を渡したい場所に書く。
例えばこんな感じ。
\(setq navi2ch-article-filter-list
      '(navi2ch-filter
        (\"perl\" \"2ch.pl\")
        (\"perl\" \"filter-with-board.pl\" \"-b\" board)
        ))

旧形式のセパレータを使用した .dat ファイルを扱いたい場合、この変数に
シンボル navi2ch-article-separator-filter を追加する。"
  :type '(repeat (choice (const :tag "旧形式 .dat 用フィルタ"
				:value navi2ch-article-separator-filter)
			 sexp))
  :group 'navi2ch-article)

(defcustom navi2ch-article-redraw-when-goto-number t
  "*non-nil なら、`navi2ch-article-goto-number' したところが範囲外のとき
自動で redraw しなおす。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-fix-range-diff 10
  "*`navi2ch-article-fix-range' したときに戻るレスの数。"
  :type 'integer
  :group 'navi2ch-article)

(defcustom navi2ch-article-fix-range-when-sync t
  "*non-nil なら、`navi2ch-article-sync' で範囲外のとき
自動的に `navi2ch-article-view-range' を変更する。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator ?_
  "*レスとレスの区切りに使う文字。"
  :type 'character
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator-width '(/ (window-width) 2)
  "*レスとレスを区切るテキストの横幅。
幅を 80 文字にしたいなら
\(setq navi2ch-article-message-separator-width 80)
window の幅いっぱいにしたいなら
\(setq navi2ch-article-message-separator-width '(window-width))
等指定する。"
  :type 'sexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator-insert-hide-number-p t
  "*hide 情報をレスとレスの区切りに表示するか。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator-insert-trailing-newline-p t
  "*レスの区切りの後にもう一個改行を表示するか。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-expunge nil
  "*non-nil なら、バッファとして保持するスレの数を
`navi2ch-article-max-buffers' 以下に保つ。この制限値を超えたときには、
いちばん古いバッファを自動的に消す。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-max-buffers 20
  "*バッファとして保持するスレの最大数。0 ならば無制限。"
  :type '(choice (const :tag "無制限" 0)
                 (integer :tag "制限値"))
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-white-space-after-old-br t
  "*non-nil なら、古い形式の <br> に対応して行頭から空白を取り除く。
ただし、すべての <br> の直後に空白がある場合に限る。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-trailing-whitespace t
  "*non-nil なら、スレの各行から末尾の空白を取り除く。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-trailing-newline t
  "*non-nil なら、スレの各レスから末尾の空行を取り除く。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-display-link-width '(1- (window-width))
  "*スレのリンク先などを minibuffer に表示するときの文字列の最大長。
これより長いテキストは切り詰められる。
数値のほか、eval で数値を返す任意の S 式を指定できる。"
  :type '(choice (integer :tag "数値で指定")
                 (sexp :tag "関数とか"))
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-decode-p nil
  "*non-nil なら、スレのエンコードされたテキストを自動展開する。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-decode-insert-text nil
  "*non-nil なら、自動展開したテキストをバッファに挿入する。
`navi2ch-article-auto-decode-p' が non-nil のときのみ効果がある。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-show-url-number 50
  "*url を表示・コピーする際、最後のレスをいくつ表示するか。"
  :type 'number
  :group 'navi2ch-article)

(defcustom navi2ch-article-mouse-face 'highlight
  "*スレでリンクをポイントした時に使用するフェイス。"
  :type '(choice (face :tag "フェイスを指定")
		 (const :tag "フェイスを使用しない" nil))
  :group 'navi2ch-article)

(defcustom navi2ch-article-get-url-text t
  "* non-nil なら `navi2ch-article-get-link-text' で URL のリンク先を得る。
nil の場合は同じスレの内容のみを得る。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-date-format-function
  'identity
  "* レスの日付をフォーマットする関数。
`navi2ch-article-default-header-format-function' から、DATE を引数
として呼び出される。"
  :type '(choice (const :tag "変更しない"
			:value identity)
		 (const :tag "Be2ch にリンクを追加する"
			:value navi2ch-article-date-format-be2ch)
		 (function :tag "関数を指定"))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-list
  '(navi2ch-article-message-filter-by-name
    navi2ch-article-message-filter-by-mail
    navi2ch-article-message-filter-by-id
    navi2ch-article-message-filter-by-message
    navi2ch-article-message-filter-by-subject
    navi2ch-article-message-filter-by-hostname)
  "*レスをフィルタするための関数のリスト。
リストの member となる関数としては、
レスの alist を引き数に取り、
フィルタ後の文字列もしくは 'hide か 'important を返すものを指定する。

下記は名前欄が「ほげ」のときに「あぼぼーん」に置換するための
\(ちょっと冗長な) 関数の例。

\(defun my-navi2ch-article-message-filter-hoge (alist)
  (let ((number (cdr (assq 'number alist)))
	(name (cdr (assq 'name alist)))
	(mail (cdr (assq 'mail alist)))
	(date (cdr (assq 'date alist)))
	(message (cdr (assq 'data alist))))
    (if (equal name \"ほげ\")
	\"あぼぼーん\"
      nil)))"
  :type '(repeat function)
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-name-alist nil
  "*レスをフィルタするための名前の条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
名前がその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法で名前を検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
名前に「ふが」が含まれているとレスが「あぼぼーん」に置き換わり、
名前に「ホゲ」が含まれているとレスが隠される。

'((\"ふが\" . \"あぼぼーん\")
  ((\"ホゲ\" S) . hide))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-message-alist nil
  "*レスをフィルタするためのレス本文の条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
レス本文がその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法でレス本文を検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
レス本文に「ふが」が含まれているとレスが「あぼぼーん」に置き換わり、
レス本文に「ホゲ」が含まれているとレスが隠される。

'((\"ふが\" . \"あぼぼーん\")
  ((\"ホゲ\" S) . hide))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-id-alist nil
  "*レスをフィルタするための ID の条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
ID がその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法で ID を検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
ID に「FUga1234」が含まれているとレスが「あぼぼーん」に置き換わり、
ID が「hoGE0987」だとレスが隠される。

'((\"FUga1234\" . \"あぼぼーん\")
  ((\"hoGE0987\" E) . hide))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-mail-alist nil
  "*レスをフィルタするためのメール欄の条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
メール欄がその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法でメール欄を検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
メール欄に「ふが」が含まれているとレスが「あぼぼーん」に置き換わり、
メール欄に「ホゲ」が含まれているとレスが隠される。

'((\"ふが\" . \"あぼぼーん\")
  ((\"ホゲ\" S) . hide))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-subject-alist nil
  "*レスをフィルタするためのスレッドのタイトルの条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
スレッドのタイトルがその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法でスレッドのタイトルを検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
スレッドのタイトルに「ふが」が含まれているとレスの得点が +1000 され、
スレッドのタイトルに「ホゲ」が含まれているとレスの得点が -1000 される。

'((\"ふが\" . 1000)
  ((\"ホゲ\" S) . -1000))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-filter-by-hostname-alist nil
  "*レスをフィルタするためのホスト名の条件と、フィルタ処理の alist。

条件には文字列か、
\(文字列 シンボル ...)の形式のリスト(拡張形式)を指定する。

文字列を指定すると、
ID がその文字列を含むときにフィルタ処理が実行される。

拡張形式を指定すると、
シンボルに合わせて下記の方法で ID を検査する。

S,s	部分一致
E,e	完全一致
F,f	あいまい一致	(空白や改行の有無や多少を無視し、
			また全角と半角を区別しない部分一致)
R,r	正規表現

大文字のシンボルを指定すると文字列の大文字と小文字を区別し、
小文字のシンボルを指定すると文字列の大文字と小文字を区別しない。

あいまい一致を小文字で指定した場合は、ひらがなとカタカナも区別しない。

シンボルの後にはキーワードを追加し、
値によってフィルタ条件を下記のように補足することができる。

:invert		t (non-nil)を指定すると、文字列一致の真偽を逆転する

:board-id	フィルタ対象となる板の ID を指定する
:artid		フィルタ対象となるスレッドの ID も指定する

:float		フィルタ条件が一致したとき、このフィルタ項目を
		`navi2ch-article-sort-message-filter-rules'を無視して
		常に alist の先頭に持ってくる場合は 1 (正数値)を指定し、
		そのままにする場合は 0 (非正数値)を指定する


フィルタ処理には、文字列・シンボル・数値のどれかを指定する。

文字列を指定すると、レスがその文字列に置き換わる。

フィルタ条件を拡張形式で指定していた場合、
置換後の文字列中の \\1〜\\9 および \\& は、一致した文字列に展開される。
\\1〜\\9 および \\& の意味については、`replace-match'を参照のこと。

シンボルを指定すると、シンボルに合わせて下記の処理が行われる。

hide		レスを隠す
important	レスをブックマークに登録する

数値を指定すると、レスの得点にその数値分の点数を加えて、
残りのフィルタを実行する。

例えば下記の値を設定すると、
ホスト名に「example.jp」が含まれているとレスが「あぼぼーん」に置き換わり、
ホスト名が「foo.example.jp」が含まれているとレスが隠される。

'((\"example.jp\" . \"あぼぼーん\")
  ((\"foo.example.jp\" S) . hide))"
  :type (let ((plist '(set :inline t
			   :format "%v"
			   (list :tag "文字列一致の真偽を逆転"
				 :inline t
				 :format "%{%t%}\n"
				 :value '(:invert t))
			   (list :tag "板を指定"
				 :inline t
				 (const :format ""
					:value :board-id)
				 (string :tag "ID")
				 (set :inline t
				      :format "%v"
				      (list :tag "スレッドも指定"
					    :inline t
					    (const :format ""
						   :value :artid)
					    (string :tag "ID"))))
			   (list :tag "条件が一致したときのフィルタの位置"
				 :inline t
				 (const :format ""
					:value :float)
				 (choice :value 0
					 (const :tag "そのまま"
						:value 0)
					 (const :tag "先頭へ"
						:value 1))))))
	  `(repeat (cons (choice :tag "条件"
				 (string)
				 (list :tag "部分一致"
				       :value ("" S)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value S)
					       (const :tag "なし"
						      :value s))
				       ,plist)
				 (list :tag "完全一致"
				       :value ("" E)
				       (string)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value E)
					       (const :tag "なし"
						      :value e))
				       ,plist)
				 (list :tag "あいまい一致"
				       :value ("" f)
				       (string)
				       (choice :tag "大文字と小文字・ひらがなとカタカナの区別"
					       (const :tag "あり"
						      :value F)
					       (const :tag "なし"
						      :value f))
				       ,plist)
				 (list :tag "正規表現"
				       :value ("" R)
				       (regexp)
				       (choice :tag "大文字と小文字の区別"
					       (const :tag "あり"
						      :value R)
					       (const :tag "なし"
						      :value r))
				       ,plist))
			 (choice :tag "処理"
				 (string :tag "置き換える"
					 :value "あぼぼーん")
				 (const :tag "隠す"
					:value hide)
				 (const :tag "ブックマークに登録する"
					:value important)
				 (number :tag "点数を加える"
					 :value 0)))))
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-activate-message-filter t
  "*non-nil なら、フィルタ機能をデフォルトで on にする。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-use-message-filter-cache t
  "*non-nil なら、フィルタ処理でキャッシュを利用する。
キャッシュは、フィルタのアンドゥ情報の保持も兼ねる。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-sort-message-filter-rules nil
  "*non-nil なら、条件が一致したフィルタ項目を alist の先頭に持ってくる。

例えば、`navi2ch-article-message-filter-by-name-alist'を
下記の値に設定していて「ホゲ」という名前欄のレスに当たった場合、

'((\"ふが\" . \"あぼぼーん\")
  ((\"ホゲ\" S) . hide))

`navi2ch-article-message-filter-by-name-alist'の値は
条件が一致した '((\"ホゲ\" S) . hide) が先頭に来るように並び換えられ、
次のように変更される。

'(((\"ホゲ\" S) . hide)
  (\"ふが\" . \"あぼぼーん\"))"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-replace-below nil
  "*フィルタによってレスを置き換えるための得点のしきい値と、
置き換える文字列。
得点がこの値より小さいとレスが置き換わる。

例えば下記の値を設定すると、
レスの得点が-1000より小さいとレスが「あぼぼーん」に置き換わる。

'(-1000 . \"あぼぼーん\")"
  :type '(choice (const :tag "off"
			:value nil)
		 (cons :tag "設定する"
		       (number :tag "しきい値"
			       :value 0)
		       (string :tag "置換後"
			       :value "あぼぼーん")))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-hide-below nil
  "*フィルタによってレスを隠すための得点のしきい値。
得点がこの値より小さいとレスが隠される。"
  :type '(choice (const :tag "off"
			:value nil)
		 (number :tag "しきい値"
			 :value 0))
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-add-important-above nil
  "*フィルタによってレスをブックマークに登録するための得点のしきい値。
得点がこの値より大きいとレスがブックマークに登録される。"
  :type '(choice (const :tag "off"
			:value nil)
		 (number :tag "しきい値"
			 :value 0))
  :group 'navi2ch-article)

(defcustom navi2ch-article-save-info-wrapper-func nil
  "*navi2ch-article-save-info 中で article の wrapper として使う関数。"
  :type 'function
  :group 'navi2ch-article)

;;; message variables
(defcustom navi2ch-message-user-name ""
  "*デフォルトの名前。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-user-name-alist nil
  "*板ごとのデフォルトの名前の alist。

たとえば次のように設定しておくと、ネットワーク板では \"anonymous\"、
テレビ番組板では \"名無しさん\" がデフォルトの名前になる。
  '((\"network\" . \"anonymous\")
    (\"tv\" . \"名無しさん\"))"
  :type '(repeat (cons (string :tag "板  ") (string :tag "名前")))
  :group 'navi2ch-message)

(defcustom navi2ch-message-mail-address ""
  "*デフォルトのメールアドレス。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-mail-address-alist nil
  "*板ごとのデフォルトのメールアドレスの alist。

たとえば次のように設定しておくと、
ニュース速報板では \"someone@example.com\"、UNIX 板では \"sage\" が
デフォルトのメールアドレスになる。
  '((\"news\" . \"someone@example.com\")
    (\"unix\" . \"sage\"))"
  :type '(repeat (cons (string :tag "板  ") (string :tag "名前")))
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-write nil
  "*non-nil なら、レスを書き始めるときに確認メッセージを表示する。"
  :type '(choice (const :tag "yes-or-no-p で確認" yes-or-no-p)
                 (const :tag "y-or-n-p で確認" y-or-n-p)
                 (const :tag "確認しない" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-send 'y-or-n-p
  "*non-nil なら、書き込み送信の確認メッセージを表示する。"
  :type '(choice (const :tag "yes-or-no-p で確認" yes-or-no-p)
                 (const :tag "y-or-n-p で確認" y-or-n-p)
                 (const :tag "確認しない" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-kill 'y-or-n-p
  "*non-nil なら、書きこみキャンセルの確認メッセージを表示する。"
  :type '(choice (const :tag "yes-or-no-p で確認" yes-or-no-p)
                 (const :tag "y-or-n-p で確認" y-or-n-p)
                 (const :tag "確認しない" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-always-pop-message nil
  "*non-nil なら、新規メッセージを作るコマンドは書きかけのレスを常に復元する。
nil なら、書きかけを破棄していいか問い合わせる。
書きかけのメッセージのバッファが残っている場合にだけ有効。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-wait-time 3
  "*レスを送ったあとスレをリロードするまでの待ち時間(秒)。"
  :type 'integer
  :group 'navi2ch-message)

(defcustom navi2ch-message-retry-wait-time 2
  "*レス送信を再試行するときの待ち時間(秒)。"
  :type 'integer
  :group 'navi2ch-message)

(defcustom navi2ch-message-remember-user-name t
  "*non-nil なら、送ったレスの名前覧とメール欄を覚えておく。
同じスレで次にレスするときは、それがデフォルトの名前になる。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-cite-prefix "> "
  "*引用するときの接頭辞。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-trip nil
  "*trip 用の文字列。書きこみ時に From の後ろに付加される。"
  :type '(choice (string :tag "trip を指定")
		 (const :tag "trip を指定しない" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-aa-prefix-key "\C-c\C-a"
  "*AA を入力する為の prefix-key。"
  :type 'string
  :group 'navi2ch-message)

(defvar navi2ch-message-aa-default-alist
  '(("a" . "(´Д｀)")
    ("b" . "ヽ(`Д´)ﾉ")
    ("B" . "(((；ﾟДﾟ))ｶﾞｸｶﾞｸﾌﾞﾙﾌﾞﾙ")
    ("f" . "( ´_ゝ`)ﾌｰﾝ")
    ("e" . "(⊃д`)")
    ("F" . "(´ー｀)")
    ("g" . "(ﾟДﾟ)ｺﾞﾙｧ")
    ("G" . "ｶﾞ━━(ﾟДﾟ;)━━ｿ!")
    ("h" . "(ﾟДﾟ)ﾊｧ?")
    ("H" . "(;´Д｀)ﾊｧﾊｧ")
    ("i" . "(･∀･)ｲｲ!!")
    ("I" . "(･Ａ･)ｲｸﾅｲ!!")
    ("j" . "(･∀･)ｼﾞｻｸｼﾞｴﾝﾃﾞｼﾀ")
    ("k" . "ｷﾀ━━━━━━(ﾟ∀ﾟ)━━━━━━ !!!!!")
    ("K" . "ｷﾀ━(ﾟ∀ﾟ)━( ﾟ∀)━( 　ﾟ)━(　　)━(ﾟ 　)━(∀ﾟ )━(ﾟ∀ﾟ)━!!!!")
    ("m" . "(´∀｀)")
    ("M" . "ヽ(´▽｀)ﾉ")
    ("n" . "(￣ー￣)ニヤリッ")
    ("N" . "(´-`).｡ｏＯ(なんでだろう？)")
    ("p" . "（　ﾟдﾟ）ﾎﾟｶｰﾝ")
    ("s" . "Σ（ﾟдﾟlll）ｶﾞｰﾝ")
    ("S" . "(´･ω･`)ｼｮﾎﾞｰﾝ")
    ("t" . "y=ｰ( ﾟдﾟ)･∵.　ﾀｰﾝ")
    ("u" . "(ﾟдﾟ)ｳﾏｰ")
    ("U" . "(-＿-)ｳﾂﾀﾞ"))
  "AA を入力するときのキーバインドと AA の alist。
`navi2ch-message-aa-alist' から値が見付からない場合はこっちから探す。")

(defcustom navi2ch-message-aa-alist nil
  "*AA を入力するときのキーバインドと AA の alist。
たとえば、((\"Z\" . \"ＺZｚz...\")) のように設定すると、Message Mode
でprefix-key (デフォルトでは (C-c C-a) あとに Z を入力すると
ＺZｚz... と入力できる。
SPC、C-l、C-g、C-vはリスト表示の際に使用されるのでキーには使用しないこと。"
  :type '(repeat (cons string string))
  :group 'navi2ch-message)

(defcustom navi2ch-message-cleanup-trailing-whitespace nil
  "*non-nil なら、送信するレスから行末の空白を取り除く。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-cleanup-trailing-newline nil
  "*non-nil なら、送信するレスから末尾の空行を取り除く。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-popup-aa-width 39
  "*aa のリストを表示する際の幅。"
  :type 'number
  :group 'navi2ch-message)

(defcustom navi2ch-message-force-sync nil
  "*non-nil なら、レスを送信したあと強制的に sync する。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-save-sendlog nil
  "*non-nil なら、送信したレスの控えをとる。
Emacs のカスタマイズインターフェイスを使ってこの値を non-nil にした場合、
Navi2chカテゴリに「送信控え」板が自動的に追加されます。
`navi2ch-init-file'に
  (setq navi2ch-message-save-sendlog t)
と書いた場合は、さらに
  (add-to-list 'navi2ch-list-navi2ch-category-alist
               navi2ch-message-sendlog-board)
を追加し、Navi2chカテゴリに「送信控え」板を追加して下さい。"
  :type 'boolean
  :group 'navi2ch-message
  :set (lambda (symbol value)
	 (when value
	   (eval-after-load "navi2ch"
	     '(add-to-list 'navi2ch-list-navi2ch-category-alist
			   navi2ch-message-sendlog-board)))
	 (set-default symbol value)))

(defcustom navi2ch-message-sendlog-subject "送信控え"
  "*送信したレスを保存するスレのタイトル。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-sendlog-response-limit 1000
  "*送信控え 1スレあたりのレス数の上限。nil ならば制限しない。"
  :type '(choice (integer :tag "レス数の上限")
		 (const :tag "無制限" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-sendlog-volume-format "%s (Part %s)"
  "*送信控えスレを分割するときのフォーマット。
最初の %s がスレのタイトルで、2番目の %s がスレの番号で置き換えられる。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-sendlog-message-format-function
  'navi2ch-message-sendlog-simple-message-format
  "*送信控えのレスをフォーマットする関数を指定する。
引数は以下:
\(MESSAGE SUBJECT URL BOARD ARTICLE)"
  :type '(choice
	  (const
	   :tag "シンプルなフォーマット"
	   :value navi2ch-message-sendlog-simple-message-format
	   :doc "Subject: スレッドタイトル\nURL: http://")
	  (const
	   :tag "板名付きのフォーマット"
	   :value navi2ch-message-sendlog-message-format-with-board-name
	   :doc "[板名]: スレッドタイトル\nURL: http://")
	  (function :tag "関数を指定"))
  :group 'navi2ch-message)

;; net variables
(defcustom navi2ch-net-http-proxy
  (if (string= (getenv "HTTP_PROXY") "")
      nil
    (getenv "HTTP_PROXY"))
  "*HTTP プロキシの URL。"
  :type '(choice (string :tag "プロキシを指定")
		 (const :tag "プロキシを使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-userid nil
  "*プロキシ認証に使うユーザ名。"
  :type '(choice (string :tag "ユーザ名を指定")
		 (const :tag "ユーザ名を使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-password nil
  "*プロキシ認証に使うパスワード。"
  :type '(choice (string :tag "パスワードを指定")
		 (const :tag "パスワードを使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-send-message-use-http-proxy t
  "*non-nil なら、レスを送る場合などでもプロキシを経由する。
このオプションを有効にするには、`navi2ch-net-http-proxy' を non-nil
に設定すること。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-for-send-message nil
  "*レス書き込みに使う HTTP プロキシの URL。
nil のときはプロキシとして `navi2ch-net-http-proxy' が使われる。
`navi2ch-net-send-message-use-http-proxy' が non-nil のときのみ有効。"
  :type '(choice (string :tag "プロキシを指定")
		 (const :tag "`navi2ch-net-http-proxy' と同じ" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-userid-for-send-message nil
  "*レス書き込み時にプロキシ認証に使うユーザ名。
`navi2ch-net-http-proxy-for-send-message' が non-nil のときのみ有効。"
  :type '(choice (string :tag "ユーザ名を指定")
		 (const :tag "ユーザ名を使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-password-for-send-message nil
  "*レス書き込み時にプロキシ認証に使うパスワード。
`navi2ch-net-http-proxy-for-send-message' が non-nil のときのみ有効。"
  :type '(choice (string :tag "パスワードを指定")
		 (const :tag "パスワードを使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-force-update nil
  "*non-nil なら、ファイルを取得するまえに更新の有無を確認しなくなる。
nil なら、更新されていないファイルの不必要な転送はしない。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-save-old-file-when-aborn 'ask
  "*あぼーんがあったとき元のファイルを保存するか。
nil なら保存しない
ask なら保存する前に質問する
それ以外の non-nil な値なら何も聞かずに保存する。"
  :type '(choice (const :tag "質問する" ask)
		 (const :tag "聞かずに保存" t)
		 (const :tag "保存しない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-inherit-process-coding-system nil
  "*`inherit-process-coding-system' の navi2ch での束縛値。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-accept-gzip t
  "*non-nil なら、ファイル受信に GZIP エンコーディングを使う。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-gunzip-program "gzip"
  "*gunzip に使うプログラム。"
  :type 'file
  :group 'navi2ch-net)

(defcustom navi2ch-net-gunzip-args '("-d" "-c" "-q")
  "*gunzip を呼び出すときの引数。"
  :type '(repeat :tag "引数" string)
  :group 'navi2ch-net)

(defcustom navi2ch-net-enable-http11 nil
  "*non-nil なら、HTTP/1.1 を使用する。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-open-network-stream-function
  #'open-network-stream
  "*open-network-stream と同等な処理をする関数。
`open-network-stream' (デフォルト)、
`navi2ch-open-network-stream-with-retry' (operation already in progress 回避)
`navi2ch-open-network-stream-via-command' (外部コマンドを使用)
などを指定する。"
  :type '(choice (const :tag "Emacs から直接接続"
			open-network-stream)
		 (const :tag
			"operation already in progress 等のエラーが出る場合"
			navi2ch-open-network-stream-with-retry)
		 (const :tag "コマンド経由で接続"
			navi2ch-open-network-stream-via-command)
		 (function :tag "関数を指定"))
  :group 'navi2ch-net)

(defcustom navi2ch-open-network-stream-command nil
  "*ホストのサービスに接続するコマンド。
`navi2ch-open-network-stream-function' が
`navi2ch-open-network-stream-via-command' の場合に使用される。
値が文字列の場合、その文字列と host service を `format' に渡し、その返
却値をシェル経由で実行する。
値が関数の場合、host service を引数として呼び出す。
指定した関数の返却値が文字列の場合はシェル経由で、リストの場合は直接実
行する。

たとえば、ssh 経由で netcat を使いたい場合は以下のいずれかのようにする。
\"ssh somehost nc %s %s\"
\(lambda (host service)
  (format \"ssh somehost nc %s %s\" host service))
\(lambda (host service)
  (list \"ssh\" \"somehost\"
        \"nc\" (format \"%s\" host) (format \"%s\" service)))"
  :type '(choice (const :tag "無効" nil)
		 (const :tag "Netcat を使用"
			(lambda (host service)
			  (list "nc" (format "%s" host)
				(format "%s" service))))
		 (string :tag "文字列を指定")
		 (function :tag "関数を指定"))
  :group 'navi2ch-net)

;;; update variables
(defcustom navi2ch-update-file "navi2ch-update.el"
  "*Navi2ch の自動更新に利用するファイルのローカルファイル名。"
  :type 'file
  :group 'navi2ch)

(defcustom navi2ch-update-base-url
  "http://navi2ch.sourceforge.net/autoupdate/"
  "*自動更新するファイルがある場所の BASE URL。"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-update-url (concat navi2ch-update-base-url
				      (file-name-nondirectory
				       navi2ch-update-file))
  "*自動更新に利用するファイルの URL。"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-auto-update nil
  "*non-nil なら、起動時に `navi2ch-update-file' を更新して実行する。
ファイルが実行されるのは、
 - `navi2ch-update-file' が更新されていて、
 - `navi2ch-pgp-verify-command-line' が non-nil の場合は検証可能で、
 - そこで表示される確認するメッセージに yes と答えたとき
のみ。

ヤバいコードが入っているとまずいので、なるべく pgp か gpg をインストー
ルして `navi2ch-pgp-verify-command-line' を設定しよう。"
  :type 'boolean
  :group 'navi2ch)

;;; auto modify variables
(defcustom navi2ch-auto-modify-file t
  "*設定を自動的に変更して保存するファイル。
t なら `navi2ch-init-file' に保存し、
nil なら、`customize'を利用して`custom-file'に保存する。

このファイル単体が自動的にロードされることはないので、
`navi2ch-init-file'以外のファイルを指定した場合
\(`navi2ch-init-file'を byte-compile した場合を含む)は必要に応じて、

\(load navi2ch-auto-modify-file)

を`navi2ch-init-file'に追加するなどの方法で明示的にファイルを
ロードすること。"
  :type '(choice (file :tag "ファイル")
		 (const :tag "custom-file" nil))
  :group 'navi2ch)

(defcustom navi2ch-auto-modify-truncate-list-alist nil
  "*リスト型変数を保存するときの、変数名とその最大要素数の alist。

例えば下記の値を設定すると、
`navi2ch-article-message-filter-by-id-alist'と
`navi2ch-article-message-filter-by-message-alist'の要素は、
自動変更・保存の際にそれぞれ10個以下・100個以下に切り詰められる。

'((navi2ch-article-message-filter-by-id-alist . 10)
  (navi2ch-article-message-filter-by-message-alist . 100))"
  :type '(repeat (cons (variable :tag "変数名")
		       (integer :tag "最大要素数")))
  :group 'navi2ch)

(defcustom navi2ch-icon-directory
  (cond ((condition-case nil
	     (progn
	       (require 'navi2ch-config)
	       navi2ch-config-icondir)
	   (error nil)))
	((fboundp 'locate-data-directory)
	 (locate-data-directory "navi2ch"))
	((let ((icons (expand-file-name "navi2ch/icons/"
					data-directory)))
	   (if (file-directory-p icons)
	       icons)))
	((let ((icons (expand-file-name "icons/"
					(file-name-directory
					 (locate-library "navi2ch")))))
	   (if (file-directory-p icons)
	       icons))))
  "* アイコンファイルが置かれたディレクトリ。nil ならアイコンを使わない。"
  :type '(choice (directory :tag "directory") (const :tag "nil" nil))
  :group 'navi2ch)


;; Splash screen.
(defcustom navi2ch-splash-display-logo (and window-system
					    (or navi2ch-on-emacs21
						navi2ch-on-xemacs)
					    nil)
  "If it is non-nil, show graphic logo in the startup screen.
You can set it to a symbol `bitmap', `xbm' or `xpm' in order
to force the image format."
  :type '(radio (const :tag "Off" nil)
                (const :tag "On (any format)" t)
                (const xpm)
                (const xbm)
                (const :tag "bitmap (using BITMAP-MULE)" bitmap))
  :group 'navi2ch)

(defcustom navi2ch-display-splash-screen t
  "*Display splash screen at start time."
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-message-samba24-show nil
  "* non-nil なら 連続投稿規制(通称SAMBA24)の経過時間カウントダウンを表示する"
  :type 'boolean
  :group 'navi2ch-message)

;; Mona fonts.
(defgroup navi2ch-mona nil
  "*Navi2ch, モナーフォント

モナーフォントは 2ちゃんねるのアスキーアート (以下 AA) を見るために作
られたフリーのフォントです。

2ちゃんねるのアスキーアートはその多くがプロポーショナルフォントである
\「MS P ゴシック 12pt」を想定してつくられており、 UNIX や Mac の固定幅
フォントで見るとずれてしまいます。モナーフォントはフリーで配布されてい
る東雲 (しののめ) フォントの文字幅を MS P ゴシックに合わせたもので、こ
れを使うと Windows ユーザ向けに作られた AA をズレなしで見ることができ
ます。

                              (http://monafont.sourceforge.net/ より)"
  :prefix "navi2ch-"
  :link '(url-link :tag "モナーフォント ホームページ"
		   "http://monafont.sourceforge.net/")
  :group 'navi2ch
  :load 'navi2ch-mona)

;; folder icons. filename relative to navi2ch-icon-directory
(defvar navi2ch-online-icon "plugged.xpm"
  "*Icon file for online state.")
(defvar navi2ch-offline-icon "unplugged.xpm"
  "*Icon file for offline state.")

;;; hooks
(defvar navi2ch-hook nil)
(defvar navi2ch-exit-hook nil)
(defvar navi2ch-save-status-hook nil)
(defvar navi2ch-load-status-hook nil)
(defvar navi2ch-before-startup-hook nil)
(defvar navi2ch-after-startup-hook nil)
(defvar navi2ch-kill-emacs-hook nil)
(defvar navi2ch-list-mode-hook nil)
(defvar navi2ch-list-exit-hook nil)
(defvar navi2ch-list-after-sync-hook nil)
(defvar navi2ch-list-get-category-list-hook nil)
(defvar navi2ch-board-mode-hook nil)
(defvar navi2ch-board-exit-hook nil)
(defvar navi2ch-board-before-sync-hook nil)
(defvar navi2ch-board-after-sync-hook nil)
(defvar navi2ch-board-select-board-hook nil)
(defvar navi2ch-board-get-subject-list-hook nil)
(defvar navi2ch-article-mode-hook nil)
(defvar navi2ch-article-exit-hook nil)
(defvar navi2ch-article-before-sync-hook nil)
(defvar navi2ch-article-after-sync-hook nil)
(defvar navi2ch-article-arrange-message-hook nil)
(defvar navi2ch-article-get-message-list-hook nil)
(defvar navi2ch-article-next-message-hook nil)
(defvar navi2ch-article-previous-message-hook nil)
(defvar navi2ch-article-hide-message-hook nil)
(defvar navi2ch-article-cancel-hide-message-hook nil)
(defvar navi2ch-article-add-important-message-hook nil)
(defvar navi2ch-article-delete-important-message-hook nil)
(defvar navi2ch-bookmark-mode-hook nil)
(defvar navi2ch-bookmark-exit-hook nil)
(defvar navi2ch-articles-mode-hook nil)
(defvar navi2ch-articles-exit-hook nil)
(defvar navi2ch-history-mode-hook nil)
(defvar navi2ch-history-exit-hook nil)
(defvar navi2ch-search-mode-hook nil)
(defvar navi2ch-search-exit-hook nil)
(defvar navi2ch-message-mode-hook nil)
(defvar navi2ch-message-exit-hook nil)
(defvar navi2ch-message-before-send-hook nil)
(defvar navi2ch-message-after-send-hook nil)
(defvar navi2ch-message-setup-message-hook nil)
(defvar navi2ch-message-setup-sage-message-hook nil)
(defvar navi2ch-bm-mode-hook nil)
(defvar navi2ch-bm-select-board-hook nil)
(defvar navi2ch-bm-exit-hook nil)
(defvar navi2ch-popup-article-mode-hook nil)
(defvar navi2ch-popup-article-exit-hook nil)
(defvar navi2ch-head-mode-hook nil)
(defvar navi2ch-head-exit-hook nil)
(defvar navi2ch-mona-setup-hook nil)
(defvar navi2ch-mona-undo-setup-hook nil)
(defvar navi2ch-directory-mode-hook nil)
(defvar navi2ch-directory-exit-hook nil)
(defvar navi2ch-auto-modify-save-hook nil)

;; load hooks
(defvar navi2ch-article-load-hook nil)
(defvar navi2ch-articles-load-hook nil)
(defvar navi2ch-board-misc-load-hook nil)
(defvar navi2ch-board-load-hook nil)
(defvar navi2ch-bookmark-load-hook nil)
(defvar navi2ch-face-load-hook nil)
(defvar navi2ch-head-load-hook nil)
(defvar navi2ch-history-load-hook nil)
(defvar navi2ch-list-load-hook nil)
(defvar navi2ch-message-load-hook nil)
(defvar navi2ch-mona-load-hook nil)
(defvar navi2ch-net-load-hook nil)
(defvar navi2ch-popup-article-load-hook nil)
(defvar navi2ch-search-load-hook nil)
(defvar navi2ch-util-load-hook nil)
(defvar navi2ch-vars-load-hook nil)
(defvar navi2ch-load-hook nil)
(defvar navi2ch-directory-load-hook nil)

;;; errors symbols
(put 'navi2ch-update-failed 'error-conditions '(error navi2ch-errors navi2ch-update-failed))

;;; global keybindings
;; 別の場所の方がいいんかな。
(defvar navi2ch-global-map nil
  "navi2ch のどのモードでも使える keymap。")
(unless navi2ch-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-e" 'navi2ch-disabled-key) ; Navi2ch 内では無効に
    (define-key map "\C-c\C-f" 'navi2ch-find-file)
    ;; (define-key map "\C-c\C-g" 'navi2ch-list-goto-board)
    (define-key map "\C-c\C-t" 'navi2ch-toggle-offline)
    (define-key map "\C-c\C-u" 'navi2ch-goto-url)
    (define-key map "\C-c\C-v" 'navi2ch-version)
    ;; (define-key map "\C-c1" 'navi2ch-one-pane)
    ;; (define-key map "\C-c2" 'navi2ch-two-pane)
    ;; (define-key map "\C-c3" 'navi2ch-three-pane)
    (define-key map "\C-c\C-o" 'navi2ch-message-jump-to-message-buffer)
    (define-key map "\C-c\C-n" 'navi2ch-article-forward-sticky-buffer)
    (define-key map "\C-c\C-p" 'navi2ch-article-backward-sticky-buffer)
    (define-key map "\C-c\C-l" 'navi2ch-be2ch-toggle-login)
    (setq navi2ch-global-map map)))

(defvar navi2ch-global-view-map nil
  "navi2ch の message モード以外で使える keymap。")
(unless navi2ch-global-view-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-map)
    (suppress-keymap map t)
    (define-key map "1" 'navi2ch-one-pane)
    (define-key map "2" 'navi2ch-two-pane)
    (define-key map "3" 'navi2ch-three-pane)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'navi2ch-end-of-buffer)
    (define-key map "B" 'navi2ch-bookmark-goto-bookmark)
    (define-key map "g" 'navi2ch-list-goto-board)
    (define-key map "G" 'navi2ch-list-goto-board)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "t" 'navi2ch-toggle-offline)
    (define-key map "V" 'navi2ch-version)
    (define-key map "\C-x\C-s" 'navi2ch-save-status)
    (setq navi2ch-global-view-map map)))

(run-hooks 'navi2ch-vars-load-hook)
;;; navi2ch-vars.el ends here
