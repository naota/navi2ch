;;; navi2ch-vars.el --- User variables for navi2ch.

;; Copyright (C) 2001 by Navi2ch Project

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

(defconst navi2ch-on-xemacs (featurep 'xemacs))
(defconst navi2ch-on-emacs21 (and (not navi2ch-on-xemacs)
                                  (>= emacs-major-version 21)))
(defconst navi2ch-on-emacs20 (and (not navi2ch-on-xemacs)
                                  (= emacs-major-version 20)))

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
  "*Navi2ch, article buffer."
  :prefix "navi2ch-"
  :group 'navi2ch)

(defgroup navi2ch-net nil
  "*Navi2ch, article buffer."
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

(defcustom navi2ch-init-file (concat
                              (file-name-as-directory navi2ch-directory)
                              "init")
  "*navi2ch の初期化ファイル。"
  :type 'file
  :group 'navi2ch)

(defcustom navi2ch-enable-readcgi nil
  "*non-nil なら、ファイル取得に read.cgi の raw mode を利用する。"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-enable-readcgi-host-list nil
  "*read.cgi の raw mode を使ってファイルを取ってくるホストのリスト。
`navi2ch-enable-readcgi' が nil の時に有効。"
  :type '(repeat (string :tag "ホスト"))
  :group 'navi2ch)

(defcustom navi2ch-disable-readcgi-host-list nil
  "*read.cgi の raw mode を使わないでファイルを取ってくるホストのリスト。
`navi2ch-enable-readcgi' が t の時に有効"
  :type '(repeat (string :tag "ホスト"))
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
  '("cocoa.2ch.net")
  "*2ちゃんねるとみなす host のリスト。"
  :type '(repeat (string :tag "ホスト"))
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

(defcustom navi2ch-list-bbstable-url "http://www6.ocn.ne.jp/~mirv/2chmenu.html"
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
  "*ヒストリの行数の制限限。nil ならば制限しない。"
  :type '(choice (integer :tag "最大の行数を指定")
		 (const :tag "制限しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-stay-board-window t
  "*non-nil なら、スレを選んだときにスレ一覧を表示したままにする。"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-bm-fetched-info-file (concat
                                         (file-name-as-directory navi2ch-directory)
                                         "fetched.txt")
  "*既読スレのリストを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-bookmark-file (concat
                                  (file-name-as-directory navi2ch-directory)
                                  "bookmark2.txt")
  "*グローバルブックマークを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-history-file (concat
                                 (file-name-as-directory navi2ch-directory)
                                 "history.txt")
  "*ヒストリを保存しておくファイル。"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-board-expire-bookmark-p nil
  "*expire するときに bookarmk されているスレも expire するかどうか。
non-nil ならば expire する。"
  :type 'boolean
  :group 'navi2ch-board)
  
(defcustom navi2ch-bm-board-name-from-file "From File"
  "*ファイルから読み込んだスレを表わす板名。"
  :type 'string
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
  "*AA を表示するプログラムにわたす一時ファイルの `coding-system'"
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
  "*non-nil なら、また表示してないスレッドの表示範囲を勝手に狭める。"
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

(defcustom navi2ch-article-number-regexp
  "[>＞<＜][>＞<＜ ]*\\(\\([0-9０-９]+,\\)*[0-9０-９]+\\(-[0-9０-９]+\\)?\\)"
  "*同じスレ内へのリンクを表わす正規表現。"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-url-regexp
  "h?ttps?://\\([-a-zA-Z0-9_=?#$@~`%&*+|\\/.,:]+\\)"
  "*レスのテキストのうち URL とみなす部分の正規表現。"
  :type 'regexp
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
        ))"
  :type '(repeat sexp)
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

(defcustom navi2ch-article-auto-decode-base64-p nil
  "*non-nil なら、スレの BASE64 でエンコードされたテキストを自動展開する。"
  :type 'boolean
  :group 'navi2ch-article)

;;; message variables
(defcustom navi2ch-message-user-name
  (cond ((featurep 'xemacs) "名無しさん＠ＸＥｍａｃｓ")
	((featurep 'meadow) "名無しさん＠Ｍｅａｄｏｗ")
	(t "名無しさん＠Ｅｍａｃｓ"))
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

(defcustom navi2ch-message-mail-address nil
  "*デフォルトのメールアドレス。"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-send t
  "*non-nil なら、書き込み送信の確認メッセージを表示する。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-kill t
  "*non-nil なら、書きこみキャンセルの確認メッセージを表示する。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-always-pop-message nil
  "*non-nil なら、新規メッセージを作るコマンドは書きかけのレスを常に復元する。
nil なら、書きかけを破棄していいか問い合わせる。
書きかけのメッセージのバッファが残っている場合にだけ有効。"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-wait-time 1
  "*レスを送ったあとスレをリロードするまでの待ち時間(秒)。"
  :type 'integer
  :group 'navi2ch-message)

(defcustom navi2ch-message-remember-user-name t
  "*non-nilなら、送ったレスのメールアドレス覧を覚えておく。
同じスレで次にレスするときは、それがデフォルトのメールアドレスになる。"
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

(defcustom navi2ch-message-aa-alist
  '(("a" . "(´Д｀)")
    ("b" . "ヽ(`Д´)ﾉ")
    ("f" . "( ´_ゝ`)ﾌｰﾝ")
    ("F" . "(´ー｀)")
    ("g" . "(ﾟДﾟ)ｺﾞﾙｧ")
    ("G" . "ｶﾞ━━(ﾟДﾟ;)━━ｿ!")
    ("h" . "(ﾟДﾟ)ﾊｧ?")
    ("H" . "(;´Д｀)ﾊｧﾊｧ")
    ("i" . "(･∀･)ｲｲ!!")
    ("j" . "(･∀･)ｼﾞｻｸｼﾞｴﾝﾃﾞｼﾀ")
    ("k" . "ｷﾀ━━━━━━(ﾟ∀ﾟ)━━━━━━ !!!!!")
    ("m" . "(´∀｀)")
    ("M" . "ヽ(´▽｀)ﾉ")
    ("n" . "(￣ー￣)ニヤリッ")
    ("N" . "(´-`).｡ｏＯ(なんでだろう？)")
    ("p" . "（　ﾟдﾟ）ﾎﾟｶｰﾝ")
    ("s" . "Σ（ﾟдﾟlll）ｶﾞｰﾝ")
    ("u" . "(ﾟдﾟ)ｳﾏｰ")
    ("U" . "(-＿-)ｳﾂﾀﾞ"))
  "*AA を入力するときのキーバインドと AA の alist。
message mode で prefix-key key と入力する事で AA を入力できる。"
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
  "プロキシ認証に使うユーザ名。"
  :type '(choice (string :tag "ユーザ名を指定")
		 (const :tag "ユーザ名を使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-password nil
  "プロキシ認証に使うパスワード。"
  :type '(choice (string :tag "パスワードを指定")
		 (const :tag "パスワードを使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-send-message-use-http-proxy t
  "*non-nil なら、レスを送る場合などでもプロキシを経由する。
このオプションを有効にするには、`navi2ch-net-http-proxy' を non-nil
に設定すること。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-force-update nil
  "*non-nil なら、ファイルを取得するまえに更新の有無を確認しなくなる。
nil なら、更新されていないファイルの不必要な転送はしない。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-check-margin 100
  "*あぼーんがあったか確認する為のバイト数。"
  :type 'integer
  :group 'navi2ch-net)

(defcustom navi2ch-net-turn-back-step 1000
  "*あぼーんがあったときに途中から読み直す為のバイト数。日本語変だな(汗)。"
  :type 'integer
  :group 'navi2ch-net)

(defcustom navi2ch-net-turn-back-when-aborn t
  "*non-nil なら、あぼーんがあったときスレを途中から読み直す。"
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

;;; update variables
(defcustom navi2ch-update-file (concat
                                (file-name-as-directory navi2ch-directory)
                                "navi2ch-update.el")
  "*Navi2ch の自動更新に利用するファイルのローカルファイル名。"
  :type 'string
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

(defcustom navi2ch-auto-update t
  "*non-nil なら、起動時に `navi2ch-update-file' を更新して実行する。
ファイルが実行されるのは、
 - `navi2ch-update-file' が更新されていて、
 - そこで表示される確認するメッセージに yes と答えたとき
のみ。

ヤバいコードが入っているとまずいので、実行する前にまず navi2ch の
スレなどを確認したほうがいい。"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-icon-directory
  (cond ((fboundp 'locate-data-directory)
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
(defcustom navi2ch-splash-display-logo (when (or (featurep 'xemacs)
                                                 (featurep 'image)
                                                 (featurep 'bitmap))
                                         t)
  "If it is T, show graphic logo in the startup screen.  You can set it to
a symbol `bitmap', `xbm' or `xpm' in order to force the image format."
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

;; Mona fonts.
(when (or navi2ch-on-xemacs navi2ch-on-emacs21)
  (defgroup navi2ch-mona nil
    "*Navi2ch, モナーフォント

Mona fonts (モナーフォント) は 2ちゃんねるのアスキーアート (以下 AA) を
X11 上で見るために作られたフリーのフォントです。

2ちゃんねるのアスキーアートはその多くが MS P ゴシック 12pt を
想定してつくられており、 X の固定幅フォントを使った Netscape 等で見ると
ずれてしまいます。 モナーフォントはフリーで配布されている
東雲 (しののめ) フォントの文字幅を MS P ゴシックに合わせたもので、
これを使うと Windows ユーザ向けに作られた AA を正しく見ることができます。

                   (http://members.tripod.co.jp/s42335/mona/ より)"
    :prefix "navi2ch-"
    :link '(url-link :tag "モナーフォント ホームページ"
                     "http://members.tripod.co.jp/s42335/mona/")
    :group 'navi2ch
    :load 'navi2ch-mona)

  (defcustom navi2ch-mona-enable nil
    "*non-nil なら、モナーフォントを使ってスレを表示する。"
    :set (function (lambda (symbol value)
                     (if value
                         (navi2ch-mona-setup)
                       (navi2ch-mona-undo-setup))
                     (set-default symbol value)))
    :initialize 'custom-initialize-default
    :type 'boolean
    :group 'navi2ch-mona)

  (when navi2ch-mona-enable
    (add-hook 'navi2ch-load-hook
              (lambda () (load "navi2ch-mona")))))

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
(defvar navi2ch-list-mode-hook nil)
(defvar navi2ch-list-exit-hook nil)
(defvar navi2ch-list-after-sync-hook nil)
(defvar navi2ch-board-mode-hook nil)
(defvar navi2ch-board-exit-hook nil)
(defvar navi2ch-board-before-sync-hook nil)
(defvar navi2ch-board-after-sync-hook nil)
(defvar navi2ch-board-select-board-hook nil)
(defvar navi2ch-article-mode-hook nil)
(defvar navi2ch-article-exit-hook nil)
(defvar navi2ch-article-before-sync-hook nil)
(defvar navi2ch-article-after-sync-hook nil)
(defvar navi2ch-article-arrange-message-hook nil)
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

;;; errors symbols
(put 'navi2ch-update-failed 'error-conditions '(error navi2ch-errors navi2ch-update-failed))

;;; global keybindings
;; 別の場所の方がいいんかな。
(defvar navi2ch-global-map nil
  "navi2ch のどのモードでも使える keymap。")
(unless navi2ch-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" 'navi2ch-article-find-file)
    ;; (define-key map "\C-c\C-g" 'navi2ch-list-goto-board)
    (define-key map "\C-c\C-t" 'navi2ch-toggle-offline)
    (define-key map "\C-c\C-u" 'navi2ch-goto-url)
    (define-key map "\C-c\C-v" 'navi2ch-version)
    ;; (define-key map "\C-c1" 'navi2ch-one-pane)
    ;; (define-key map "\C-c2" 'navi2ch-two-pane)
    ;; (define-key map "\C-c3" 'navi2ch-three-pane)
    (setq navi2ch-global-map map)))

(defvar navi2ch-global-view-map nil
  "navi2ch の message モード以外で使える keymap。")
(unless navi2ch-global-view-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-map)
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
    (setq navi2ch-global-view-map map)))

(run-hooks 'navi2ch-vars-load-hook)
;;; navi2ch-vars.el ends here
