;;; navi2ch-vars.el --- User variables for navi2ch.

;; Copyright (C) 2001 by 2ちゃんねる

;; Author: (not 1)
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

(defgroup navi2ch nil
  "*Navigator for 2ch."
  :prefix "navi2ch-"
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
(defcustom navi2ch-ask-when-exit t
  "*終了時に本当に終わるか聞くかどうか。
`non-nil' なら聞く"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-directory "~/.navi2ch"
  "*navi2ch のキャッシュなどを置くディレクトリ"
  :type 'directory
  :group 'navi2ch)

(defcustom navi2ch-uudecode-program "uudecode"
  "*uudecode のプログラム名"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-uudecode-args nil
  "*uudecode を実行するときの引数"
  :type '(repeat :tag "引数" string)
  :group 'navi2ch)

(defcustom navi2ch-init-file (expand-file-name "init.el"
					       navi2ch-directory)
  "*navi2ch の初期化ファイル"
  :type 'file
  :group 'navi2ch)

(defcustom navi2ch-enable-readcgi t
  "*read.cgi の raw mode を使ってファイルを取ってくるかどうか。
non-nil なら read.cgi を使う"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-enable-readcgi-host-list nil
  "*read.cgi の raw mode を使ってファイルを取ってくるホストのリスト。
`navi2ch-enable-readcgi' が nil の時に有効"
  :type '(repeat (string :tag "ホスト"))
  :group 'navi2ch)

(defcustom navi2ch-disable-readcgi-host-list nil
  "*read.cgi の raw mode を使わないでファイルを取ってくるホストのリスト。
`navi2ch-enable-readcgi' が nil の時に有効"
  :type '(repeat (string :tag "ホスト"))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-program nil
  "*`navi2ch-browse-url-image'で使われるプログラム名"
  :type '(choice string (const :tag "None" nil))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-args nil
  "*`navi2ch-browse-url-image-program'の引数。"
  :type '(repeat (string :tag "Argument"))
  :group 'navi2ch)

(defcustom navi2ch-browse-url-image-extentions '("jpg" "jpeg" "gif" "png")
  "*`navi2ch-browse-url-image'を使う拡張子"
  :type '(repeat (string :tag "拡張子"))
  :group 'navi2ch)

(defcustom navi2ch-base64-fill-column 64
  "*base64でエンコードされた文字列を何文字でfillするか。"
  :type 'integer
  :group 'navi2ch)

(defcustom navi2ch-2ch-host-list
  '("cocoa.2ch.net")
  "*2ch とみなす host のリスト。"
  :type '(repeat (string :tag "ホスト"))
  :group 'navi2ch)

;;; list variables
(defcustom navi2ch-list-window-width 20
  "*list window の幅"
  :type 'integer
  :group 'navi2ch-list)

(defcustom navi2ch-list-etc-file-name "etc.txt"
  "*その他カテゴリに入れる板を書いておくファイル!"
  :type 'file
  :group 'navi2ch-list)

(defcustom navi2ch-list-stay-list-window nil
  "*板を選んだときに list window を表示したままにするか。
`non-nil' なら表示したままにする。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-bbstable-url "http://www.2ch.net/newbbsmenu.html"
  "*bbstable の url"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-init-open-category nil
  "*最初から全てのカテゴリを開くかどうか。
`non-nil' で全て開く。"
  :type 'boolean
  :group 'navi2ch-list)

(defcustom navi2ch-list-indent-width 2
  "*板名のインデント幅"
  :type 'integer
  :group 'navi2ch-list)

(defcustom navi2ch-list-etc-category-name "その他"
  "*その他カテゴリの名前"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-global-bookmark-category-name "ブックマーク"
  "*ブックマークカテゴリの名前"
  :type 'string
  :group 'navi2ch-list)

(defcustom navi2ch-list-sync-update-on-boot t
  "*navi2ch 起動時に板一覧を取りに行くか。
`nil' にすると s しないかぎり取りに行かない。"
  :type 'boolean
  :group 'navi2ch-list)

;;; board variables
(defcustom navi2ch-board-max-line nil
  "*ダウンロードする subject.txt の行数。
nil なら全部ダウンロードする"
  :type '(choice (integer :tag "行数を指定")
		 (const :tag "全て" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-board-expire-date 30
  "*最後に変更されてからこの日数以上たったファイルは expire する
nil なら expire しない"
  :type '(choice (integer :tag "日数を指定")
		 (const :tag "expire しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-board-window-height 10
  "*board window の高さ"
  :type 'integer
  :group 'navi2ch-board)

(defcustom navi2ch-board-check-updated-article-p t
  "*新しいレスがあったかチェックするかどうか"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-board-view-logo-program
  (if (eq window-system 'w32)
      "fiber"
    "xv")
  "*ロゴを見るのに使うプログラム"
  :type 'file
  :group 'navi2ch-board)

(defcustom navi2ch-board-view-logo-args nil
  "*ロゴを見るのに使うプログラムの引数"
  :type '(repeat (string :tag "引数"))
  :group 'navi2ch-board)

(defcustom navi2ch-board-delete-old-logo t
  "*新しいロゴをダウンロードしたときに古いロゴを消すかどうか"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-bm-subject-width 50
  "*各スレの題名の幅"
  :type 'integer
  :group 'navi2ch-board)

(defcustom navi2ch-bm-mark-and-move t
  "*マークした後に移動するかどうか
nil なら移動しない
non-nil なら下に移動する
'follow なら以前移動した方向に移動する"
  :type '(choice (const :tag "移動しない" nil)
		 (const :tag "下に移動" t)
		 (const :tag "以前移動した方向に移動" follow))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-empty-subject "navi2ch: no subject"
  "*subject が無いときに代り表示する subject"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-history-max-line 100
  "*ヒストリの最大の行数
nil ならば制限しない"
  :type '(choice (integer :tag "最大の行数を指定")
		 (const :tag "制限しない" nil))
  :group 'navi2ch-board)

(defcustom navi2ch-bm-stay-board-window t
  "*スレを選んだときに board window を表示したままにするか。
`non-nil' なら表示したままにする"
  :type 'boolean
  :group 'navi2ch-board)

(defcustom navi2ch-bm-fetched-info-file (expand-file-name "fetched.txt"
							  navi2ch-directory)
  "*すでに読んだスレを保存するファイル"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-bookmark-file (expand-file-name "bookmark2.txt"
						   navi2ch-directory)
  "*グローバルブックマークを保存するファイル"
  :type 'string
  :group 'navi2ch-board)

(defcustom navi2ch-history-file (expand-file-name "history.txt"
						  navi2ch-directory)
  "*ヒストリを保存するファイル"
  :type 'string
  :group 'navi2ch-board)


;;; article variables
(defcustom navi2ch-article-aadisplay-program
  (if (eq window-system 'w32)
      "notepad"
    "aadisplay")
  "*aa を見るのに使うプログラム名"
  :type 'string
  :group 'navi2ch-article)

(defcustom navi2ch-article-aadisplay-coding-system
  (if (eq window-system 'w32)
      'shift_jis-dos
    'euc-jp-unix)
  "*navi2ch-article-aadisplay-program 用の一時ファイルの coding-system"
  :type 'symbol
  :group 'navi2ch-article)
  
(defcustom navi2ch-article-view-aa-function
  (if (eq window-system 'w32)
      'navi2ch-article-popup-dialog
    'navi2ch-article-call-aadisplay)
  "*aa を見るのに使う関数名"
  :type 'function
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-diff t
  "*差分を取ってくるかどうか。nil なら常に差分を取ってこない"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-max-line nil
  "*ダウンロードする記事の行数。
nil なら差分全てをダウンロードする。"
  :type '(choice (integer :tag "件数を指定")
		 (const :tag "全て" nil))
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-fill nil
  "*fill-region するかどうか"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-enable-fill-list nil
  "*fill-region する 板のリスト"
  :type '(repeat string)
  :group 'navi2ch-article)

(defcustom navi2ch-article-disable-fill-list nil
  "*fill-region しない板のリスト"
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
  "*parse するフィールドのリスト。
遅くてもいいんなら '(data mail name) とかするといいかも"
  :type '(set (const :tag "記事" data)
	       (const :tag "メール" mail)
	       (const :tag "名前" name))
  :group 'navi2ch-article)

(defcustom navi2ch-article-goto-number-recenter t
  "*goto-number したときに recenter するかどうか"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-new-message-range '(100 . 1)
  "*新しい スレッドを取ってきたときの表示する範囲"
  :type '(cons integer integer)
  :group 'navi2ch-article)

(defcustom navi2ch-article-exist-message-range '(1 . 100)
  "*すでにあるスレッドを取ってきたときの表示する範囲"
  :type '(cons integer integer)
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-range t
  "*また表示してないスレッドを取ってきたときに勝手に範囲を狭めるか。
non-nil で狭める"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-view-range-list
  '((1 . 50)
    (50 . 50)
    (1 . 100)
    (100 . 100))
  "*表示するスレッドの範囲を選択するときに使うリスト"
  :type '(repeat (cons integer integer))
  :group 'navi2ch-article)
  
(defcustom navi2ch-article-header-format-function
  'navi2ch-article-default-header-format-function
  "*NUMBER NAME MAIL DATE を引数に取り、レスのヘッダを返す関数"
  :type 'function
  :group 'navi2ch-article)

(defcustom navi2ch-article-citation-regexp
  "^[>＞]\\($\\|[^$>＞0-9０-９].*\\)"
  "*引用部分の正規表現"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-number-regexp
  "[>＞][>＞]?\\(\\([0-9０-９]+,\\)*[0-9０-９]+\\(-[0-9０-９]+\\)?\\)"
  "*同じスレ内へのリンクを表わす正規表現"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-url-regexp
  "h?ttps?://\\([-a-zA-Z0-9_=?#$@~`%&*+|\\/.,:]+\\)"
  "*url を表わす正規表現"
  :type 'regexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-filter-list nil
  "*スレッドの記事をいじるフィルターの list。
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
  "*`navi2ch-article-goto-number' で、範囲外ならば、redraw しなおすかどうか。
non-nil なら redraw しなおす。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-fix-range-diff 10
  "*`navi2ch-article-fix-range' したときに戻る量"
  :type 'integer
  :group 'navi2ch-article)

(defcustom navi2ch-article-fix-range-when-sync t
  "*`navi2ch-article-sync' で範囲外ならば `navi2ch-article-view-range' を変更するか。
non-nil なら範囲内に変更する"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator ?_
  "*レスとレスの区切りに使う文字。"
  :type 'character
  :group 'navi2ch-article)

(defcustom navi2ch-article-message-separator-width '(/ (window-width) 2)
  "*レスとレスの区切り文字の幅。
幅を 80 文字分にしたいなら
\(setq navi2ch-article-message-separator-width 80)
window の幅と同じにしたいなら
\(setq navi2ch-article-message-separator-width '(window-width))
等指定する。"
  :type 'sexp
  :group 'navi2ch-article)

(defcustom navi2ch-article-auto-expunge nil
  "*スレを開いた時に自動的に古いバッファを消すか。
`non-nil' なら navi2ch-article-max-buffers 以上にならないようにする。"
  :type 'boolean
  :group 'navi2ch-article)

(defcustom navi2ch-article-max-buffers 20
  "*バッファとして保持するスレの最大数。
0 ならば無制限。"
  :type 'integer
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-white-space-after-old-br t
  "*`non-nil' の場合、<br> の後にある空白を取り除く。
ただし、すべての <br> の直後に空白がある場合のみ。"
  :type 'integer
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-trailing-whitespace t
  "*`non-nil' の場合、各行の末尾の空白を取り除く。"
  :type 'integer
  :group 'navi2ch-article)

(defcustom navi2ch-article-cleanup-trailing-blankline t
  "*`non-nil' の場合、各レスの末尾の空行を取り除く。"
  :type 'interger
  :group 'navi2ch-article)

;;; message variables
(defcustom navi2ch-message-user-name
  (if (featurep 'xemacs)
      "名無しさん＠ＸＥｍａｃｓ"
    "名無しさん＠Ｅｍａｃｓ")
  "*名前"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-user-name-alist
  '(("network" . "anonymous")
    ("tv" . "名無しさん"))
  "*板ごとのデフォルトの名前の alist"
  :type '(repeat (cons string string))
  :group 'navi2ch-message)

(defcustom navi2ch-message-mail-address nil
  "*デフォルトのメールアドレス"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-send t
  "*送信する前に確認するか。
`non-nil' なら確認する"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-ask-before-kill t
  "*書きこみをキャンセルする前に確認するか
`non-nil' なら確認する"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-always-pop-message nil
  "*書きかけの message を常に復元するかどうか
`non-nil' なら復元する"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-wait-time 1
  "*送った後 sync する前に待つ時間(秒)"
  :type 'integer
  :group 'navi2ch-message)

(defcustom navi2ch-message-remember-user-name t
  "*送った後 `navi2ch-message-user-name' を送ったメールアドレスに変更するか。
`non-nil' なら変更する"
  :type 'boolean
  :group 'navi2ch-message)

(defcustom navi2ch-message-cite-prefix "> "
  "*引用するときの接頭辞"
  :type 'string
  :group 'navi2ch-message)

(defcustom navi2ch-message-trip nil
  "*trip 用の文字列。
書きこみ時に From の後ろに付加される。"
  :type '(choice (string :tag "trip を指定")
		 (const :tag "trip を指定しない" nil))
  :group 'navi2ch-message)

(defcustom navi2ch-message-aa-prefix-key "\C-c\C-a"
  "*aa を入力する為の prefix-key。"
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
  "*aa を入力するときの keyと aa の alist。
message mode で prefix-key key と入力する事で aa を入力できる。"
  :type '(repeat (cons string string))
  :group 'navi2ch-message)

;; net variables
(defcustom navi2ch-net-http-proxy
  (if (string= (getenv "HTTP_PROXY") "")
      nil
    (getenv "HTTP_PROXY"))
  "*Proxy Server の url"
  :type '(choice (string :tag "proxy を指定")
		 (const :tag "proxy を使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-userid nil
  "Proxy 認証に使うユーザ名。"
  :type '(choice (string :tag "ユーザ名を指定")
		 (const :tag "ユーザ名を使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-http-proxy-password nil
  "Proxy 認証に使うパスワード。"
  :type '(choice (string :tag "パスワードを指定")
		 (const :tag "パスワードを使わない" nil))
  :group 'navi2ch-net)

(defcustom navi2ch-net-force-update nil
  "*更新があったかを確認せずに更新するか。
`non-nil' ならば確認しない"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-check-margin 100
  "*あぼーんがあったか確認する為のバイト数"
  :type 'integer
  :group 'navi2ch-net)

(defcustom navi2ch-net-turn-back-step 1000
  "*あぼーんがあったときに途中から読み直す為のバイト数。
日本語変だな(汗)。"
  :type 'integer
  :group 'navi2ch-net)

(defcustom navi2ch-net-turn-back-when-aborn t
  "*あぼーんがあったとき途中から読み直すか。
`non-nil'なら読み直す"
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
  "*`inherit-process-coding-system' の navi2ch での束縛値"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-accept-gzip t
  "*Accept-Encoding: gzip を付加するかどうか。
non-nil なら付加する。"
  :type 'boolean
  :group 'navi2ch-net)

(defcustom navi2ch-net-gunzip-program "gzip"
  "*gunzip のプログラム名。"
  :type 'file
  :group 'navi2ch-net)

(defcustom navi2ch-net-gunzip-args '("-d" "-c")
  "*gunzip を呼出すときの引数。"
  :type '(repeat :tag "引数" string)
  :group 'navi2ch-net)

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
    (define-key map "\C-c\C-g" 'navi2ch-list-goto-board)
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
    ;; (define-key map "2" 'navi2ch-two-pane)
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
