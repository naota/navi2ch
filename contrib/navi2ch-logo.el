;;; navi2ch-logo.el --- Inline logo module for navi2ch

;; Copyright (C) 2002 by navi2ch Project

;; Author:
;;   (not 1)
;;   http://pc.2ch.net/test/read.cgi/unix/999166513/895 の名無しさん
;; Keywords: 2ch, network

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
;; レス一覧の先頭にその板のロゴを貼りつける。
;;
;; Emacs 21 なら navi2ch-logo.elc? に load-path を通してこうしる！
;;
;; (require 'navi2ch-logo)
;; (add-hook 'navi2ch-hook 'navi2ch-logo-init)
;;
;; いまのところ外部プログラム `gifsicle' と `convert' が必須。
;;
;; gifsicle はアニメーション GIF の非アニメーション化に使う。
;; convert は各種画像フォーマットを問答無用に XPM にコンバート
;; するために使ってる。ハードコードしているのは直さなくちゃ。
;;
;; 板一覧の読み込みは、ロゴを持ってくるぶん当然遅くなる。
;; ファイル取得は wget とかに任せて非同期化すればいいのかも。
;;

;;; Code:
(provide 'navi2ch-logo)

(eval-when-compile (require 'cl))

(require 'navi2ch-board)
(require 'navi2ch-board-misc)
(require 'navi2ch-net)

(defvar navi2ch-logo-temp-directory nil
  "コンバートした画像ファイルを入れるテンポラリディレクトリ。")
(defvar navi2ch-logo-temp-directory-prefix ".navi2ch-logo-"
  "テンポラリティレクトリのサフィックス。これにランダムな文字列を
足したものがディレクトリ名になる")
(defvar navi2ch-logo-temp-name-prefix "img-"
  "テンポラリファイルのサフィックス。")
(defvar navi2ch-logo-image-alist nil
  "\`(板のid  その板のロゴのimage)\' からなる alist。
いちど create-image した画像はここに追加して再利用する。")
(defvar navi2ch-logo-previous-image nil)

(defun navi2ch-logo-init ()
  "navi2ch-logo を初期化して使えるようにする。"
  (when (and navi2ch-on-emacs21 (not navi2ch-logo-temp-directory))
    (setq navi2ch-logo-temp-directory
          (file-name-as-directory
           (make-temp-file navi2ch-logo-temp-directory-prefix t)))
    (add-hook 'navi2ch-exit-hook 'navi2ch-logo-cleanup)
    (add-hook 'navi2ch-bm-select-board-hook 'navi2ch-logo-update)
    (add-hook 'navi2ch-board-after-sync-hook 'navi2ch-logo-update)))

(defun navi2ch-logo-cleanup ()
  "テンポラリファイルの後始末などをして、変数を初期値に戻す。"
  (when (and navi2ch-logo-temp-directory
             (file-directory-p navi2ch-logo-temp-directory))
    (dolist (file (directory-files navi2ch-logo-temp-directory t))
      (and (file-regular-p file)
           (delete-file file)))
    (delete-directory navi2ch-logo-temp-directory))
  (setq navi2ch-logo-temp-directory nil
        navi2ch-logo-image-alist nil))

(defun navi2ch-logo-update ()
  "`navi2ch-board-mode' で動作し、ロゴを読み込んでバッファ上部に貼り付ける。
`navi2ch-board-select-board-hook' から呼ばれる。"
  (if (eq major-mode 'navi2ch-board-mode)
      (let* ((id (cdr (assq 'id navi2ch-board-current-board)))
             (image (cdr (assoc id navi2ch-logo-image-alist))))
        (if (eq image t)
            (navi2ch-logo-remove-image (point-min))
          (when (and (not image) (not navi2ch-offline))
            (condition-case err
                (catch 'quit
                  (setq image (navi2ch-logo-create-logo-image)))
              (t nil))
            (setq navi2ch-logo-image-alist
                  (navi2ch-put-alist id (or image t) navi2ch-logo-image-alist)))
          (when (and image (not (eq image navi2ch-logo-previous-image)))
            (navi2ch-logo-remove-image (point-min))
            (navi2ch-logo-put-image (point-min) image)
            (setq navi2ch-logo-previous-image image))))
    (navi2ch-logo-remove-image (point-min))))

(defun navi2ch-logo-put-image (point image)
  "POINT 位置に IMAGE を貼り付ける。

IMAGE を直接バッファテキストの display property にするのはマズい。
なぜならこの画像は文字とは独立したものだから。

そこで、
 (1) まず POINT 位置に長さ 0 のオーバーレイを作る
 (2) 適当な文字列の text property (の `display' property) に IMAGE を
     指定して、テキストとして扱えるようにする
 (3) (1) で作ったオーバーレイの `before-string' 属性に、(2) の文字列を
     指定する
という手順を踏んでいる。"
  (let ((overlay (make-overlay point point))
        (str (propertize (concat (propertize " " 'display image)
                                 "\n")
                         'face 'default)))
    ;; 画像の上へのポイント移動を禁止。
    (overlay-put overlay 'intangible t)

    ;; `face' に `default' を指定して、近くの文字のテキスト
    ;; プロパティの underline や stroke の影響を排除。
    (overlay-put overlay 'face 'default)  

    ;; navi2ch-logo が作ったということがわかるように。
    (overlay-put overlay 'navi2ch-logo t) 

    (overlay-put overlay 'before-string str)))

(defun navi2ch-logo-remove-image (&optional point)
  "`navi2ch-logo-put-image' が置いた画像を POINT 位置から消す。
消すべき画像がなければ何もしない。"
  (unless point
    (setq point (point-min)))
  (let ((ls (overlays-in point point))
        overlay)
    (while (and ls (not overlay))
      (when (overlay-get (car ls) 'navi2ch-logo)
        (setq overlay (car ls)))
      (setq ls (cdr ls)))
    (when overlay
      (delete-overlay overlay))))

(defun navi2ch-logo-create-logo-image ()
  "ロゴをダウンロードして `create-image' した結果を返す。

外部プログラム \`gifsicle\' はアニメーション GIF を非アニメ化
するために使う。\`convert\' は画像を一律に XPM に変換するために使ってる。

Emacs は XPM 以外ももちろんサポートしてるから、本来は全部 XPM に
することはない。`image-types' を参照するなり `create-image' の
返り値を見るなりして、外部プログラムの起動は必要最少限にとどめた
ほうがいい。"
  (let ((logo-file (navi2ch-net-download-logo navi2ch-board-current-board))
        (xpm-file (concat navi2ch-logo-temp-directory
                          (make-temp-name navi2ch-logo-temp-name-prefix)
                          ".xpm"))
        temp-file)
    (unless logo-file (throw 'quit nil))
    (when (string-match "\\.gif$" logo-file)
      (setq temp-file (concat navi2ch-logo-temp-directory
                              (make-temp-name navi2ch-logo-temp-name-prefix)
                              ".gif"))
      (when (/= 0 (call-process "gifsicle" logo-file nil nil
                                "#-1" "--output" temp-file))
        (throw 'quit nil))
      (setq logo-file temp-file))
    (when (/= 0 (call-process "convert" nil nil nil
                              "-border" "1x1"
                              "-bordercolor" "black"
                              logo-file xpm-file))
      (throw 'quit nil))
    (if temp-file (delete-file temp-file))
    (create-image xpm-file 'xpm)))

;;; navi2ch-logo.el ends here
