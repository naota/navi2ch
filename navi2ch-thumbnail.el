;;; navi2ch-thumbnail.el --- thumbnail view for navi2ch

;; Copyright (C) 2010 by Navi2ch Project

;; Authors: MIZUNUMA Yuto <mizmiz@users.sourceforge.net>
;; Keywords: network 2ch

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

;; サムネイルを表示する機能です
;; 画像表示に対応したemacsで動きます

;; 画像リンクURL上で','を押すとサムネイル画像を挿入表示します。自動取得、
;; 自動表示はしません。基本的にキーで駆動です。キャッシュを持っている画
;; 像は自動表示されます。キャッシュの自動削除機能はありません。基本的に
;; コマンドはImageMagick依存です。
;;
;; 参考にしたコード(navi2chスレのどこかで見た)は非同期だったり外部シェ
;; ル叩きだったりと複雑なので、なるべくシンプルに再構成しました

;; To Do
;; - どこか(全体?)を(display-images-p)で囲むべきだが要検討
;; - キーバインド調整
;; - スレ再描画時にサムネを読まなかったり読んだりがあるかも

;; 設定例
;; Windows
;;   (setq navi2ch-thumbnail-image-convert-program
;;         "C:/Program Files/ImageMagick-6.2.8-Q16/convert.exe")
;;   (setq navi2ch-thumbnail-image-identify-program
;;         "C:/Program Files/ImageMagick-6.2.8-Q16/identify.exe")
;;   (setq navi2ch-browse-url-image-program "c:/win/iview425j/i_view32.exe") ;; IrfanView
;; MacOSX
;;   (setq navi2ch-browse-url-image-program "/Applications/Preview.app/Contents/MacOS/Preview")
;;   (setq navi2ch-thumbnail-image-convert-program "/opt/local/bin/convert") ;; MacPort ImageMagick
;;   (setq navi2ch-thumbnail-image-identify-program "/opt/local/bin/identify")

;; 使い方、兼キーバインド
;;
;; URLにカーソルがある状態で','を押すとサムネイル挿入. サムネイルにカー
;; ソルがある状態で','を押すと外部ビューアーでオリジナル画像表示(本当は
;; enterキーでやるほうが奇麗な気もする)
;;
;; サムネイルにカーソルがある状態で'v'で画像を保存(サムネイルではなく、
;; 元の大きい画像)
;;
;; Esc+EnterでURLをブラウザで開く(既存機能に丸投げ)。画像ビューアーが指
;; 定されてると、そのURLを開くのでリモートなファイルを開けるビューアー
;; が必要(元々その動作)
;;
;; サムネイルにカーソルがある状態で'D'を押すとキャッシュ画像を削除. 既
;; にキーバインドがダブってるが、分かりやすさでオーバーライド(要検討)
;;
;; 'T'を押すとカーソルがあるレス1個のレス内のURLを全取得

;;; Code

(provide 'navi2ch-thumbnail)

(defcustom navi2ch-thumbnail-thumbnail-directory (concat navi2ch-directory "/navi2ch-thumbnails/")
  "* 画像キャッシュディレクトリ"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-save-content-dir "~/"
  "* 画像保存時のディフォルトディレクトリ"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-image-convert-program (executable-find "convert")
  "* サムネイル作成プログラム"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-image-identify-program (executable-find "identify")
  "* サムネイル画像判別プログラム"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-thumbsize-width 300
  "* サムネイル表示サイズ横(等倍縮小でアスペクト比保持)"
  :type 'integer
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-thumbsize-height 150
  "* サムネイル表示サイズ縦(等倍縮小でアスペクト比保持)"
  :type 'integer
  :group 'navi2ch)

(defun navi2ch-thumbnail-save-content ()
  "キャッシュから画像を保存(サムネイルではなく元画像)"
	(interactive)
	(let ((prop (get-text-property (point) 'navi2ch-link))
	      (default-filename (get-text-property (point) 'file-name))
	      (default-directory-org default-directory)
	      filename)
	  (setq default-directory navi2ch-thumbnail-save-content-dir)
	  (when default-filename
	    (setq default-filename (file-name-nondirectory default-filename)))
	  (setq filename (read-file-name
			  (if default-filename
			      (format "Save file (default `%s'): "
				      default-filename)
			    "Save file: ")
			  nil default-filename))
	  (when (file-directory-p filename)
	    (if default-filename
		(setq filename (expand-file-name default-filename filename))
	      (error "%s is a directory" filename)))
	  (if (not (file-writable-p filename))
	      (error "File not writable: %s" filename)
	    (if (or (not (file-exists-p filename))
		    (y-or-n-p (format "File `%s' exists; overwrite? "
				      filename)))
		(copy-file prop filename t)))
	  (setq default-directory default-directory-org)))

(defun navi2ch-thumbnail-show-image-not-image-url (url &optional force)
  "imepita等のURLが画像っぽくない場合の処理"
  (cond
   ;;imepita
   ((string-match "h?ttp://w*\.?imepita.jp/\\([0-9/]+\\)" url)
    (setq alturl (format "http://imepita.jp/image/%s" (match-string 1 url)))
    (message "imepita:%s %s" url alturl)
    (if (navi2ch-thumbnail-insert-image-cache url)
;    (if (navi2ch-thumbnail-insert-image-cache alturl)
	(message "cache read")
      (if force
	  (progn
	    (setq rtn (navi2ch-thumbnail-show-image alturl url))
	    (message "return %s" rtn)))))
   
   ((string-match "h?ttp://i-bbs.sijex.net/imageDisp.jsp\\?id=watahiki&file=\\([0-9o]+\.jpg\\)" url)
    (message "sjex %s" url)
    (setq alturl (format "http://image.i-bbs.sijex.net/bbs/watahiki/%s" (match-string 1 url)))
    (if (navi2ch-thumbnail-insert-image-cache alturl)
	(message "sijex キャッシュから読み込みました")
      (message "sijex:%s %s" url alturl)
      (if force
	  (navi2ch-thumbnail-show-image alturl url))))
   (t nil)
   ))
  
(defun navi2ch-thumbnail-show-image-external ()
  "外部ビューアーで表示"
  (interactive)
  (let ((type (car (get-text-property (point) 'display)))
	(prop (get-text-property (point) 'navi2ch-link)))
    (when (eq type 'image)
      (if (equal system-type 'windows-nt)
	  (setq prop (navi2ch-replace-string "/" "\\\\" prop t )))
      (navi2ch-browse-url-image prop))))

(defun navi2ch-thumbnail-image-delete-cache ()
  "取得した画像を削除。キャッシュが無くなるの表示されなくなる"
  (interactive)
  (let* ((type (car (get-text-property (point) 'display)))
	 (file (get-text-property (point) 'navi2ch-link))
	 (thumb (concat file ".jpg")))
    (when (file-exists-p file)
      (delete-file file)
      (message "deleting file:%s " file))
    (when (file-exists-p thumb)
      (delete-file thumb)
      (message "deleting thumbnail:%s " thumb))
    ))

(defun navi2ch-thumbnail-insert-image-cache (url)
  (if (string-match "h?ttp://\\(.+\\)$" url)
      (setq url (match-string 1 url)))
	
  (let ((thumb_dir navi2ch-thumbnail-thumbnail-directory)
	file thumb)
    (setq url (navi2ch-thumbnail-image-escape-filename url))
    (setq file (concat thumb_dir url))
    (setq thumb (concat thumb_dir url ".jpg"))
    (when (and (not (file-exists-p thumb)) (file-exists-p file))
      (setq thumb file))
    (let ((buffer-read-only nil))
      (when (file-exists-p thumb)
;	(message "file is exist:%s" thumb)
	(move-beginning-of-line nil)
	(insert-image (create-image thumb))
	(add-text-properties
	 (1- (point)) (point)
	 (list 'link t 'link-head t
	       'url file'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name file))
        (setq image-attr (navi2ch-thumbnail-image-identify file))
	(insert (format " (%sx%s:%sk%s)" (nth 0 image-attr)
                        (nth 1 image-attr) (round (/ (nth 7 (file-attributes file)) 1024))
                        (if (nth 2 image-attr) " GIF ANIME" "")))
;	(insert " ")
      (if (re-search-forward
           (concat "h?ttp://\\([^ \t\n\r]+\\.\\("
                   (mapconcat (lambda (s) s)
                              navi2ch-browse-url-image-extentions "\\|")
                   "\\)\\)") nil t)
          (save-excursion
            (let ((url (concat "http://" (match-string 1)))
                  (beg (match-beginning 0))
                  (end (match-end 0)))
              (add-text-properties beg end '(my-navi2ch "shown")))))
	(move-end-of-line nil)
	t))))

(defun navi2ch-thumbnail-insert-image-reload ()
  "スレが再描画される時にサムネも再描画"
  (interactive)
  (let (url file thumb_dir)
    (if (display-images-p)
	(save-excursion
	  (let ((buffer-read-only nil))
            (goto-char (point-min))
	    (while (re-search-forward
		    (concat "\\(h?t?tps?://imepita.jp/[0-9/]+\\|h?t?tps?://i-bbs.sijex.net/imageDisp.jsp\\?id=watahiki&file=[0-9o]+\.jpg\\|"
			    (concat "h?t?tps?://[^ \t\n\r]+\\."
				    "\\(gif\\|jpg\\|jpeg\\|png\\)"
			    "\\)"))
		    nil t)
	      (setq url (match-string 1))
	      (if  (string-match "\\(h?t?tps?://imepita.jp/[0-9/]+\\|h?t?tps?://i-bbs.sijex.net/imageDisp.jsp\\?id=watahiki&file=[0-9o]+\.jpg\\)" url)
		    (navi2ch-thumbnail-show-image-not-image-url url)
		(navi2ch-thumbnail-insert-image-cache url))
	    ))))))

(defun navi2ch-thumbnail-all-show ()
  "1レス内の画像を連続取得表示"
  (interactive)
  (save-excursion
   (let (b e)
     ;;当該スレの範囲を取得する(もっと良い方法があるかも)
      (when (or (when (= 1 (navi2ch-article-get-current-number))
		    (beginning-of-buffer)
		    t)
		(re-search-backward
                 "______________"
                 nil t))
	(next-line)
	  (setq b (point))
	  (when (re-search-forward
		 (make-string (max 0
			      (- (eval navi2ch-article-message-separator-width)
				 (current-column)))
			 navi2ch-article-message-separator) nil t)
	    (setq e (point))
	    (navi2ch-thumbnail-image-show-region b e))))))

(defun navi2ch-thumbnail-image-show-region (b e &optional force)
  "リージョン内の画像URLを表示"
  (interactive "rP")
  (save-restriction
    (save-excursion
      (let* ((num (navi2ch-article-get-current-number))
             (board (cdr (assq 'uri navi2ch-article-current-board))))
        (narrow-to-region b e)
        (goto-char b)
        (while (re-search-forward
                (concat "h?ttp://\\([^ \t\n\r]+\\.\\("
                        (mapconcat (lambda (s) s)
                                   navi2ch-browse-url-image-extentions "\\|")
                        "\\)\\)") nil t)
          (message "while navi2ch-thumbnail-show-image-region")
          (let ((url (concat "http://" (match-string 1)))
                (beg (match-beginning 0))
                (end (match-end 0))
                (prop (get-text-property (match-beginning 1)
                                         'my-navi2ch)))
            ;既に表示済みの画像は無視
            (when (not (string= prop "shown"))
              (goto-char beg)
              (navi2ch-thumbnail-select-current-link)
;(navi2ch-thumbnail-show-image url)
              )
            ))))))

(defun navi2ch-thumbnail-image-escape-filename (filename)
  "ファイル名に使えない文字をエスケープ"
  (setq filename (navi2ch-replace-string "-" "%2d" filename t))
  (setq filename (navi2ch-replace-string ":" "%3a" filename t))
  (setq filename (navi2ch-replace-string "?" "%63" filename t)))

(defun navi2ch-thumbnail-show-image (url &optional alturl)
  "画像を縮小しインラインに表示する．"
  (interactive)
  (let* ((point (point))
         (prop  (get-text-property point 'my-navi2ch))
         (ext (when url
                (file-name-extension url))))
;    (when (not (member (downcase ext) navi2ch-browse-url-image-extentions))
;      (error "画像ファイルではありません %s" url))
    (when (not (string= prop "shown"))
;    (when (or (and ext (not (string= prop "shown"))) alturl)
      (if alturl
          (navi2ch-thumbnail-show-image-subr url alturl)
;        (string-match "\\(http://.+\\)/.+" url)
;        (setq alturl (match-string 1 url))
        (navi2ch-thumbnail-show-image-subr url alturl)))
    ))

(defun navi2ch-thumbnail-show-image-subr (url &optional org-url)
  (save-excursion
    (let ((buffer-read-only nil)
          (thumb-dir navi2ch-thumbnail-thumbnail-directory)
          thumb-file file width height size anime filename)
      (string-match "tp://\\(.+\\)$" org-url)
      (setq file (concat thumb-dir (navi2ch-thumbnail-image-escape-filename (match-string 1 org-url))))
      (setq thumb-file (concat file ".jpg"))
      (when (if org-url
                (navi2ch-net-update-file  url file nil nil nil nil (list (cons "Referer" org-url)))
              (navi2ch-net-update-file url file))
        (if (not (file-exists-p file))
            (error "ファイルがありません %s" file))
        (when (not (image-type-from-file-header file))
          (with-temp-buffer
            (insert-file-contents file nil 0 500)
            (setq buffer-error (buffer-string)))
;          (copy-file file "c:/")
          (delete-file file)
          (error "画像ファイルではありません %s %s" file buffer-error))
        (string-match "/\\([^/]+\\)$" file)
        (setq filename (match-string 1 file))
        ;; (with-temp-buffer
        ;;   (call-process navi2ch-thumbnail-image-identify-program nil t nil
        ;;                 "-quiet" "-format" "\"%n %w %h %b\"" file)
        ;;   (goto-char (point-min))
        ;;   (when (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
        ;;     (setq anime (match-string 1))
        ;;     (setq width (match-string 2))
        ;;     (setq height (match-string 3))
        ;;     (setq size (match-string 4))))

        (setq image-attr (navi2ch-thumbnail-image-identify file))
        (if (not image-attr)
            (error "画像ファイルを識別できません %s" file))
        (setq anime (nth 2 image-attr))
        (setq width (nth 0 image-attr))
        (setq height (nth 1 image-attr))
        (setq size (nth 7 (file-attributes file)))
            
      (if (or (> width navi2ch-thumbnail-thumbsize-width)
              (> height navi2ch-thumbnail-thumbsize-height))
          (progn 
            (with-temp-buffer
              (if (not anime)
                  (call-process navi2ch-thumbnail-image-convert-program nil t nil
                                "-sample" (format "%sx%s" navi2ch-thumbnail-thumbsize-width navi2ch-thumbnail-thumbsize-height) file thumb-file)
                ;;GIFアニメは1フレームだけを使う
                (call-process navi2ch-thumbnail-image-convert-program nil "real buffer" nil
                              "-scene" "0" "-sample" (format "%sx%s" navi2ch-thumbnail-thumbsize-width navi2ch-thumbnail-thumbsize-height) file (concat  file ".jpg"))
                (rename-file (concat (concat file "-0") ".jpg") thumb-file)
                
;                (let ((anime-num (string-to-number anime)) (count 1) delfile)
                (setq delete-taraget-file-list (directory-files (file-name-directory thumb-file) t (concat (file-name-nondirectory file) "-.+\.jpg")))
;                (message "delete target file %s" delete-taraget-file-list)
                (while (setq delfile (pop delete-taraget-file-list))
                  (delete-file delfile)
;                  (message "delete %s" delfile)
                  )
;                  (while (< count anime-num) ; 判定条件
;                    (setq delfile (format "%s-%s.jpg" file count))
;                    (delete-file delfile)
;                    (message "delete %s" delfile)
;                    (setq count (1+ count)))) ; 1増やす
                (message "gif anime %s" anime)))

            (insert-image (create-image thumb-file))
            (add-text-properties (1- (point)) (point)
                                 (list 'link t 'link-head t 
                                       'url file 'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name filename 'width width 'height height 'size size)))
        (insert-image (create-image file))
        (add-text-properties (1- (point)) (point)
                             (list 'link t 'link-head t 
                                   'url file 'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name filename 'width width 'height height 'size size)))
		
;      (if (not (string= anime "1"))
;          (insert (format " (%s x %s : GIF ANIME %sk) " width height (round (/ (string-to-number size) 1024))))
;        (insert (format " (%s x %s : %sk) " width height (/ (string-to-number size) 1024))))
          (insert (format " (%s x %s :%s%sk) " width height (if anime " GIF ANIME" "") (round (/ size 1024))))

      (if (re-search-forward
           (concat "h?ttp://\\([^ \t\n\r]+\\.\\("
                   (mapconcat (lambda (s) s)
                              navi2ch-browse-url-image-extentions "\\|")
                   "\\)\\)") nil t)
          (save-excursion
            (let ((url (concat "http://" (match-string 1)))
                  (beg (match-beginning 0))
                  (end (match-end 0)))
              (add-text-properties beg end '(my-navi2ch "shown")))))
      ))))

(setq navi2ch-thumbnail-404-list
      (list
	    "/404\.s?html$"
            "10mai_404\.html$"
;	    "/404_\.\*\\.s?html$"
            ))

(defun navi2ch-thumbnail-select-current-link (&optional browse-p)
  (interactive "P")
  (let ((type (get-text-property (point) 'navi2ch-link-type))
	(prop (get-text-property (point) 'navi2ch-link)))
    (cond
	  ((eq type 'url)
           (cond
            ((navi2ch-thumbnail-show-image-not-image-url prop t)
             (message "not image url but image"))
	  
            ((and (file-name-extension prop)
                  (member (downcase (file-name-extension prop))
                          navi2ch-browse-url-image-extentions))
             (when (not (navi2ch-thumbnail-insert-image-cache (substring prop 7 nil)))
               (setq url (navi2ch-thumbnail-url-status-check prop))
               (dolist (l navi2ch-thumbnail-404-list)
                 (if (string-match l url)
                     (error "ファイルが404 url=%s" url)))
;               (if (eq url prop)
;                   (navi2ch-thumbnail-show-image url)
                 (navi2ch-thumbnail-show-image url prop)
                 ))))

          ((eq type 'image)
           (navi2ch-thumbnail-show-image-external))
          )))

(setq navi2ch-thumbnail-enable-status-check t)

(defun navi2ch-thumbnail-url-status-check (url)
  "画像取得前に302や404のチェック。302の場合移動先URLを返す"
  (when navi2ch-thumbnail-enable-status-check
    (let (header status md5)
      (while (not (or (string= status "200")
		      (string= status "201")
		      (string= status "400")
		      (string= status "405")
;		      (string= status "503")
		      ))
	(setq proc (navi2ch-net-send-request url "HEAD"
					     (list '("User-Agent:" . "navi2ch 1.6" )
						   (cons "Referer" url ))))
	(if (not proc) (error "サーバに接続できません url=%s" url))
	(setq status (navi2ch-net-get-status proc))
	(if (not status) (error "サーバに接続できません url=%s" url))
	(message "status %s" status)
	
	(setq header (navi2ch-net-get-header proc))
	(setq md5 (cdr (assq 'Content-MD5 header)))
	(if md5
	    (error "Content-MD5 %s" md5))

	(cond ((or (string= status "404")
		   (string= status "403")
		   (string= status "408")
		   (string= status "503")
		   )
	       (error "ブラウズするのやめました return code %s" status))
		
	      ((or (string= status "301")
		   (string= status "302")
		   (string= status "303"))
	       (setq header (navi2ch-net-get-header proc))
	       (setq url (cdr (assq 'location header)))
	       (message "loacation %s" url))
	      ))))
  url)

(defun navi2ch-thumbnail-image-jpeg-identify (data)
  (let ((len (length data)) (i 2) (anime nil))
    (catch 'jfif
      (while (< i len)
        (let ((nbytes (+ (lsh (aref data (+ i 2)) 8)
                         (aref data (+ i 3))))
              (code (aref data (1+ i))))
          (cond
           ((= code #xc4)
            ;; DHT
	      (message "navi2ch-thumbnail-image-jpeg-identify:code FFC4 DHT")
            )
           ((and (>= code #xc0) (<= code #xcF))
            ;; SOF0 DCT
            ;; SOF2
            (if (= code #xc2)
                (message "navi2ch-thumbnail-image-jpeg-identify:SOF2"))
            (let (
                  (sample (aref data (+ i 4)))
                  (ysize (+ (lsh (aref data (+ i 5)) 8)
                            (aref data (+ i 6))))
                  (xsize (+ (lsh (aref data (+ i 7)) 8)
                            (aref data (+ i 8)))))
              (throw 'jfif (list xsize ysize anime))
              )))
          ;;skip x00(end marker) xff(start marker)
          (setq i (+ i 2 nbytes)))))))

(defun navi2ch-thumbnail-image-png-identify (data)
    (let ((i 8)
          (anime nil))
      ;;magic number
      (when (string-match "\\`\x49\x48\x44\x52"
                            (substring data (+ i 4)))
        (let (
              ;;4byte
              (xsize
               (+
                (lsh (aref data (+ i 8)) 24)
                (lsh (aref data (+ i 9)) 16)
                (lsh (aref data (+ i 10)) 8)
                (aref data (+ i 11))))
              ;;4byte
              (ysize
               (+
                (lsh (aref data (+ i 12)) 24)
                (lsh (aref data (+ i 13)) 16)
                (lsh (aref data (+ i 14)) 8)
                (aref data (+ i 15)))))
          (list xsize ysize anime)
          ))))

(defun navi2ch-thumbnail-image-gif-identify (data)
    (let ((i 0)
          (len (length data))
          xsize
          ysize
          (anime nil)
          sgct)
      (setq i (+ i 6))

      ;;GIF Header
      ;;2byte
      (setq xsize (+
                   (lsh (aref data (+ i 1)) 8)
                   (aref data i)))
      (setq i (+ i 2))
      ;;2byte
      (setq ysize (+
                   (lsh (aref data (+ i 1)) 8)
                   (aref data (+ i 0))))
      (setq i (+ i 2))
      ;;Size of Global Color Table(3 Bits)
      (setq sgct (+ 1 (logand (aref data i) 7)))
      (setq i (+ i 3))

      ;;skip Global Color Table
      (setq i (+ i (* (expt 2 sgct) 3)))

      ;;Block
      (while (< i len)
        (cond
         ((= (aref data (+ i 0)) #x21)
          (setq i (+ i 1))
          (cond
           ;;Graphic Control Extension
           ((= (aref data (+ i 0)) #xf9)
            (message "Graphic Control Extension")
            (setq i (+ i 7)))

           ;;maybe GIF Anime
           ((= (aref data (+ i 0)) #xff)
            (message "Application Extension GIF ANIME")
            (setq anime t)
            (setq i (+ i 7)))

           ((= (aref data (+ i 0)) #xfe)
            (message "Comment Extension")
            (setq i (+ i 1))
            (setq i (+ i (aref data i)))
            (setq i (+ i 2))
            )))
         
         ;;image block table
         ((= (aref data (+ i 0)) #x2c)
          (message "Image Block")
          (setq i (+ i 9))
          (setq slct (+ 1(logand (aref data i) 7)))
          (setq i (+ i (* (expt 2 slct) 3)))
          (setq i (+ i (aref data i)))
          (setq i (+ i 1))
          (message "last i:%s" i)
          )
         (t
          (setq i (+ i 1024)))
        ))
      (list xsize ysize anime)))

(defun navi2ch-thumbnail-image-identify (file &optional size)
  "画像ファイルから幅,高さ,GIFアニメか？を取得してlistで返す。
取得できなかった場合は外部プログラム(navi2ch-thumbnail-image-identify-program)に頼る。
それでもダメならnilを返す。sizeで読み込むサイズを指定もできる"
  (let ((file-size (nth 7 (file-attributes file))))
    (catch 'identify
      (when (file-readable-p file)
;      (setq file (image-search-load-path file))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (unless size
          (setq size 1024))
        (insert-file-contents-literally file nil 0 size)
        (setq data (buffer-substring (point-min) (min (point-max)
					      (+ (point-min) size))))
        (cond
         ;;gif
         ((string-match "^GIF" data)
          (setq rtn (navi2ch-thumbnail-image-gif-identify data)))
         ;;png
         ((string-match "\\`\x89\x50\x4E\x47\x0D\x0A\x1A\x0A" data)
          (setq rtn (navi2ch-thumbnail-image-png-identify data)))
         ;;jpeg
         ((string-match "\\`\xff\xd8" data)
          (setq rtn (navi2ch-thumbnail-image-jpeg-identify data))))
        (if rtn (throw 'identify rtn)))
      
      ;;情報が取得できなかった場合はヘッダをさらに読み込む
      (setq size (* size 10))
      (if (> size file-size)
          (setq size file-size))
      (message "navi2ch-thumbnail-image-identify:re-read size=%s %s" size file)
      (setq rtn (navi2ch-thumbnail-image-identify file size))
      (if rtn (throw 'identify rtn))
      ;;それでも無理なら外部プログラムに頼る
      (when (and (= size file-size)
                 navi2ch-thumbnail-image-identify-program)
        (message "identify called %s" file)
        (with-temp-buffer
          (call-process navi2ch-thumbnail-image-identify-program nil t nil
                        "-quiet" "-format" "\"%n %w %h %b\"" file)
          (goto-char (point-min))
          (when (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
            (list (string-to-number (match-string 2))
                  (string-to-number (match-string 3))
                  (> (string-to-number (match-string 1)) 1)
                  ))))
         ))))
