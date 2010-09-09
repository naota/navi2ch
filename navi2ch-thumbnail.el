(provide 'navi2ch-thumbnail)

;; サムネイルを表示する機能です
;; 画像表示に対応したemacsで動きます

;; 画像リンクURL上で','を押すとサムネイル画像を挿入表示します。
;; 自動取得、自動表示はしません。基本的にキーで駆動です。
;; キャッシュを持っている画像は自動表示されます
;; キャッシュの自動削除機能はありません。
;; 基本的にコマンドはImageMagick依存です
;;
;; 参考にしたコード(navi2chスレのどこかで見た)は非同期だったり外部シェル叩きだったりと複雑なので、
;; なるべくシンプルに再構成しました

;;To Do
;;どこか(全体?)を(display-images-p)で囲むべきだが要検討
;;キーバインド調整
;;スレ再描画時にサムネを読まなかったり読んだりがあるかも
;; emacs付属のimage.elを参考にして画像ファイルのヘッダをバイナリ読みすればidentifyコールしなくて済むかも

;; 設定例
;; Windows
;;   (setq navi2ch-thumbnail-image-convert-program "C:/Program Files/ImageMagick-6.2.8-Q16/convert.exe")
;;   (setq navi2ch-thumbnail-image-identify-program "C:/Program Files/ImageMagick-6.2.8-Q16/identify.exe")
;;   (setq navi2ch-browse-url-image-program "c:/win/iview425j/i_view32.exe") ;;IrfanView
;; MacOSX
;;   (setq navi2ch-browse-url-image-program "/Applications/Preview.app/Contents/MacOS/Preview")
;;   (setq navi2ch-thumbnail-image-convert-program "/opt/local/bin/convert") ;;MacPort ImageMagick 
;;   (setq navi2ch-thumbnail-image-identify-program "/opt/local/bin/identify")

;;使い方、兼キーバインド

;; URLにカーソルがある状態で','を押すとサムネイル挿入
;; サムネイルにカーソルがある状態で','を押すと外部ビューアーでオリジナル画像表示
;;(本当はenterキーでやるほうが奇麗な気もする)
(define-key navi2ch-article-mode-map "," 'navi2ch-thumbnail-select-current-link)
(define-key navi2ch-popup-article-mode-map "," 'navi2ch-thumbnail-select-current-link)

;;サムネイルにカーソルがある状態で'v'で画像を保存(サムネイルではなく、元の大きい画像)
(define-key navi2ch-article-mode-map "v" 'navi2ch-thumbnail-save-content)
(define-key navi2ch-popup-article-mode-map "v" 'navi2ch-thumbnail-save-content)

;;Esc+EnterでURLをブラウザで開く(既存機能に丸投げ)。
;;画像ビューアーが指定されてると、そのURLを開くのでリモートなファイルを開けるビューアーが必要(元々その動作)
(define-key navi2ch-article-mode-map "\e\r" 'navi2ch-article-select-current-link)
(define-key navi2ch-popup-article-mode-map "\e\r" 'navi2ch-article-select-current-link)

;;サムネイルにカーソルがある状態で'd'を押すとキャッシュ画像を削除
;;既にキーバインドがダブってるが、分かりやすさでオーバーライド(要検討)
(define-key navi2ch-article-mode-map "d" 'navi2ch-thumbnail-image-delete-cache)
(define-key navi2ch-popup-article-mode-map "d" 'navi2ch-thumbnail-image-delete-cache)

;;'T'を押すとカーソルがあるレス1個のレス内のURLを全取得
(define-key navi2ch-article-mode-map "T" 'navi2ch-thumbnail-all-show)
(define-key navi2ch-popup-article-mode-map "T" 'navi2ch-thumbnail-all-show)

(defcustom navi2ch-thumbnail-thumbnail-directory (concat navi2ch-directory "/navi2ch-thumbnails/")
  "* 画像キャッシュディレクトリ"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-save-content-dir "~/"
  "* 画像保存時のディフォルトディレクトリ"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-image-convert-program "/opt/local/bin/convert"
  "* サムネイル作成プログラム"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-image-identify-program "/opt/local/bin/identify"
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
    (if (navi2ch-thumbnail-insert-image-cache alturl)
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
	(message "file is exist:%s" thumb)
	(move-beginning-of-line nil)
	(insert-image (create-image thumb))
	(add-text-properties
	 (1- (point)) (point)
	 (list 'link t 'link-head t
	       'url file'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name file))
	(insert " ")
	(move-end-of-line nil)
	t))))

(defun navi2ch-thumbnail-insert-image-reload ()
  "スレが再描画される時にサムネも再描画"
  (interactive)
  (let (url file thumb_dir)
    (if (display-images-p)
	(save-excursion
	  (let ((buffer-read-only nil))
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
              (navi2ch-thumbnail-show-image url))
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
    (when (or (and ext
                   (not (string= prop "shown"))
                   (member (downcase ext) navi2ch-browse-url-image-extentions))
              alturl)
    (if alturl
        (navi2ch-thumbnail-show-image-subr url alturl)
      (string-match "\\(http://.+\\)/.+" url)
      (setq alturl (match-string 1 url))
      (navi2ch-thumbnail-show-image-subr url alturl)))
    ))

(defun navi2ch-thumbnail-show-image-subr (url &optional referer)
  (save-excursion
    (let ((buffer-read-only nil)
          (thumb-dir navi2ch-thumbnail-thumbnail-directory)
          thumb-file file width height size anime filename)
      (string-match "tp://\\(.+\\)$" url)
      (setq file (concat thumb-dir (navi2ch-thumbnail-image-escape-filename (match-string 1 url))))
      (setq thumb-file (concat file ".jpg"))
      (when (if referer
                (navi2ch-net-update-file  url file nil nil nil nil (list (cons "Referer" referer)))
              (navi2ch-net-update-file url file))
        (if (not (file-exists-p file))
            (error "ファイルがありません %s" file))
        (when (not (image-type-from-file-header file))
;          (copy-file file "c:/")
          (delete-file file)
          (error "画像ファイルではありません %s" file))
        (string-match "/\\([^/]+\\)$" file)
        (setq filename (match-string 1 file))
        (with-temp-buffer
          (call-process navi2ch-thumbnail-image-identify-program nil t nil
                        "-quiet" "-format" "\"%n %w %h %b\"" file)
          (goto-char (point-min))
          (when (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
            (setq anime (match-string 1))
            (setq width (match-string 2))
            (setq height (match-string 3))
            (setq size (match-string 4))))

      (if (or (> (string-to-number width) navi2ch-thumbnail-thumbsize-width)
              (> (string-to-number height) navi2ch-thumbnail-thumbsize-height))
          (progn 
            (with-temp-buffer
              (if (string= anime "1")
                  (call-process navi2ch-thumbnail-image-convert-program nil t nil
                                "-sample" (format "%sx%s" navi2ch-thumbnail-thumbsize-width navi2ch-thumbnail-thumbsize-height) file thumb-file)
                ;;GIFアニメは1フレームだけを使う
                (call-process navi2ch-thumbnail-image-convert-program nil "real buffer" nil
                              "-scene" "0" "-sample" (format "%sx%s" navi2ch-thumbnail-thumbsize-width navi2ch-thumbnail-thumbsize-height) file (concat  file ".jpg"))
                (rename-file (concat (concat file "-0") ".jpg") thumb-file)
                (let ((anime-num (string-to-number anime)) (count 1) delfile)
                  (while (< count anime-num) ; 判定条件
                    (setq delfile (format "%s-%s.jpg" file count))
                    (delete-file delfile)
                    (message "delete %s" delfile)
                    (setq count (1+ count)))) ; 1増やす
                (message "gif anime %s" anime)))

            (insert-image (create-image thumb-file))
            (add-text-properties (1- (point)) (point)
                                 (list 'link t 'link-head t 
                                       'url file 'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name filename 'width width 'height height 'size size)))
        (insert-image (create-image file))
        (add-text-properties (1- (point)) (point)
                             (list 'link t 'link-head t 
                                   'url file 'help-echo file 'navi2ch-link-type 'image 'navi2ch-link file 'file-name filename 'width width 'height height 'size size)))
		
      (if (not (string= anime "1"))
          (insert (format " (%s x %s : GIF ANIME %sk) " width height (round (/ (string-to-number size) 1024))))
        (insert (format " (%s x %s : %sk) " width height (/ (string-to-number size) 1024))))

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
      
(defun navi2ch-thumbnail-select-current-link (&optional browse-p)
  (interactive "P")
  (let ((type (get-text-property (point) 'navi2ch-link-type))
	(prop (get-text-property (point) 'navi2ch-link)))
    (cond ((eq type 'number)
	   (navi2ch-article-select-current-link-number 
	    (navi2ch-article-get-number-list prop)
	    browse-p))

	  ((eq type 'url)
           (cond
            ((navi2ch-thumbnail-show-image-not-image-url prop t)
             (message "not image url but image"))
	  
            ((and (file-name-extension prop)
                  (member (downcase (file-name-extension prop))
                          navi2ch-browse-url-image-extentions))
             (when (not (navi2ch-thumbnail-insert-image-cache (substring prop 7 nil)))
                 (if (string-match "http://\\([^/]+\\)" prop)
                     (setq prop (navi2ch-thumbnail-url-status-check prop)))
                 (navi2ch-thumbnail-show-image prop)))))

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
