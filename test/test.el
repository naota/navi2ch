;; run with `emacs --batch --script test.el'

;;; load ert
(when load-file-name
  (add-to-list 'load-path
	       (expand-file-name "ert"
				 (file-name-directory load-file-name))))
(require 'ert-batch)

;;; load navi2ch

(setq load-path 
      (cons (expand-file-name ".."
			      (file-name-directory load-file-name))
	    load-path))
(require 'navi2ch-util)
(require 'navi2ch-http-date)
(require 'navi2ch-net)
(require 'navi2ch-thumbnail)

;;; define tests

(ert-deftest replace-html-tag ()
  (should (string=
	   (navi2ch-replace-html-tag 
	    " <a href=\"../test/read.cgi/***/***/***\" target=\"_blank\">&gt;&gt;139</a> <br> foo")
	   " >>139 \n foo")))

(ert-deftest http-date-test-1 ()
  (let ((expected "Sun, 06 Nov 1994 08:49:37 GMT"))
    (should (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sun, 06 Nov 1994 08:49:37 GMT"))))))
(ert-deftest http-date-test-2 ()
  (let ((expected "Sun, 06 Nov 1994 08:49:37 GMT"))
    (should (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sunday, 06-Nov-94 08:49:37 GMT"))))))
(ert-deftest http-date-test-3 ()
  (let ((expected "Sun, 06 Nov 1994 08:49:37 GMT"))
    (should (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sun Nov  6 08:49:37 1994"))))))

(ert-deftest tanpan-check-1 ()
  (should (navi2ch-net-is-tanpan-thread-p 
	   "短パンマン ★<><>2010/09/15 01:09:42 ID:TanpanM<>いろいろあるさ ＠bg r.so ver 2008/02/19<br>ERROR = 5656 <br>(e_mes = []) <br> <>名古屋はエ〜エ〜で ♪
")))

(ert-deftest thumbnail-save-good-work ()
  (let* ((workdir (make-temp-file "workdir." t))
	 (fileFoo (expand-file-name "foo" workdir))
	 (fileBar (expand-file-name "bar" workdir)))
    (unwind-protect
	(progn
	  (with-temp-file fileFoo (insert "foo"))
	  ;; copy する
	  (navi2ch-thumbnail-save-content fileFoo fileBar)
	  (should
	   (string= (with-temp-buffer (insert-file-contents fileBar) (buffer-string))
		    "foo"))
	  (with-temp-file fileBar (insert "bar"))
	  ;; overwrite
	  (navi2ch-thumbnail-save-content fileFoo fileBar t)
	  (should
	   (string= (with-temp-buffer (insert-file-contents fileBar) (buffer-string))
		    "foo")))
      (delete-directory workdir t))))

(ert-deftest thumbnail-save-interactive-check ()
  (let ((si:read-file-name (symbol-function 'read-file-name))
	intform)
    (unwind-protect
	(progn
	  (fset 'read-file-name (lambda (a b c)
				  (expand-file-name (or c "hoge") b)))
	  (lexical-let ((evalform (cadr (interactive-form 'navi2ch-thumbnail-save-content))))
	    (fset 'intform (lambda () (eval evalform))))
	  (with-temp-buffer
	    ;; prop `navi2ch-link' から読みだす。読みだせないとエラー。
	    (should-error (intform))
	    (save-excursion (insert (propertize "foo" 'navi2ch-link "foobar")))
	    (let ((res (intform)))
	      (should (equal res (list "foobar"
				       (expand-file-name "hoge"
							 navi2ch-thumbnail-save-content-dir)
				       0))))
	    (put-text-property (point-min) (point-max) 'file-name "barbaz")
	    (let ((res (intform)))
	      (should (equal res (list "foobar"
				       (expand-file-name "barbaz"
							 navi2ch-thumbnail-save-content-dir)
				       0))))))
      (fset 'read-file-name si:read-file-name))))

(ert-deftest thumbnail-save-file-invalid ()
  (let* ((workdir (make-temp-file "workdir." t))
	 (fileFoo (expand-file-name "foo" workdir))
	 (fileBar (expand-file-name "bar" workdir)))
    (unwind-protect
	(progn
	  ;; ファイルがない
	  (should-error
	   (navi2ch-thumbnail-save-content fileFoo fileBar))
	  ;; ファイルが書きこみできない
	  (with-temp-file fileFoo (insert "foo"))
	  (with-temp-file fileBar (insert "bar"))
	  (chmod fileBar 256)
	  (should-error
	   (navi2ch-thumbnail-save-content fileFoo fileBar)))
      (delete-directory workdir t))))

(ert-deftest thumbnail-save-invalid-argument ()
  ;; 無効な引数
  (let* ((workdir (make-temp-file "workdir." t))
	 (fileFoo (expand-file-name "foo" workdir)))
    (unwind-protect
	(progn
	  (should-error
	   (navi2ch-thumbnail-save-content nil nil))
	  (should-error
	   (navi2ch-thumbnail-save-content nil fileFoo))
	  (should-error
	   (navi2ch-thumbnail-save-content fileFoo nil)))
      (delete-directory workdir t))))

(ert-deftest thumbnail-save-same-file ()
  ;; 同じファイル
  (let* ((workdir (make-temp-file "workdir." t))
	 (file (expand-file-name "foo" workdir)))
    (unwind-protect
	(progn
	  (with-temp-file file (insert "foo"))
	  (should-error
	   (navi2ch-thumbnail-save-content file file t))
	  (should-error
	   (navi2ch-thumbnail-save-content file file nil)))
      (delete-directory workdir t))))


;;; run the tests

(ert-run-tests-batch)
