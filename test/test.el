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

;;; run the tests

(ert-run-tests-batch)
