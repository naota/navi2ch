;;; navi2ch-util.el --- useful utilities for navi2ch

;; Copyright (C) 2000-2002 by Navi2ch Project
;; Copyright (C) 1993-2000 Free Software Foundation, Inc.

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network, 2ch

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

;;; Code:
(provide 'navi2ch-util)
(defvar navi2ch-util-ident
  "$Id$")

(eval-when-compile (require 'cl))
(require 'timezone)
(require 'browse-url)
(require 'base64)

(require 'navi2ch-vars)

(defvar navi2ch-mode-line-identification nil)
(make-variable-buffer-local 'navi2ch-mode-line-identification)

(defvar navi2ch-replace-html-tag-alist
  '(("&gt;" . ">")
    ("&lt;" . "<")
    ("&quot;" . "\"")
    ("&nbsp;" . " ")
    ("&amp;" . "&")
    ("<br>" . "\n")
    ("＠｀" . ","))
  "置換する html のタグの連想リスト(正規表現は使えない)")

(defvar navi2ch-replace-html-tag-regexp-alist
  '(("<[^<>]+>" . "")
    ("&[a-zA-Z]+;" . navi2ch-entity-reference-to-str)
    ("&#[0-9]+;?" . navi2ch-numeric-reference-to-str))
  "置換する html のタグの連想リスト(正規表現)
置換先が関数だと、置換元を引数としてその関数を呼びだしたもので置き替える。
正規表現が必要ない場合は `navi2ch-replace-html-tag-alist' に入れる")

(defvar navi2ch-replace-html-tag-regexp
  (concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
	  "\\|"
	  (mapconcat 'car navi2ch-replace-html-tag-regexp-alist "\\|"))
  "置換する html のタグの正規表現
`navi2ch-replace-html-tag-alist' から生成される")

(defvar navi2ch-entity-alist
  '(("iexcl"    .  161) ("cent"     .  162) ("pound"    .  163)
    ("curren"   .  164) ("yen"      .  165) ("brvbar"   .  166)
    ("sect"     .  167) ("uml"      .  168) ("copy"     .  169)
    ("ordf"     .  170) ("laquo"    .  171) ("not"      .  172)
    ("shy"      .  173) ("reg"      .  174) ("macr"     .  175)
    ("deg"      .  176) ("plusmn"   .  177) ("sup2"     .  178)
    ("sup3"     .  179) ("acute"    .  180) ("micro"    .  181)
    ("para"     .  182) ("middot"   .  183) ("cedil"    .  184)
    ("sup1"     .  185) ("ordm"     .  186) ("raquo"    .  187)
    ("frac14"   .  188) ("frac12"   .  189) ("frac34"   .  190)
    ("iquest"   .  191) ("Agrave"   .  192) ("Aacute"   .  193)
    ("Acirc"    .  194) ("Atilde"   .  195) ("Auml"     .  196)
    ("Aring"    .  197) ("AElig"    .  198) ("Ccedil"   .  199)
    ("Egrave"   .  200) ("Eacute"   .  201) ("Ecirc"    .  202)
    ("Euml"     .  203) ("Igrave"   .  204) ("Iacute"   .  205)
    ("Icirc"    .  206) ("Iuml"     .  207) ("ETH"      .  208)
    ("Ntilde"   .  209) ("Ograve"   .  210) ("Oacute"   .  211)
    ("Ocirc"    .  212) ("Otilde"   .  213) ("Ouml"     .  214)
    ("times"    .  215) ("Oslash"   .  216) ("Ugrave"   .  217)
    ("Uacute"   .  218) ("Ucirc"    .  219) ("Uuml"     .  220)
    ("Yacute"   .  221) ("THORN"    .  222) ("szlig"    .  223)
    ("agrave"   .  224) ("aacute"   .  225) ("acirc"    .  226)
    ("atilde"   .  227) ("auml"     .  228) ("aring"    .  229)
    ("aelig"    .  230) ("ccedil"   .  231) ("egrave"   .  232)
    ("eacute"   .  233) ("ecirc"    .  234) ("euml"     .  235)
    ("igrave"   .  236) ("iacute"   .  237) ("icirc"    .  238)
    ("iuml"     .  239) ("eth"      .  240) ("ntilde"   .  241)
    ("ograve"   .  242) ("oacute"   .  243) ("ocirc"    .  244)
    ("otilde"   .  245) ("ouml"     .  246) ("divide"   .  247)
    ("oslash"   .  248) ("ugrave"   .  249) ("uacute"   .  250)
    ("ucirc"    .  251) ("uuml"     .  252) ("yacute"   .  253)
    ("thorn"    .  254) ("yuml"     .  255) ("fnof"     .  402)
    ("Alpha"    .  913) ("Beta"     .  914) ("Gamma"    .  915)
    ("Delta"    .  916) ("Epsilon"  .  917) ("Zeta"     .  918)
    ("Eta"      .  919) ("Theta"    .  920) ("Iota"     .  921)
    ("Kappa"    .  922) ("Lambda"   .  923) ("Mu"       .  924)
    ("Nu"       .  925) ("Xi"       .  926) ("Omicron"  .  927)
    ("Pi"       .  928) ("Rho"      .  929) ("Sigma"    .  931)
    ("Tau"      .  932) ("Upsilon"  .  933) ("Phi"      .  934)
    ("Chi"      .  935) ("Psi"      .  936) ("Omega"    .  937)
    ("alpha"    .  945) ("beta"     .  946) ("gamma"    .  947)
    ("delta"    .  948) ("epsilon"  .  949) ("zeta"     .  950)
    ("eta"      .  951) ("theta"    .  952) ("iota"     .  953)
    ("kappa"    .  954) ("lambda"   .  955) ("mu"       .  956)
    ("nu"       .  957) ("xi"       .  958) ("omicron"  .  959)
    ("pi"       .  960) ("rho"      .  961) ("sigmaf"   .  962)
    ("sigma"    .  963) ("tau"      .  964) ("upsilon"  .  965)
    ("phi"      .  966) ("chi"      .  967) ("psi"      .  968)
    ("omega"    .  969) ("thetasym" .  977) ("upsih"    .  978)
    ("piv"      .  982) ("bull"     . 8226) ("hellip"   . 8230)
    ("prime"    . 8242) ("Prime"    . 8243) ("oline"    . 8254)
    ("frasl"    . 8260) ("weierp"   . 8472) ("image"    . 8465)
    ("real"     . 8476) ("trade"    . 8482) ("alefsym"  . 8501)
    ("larr"     . 8592) ("uarr"     . 8593) ("rarr"     . 8594)
    ("darr"     . 8595) ("harr"     . 8596) ("crarr"    . 8629)
    ("lArr"     . 8656) ("uArr"     . 8657) ("rArr"     . 8658)
    ("dArr"     . 8659) ("hArr"     . 8660) ("forall"   . 8704)
    ("part"     . 8706) ("exist"    . 8707) ("empty"    . 8709)
    ("nabla"    . 8711) ("isin"     . 8712) ("notin"    . 8713)
    ("ni"       . 8715) ("prod"     . 8719) ("sum"      . 8721)
    ("minus"    . 8722) ("lowast"   . 8727) ("radic"    . 8730)
    ("prop"     . 8733) ("infin"    . 8734) ("ang"      . 8736)
    ("and"      . 8743) ("or"       . 8744) ("cap"      . 8745)
    ("cup"      . 8746) ("int"      . 8747) ("there4"   . 8756)
    ("sim"      . 8764) ("cong"     . 8773) ("asymp"    . 8776)
    ("ne"       . 8800) ("equiv"    . 8801) ("le"       . 8804)
    ("ge"       . 8805) ("sub"      . 8834) ("sup"      . 8835)
    ("nsub"     . 8836) ("sube"     . 8838) ("supe"     . 8839)
    ("oplus"    . 8853) ("otimes"   . 8855) ("perp"     . 8869)
    ("sdot"     . 8901) ("lceil"    . 8968) ("rceil"    . 8969)
    ("lfloor"   . 8970) ("rfloor"   . 8971) ("lang"     . 9001)
    ("rang"     . 9002) ("loz"      . 9674) ("spades"   . 9824)
    ("clubs"    . 9827) ("hearts"   . 9829) ("diams"    . 9830)
    ("OElig"    .  338) ("oelig"    .  339) ("Scaron"   .  352)
    ("scaron"   .  353) ("Yuml"     .  376) ("circ"     .  710)
    ("tilde"    .  732) ("ensp"     . 8194) ("emsp"     . 8195)
    ("thinsp"   . 8201) ("zwnj"     . 8204) ("zwj"      . 8205)
    ("lrm"      . 8206) ("rlm"      . 8207) ("ndash"    . 8211)
    ("mdash"    . 8212) ("lsquo"    . 8216) ("rsquo"    . 8217)
    ("sbquo"    . 8218) ("ldquo"    . 8220) ("rdquo"    . 8221)
    ("bdquo"    . 8222) ("dagger"   . 8224) ("Dagger"   . 8225)
    ("permil"   . 8240) ("lsaquo"   . 8249) ("rsaquo"   . 8250)
    ("euro"     . 8364)))

(defconst navi2ch-base64-begin-delimiter "----BEGIN BASE64----"
  "base64コードの前に挿入するデリミタ。")
(defconst navi2ch-base64-end-delimiter "----END BASE64----"
  "base64コードの後に挿入するデリミタ。")

(defconst navi2ch-base64-begin-delimiter-regexp
  (format "^%s\\((\\([^\)]+\\))\\)?.*$"
          (regexp-quote navi2ch-base64-begin-delimiter))
  "base64コードの前のデリミタにマッチする正規表現。")
(defconst navi2ch-base64-end-delimiter-regexp
  (format "^%s.*$" (regexp-quote navi2ch-base64-end-delimiter))
  "base64コードの後のデリミタにマッチする正規表現。")
(defconst navi2ch-base64-susv3-begin-delimiter-regexp
  "^begin-base64 \\([0-7]+\\) \\([^ \n]+\\)$"
  "SUSv3のuuencodeで作成されるbase64コードの前のデリミタにマッチする正規表現")
(defconst navi2ch-base64-susv3-end-delimiter-regexp
  "^====$"
  "SUSv3のuuencodeで作成されるbase64コードの後のデリミタにマッチする正規表現")

(defconst navi2ch-base64-line-regexp
  (concat
   "^\\([+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z]\\)*"
   "[+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z=][+/0-9A-Za-z=] *$")
  "base64コードのみが含まれる行にマッチする正規表現。")

(defvar navi2ch-coding-system 'shift_jis)

(defvar navi2ch-offline nil "オフラインモードかどうか")
(defvar navi2ch-online-indicator  "[ON] ")
(put 'navi2ch-online-indicator 'risky-local-variable t)
(defvar navi2ch-offline-indicator "[--] ")
(put 'navi2ch-offline-indicator 'risky-local-variable t)
(defvar navi2ch-modeline-online navi2ch-online-indicator)
(defvar navi2ch-modeline-offline navi2ch-offline-indicator)

;; shut up XEmacs warnings
(eval-when-compile
  (defvar message-log-max)
  (defvar minibuffer-allow-text-properties))

;;;; macros
(defmacro navi2ch-ifxemacs (then &rest else)
  "If on XEmacs, do THEN, else do ELSE.
Like \"(if (featurep 'xemacs) THEN ELSE)\", but expanded at
compilation time.  Because byte-code of XEmacs is not compatible with
GNU Emacs's one, this macro is very useful."
  (if (featurep 'xemacs)
      then
    (cons 'progn else)))
;; Navi2chのコードをハクする人は↓を~/.emacsにも入れときましょう。
(put 'navi2ch-ifxemacs 'lisp-indent-function 1)

(defmacro navi2ch-define-mouse-key (map num command)
  (if (featurep 'xemacs)
      `(define-key ,map ',(intern (format "button%d" num)) ,command)
    `(define-key ,map ,(vector (intern (format "mouse-%d" num))) ,command)))

;; from apel
(defmacro navi2ch-defalias-maybe (symbol definition)
  "Define SYMBOL as an alias for DEFINITION if SYMBOL is not defined.
See also the function `defalias'."
  (setq symbol (eval symbol))
  (or (and (fboundp symbol)
           (not (get symbol 'defalias-maybe)))
      (` (or (fboundp (quote (, symbol)))
             (prog1
                 (defalias (quote (, symbol)) (, definition))
               ;; `defalias' updates `load-history' internally.
               (put (quote (, symbol)) 'defalias-maybe t))))))

;; from apel
(defmacro navi2ch-set-buffer-multibyte (flag)
  (if (featurep 'xemacs)
      flag
    `(set-buffer-multibyte ,flag)))

;; from apel
(defmacro navi2ch-string-as-unibyte (string)
  (if (featurep 'xemacs)
      string
    `(string-as-unibyte ,string)))

(defmacro navi2ch-string-as-multibyte (string)
  (if (featurep 'xemacs)
      string
    `(string-as-multibyte ,string)))

;;; from Wanderlust (elmo-date.el)
(defmacro navi2ch-make-sortable-date (datevec)
  "Make a sortable string from DATEVEC."
  (` (timezone-make-sortable-date
      (aref (, datevec) 0)
      (aref (, datevec) 1)
      (aref (, datevec) 2)
      (aref (, datevec) 3))))

(defmacro navi2ch-match-string-no-properties (num &optional string)
  (if (featurep 'xemacs)
      `(match-string ,num ,string)
    `(match-string-no-properties ,num ,string)))

(defmacro navi2ch-with-default-file-modes (mode &rest body)
  "default-file-modes を MODE にして BODY を実行する。"
  (let ((temp (make-symbol "--file-modes-temp--")))
    `(let ((,temp (default-file-modes)))
       (unwind-protect
	   (progn
	     (set-default-file-modes
	      (navi2ch-ifxemacs
		  (if (integerp ,mode)
		      ,mode
		    (char-to-int ,mode))
		,mode))
	     ,@body)
	 (set-default-file-modes ,temp)))))

(put 'navi2ch-with-default-file-modes 'lisp-indent-function 1)


;;;; other misc stuff
(defsubst navi2ch-replace-string (regexp rep string
					 &optional all fixedcase literal)
  "STRING に含まれる REGEXP を REP で置換する。
REP が関数の場合は、マッチした文字列を引数にしてその関数を呼び出す。

FIXEDCASE、LITERAL は `replace-match' にそのまま渡される。

ALL が non-nil ならば、マッチしたテキストをすべて置換する。nil なら
最初の1つだけを置換する。

REGEXP が見つからない場合、STRING をそのまま返す。"
  (save-match-data
    (if all
	;; Emacs 21 の replace-regexp-in-string のパクり。
	(let ((start 0)
	      (l (length string))
	      mb me str matches)
	  (while (and (< start l)
		      (string-match regexp string start))
	    (setq mb (match-beginning 0)
		  me (match-end 0))
	    (if (= mb me)
		(setq me (min l (1+ mb))))
	    (string-match regexp (setq str (substring string mb me)))
	    (setq matches
		  (cons (replace-match (if (stringp rep)
					   rep
					 (funcall rep (match-string 0 str)))
				       fixedcase literal str)
			(cons (substring string start mb)
			      matches)))
	    (setq start me))
	  (apply #'concat (nreverse (cons (substring string start l)
					  matches))))
      (when (string-match regexp string)
	(setq string (replace-match (if (stringp rep)
					rep
				      (funcall rep (match-string 0 string)))
				    fixedcase literal string)))
      string)))

(defun navi2ch-bigint-int-to-list (i)
  (if (listp i)
      i
    (mapcar (lambda (x)
              (- x 48))
            (string-to-list (int-to-string i)))))

(defun navi2ch-bigint-multiply (a b)
  (setq a (reverse (navi2ch-bigint-int-to-list a))
        b (reverse (navi2ch-bigint-int-to-list b)))
  (let (list c)
    (dolist (y b)
      (let ((z 0))
        (setq list (cons
                    (append c (mapcar
                               (lambda (x)
                                 (let (w)
                                   (setq w (+ (* x y) z))
                                   (setq z (/ w 10))
                                   (mod w 10))) a)
                            (if (> z 0) (list z)))
                    list)))
      (setq c (cons 0 c)))
    (let (list2)
      (dolist (x list)
        (setq list2 (navi2ch-bigint-add list2 (reverse x))))
      list2)))

(defun navi2ch-bigint-add (a b)
  (setq a (reverse (navi2ch-bigint-int-to-list a))
        b (reverse (navi2ch-bigint-int-to-list b)))
  (let ((x 0) list)
    (while (or a b)
      (let (y)
        (setq y (+ (or (car a) 0) (or (car b) 0) x))
        (setq x (/ y 10))
        (setq list (cons (mod y 10) list))
        (setq a (cdr a)
              b (cdr b))))
    (if (> x 0) (setq list (cons x list)))
    list))

(defun navi2ch-insert-file-contents (file &optional begin end)
  (let ((coding-system-for-read navi2ch-coding-system)
        (coding-system-for-write navi2ch-coding-system))
    (insert-file-contents file nil begin end)))

(defun navi2ch-expand-file-name (file)
  (expand-file-name file navi2ch-directory))

(defun navi2ch-uudecode-region (start end)
  (interactive "r")
  (let (dir)
    (save-window-excursion
      (delete-other-windows)
      (setq dir (read-file-name "directory name: ")))
    (unless (file-directory-p dir)
      (error "%s is not directory" dir))

    (let ((default-directory dir)
          (coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          rc)
      (setq rc (apply
                'call-process-region
                start end
                navi2ch-uudecode-program
                nil nil nil
                navi2ch-uudecode-args))
      (when (not (= rc 0))
        (error "uudecode error")))))

;; (defun navi2ch-read-number (prompt)
;;   "数字を minibuffer から読み込む"
;;   (catch 'loop
;;     (while t
;;       (let (elt)
;;         (setq elt (read-string prompt init history default))
;;         (cond ((string= elt "")
;;                (throw 'loop nil))
;;               ((string-match "^[ \t]*0+[ \t]*$" elt)
;;                (throw 'loop 0))
;;               ((not (eq (string-to-number elt) 0))
;;                (throw 'loop (string-to-int elt)))))
;;       (message "Please enter a number.")
;;       (sit-for 1))))

(defsubst navi2ch-replace-html-tag-to-string (str)
  (let ((ret
	 (or (cdr (if case-fold-search
		      (assoc-ignore-case str navi2ch-replace-html-tag-alist)
		    (assoc str navi2ch-replace-html-tag-alist)))
	     (save-match-data
	       (let ((alist navi2ch-replace-html-tag-regexp-alist)
		     elt value)
		 (while alist
		   (setq elt (car alist)
			 alist (cdr alist))
		   (when (string-match (car elt) str)
		     (setq value (cdr elt)
			   alist nil)))
		 value))
	     "")))
    (if (functionp ret)
	(funcall ret str)
      ret)))

(defsubst navi2ch-replace-html-tag (str)
  (let ((case-fold-search t))
    (navi2ch-replace-string navi2ch-replace-html-tag-regexp
			    'navi2ch-replace-html-tag-to-string
			    str t)))

(defsubst navi2ch-replace-html-tag-with-buffer ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward navi2ch-replace-html-tag-regexp nil t)
      (replace-match (navi2ch-replace-html-tag-to-string (match-string 0))))))

(defsubst navi2ch-replace-html-tag-with-temp-buffer (str)
  (with-temp-buffer
    (insert str)
    (navi2ch-replace-html-tag-with-buffer)
    (buffer-string)))

(defun navi2ch-entity-reference-to-str (ref)
  "文字実体参照をデコード。"
  (save-match-data
    (if (and navi2ch-article-decode-character-references
	     (string-match "&\\([^;]+\\)" ref))
	(let ((code (cdr (assoc (match-string 1 ref) navi2ch-entity-alist))))
	  (or (and code (navi2ch-ucs-to-str code)) ref))
      ref)))

(defun navi2ch-numeric-reference-to-str (ref)
  "数値文字参照をデコード。"
  (save-match-data
    (if (and navi2ch-article-decode-character-references
	     (string-match "&#\\([^;]+\\)" ref))
	(or (navi2ch-ucs-to-str (string-to-int (match-string 1 ref))) "〓")
      ref)))

;; shut up byte-compile warnings
(eval-when-compile
  (autoload 'ucs-to-char "unicode")
  (navi2ch-defalias-maybe 'unicode-to-char 'ignore)
  (navi2ch-defalias-maybe 'decode-char 'ignore))

(defun navi2ch-ucs-to-str (code)
  (let ((c (cond
	    ((featurep 'un-define)
	     (ucs-to-char code))
	    ((and (fboundp 'unicode-to-char)
		  (subrp (symbol-function 'unicode-to-char)))
	     (unicode-to-char code))
	    (navi2ch-on-emacs21
	     (decode-char 'ucs code)))))
    (if (navi2ch-char-valid-p c)
	(char-to-string c)
      nil)))

(defun navi2ch-read-char (&optional prompt)
  "PROMPT (non-nil の場合) を表示して `read-char' を呼び出す。"
  (let ((cursor-in-echo-area t)
	(message-log-max nil)
	c)
    (if prompt
	(message "%s" prompt))
    (setq c (read-char))
    (if prompt
	(message "%s%c" prompt c))
    c))

(defun navi2ch-read-char-with-retry (prompt retry-prompt list)
  "PROMPT を表示 (non-nil の場合) して `read-char' を呼び出す。
入力された文字が LIST に含まれない場合、RETRY-PROMPT (nil の場合は
PROMPT) を表示して再度 `read-char' を呼ぶ。"
  (let ((retry t) c)
    (while retry
      (setq c (navi2ch-read-char prompt))
      (cond ((memq c list)
	     (setq retry nil))
	    ((eq c 12)
	     (recenter))
	    (t
	     (ding)
	     (setq prompt (or retry-prompt prompt)))))
    c))

(defun navi2ch-y-or-n-p (prompt &optional quit-symbol)
  (let* ((prompt (concat prompt "(y, n, or q) "))
	 (c (navi2ch-read-char-with-retry
	     prompt
	     (concat "Please answer y, n, or q.  " prompt)
	     '(?q ?Q ?y ?Y ?\  ?n ?N ?\177))))
    (cond ((memq c '(?q ?Q))
	   (or quit-symbol nil))
	  ((memq c '(?y ?Y ?\ ))
	   t)
	  ((memq c '(?n ?N ?\177))
	   nil))))

(defsubst navi2ch-boundp (symbol)
  "SYMBOL がバインドされていない時は nil を返す。
boundp と違い、SYMBOL がバインドされている時は t ではなくシンボルを返す。"
  (and (boundp symbol) symbol))

(defsubst navi2ch-fboundp (symbol)
  "SYMBOL がバインドされていない時は nil を返す。
fboundp と違い、SYMBOL がバインドされている時は t ではなくシンボルを返す。"
  (and (fboundp symbol) symbol))

(defun navi2ch-browse-url-internal (url &rest args)
  (let ((browse-url-browser-function (or navi2ch-browse-url-browser-function
					 browse-url-browser-function))
	(new-window-flag (symbol-value (or (navi2ch-boundp
					    'browse-url-new-window-flag)
					   (navi2ch-boundp
					    'browse-url-new-window-p)))))
    (if (eq browse-url-browser-function 'navi2ch-browse-url)
	(error "Set navi2ch-browse-url-browser-function correctly."))
    (cond ((and navi2ch-browse-url-image-program ; images
		(file-name-extension url)
		(member (downcase (file-name-extension url))
			navi2ch-browse-url-image-extentions))
	   (navi2ch-browse-url-image url))
	  (t				; others
	   (setq args (or args (list new-window-flag)))
	   (apply 'browse-url url args)))))

(defun navi2ch-browse-url-image (url &optional new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-image-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`navi2ch-browse-url-image-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not navi2ch-browse-url-image-program)
    (error "No browser defined (`navi2ch-browse-url-image-program')"))
  (apply 'start-process (concat navi2ch-browse-url-image-program url) nil
         navi2ch-browse-url-image-program
         (append navi2ch-browse-url-image-args (list url))))

;; from apel
(defsubst navi2ch-put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
        (progn
          (setcdr pair value)
          alist)
      (cons (cons item value) alist))))

(defun navi2ch-next-property (point prop)
  (when (get-text-property point prop)
    (setq point (next-single-property-change point prop)))
  (when point
    (setq point (next-single-property-change point prop)))
  point)

(defun navi2ch-previous-property (point prop)
  (when (get-text-property point prop)
    (setq point (previous-single-property-change point prop)))
  (when point
    (unless (get-text-property (1- point) prop)
      (setq point (previous-single-property-change point prop)))
    (when point
      (1- point))))

(defun navi2ch-change-text-property (point prop value)
  (unless (get-text-property point prop)
    (error "POINT (%d) does not have property %s" point prop))
  (let ((start (if (or (= (point-min) point)
		       (not (eq (get-text-property (1- point) prop)
				(get-text-property point prop))))
		   point
		 (or (previous-single-property-change point prop) point)))
	(end (or (next-single-property-change point prop) point)))
    (put-text-property start end prop value)))

(defun navi2ch-set-minor-mode (mode name map)
  (make-variable-buffer-local mode)
  (unless (assq mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list mode name) minor-mode-alist)))
  (unless (assq mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons mode map) minor-mode-map-alist))))

(defun navi2ch-call-process-buffer (program &rest args)
  "今の buffer で PROGRAM を呼んで変更する"
  (apply 'call-process-region (point-min) (point-max) program t t nil args))

(defun navi2ch-alist-list-to-alist (list key1 &optional key2)
  (mapcar
   (lambda (x)
     (cons (cdr (assq key1 x))
	   (if key2
	       (cdr (assq key2 x))
	     x)))
   list))

(defun navi2ch-write-region (begin end filename)
  (write-region begin end filename nil 'no-msg))

(defun navi2ch-enable-readcgi-p (host)
  "HOST が read.cgi を使うホストかどうかを返す。"
  (if navi2ch-enable-readcgi
      (not (member host
		   navi2ch-disable-readcgi-host-list))
    (member host
	    navi2ch-enable-readcgi-host-list)))

(defun navi2ch-get-major-mode (buffer)
  (when (get-buffer buffer)
    (save-excursion
      (set-buffer buffer)
      major-mode)))

(defun navi2ch-set-mode-line-identification ()
  (let ((offline '(navi2ch-offline navi2ch-modeline-offline navi2ch-modeline-online)))
    (unless navi2ch-mode-line-identification
      (setq navi2ch-mode-line-identification
	    (default-value 'mode-line-buffer-identification)))
    (setq mode-line-buffer-identification
          (list offline
                navi2ch-mode-line-identification)))
  (force-mode-line-update t))

(defun navi2ch-make-datevec (time)
  (timezone-fix-time
   (let ((dtime (decode-time time)))
     (apply 'timezone-make-arpa-date
            (mapcar (lambda (x) (nth x dtime)) '(5 4 3 2))))
   nil nil))

;;; from Wanderlust (elmo-date.el)
(defun navi2ch-get-offset-datevec (datevec offset &optional time)
  (let ((year  (aref datevec 0))
        (month (aref datevec 1))
        (day   (aref datevec 2))
        (hour     (aref datevec 3))
        (minute   (aref datevec 4))
        (second   (aref datevec 5))
        (timezone (aref datevec 6))
        day-number p
        day-of-month)
    (setq p 1)
    (setq day-number (- (timezone-day-number month day year)
                        offset))
    (while (<= day-number 0)
      (setq year (1- year)
            day-number (+ (timezone-day-number 12 31 year)
                          day-number)))
    (while (> day-number (setq day-of-month
                               (timezone-last-day-of-month p year)))
      (setq day-number (- day-number day-of-month))
      (setq p (1+ p)))
    (setq month p)
    (setq day day-number)
    (timezone-fix-time
     (format "%d %s %d %s %s"
             day
             (car (rassq month timezone-months-assoc))
             year
             (if time
                 (format "%d:%d:%d" hour minute second)
               "0:00")
             (cadr timezone)) nil nil)))

(defun navi2ch-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun navi2ch-base64-write-region (start end &optional filename)
  "STARTとENDの間のリージョンをbase64デコードし、FILENAMEに書き出す。

リージョン内に`navi2ch-base64-begin-delimiter'がある場合はそれ以前を無
視し、`navi2ch-base64-end-delimiter'がある場合はそれ以降の
`navi2ch-base64-begin-delimiter'まで、もしくはリージョンの最後までを無
視する。さらに、最初に`navi2ch-base64-line-regexp'にマッチする直前まで
と、最後に`navi2ch-base64-line-regexp'にマッチする直後までも無視する。

base64デコードすべき内容がない場合はエラーになる。"
  (interactive "r")
  (save-excursion
    (let ((buf (current-buffer))
	  (default-filename nil)
	  (mode nil)
	  (susv3 nil))
      ;; insertした後に削るのは無駄なのであらかじめ絞り込んでおく
      (goto-char start)
      (cond
       ((re-search-forward navi2ch-base64-begin-delimiter-regexp end t)
	(setq default-filename (match-string 2))
	(goto-char (match-end 0)))
       ((re-search-forward navi2ch-base64-susv3-begin-delimiter-regexp end t)
	(setq default-filename (match-string 2)
	      mode (string-to-number (match-string 1) 8)
	      susv3 t)
	(goto-char (match-end 0))))
      (if (re-search-forward navi2ch-base64-line-regexp end t)
	  (setq start (match-beginning 0))
	(error "No base64 data"))
      (goto-char end)
      (if (or (and susv3 (re-search-backward
			  navi2ch-base64-susv3-end-delimiter-regexp start t))
	      (re-search-backward navi2ch-base64-end-delimiter-regexp start t))
	  (goto-char (match-beginning 0)))
      (if (re-search-backward navi2ch-base64-line-regexp start t)
	  (setq end (match-end 0)))
      (with-temp-buffer
	(let ((buffer-file-coding-system 'binary)
	      (coding-system-for-write 'binary)
	      ;; auto-compress-modeをdisableにする
	      (inhibit-file-name-operation 'write-region)
	      (inhibit-file-name-handlers (cons 'jka-compr-handler
						inhibit-file-name-handlers))
	      cur-point)
	  (insert-buffer-substring buf start end)
	  (goto-char (point-min))
	  (while (re-search-forward navi2ch-base64-end-delimiter-regexp
				    nil t)
	    (setq cur-point (match-beginning 0))
	    (if (re-search-forward navi2ch-base64-begin-delimiter-regexp
				   nil t)
		(delete-region cur-point (match-end 0))
	      (delete-region cur-point (point-max)))
	    (goto-char cur-point))
	  (base64-decode-region (point-min) (point-max))
	  (if (not filename)
	      (setq filename (read-file-name
			      (if default-filename
				  (format "Decode to file (default `%s'): "
					  default-filename)
				"Decode to file: ")
			      nil default-filename)))
	  (when (file-directory-p filename)
	    (setq filename (expand-file-name default-filename filename)))
	  (when (or (not (file-exists-p filename))
		    (y-or-n-p (format "File `%s' exists; overwrite? "
				      filename)))
	    (write-region (point-min) (point-max) filename)
	    (if (and susv3 mode)
		(condition-case nil
		    (set-file-modes filename mode)
		  (error nil)))))))))

(defun navi2ch-base64-insert-file (filename)
  "FILENAMEをbase64エンコードし、現在のポイントに挿入する。"
  (interactive "fEncode and insert file: ")
  (save-excursion
    (let ((str nil))
      (with-temp-buffer
	(let ((buffer-file-coding-system 'binary))
	  (insert-file-contents-literally filename)
	  (base64-encode-region (point-min) (point-max))
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (insert (format "%s(%s)\n" navi2ch-base64-begin-delimiter
			  (file-name-nondirectory filename)))
	  (while (= (move-to-column navi2ch-base64-fill-column)
		    navi2ch-base64-fill-column)
	    (insert "\n"))
	  (goto-char (point-max))
	  (insert (format "\n%s\n" navi2ch-base64-end-delimiter))
	  (setq str (buffer-string))))
      (insert str))))

(defun navi2ch-url-to-host (url)
  (when url
    (cond
     ((string-match "http://\\([^/]+\\)" url)
      (match-string 1 url))
     ((string-match "x-localbbs://" url)
      "localhost"))))

(defun navi2ch-read-string (prompt &optional initial-input history)
  (let ((minibuffer-allow-text-properties nil))
    (read-string prompt initial-input history)))

(defun navi2ch-temp-directory ()
  (let ((dir (expand-file-name "tmp" navi2ch-directory)))
    (or (file-directory-p dir)
	(make-directory dir))
    dir))

(defun navi2ch-strip-properties (obj)
  "OBJ 中の文字列を再帰的に探し、テキスト属性を外したオブジェクトを返す。
元の OBJ は変更しない。"
  (cond
   ((consp obj)
    (let* ((ret (cons (car obj) (cdr obj)))
	   (seq ret))
      ;; 長いリストをコピーする際にスタックオーバーフローになるので
      ;; 再帰をループに展開。
      (while (consp seq)
	(setcar seq (navi2ch-strip-properties (car seq)))
	(if (consp (cdr seq))
	    (setcdr seq (cons (cadr seq) (cddr seq)))
	  (setcdr seq (navi2ch-strip-properties (cdr seq))))
	(setq seq (cdr seq)))
      ret))
   ((stringp obj)
    (let ((str (copy-sequence obj)))
      (set-text-properties 0 (length str) nil str)
      str))
   ((vectorp obj)
    (vconcat (mapcar 'navi2ch-strip-properties obj)))
   (t obj)))

(defun navi2ch-add-replace-html-tag (tag value)
  "TAG を表示する際に VALUE で置き換える。
ののたんのAAを表示するなら ~/.navi2ch/init.el に
\(navi2ch-add-replace-html-tag (navi2ch-string-as-multibyte \"\\372D\")
                              \"ｖ\")
と書く。"
  ;; ののたんの口を navi2ch-replace-html-tag-alist に入れると
  ;; regexp-opt が無限再帰になっちゃうのれす。
  (add-to-list 'navi2ch-replace-html-tag-regexp-alist
	       (cons (regexp-quote tag) value))
  (setq navi2ch-replace-html-tag-regexp
	(concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
		"\\|"
		(mapconcat 'car
			   navi2ch-replace-html-tag-regexp-alist "\\|"))))

(defun navi2ch-add-replace-html-tag-regexp (tag value)
  "TAG を表示する際に VALUE で置き換える。
TAG は正規表現。"
  (add-to-list 'navi2ch-replace-html-tag-regexp-alist
	       (cons tag value))
  (setq navi2ch-replace-html-tag-regexp
	(concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
		"\\|"
		(mapconcat 'car
			   navi2ch-replace-html-tag-regexp-alist "\\|"))))

(defun navi2ch-filename-to-url (filename)
  (concat "file://" (expand-file-name filename)))

(defun navi2ch-chop-/ (dirname)
  (save-match-data
    (if (string-match "/\\'" dirname)
	(replace-match "" nil t dirname)
      dirname)))

(defun navi2ch-rename-file (file newname &optional ok-if-already-exists)
  (rename-file (navi2ch-chop-/ file)
	       (navi2ch-chop-/ newname) ok-if-already-exists))

(defsubst navi2ch-propertize (string &rest properties)
  "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result"
  ;; ドキュメントは Emacs 21 からコピペ
  (prog1
      (setq string (copy-sequence string))
    (add-text-properties 0 (length string) properties string)))

(defun navi2ch-set-keymap-default-binding (map command)
  "キーマップのデフォルトバインドを設定する。"
  (funcall (if (fboundp 'set-keymap-default-binding)
	       'set-keymap-default-binding
	     (lambda (map command)
	       (setcdr map (cons (cons t command) (cdr map)))
	       command))
	   map command))

(defun navi2ch-char-valid-p (obj)
  "オブジェクトがキャラクタかどうか調べる。"
  (navi2ch-ifxemacs
      (characterp obj)
    (char-valid-p obj)))

;;; ロック
;; 最も汎用的な mkdir ロックを実装してみた。
;; DIRECTORY に LOCKNAME というディレクトリがある場合はそのディレクトリは
;; ロックされているということになる。
(defun navi2ch-lock-directory (directory &optional lockname)
  "LOCKNAME を使い、DIRECTORY をロックする。
LOCKNAME が省略された場合は \"lockdir\" を使用する。
LOCKNAME が絶対パスではない場合、DIRECTORY からの相対パスとして扱う。
ロックに成功したら non-nil を、失敗したら nil を返す。"
  (setq lockname (navi2ch-chop-/ (expand-file-name (or lockname "lockdir")
						   directory))
	directory (file-name-directory lockname))
  (let ((make-directory-function (or (navi2ch-fboundp 'make-directory-internal)
				     'make-directory)))
    (if (not (file-exists-p lockname))	; lockdir がすでにあると失敗
	(condition-case error
	    (and (progn
		   ;; まず、親ディレクトリを作っておく。
		   (unless (file-directory-p directory)
		     (make-directory directory t))
		   (file-directory-p directory))
		 (progn
		   ;; file-name-handler-alist があると mkdir が直接呼
		   ;; ばれない可能性がある。
		   (let ((file-name-handler-alist nil))
		     (funcall make-directory-function lockname))
		   (file-exists-p lockname))) ; 念のため、確認しておく
	  (error
	   (message "%s" (error-message-string error))
	   (sit-for 3)
	   (discard-input)
	   nil)))))

(defun navi2ch-unlock-directory (directory &optional lockname)
  "LOCKNAME を使い、DIRECTORY のロックを解除する。
LOCKNAME が省略された場合は \"lockdir\" を使用する。
LOCKNAME が絶対パスではない場合、DIRECTORY からの相対パスとして扱う。
ロックの解除に成功したら non-nil を、失敗したら nil を返す。"
  (setq lockname (navi2ch-chop-/ (expand-file-name (or lockname "lockdir")
						   directory)))
  (ignore-errors
    (delete-directory lockname))
  (not (file-exists-p lockname)))

(navi2ch-ifxemacs
    (progn
      (defalias 'navi2ch-line-beginning-position 'point-at-bol)
      (defalias 'navi2ch-line-end-position 'point-at-eol))
  (defalias 'navi2ch-line-beginning-position 'line-beginning-position)
  (defalias 'navi2ch-line-end-position 'line-end-position))

(defun navi2ch-count-lines-file (file)
  "そのファイルの行数を数える"
  (with-temp-buffer
    (insert-file-contents file)
    (count-lines (point-min) (point-max))))
    
(run-hooks 'navi2ch-util-load-hook)
;;; navi2ch-util.el ends here
