;;; navi2ch-util.el --- useful utilities for navi2ch

;; Copyright (C) 2000-2004 by Navi2ch Project
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
(defconst navi2ch-util-ident
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
    ("<hr>" . "\n--\n")
    ("＠｀" . ","))
  "置換する html のタグの連想リスト(正規表現は使えない)")

(defvar navi2ch-replace-html-tag-regexp-alist
  '(("</?[?!a-zA-Z][^<>]*>" . "")
    ("&[a-z][a-z0-9]*;?" . navi2ch-entity-reference-to-str)
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

(defconst navi2ch-uuencode-begin-delimiter-regexp
  "^begin \\([0-7]+\\) \\([^ \n]+\\)$"
  "uuencode されたコードの前のデリミタにマッチする正規表現。")
(defconst navi2ch-uuencode-end-delimiter-regexp
  "^end\\([ \t]*\\)$"
  "uuencode されたコードの後のデリミタにマッチする正規表現。")

(defconst navi2ch-uuencode-line-regexp
  "^[!-`]+$"
  "uuencode されたコードのみが含まれる行にマッチする正規表現。")

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

(defvar navi2ch-coding-system
  (or (car (memq 'cp932 (coding-system-list)))
      'shift_jis))

(defvar navi2ch-offline nil "オフラインモードかどうか")
(defvar navi2ch-online-indicator  "[ON] ")
(defvar navi2ch-offline-indicator "[--] ")
(defvar navi2ch-modeline-online navi2ch-online-indicator)
(defvar navi2ch-modeline-offline navi2ch-offline-indicator)
(put 'navi2ch-modeline-online 'risky-local-variable t)
(put 'navi2ch-modeline-offline 'risky-local-variable t)

;; shut up XEmacs warnings
(eval-when-compile
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

(defmacro navi2ch-ifemacsce (then &rest else)
  "If on EmacsCE, do THEN, else do ELSE.
Expanded at compilation time."
  `(if (string-match "windowsce" system-configuration)
       ,then
     (progn ,@else)))
(put 'navi2ch-ifemacsce 'lisp-indent-function 1)

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
(defun navi2ch-mouse-key (num)
  (navi2ch-ifxemacs
      (intern (format "button%d" num))
    (vector (intern (format "mouse-%d" num)))))

(defun navi2ch-define-mouse-key (map num command)
  (define-key map (navi2ch-mouse-key num) command))

(defvar navi2ch-delete-keys
  (list "\d" [del] [delete] [backspace]
	(navi2ch-ifxemacs
	    [(shift space)]
	  [(shift ? )])))

(defun navi2ch-define-delete-keys (map command)
  (dolist (key navi2ch-delete-keys)
    (define-key map key command)))

;; from apel
(defalias 'navi2ch-set-buffer-multibyte
  (navi2ch-ifxemacs #'identity #'set-buffer-multibyte))

;; from apel
(defalias 'navi2ch-string-as-unibyte
  (navi2ch-ifxemacs #'identity #'string-as-unibyte))

(defalias 'navi2ch-string-as-multibyte
  (navi2ch-ifxemacs #'identity #'string-as-multibyte))

(defsubst navi2ch-match-string-no-properties (num &optional string)
  (navi2ch-ifxemacs
      (match-string num string)
    (match-string-no-properties num string)))

(defsubst navi2ch-no-logging-message (fmt &rest args)
  (navi2ch-ifxemacs
      (apply #'lmessage 'no-log fmt args)
    (let ((message-log-max nil))
      (apply #'message fmt args))))

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

(defun navi2ch-insert-file-contents (file &optional begin end)
  (let ((coding-system-for-read navi2ch-coding-system)
        (coding-system-for-write navi2ch-coding-system))
    (insert-file-contents file nil begin end)))

(defun navi2ch-expand-file-name (file)
  (let ((result (expand-file-name (navi2ch-replace-string
				   navi2ch-file-name-reserved-char-regexp
				   (lambda (x)
				     (format "%%%X" (string-to-char x)))
				   file t)
				  navi2ch-directory)))
    (if (string-match (format "^%s"
			      (regexp-quote (file-name-as-directory
					     (expand-file-name navi2ch-directory))))
		      result)
	result
      (error "Wrong file name"))))

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
			    str t nil t)))

(defsubst navi2ch-replace-html-tag-with-buffer ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward navi2ch-replace-html-tag-regexp nil t)
      (replace-match (navi2ch-replace-html-tag-to-string (match-string 0))
		     nil t))))

(defsubst navi2ch-replace-html-tag-with-temp-buffer (str)
  (with-temp-buffer
    (insert str)
    (navi2ch-replace-html-tag-with-buffer)
    (buffer-string)))

(defun navi2ch-entity-reference-to-str (ref)
  "文字実体参照をデコード。"
  (save-match-data
    (if (and navi2ch-decode-character-references
	     (string-match "&\\([^;]+\\)" ref))
	(let ((code (cdr (assoc (match-string 1 ref) navi2ch-entity-alist))))
	  (or (and code (navi2ch-ucs-to-str code)) ref))
      ref)))

(defun navi2ch-numeric-reference-to-str (ref)
  "数値文字参照をデコード。"
  (save-match-data
    (if (and navi2ch-decode-character-references
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
	c)
    (if prompt
	(navi2ch-no-logging-message "%s" prompt))
    (setq c (read-char))
    (if (and prompt
	     (navi2ch-char-valid-p c))
	(navi2ch-no-logging-message "%s%c" prompt c))
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

(defun navi2ch-read-event (&optional prompt)
  "PROMPT (non-nil の場合) を表示して event を読む。"
  (let ((cursor-in-echo-area t)
	e)
    (if prompt
	(navi2ch-no-logging-message "%s" prompt))
    (navi2ch-ifxemacs
	(setq e (next-command-event nil prompt))
      (setq e (read-event prompt)))
    (if prompt
	(navi2ch-no-logging-message "%s%s" prompt (single-key-description e)))
    e))

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

;; (defun navi2ch-change-text-property (point prop value)
;;   (unless (get-text-property point prop)
;;     (error "POINT (%d) does not have property %s" point prop))
;;   (let ((start (if (or (= (point-min) point)
;; 		       (not (eq (get-text-property (1- point) prop)
;; 				(get-text-property point prop))))
;; 		   point
;; 		 (or (previous-single-property-change point prop) point)))
;; 	(end (or (next-single-property-change point prop) point)))
;;     (put-text-property start end prop value)))

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
		'navi2ch-mode-line-identification)))
  (force-mode-line-update t))

(defun navi2ch-end-of-buffer (&optional arg)
  "バッファの最終行に移動。"
  (interactive "P")
  (end-of-buffer arg)
  (when (eobp) (forward-line -1)))

(defun navi2ch-uudecode-region (start end &optional filename)
  "STARTとENDの間のリージョンをuudecodeする。
FILENAMEが指定されると、FILENAMEにも書き出す。"
  (interactive "r")
  (let* ((coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (mode "600")
	 (file (expand-file-name
		(or filename
		    (make-temp-name (navi2ch-temp-directory)))))
	 (default-directory (file-name-directory file))
	 (buf (current-buffer))
	 rc)
    (unwind-protect
	(progn
	  (with-temp-buffer
	    (insert-buffer-substring buf start end)
	    (goto-char (point-min))
	    (when (re-search-forward navi2ch-uuencode-begin-delimiter-regexp
				     nil t)
	      (setq mode (navi2ch-match-string-no-properties 1))
	      (forward-line)
	      (delete-region (point-min) (point)))
	    (insert (format "begin %s %s\n"
			    mode (file-name-nondirectory file)))
	    (goto-char (point-max))
	    (when (re-search-backward navi2ch-uuencode-end-delimiter-regexp
				      nil t)
	      (delete-region (match-beginning 0) (point-max)))
	    (insert "end\n")
	    (setq rc (apply 'call-process-region
			    (point-min) (point-max)
			    navi2ch-uudecode-program
			    nil nil nil
			    navi2ch-uudecode-args)))
	  (when (and (= rc 0)
		     (file-exists-p file))
	    (delete-region start end)
	    (insert-file-contents-literally file)
	    (when filename
	      (message "Wrote %s" filename))))
      (ignore-errors (unless filename (delete-file file))))
    (when (not (= rc 0))
      (error "uudecode error"))))

(defun navi2ch-uudecode-write-region (start end &optional filename)
  "STARTとENDの間のリージョンをuudecodeし、FILENAMEに書き出す。

リージョン内に`navi2ch-uuencode-begin-delimiter-regexp'にマッチする行がある
場合はそれ以前を無視し、`navi2ch-uuencode-end-delimiter-regexp'にマッチする行
がある場合は最後のそれ以降を無視する。
さらに、uuencode のフォーマットに従っていない行も無視する。"
  (interactive "r")
  (let ((buf (current-buffer))
	(default-filename nil))
    (save-excursion
      (goto-char start)
      (when (re-search-forward navi2ch-uuencode-begin-delimiter-regexp end t)
	(setq start (match-beginning 0)
	      default-filename (match-string 2)))
      (goto-char end)
      (when (re-search-backward navi2ch-uuencode-end-delimiter-regexp start t)
	;; exclude "end"
	(setq end (match-beginning 0))))
    (unless filename
      (setq filename (expand-file-name
		      (read-file-name
		       (if default-filename
			   (format "Uudecode to file (default `%s'): "
				   default-filename)
			 "Uudecode to file: ")
		       nil default-filename))))
    (when (file-directory-p filename)
      (if default-filename
	  (setq filename (expand-file-name default-filename filename))
	(error "%s is a directory" filename)))
    (when (or (not (file-exists-p filename))
	      (y-or-n-p (format "File `%s' exists; overwrite? "
				filename)))
      (with-temp-buffer
	(insert-buffer-substring buf start end)
	(goto-char (point-min))
	(while (search-forward "？" nil t) ;for 2ch
	  (replace-match "&#" nil t))
	(goto-char (point-min))
	(forward-line)
	(while (not (eobp))
	  (let* ((char (char-after))
		 (len (- (navi2ch-line-beginning-position 2) (point))))
	    (when (char-equal char ?`)
	      (setq char ? ))
	    (if (and (looking-at navi2ch-uuencode-line-regexp)
		     (< len 63)
		     (= len (- (* (/ char 3) 4) 38)))
		(forward-line)
	      (delete-region (point) (navi2ch-line-beginning-position 2)))))
	(insert "end\n")
	(navi2ch-uudecode-region (point-min) (point-max) filename)))))

(defun navi2ch-base64-write-region (start end &optional filename)
  "STARTとENDの間のリージョンをbase64デコードし、FILENAMEに書き出す。

リージョン内に`navi2ch-base64-begin-delimiter-regexp' か
`navi2ch-base64-susv3-begin-delimiter-regexp' にマッチする行がある場合は
それ以前を無視し、
`navi2ch-base64-end-delimiter-regexp' か
`navi2ch-base64-susv3-end-delimiter-regexp' にマッチする行
がある場合は最後のそれ以降を無視する。
さらに、`navi2ch-base64-line-regexp'にマッチしない行も無視する。

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
      (unless filename
	(setq filename (expand-file-name
			(read-file-name
			 (if default-filename
			     (format "Base64-decode to file (default `%s'): "
				     default-filename)
			   "Base64-decode to file: ")
			 nil default-filename))))
      (when (file-directory-p filename)
	(if default-filename
	    (setq filename (expand-file-name default-filename filename))
	  (error "%s is a directory" filename)))
      (when (or (not (file-exists-p filename))
		(y-or-n-p (format "File `%s' exists; overwrite? "
				  filename)))
	(with-temp-buffer
	  (let ((buffer-file-coding-system 'binary)
		(coding-system-for-write 'binary)
		;; auto-compress-modeをdisableにする
		(inhibit-file-name-operation 'write-region)
		(inhibit-file-name-handlers (cons 'jka-compr-handler
						  inhibit-file-name-handlers)))
	    (insert-buffer-substring buf start end)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (looking-at navi2ch-base64-line-regexp)
		  (forward-line)
		(delete-region (point) (navi2ch-line-beginning-position 2))))
	    (base64-decode-region (point-min) (point-max))
	    (write-region (point-min) (point-max) filename)
	    (if (and susv3 mode)
		(condition-case nil
		    ;; 511 = (string-to-number "0777" 8)
		    (set-file-modes filename (logand mode 511))
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
     ((string-match "^http://\\([^/]+\\)" url)
      (match-string 1 url))
     ((string-match "^x-localbbs://" url)
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

(defun navi2ch-update-html-tag-regexp ()
  (setq navi2ch-replace-html-tag-regexp
	(concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
		"\\|"
		(mapconcat 'car
			   navi2ch-replace-html-tag-regexp-alist "\\|"))))

(defun navi2ch-add-replace-html-tag (tag value)
  "TAG を表示する際に VALUE で置き換える。"
  (let ((as-regexp (condition-case nil
		       (progn
			 ;; 文字列によっては regexp-opt-group() が無限
			 ;; 再帰になる
			 (regexp-opt (list "あ" tag))
			 nil)
		     (error t))))
    (if as-regexp
	(navi2ch-add-replace-html-tag-regexp (regexp-quote tag) value)
      (add-to-list 'navi2ch-replace-html-tag-alist
		   (cons tag value))
      (navi2ch-update-html-tag-regexp))))

(defun navi2ch-add-replace-html-tag-regexp (regexp value)
  "REGEXP にマッチする tag を表示する際に VALUE で置き換える。"
  (add-to-list 'navi2ch-replace-html-tag-regexp-alist
	       (cons regexp value))
  (navi2ch-update-html-tag-regexp))

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
  (funcall (or (navi2ch-fboundp 'set-keymap-default-binding)
	       (lambda (map command)
		 (define-key map [t] command)))
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

(defalias 'navi2ch-line-beginning-position
  (navi2ch-ifxemacs 'point-at-bol 'line-beginning-position))
(defalias 'navi2ch-line-end-position
  (navi2ch-ifxemacs 'point-at-eol 'line-end-position))

(defun navi2ch-count-lines-file (file)
  "そのファイルの行数を数える"
  (with-temp-buffer
    (insert-file-contents file)
    (count-lines (point-min) (point-max))))

(defun navi2ch-compare-number-list (list1 list2)
  "LIST1 と LIST2 の各要素を数値として比較する。
LIST1 と LIST2 が等しい場合は 0 を、LIST1 の方が大きい場合は正数を、
LIST2 の方が大きい場合は負数を返す。"
  (let ((r 0)
	n1 n2)
    (while (and (or list1 list2)
		(= r 0))
      (setq n1 (car-safe list1)
	    list1 (cdr-safe list1)
	    n2 (car-safe list2)
	    list2 (cdr-safe list2)
	    r (- (or n1 0) (or n2 0))))
    r))

(defun navi2ch-lists-to-list-of-list (&rest lists)
  "LISTS の各要素をまとめた要素からなるリストを返す。
例えば '(0 1 2) '(a b) を渡すと ((0 a) (1 b) (2 nil)) を返す。"
  (let (r)
    (while (remq nil lists)
      (push (mapcar #'car lists) r)
      (setq lists (mapcar #'cdr lists)))
    (nreverse r)))

(defun navi2ch-add-number-lists (&rest lists)
  "LISTS 中の各リストの各要素を数値として加算する。"
  (mapcar (lambda (l)
	    (apply #'+ (mapcar (lambda (x) (or x 0)) l)))
	  (apply #'navi2ch-lists-to-list-of-list lists)))

(defun navi2ch-compare-times (t1 t2)
  "T1 が T2 より新しければ non-nil を返す。"
  (> (navi2ch-compare-number-list t1 t2) 0))

(defun navi2ch-add-days-to-time (time days)
  "TIME の DAYS 日後 (負の場合は前) の TIME を返す。"
  (let ((decoded (decode-time time)))
    (setf (nth 3 decoded) (+ (nth 3 decoded) days))
    (apply #'encode-time decoded)))

(defun navi2ch-which (file)
  (when (stringp file)
    (catch 'loop
      (dolist (path exec-path)
	(setq path (expand-file-name file path))
	(dolist (candidate (list path (concat path ".exe")))
	  (when (and (file-exists-p candidate)
		     (file-executable-p candidate)
		     (not (file-directory-p candidate)))
	    (throw 'loop candidate)))))))

(defun navi2ch-union (list1 list2)
  "Combine LIST1 and LIST2.
This function is a cutdown version of cl-seq's one."
  (cond ((null list1) list2) ((null list2) list1)
	((equal list1 list2) list1)
	(t (dolist (x list2)
	     (unless (member x list1)
	       (setq list1 (cons x list1))))
	   list1)))

(defun navi2ch-set-difference (list1 list2)
  "Combine LIST1 and LIST2.
This function is a cutdown version of cl-seq's one."
  (if (or (null list1) (null list2)) list1
    (let ((res nil))
      (dolist (x list1)
	(unless (member x list2)
	  (setq res (cons x res))))
      res)))

(defun navi2ch-expand-newtext (newtext original)
  (substring (replace-match newtext (not case-fold-search) nil original)
	     (match-beginning 0)
	     (and (< (match-end 0) (length original))
		  (- (match-end 0) (length original)))))

(defun navi2ch-apply-filters (board filter-list)
  (dolist (filter filter-list)
    (if (stringp (car-safe filter))
        (apply 'navi2ch-call-process-buffer
               (mapcar (lambda (x)
                         (if (eq x 'board)
                             (cdr (assq 'id board))
                           x))
                       filter))
      (funcall filter))))

;; shut up byte-compile warnings
(eval-when-compile
  (navi2ch-defalias-maybe 'keywordp 'ignore)
  (navi2ch-defalias-maybe 'characterp 'ignore))

(defun navi2ch-quote-maybe (sexp)
  "Quote SEXP iff it is not self quoting."
  ;; `custom-quote'のパクり。
  (if (or (memq sexp '(t nil))
	  (if (fboundp 'keywordp)
	      (keywordp sexp)
	    (and (symbolp sexp)
		 (eq (aref (symbol-name sexp) 0) ?:)))
	  (eq (car-safe sexp) 'lambda)
	  (stringp sexp)
	  (numberp sexp)
	  (and (fboundp 'characterp)
	       (characterp sexp))
	  (vectorp sexp)
	  (navi2ch-ifxemacs
	      (bit-vector-p sexp)))
      sexp
    (list 'quote sexp)))

(defsubst navi2ch-right-align-strings (s1 s2)
  (let* ((l (max (length s1) (length s2)))
	 (f (format "%%%ds" l)))
    (list (format f s1) (format f s2))))

(defun navi2ch-right-aligned-string< (s1 s2)
  (apply #'string< (navi2ch-right-align-strings s1 s2)))

(defun navi2ch-right-aligned-string= (s1 s2)
  (apply #'string= (navi2ch-right-align-strings s1 s2)))

(defsubst navi2ch-regexp-alist-to-number-alist (regexp-alist)
  (if (integerp (caar regexp-alist))
      regexp-alist
    (let ((n 1))
      (mapcar (lambda (elt)
		(let ((r (concat "\\(" (car elt) "\\)")))
		  (prog1
		      (list n r elt)
		    (setq n (+ n (regexp-opt-depth r))))))
	      regexp-alist))))

(defsubst navi2ch-match-regexp-alist-subr (match-function regexp-alist)
  "REGEXP-ALIST の各要素の car を正規表現とし、MATCH-FUNCTION を呼び出す。
マッチした要素を返す。
REGEXP-ALIST 中の正規表現は連結されるため、正規表現中の \\数字等の
back reference は有効に動作しない。
`navi2ch-regexp-alist-to-number-alist' を使用して REGEXP-ALIST を
あらかじめ内部形式に変換しておくことも可能。"
  (let* ((number-alist (navi2ch-regexp-alist-to-number-alist regexp-alist))
	 (number-list (mapcar #'car number-alist))
	 (combined-regexp (mapconcat #'cadr number-alist "\\|")))
    (when (funcall match-function combined-regexp)
      (dolist (n number-list)
	(when (match-beginning n)
	  (return (nth 2 (assq n number-alist))))))))

(defun navi2ch-re-search-forward-regexp-alist
  (regexp-alist &optional bound noerror count)
  "REGEXP-ALIST の各要素の car を正規表現とし、`re-search-forward' を呼び出す。
`match-data' をマッチした正規表現の物にし、マッチした要素を返す。
REGEXP-ALIST については `navi2ch-match-regexp-alist-subr' を参照。
BOUND NOERROR COUNT は `re-search-forward' にそのまま渡される。"
  (let ((matched-elt (navi2ch-match-regexp-alist-subr
		      (lambda (regexp)
			(re-search-forward regexp bound noerror count))
		      regexp-alist)))
    (when matched-elt
      (goto-char (match-beginning 0))
      (re-search-forward (car matched-elt) bound noerror count))
    matched-elt))

(defun navi2ch-string-match-regexp-alist (regexp-alist string &optional start)
  "REGEXP-ALIST の各要素の car を正規表現とし、`string-match' を呼び出す。
`match-data' をマッチした正規表現の物にし、マッチした要素を返す。
REGEXP-ALIST については `navi2ch-match-regexp-alist-subr' を参照。
START は `string-match' にそのまま渡される。"
  (let ((matched-elt (navi2ch-match-regexp-alist-subr
		      (lambda (regexp)
			(string-match regexp string start))
		      regexp-alist)))
    (when matched-elt
      (string-match (car matched-elt) string (match-beginning 0)))
    matched-elt))

(defun navi2ch-replace-regexp-alist (regexp-alist &optional fixedcase literal)
  "REGEXP-ALIST の各要素の car を正規表現とし、cdr で置き換える。
cdr が文字列の場合はそれ自身と、関数の場合はマッチした文字列を引数
として呼び出した結果と置き換える。
REGEXP-ALIST については `navi2ch-match-regexp-alist-subr' を参照。
FIXEDCASE、LITERAL は `replace-match' にそのまま渡される。"
  (let ((alist (navi2ch-regexp-alist-to-number-alist regexp-alist))
	elt rep)
    (while (setq elt (navi2ch-re-search-forward-regexp-alist alist nil t))
      (setq rep (cdr elt))
      (replace-match (cond ((stringp rep) rep)
			   ((functionp rep) (funcall rep (match-string 0)))
			   (t (signal 'wrong-type-argument
				      (list 'stringp-or-functionp
					    rep))))
		     fixedcase literal))))

;; XEmacs では `char-width' を考慮してくれないので。
(defun navi2ch-truncate-string-to-width
  (str end-column &optional start-column padding)
  "`truncate-string-to-width' と同等。"
  (let ((col 0)
	(start-column (or start-column 0))
	r)
    (dolist (c (string-to-list str))
      (when (and (>= col start-column)
		 (< col end-column))
	(push c r)
	(setq col (+ col (char-width c)))))
    (when padding
      (while (and (>= col start-column)
		  (< col end-column))
	(push padding r)
	(setq col (+ col (char-width padding)))))
    (concat (nreverse r))))

(defun navi2ch-disabled-key ()
  (interactive)
  (ding)
  (let ((key (this-command-keys)))
    (message "%s (%s) is disabled in Navi2ch."
	     (key-description key)
	     (lookup-key (current-global-map) key))))

(defun navi2ch-caller-p (function-list)
  "呼び出し元の関数が function-list に含まれていれば non-nil を返す。"
  (let ((n 1)
	frame function)
    (while (and (not function)
		(setq frame (backtrace-frame n)))
      (setq n (1+ n))
      (when (car frame)
	(setq function (car (memq (cadr frame) function-list)))))
    function))

(defun navi2ch-compare-version-string (string1 string2)
  "STRING1 と STRING2 をバージョン番号として比較する。
STRING1 と STRING2 が等しければ 0 を、STRING1 の方が大きい場合は正数を、
STRING2 の方が大きい場合は負数を返す。
一般の小数点数とは事なり、4.10 > 4.9 となる。"
  (apply #'navi2ch-compare-number-list
	 (mapcar (lambda (s)
		   (mapcar #'string-to-number
			   (when (string-match "[0-9]+\\(\\.[0-9]+\\)+" s)
			     (split-string (match-string 0 s) "\\."))))
		 (list string1 string2))))

(defun navi2ch-verify-signature-file (signature-file file)
  "FILE を SIGNATURE-FILE で検証する。
正しく検証できると non-nil を返す。"
  (interactive "f署名ファイル: \nf検証ファイル: ")
  (let (exitcode)
    (with-temp-buffer
      (setq exitcode
	    (call-process shell-file-name nil t nil
			  shell-command-switch
			  (format navi2ch-pgp-verify-command-line
				  signature-file file)))
      (goto-char (point-min))
      ;; 後から *Message* バッファで参照できるよう、コマンド出力をすべ
      ;; て表示しておく
      (while (not (eobp))
	(let ((s (buffer-substring (navi2ch-line-beginning-position)
				   (navi2ch-line-end-position))))
	  (when (> (length s) 0)
	    (message "%s" s)))
	(forward-line)))
    (= exitcode 0)))

(defun navi2ch-decode-coding-region-linewise (start end coding-system)
  (save-restriction
    (narrow-to-region start end)
    (let ((bol (point-min)))
      (while (< bol (point-max))
	(goto-char bol)
	;; decode 前後で (navi2ch-line-end-position) の値がずれるのに注意
	(decode-coding-region bol (navi2ch-line-end-position) coding-system)
	(goto-char bol)			; 念のため
	(setq bol (1+ (navi2ch-line-end-position))))))
  (goto-char start))

(run-hooks 'navi2ch-util-load-hook)
;;; navi2ch-util.el ends here
