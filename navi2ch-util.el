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

;;; Commentary:

;;

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
    ("$B!w!.(B" . ","))
  "$BCV49$9$k(B html $B$N%?%0$NO"A[%j%9%H(B ($B@55,I=8=$O;H$($J$$(B)$B!#(B")

(defvar navi2ch-replace-html-tag-regexp-alist
  '(("</?[?!a-zA-Z][^<>]*>" . "")
    ("&[a-z][a-z0-9]*;?" . navi2ch-entity-reference-to-str)
    ("&#[0-9]+;?" . navi2ch-numeric-reference-to-str))
  "$BCV49$9$k(B html $B$N%?%0$NO"A[%j%9%H(B($B@55,I=8=(B)
$BCV49@h$,4X?t$@$H!"CV4985$r0z?t$H$7$F$=$N4X?t$r8F$S$@$7$?$b$N$GCV$-BX$($k!#(B
$B@55,I=8=$,I,MW$J$$>l9g$O(B `navi2ch-replace-html-tag-alist' $B$KF~$l$k(B")

(defvar navi2ch-replace-html-tag-regexp
  (concat (regexp-opt (mapcar 'car navi2ch-replace-html-tag-alist))
	  "\\|"
	  (mapconcat 'car navi2ch-replace-html-tag-regexp-alist "\\|"))
  "$BCV49$9$k(B html $B$N%?%0$N@55,I=8=!#(B
`navi2ch-replace-html-tag-alist' $B$+$i@8@.$5$l$k(B")

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
  "uuencode $B$5$l$?%3!<%I$NA0$N%G%j%_%?$K%^%C%A$9$k@55,I=8=!#(B")
(defconst navi2ch-uuencode-end-delimiter-regexp
  "^end\\([ \t]*\\)$"
  "uuencode $B$5$l$?%3!<%I$N8e$N%G%j%_%?$K%^%C%A$9$k@55,I=8=!#(B")

(defconst navi2ch-uuencode-line-regexp
  "^[!-`]+$"
  "uuencode $B$5$l$?%3!<%I$N$_$,4^$^$l$k9T$K%^%C%A$9$k@55,I=8=!#(B")

(defconst navi2ch-base64-begin-delimiter "----BEGIN BASE64----"
  "base64 $B%3!<%I$NA0$KA^F~$9$k%G%j%_%?!#(B")
(defconst navi2ch-base64-end-delimiter "----END BASE64----"
  "base64 $B%3!<%I$N8e$KA^F~$9$k%G%j%_%?!#(B")

(defconst navi2ch-base64-begin-delimiter-regexp
  (format "^%s\\((\\([^\)]+\\))\\)?.*$"
          (regexp-quote navi2ch-base64-begin-delimiter))
  "base64 $B%3!<%I$NA0$N%G%j%_%?$K%^%C%A$9$k@55,I=8=!#(B")
(defconst navi2ch-base64-end-delimiter-regexp
  (format "^%s.*$" (regexp-quote navi2ch-base64-end-delimiter))
  "base64 $B%3!<%I$N8e$N%G%j%_%?$K%^%C%A$9$k@55,I=8=!#(B")
(defconst navi2ch-base64-susv3-begin-delimiter-regexp
  "^begin-base64 \\([0-7]+\\) \\([^ \n]+\\)$"
  "SUSv3 $B$N(B uuencode $B$G:n@.$5$l$k(B base64 $B%3!<%I$NA0$N%G%j%_%?$K%^%C%A$9$k@55,I=8=(B")
(defconst navi2ch-base64-susv3-end-delimiter-regexp
  "^====$"
  "SUSv3 $B$N(B uuencode $B$G:n@.$5$l$k(B base64 $B%3!<%I$N8e$N%G%j%_%?$K%^%C%A$9$k@55,I=8=(B")

(defconst navi2ch-base64-line-regexp
  (concat
   "^\\([+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z]\\)*"
   "[+/0-9A-Za-z][+/0-9A-Za-z][+/0-9A-Za-z=][+/0-9A-Za-z=] *$")
  "base64 $B%3!<%I$N$_$,4^$^$l$k9T$K%^%C%A$9$k@55,I=8=!#(B")

(defvar navi2ch-coding-system
  (or (car (memq 'cp932 (coding-system-list)))
      'shift_jis))

(defvar navi2ch-offline nil "$B%*%U%i%$%s%b!<%I$+$I$&$+!#(B")
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
;; Navi2ch$B$N%3!<%I$r%O%/$9$k?M$O"-$r(B~/.emacs$B$K$bF~$l$H$-$^$7$g$&!#(B
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
  "default-file-modes $B$r(B MODE $B$K$7$F(B BODY $B$r<B9T$9$k!#(B"
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
  "STRING $B$K4^$^$l$k(B REGEXP $B$r(B REP $B$GCV49$9$k!#(B
REP $B$,4X?t$N>l9g$O!"%^%C%A$7$?J8;zNs$r0z?t$K$7$F$=$N4X?t$r8F$S=P$9!#(B

FIXEDCASE$B!"(BLITERAL $B$O(B `replace-match' $B$K$=$N$^$^EO$5$l$k!#(B

ALL $B$,(B non-nil $B$J$i$P!"%^%C%A$7$?%F%-%9%H$r$9$Y$FCV49$9$k!#(Bnil $B$J$i(B
$B:G=i$N(B1$B$D$@$1$rCV49$9$k!#(B

REGEXP $B$,8+$D$+$i$J$$>l9g!"(BSTRING $B$r$=$N$^$^JV$9!#(B"
  (save-match-data
    (if all
	;; Emacs 21 $B$N(B replace-regexp-in-string $B$N%Q%/$j!#(B
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

(defun navi2ch-insert-file-contents (file &optional begin end coding-system)
  (setq coding-system (or coding-system navi2ch-coding-system))
  (let ((coding-system-for-read coding-system)
	(coding-system-for-write coding-system))
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
;;   "$B?t;z$r(B minibuffer $B$+$iFI$_9~$`(B"
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
;;       (message "Please enter a number")
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
  "$BJ8;z<BBN;2>H$r%G%3!<%I!#(B"
  (save-match-data
    (if (and navi2ch-decode-character-references
	     (string-match "&\\([^;]+\\)" ref))
	(let ((code (cdr (assoc (match-string 1 ref) navi2ch-entity-alist))))
	  (or (and code (navi2ch-ucs-to-str code)) ref))
      ref)))

(defun navi2ch-numeric-reference-to-str (ref)
  "$B?tCMJ8;z;2>H$r%G%3!<%I!#(B"
  (save-match-data
    (if (and navi2ch-decode-character-references
	     (string-match "&#\\([^;]+\\)" ref))
	(or (navi2ch-ucs-to-str (string-to-int (match-string 1 ref))) "$B".(B")
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
  "PROMPT (non-nil $B$N>l9g(B) $B$rI=<($7$F(B `read-char' $B$r8F$S=P$9!#(B"
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
  "PROMPT $B$rI=<((B (non-nil $B$N>l9g(B) $B$7$F(B `read-char' $B$r8F$S=P$9!#(B
$BF~NO$5$l$?J8;z$,(B LIST $B$K4^$^$l$J$$>l9g!"(BRETRY-PROMPT (nil $B$N>l9g$O(B
PROMPT) $B$rI=<($7$F:FEY(B `read-char' $B$r8F$V!#(B"
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
  "PROMPT (non-nil $B$N>l9g(B) $B$rI=<($7$F(B event $B$rFI$`!#(B"
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
  "SYMBOL $B$,%P%$%s%I$5$l$F$$$J$$;~$O(B nil $B$rJV$9!#(B
boundp $B$H0c$$!"(BSYMBOL $B$,%P%$%s%I$5$l$F$$$k;~$O(B t $B$G$O$J$/%7%s%\%k$rJV$9!#(B"
  (and (boundp symbol) symbol))

(defsubst navi2ch-fboundp (symbol)
  "SYMBOL $B$,%P%$%s%I$5$l$F$$$J$$;~$O(B nil $B$rJV$9!#(B
fboundp $B$H0c$$!"(BSYMBOL $B$,%P%$%s%I$5$l$F$$$k;~$O(B t $B$G$O$J$/%7%s%\%k$rJV$9!#(B"
  (and (fboundp symbol) symbol))

(defun navi2ch-browse-url-internal (url &rest args)
  (let ((browse-url-browser-function (or navi2ch-browse-url-browser-function
					 browse-url-browser-function))
	(new-window-flag (symbol-value (or (navi2ch-boundp
					    'browse-url-new-window-flag)
					   (navi2ch-boundp
					    'browse-url-new-window-p)))))
    (if (eq browse-url-browser-function 'navi2ch-browse-url)
	(error "Set navi2ch-browse-url-browser-function correctly"))
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
  "$B:#$N(B buffer $B$G(B PROGRAM $B$r8F$s$GJQ99$9$k!#(B"
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
  "HOST $B$,(B read.cgi $B$r;H$&%[%9%H$+$I$&$+$rJV$9!#(B"
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
  "$B%P%C%U%!$N:G=*9T$K0\F0!#(B"
  (interactive "P")
  (end-of-buffer arg)
  (when (eobp) (forward-line -1)))

(defun navi2ch-uudecode-region (start end &optional filename)
  "START $B$H(B END $B$N4V$N%j!<%8%g%s$r(B uudecode $B$9$k!#(B
FILENAME $B$,;XDj$5$l$k$H!"(BFILENAME $B$K$b=q$-=P$9!#(B"
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
  "START $B$H(B END $B$N4V$N%j!<%8%g%s$r(B uudecode $B$7!"(BFILENAME $B$K=q$-=P$9!#(B

$B%j!<%8%g%sFb$K(B `navi2ch-uuencode-begin-delimiter-regexp' $B$K%^%C%A$9$k9T$,$"$k(B
$B>l9g$O$=$l0JA0$rL5;k$7!"(B`navi2ch-uuencode-end-delimiter-regexp' $B$K%^%C%A$9$k9T(B
$B$,$"$k>l9g$O:G8e$N$=$l0J9_$rL5;k$9$k!#(B
$B$5$i$K!"(Buuencode $B$N%U%)!<%^%C%H$K=>$C$F$$$J$$9T$bL5;k$9$k!#(B"
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
	(while (search-forward "$B!)(B" nil t) ;for 2ch
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
  "START $B$H(B END $B$N4V$N%j!<%8%g%s$r(B base64 $B%G%3!<%I$7!"(BFILENAME $B$K=q$-=P$9!#(B

$B%j!<%8%g%sFb$K(B `navi2ch-base64-begin-delimiter-regexp' $B$+(B
`navi2ch-base64-susv3-begin-delimiter-regexp' $B$K%^%C%A$9$k9T$,$"$k>l9g$O(B
$B$=$l0JA0$rL5;k$7!"(B
`navi2ch-base64-end-delimiter-regexp' $B$+(B
`navi2ch-base64-susv3-end-delimiter-regexp' $B$K%^%C%A$9$k9T(B
$B$,$"$k>l9g$O:G8e$N$=$l0J9_$rL5;k$9$k!#(B
$B$5$i$K!"(B`navi2ch-base64-line-regexp' $B$K%^%C%A$7$J$$9T$bL5;k$9$k!#(B

base64$B%G%3!<%I$9$Y$-FbMF$,$J$$>l9g$O%(%i!<$K$J$k!#(B"
  (interactive "r")
  (save-excursion
    (let ((buf (current-buffer))
	  (default-filename nil)
	  (mode nil)
	  (susv3 nil))
      ;; insert$B$7$?8e$K:o$k$N$OL5BL$J$N$G$"$i$+$8$a9J$j9~$s$G$*$/(B
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
		;; auto-compress-mode$B$r(Bdisable$B$K$9$k(B
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
  "FILENAME $B$r(B base64 $B%(%s%3!<%I$7!"8=:_$N%]%$%s%H$KA^F~$9$k!#(B"
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
  "OBJ $BCf$NJ8;zNs$r:F5"E*$KC5$7!"%F%-%9%HB0@-$r30$7$?%*%V%8%'%/%H$rJV$9!#(B
$B85$N(B OBJ $B$OJQ99$7$J$$!#(B"
  (cond
   ((consp obj)
    (let* ((ret (cons (car obj) (cdr obj)))
	   (seq ret))
      ;; $BD9$$%j%9%H$r%3%T!<$9$k:]$K%9%?%C%/%*!<%P!<%U%m!<$K$J$k$N$G(B
      ;; $B:F5"$r%k!<%W$KE83+!#(B
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
  "TAG $B$rI=<($9$k:]$K(B VALUE $B$GCV$-49$($k!#(B"
  (let ((as-regexp (condition-case nil
		       (progn
			 ;; $BJ8;zNs$K$h$C$F$O(B regexp-opt-group() $B$,L58B(B
			 ;; $B:F5"$K$J$k(B
			 (regexp-opt (list "$B$"(B" tag))
			 nil)
		     (error t))))
    (if as-regexp
	(navi2ch-add-replace-html-tag-regexp (regexp-quote tag) value)
      (add-to-list 'navi2ch-replace-html-tag-alist
		   (cons tag value))
      (navi2ch-update-html-tag-regexp))))

(defun navi2ch-add-replace-html-tag-regexp (regexp value)
  "REGEXP $B$K%^%C%A$9$k(B tag $B$rI=<($9$k:]$K(B VALUE $B$GCV$-49$($k!#(B"
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
  ;; $B%I%-%e%a%s%H$O(B Emacs 21 $B$+$i%3%T%Z(B
  (prog1
      (setq string (copy-sequence string))
    (add-text-properties 0 (length string) properties string)))

(defun navi2ch-set-keymap-default-binding (map command)
  "$B%-!<%^%C%W$N%G%U%)%k%H%P%$%s%I$r@_Dj$9$k!#(B"
  (funcall (or (navi2ch-fboundp 'set-keymap-default-binding)
	       (lambda (map command)
		 (define-key map [t] command)))
	   map command))

(defun navi2ch-char-valid-p (obj)
  "$B%*%V%8%'%/%H$,%-%c%i%/%?$+$I$&$+D4$Y$k!#(B"
  (navi2ch-ifxemacs
      (characterp obj)
    (char-valid-p obj)))

;;; $B%m%C%/(B
;; $B:G$bHFMQE*$J(B mkdir $B%m%C%/$r<BAu$7$F$_$?!#(B
;; DIRECTORY $B$K(B LOCKNAME $B$H$$$&%G%#%l%/%H%j$,$"$k>l9g$O$=$N%G%#%l%/%H%j$O(B
;; $B%m%C%/$5$l$F$$$k$H$$$&$3$H$K$J$k!#(B
(defun navi2ch-lock-directory (directory &optional lockname)
  "LOCKNAME $B$r;H$$!"(BDIRECTORY $B$r%m%C%/$9$k!#(B
LOCKNAME $B$,>JN,$5$l$?>l9g$O(B \"lockdir\" $B$r;HMQ$9$k!#(B
LOCKNAME $B$,@dBP%Q%9$G$O$J$$>l9g!"(BDIRECTORY $B$+$i$NAjBP%Q%9$H$7$F07$&!#(B
$B%m%C%/$K@.8y$7$?$i(B non-nil $B$r!"<:GT$7$?$i(B nil $B$rJV$9!#(B"
  (setq lockname (navi2ch-chop-/ (expand-file-name (or lockname "lockdir")
						   directory))
	directory (file-name-directory lockname))
  (let ((make-directory-function (or (navi2ch-fboundp 'make-directory-internal)
				     'make-directory)))
    (if (not (file-exists-p lockname))	; lockdir $B$,$9$G$K$"$k$H<:GT(B
	(condition-case error
	    (and (progn
		   ;; $B$^$:!"?F%G%#%l%/%H%j$r:n$C$F$*$/!#(B
		   (unless (file-directory-p directory)
		     (make-directory directory t))
		   (file-directory-p directory))
		 (progn
		   ;; file-name-handler-alist $B$,$"$k$H(B mkdir $B$,D>@\8F(B
		   ;; $B$P$l$J$$2DG=@-$,$"$k!#(B
		   (let ((file-name-handler-alist nil))
		     (funcall make-directory-function lockname))
		   (file-exists-p lockname))) ; $BG0$N$?$a!"3NG'$7$F$*$/(B
	  (error
	   (message "%s" (error-message-string error))
	   (sit-for 3)
	   (discard-input)
	   nil)))))

(defun navi2ch-unlock-directory (directory &optional lockname)
  "LOCKNAME $B$r;H$$!"(BDIRECTORY $B$N%m%C%/$r2r=|$9$k!#(B
LOCKNAME $B$,>JN,$5$l$?>l9g$O(B \"lockdir\" $B$r;HMQ$9$k!#(B
LOCKNAME $B$,@dBP%Q%9$G$O$J$$>l9g!"(BDIRECTORY $B$+$i$NAjBP%Q%9$H$7$F07$&!#(B
$B%m%C%/$N2r=|$K@.8y$7$?$i(B non-nil $B$r!"<:GT$7$?$i(B nil $B$rJV$9!#(B"
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
  "$B$=$N%U%!%$%k$N9T?t$r?t$($k!#(B"
  (with-temp-buffer
    (insert-file-contents file)
    (count-lines (point-min) (point-max))))

(defun navi2ch-compare-number-list (list1 list2)
  "LIST1 $B$H(B LIST2 $B$N3FMWAG$r?tCM$H$7$FHf3S$9$k!#(B
LIST1 $B$H(B LIST2 $B$,Ey$7$$>l9g$O(B 0 $B$r!"(BLIST1 $B$NJ}$,Bg$-$$>l9g$O@5?t$r!"(B
LIST2 $B$NJ}$,Bg$-$$>l9g$OIi?t$rJV$9!#(B"
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
  "LISTS $B$N3FMWAG$r$^$H$a$?MWAG$+$i$J$k%j%9%H$rJV$9!#(B
$BNc$($P(B '(0 1 2) '(a b) $B$rEO$9$H(B ((0 a) (1 b) (2 nil)) $B$rJV$9!#(B"
  (let (r)
    (while (remq nil lists)
      (push (mapcar #'car lists) r)
      (setq lists (mapcar #'cdr lists)))
    (nreverse r)))

(defun navi2ch-add-number-lists (&rest lists)
  "LISTS $BCf$N3F%j%9%H$N3FMWAG$r?tCM$H$7$F2C;;$9$k!#(B"
  (mapcar (lambda (l)
	    (apply #'+ (mapcar (lambda (x) (or x 0)) l)))
	  (apply #'navi2ch-lists-to-list-of-list lists)))

(defun navi2ch-compare-times (t1 t2)
  "T1 $B$,(B T2 $B$h$j?7$7$1$l$P(B non-nil $B$rJV$9!#(B"
  (> (navi2ch-compare-number-list t1 t2) 0))

(defun navi2ch-add-days-to-time (time days)
  "TIME $B$N(B DAYS $BF|8e(B ($BIi$N>l9g$OA0(B) $B$N(B TIME $B$rJV$9!#(B"
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
  ;; `custom-quote'$B$N%Q%/$j!#(B
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
  "REGEXP-ALIST $B$N3FMWAG$N(B car $B$r@55,I=8=$H$7!"(BMATCH-FUNCTION $B$r8F$S=P$9!#(B
$B%^%C%A$7$?MWAG$rJV$9!#(B
REGEXP-ALIST $BCf$N@55,I=8=$OO"7k$5$l$k$?$a!"@55,I=8=Cf$N(B \\$B?t;zEy$N(B
back reference $B$OM-8z$KF0:n$7$J$$!#(B
`navi2ch-regexp-alist-to-number-alist' $B$r;HMQ$7$F(B REGEXP-ALIST $B$r(B
$B$"$i$+$8$aFbIt7A<0$KJQ49$7$F$*$/$3$H$b2DG=!#(B"
  (let* ((number-alist (navi2ch-regexp-alist-to-number-alist regexp-alist))
	 (number-list (mapcar #'car number-alist))
	 (combined-regexp (mapconcat #'cadr number-alist "\\|")))
    (when (funcall match-function combined-regexp)
      (dolist (n number-list)
	(when (match-beginning n)
	  (return (nth 2 (assq n number-alist))))))))

(defun navi2ch-re-search-forward-regexp-alist
  (regexp-alist &optional bound noerror count)
  "REGEXP-ALIST $B$N3FMWAG$N(B car $B$r@55,I=8=$H$7!"(B`re-search-forward' $B$r8F$S=P$9!#(B
`match-data' $B$r%^%C%A$7$?@55,I=8=$NJ*$K$7!"%^%C%A$7$?MWAG$rJV$9!#(B
REGEXP-ALIST $B$K$D$$$F$O(B `navi2ch-match-regexp-alist-subr' $B$r;2>H!#(B
BOUND NOERROR COUNT $B$O(B `re-search-forward' $B$K$=$N$^$^EO$5$l$k!#(B"
  (let ((matched-elt (navi2ch-match-regexp-alist-subr
		      (lambda (regexp)
			(re-search-forward regexp bound noerror count))
		      regexp-alist)))
    (when matched-elt
      (goto-char (match-beginning 0))
      (re-search-forward (car matched-elt) bound noerror count))
    matched-elt))

(defun navi2ch-string-match-regexp-alist (regexp-alist string &optional start)
  "REGEXP-ALIST $B$N3FMWAG$N(B car $B$r@55,I=8=$H$7!"(B`string-match' $B$r8F$S=P$9!#(B
`match-data' $B$r%^%C%A$7$?@55,I=8=$NJ*$K$7!"%^%C%A$7$?MWAG$rJV$9!#(B
REGEXP-ALIST $B$K$D$$$F$O(B `navi2ch-match-regexp-alist-subr' $B$r;2>H!#(B
START $B$O(B `string-match' $B$K$=$N$^$^EO$5$l$k!#(B"
  (let ((matched-elt (navi2ch-match-regexp-alist-subr
		      (lambda (regexp)
			(string-match regexp string start))
		      regexp-alist)))
    (when matched-elt
      (string-match (car matched-elt) string (match-beginning 0)))
    matched-elt))

(defun navi2ch-replace-regexp-alist (regexp-alist &optional fixedcase literal)
  "REGEXP-ALIST $B$N3FMWAG$N(B car $B$r@55,I=8=$H$7!"(Bcdr $B$GCV$-49$($k!#(B
cdr $B$,J8;zNs$N>l9g$O$=$l<+?H$H!"4X?t$N>l9g$O%^%C%A$7$?J8;zNs$r0z?t(B
$B$H$7$F8F$S=P$7$?7k2L$HCV$-49$($k!#(B
REGEXP-ALIST $B$K$D$$$F$O(B `navi2ch-match-regexp-alist-subr' $B$r;2>H!#(B
FIXEDCASE$B!"(BLITERAL $B$O(B `replace-match' $B$K$=$N$^$^EO$5$l$k!#(B"
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

;; XEmacs $B$G$O(B `char-width' $B$r9MN8$7$F$/$l$J$$$N$G!#(B
(defun navi2ch-truncate-string-to-width
  (str end-column &optional start-column padding)
  "`truncate-string-to-width' $B$HF1Ey!#(B"
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
  "$B8F$S=P$785$N4X?t$,(B FUNCTION-LIST $B$K4^$^$l$F$$$l$P(B non-nil $B$rJV$9!#(B"
  (let ((n 1)
	frame function)
    (while (and (not function)
		(setq frame (backtrace-frame n)))
      (setq n (1+ n))
      (when (car frame)
	(setq function (car (memq (cadr frame) function-list)))))
    function))

(defun navi2ch-compare-version-string (string1 string2)
  "STRING1 $B$H(B STRING2 $B$r%P!<%8%g%sHV9f$H$7$FHf3S$9$k!#(B
STRING1 $B$H(B STRING2 $B$,Ey$7$1$l$P(B 0 $B$r!"(BSTRING1 $B$NJ}$,Bg$-$$>l9g$O@5?t$r!"(B
STRING2 $B$NJ}$,Bg$-$$>l9g$OIi?t$rJV$9!#(B
$B0lHL$N>.?tE@?t$H$O;v$J$j!"(B4.10 > 4.9 $B$H$J$k!#(B"
  (apply #'navi2ch-compare-number-list
	 (mapcar (lambda (s)
		   (mapcar #'string-to-number
			   (when (string-match "[0-9]+\\(\\.[0-9]+\\)+" s)
			     (split-string (match-string 0 s) "\\."))))
		 (list string1 string2))))

(defun navi2ch-verify-signature-file (signature-file file)
  "FILE $B$r(B SIGNATURE-FILE $B$G8!>Z$9$k!#(B
$B@5$7$/8!>Z$G$-$k$H(B non-nil $B$rJV$9!#(B"
  (interactive "f$B=pL>%U%!%$%k(B: \nf$B8!>Z%U%!%$%k(B: ")
  (let (exitcode)
    (with-temp-buffer
      (setq exitcode
	    (call-process shell-file-name nil t nil
			  shell-command-switch
			  (format navi2ch-pgp-verify-command-line
				  signature-file file)))
      (goto-char (point-min))
      ;; $B8e$+$i(B *Message* $B%P%C%U%!$G;2>H$G$-$k$h$&!"%3%^%s%I=PNO$r$9$Y(B
      ;; $B$FI=<($7$F$*$/(B
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
	;; decode $BA08e$G(B (navi2ch-line-end-position) $B$NCM$,$:$l$k$N$KCm0U(B
	(decode-coding-region bol (navi2ch-line-end-position) coding-system)
	(goto-char bol)			; $BG0$N$?$a(B
	(setq bol (1+ (navi2ch-line-end-position))))))
  (goto-char start))

(defun navi2ch-read-only-string (string)
  (navi2ch-propertize string 'read-only t 'front-sticky t 'rear-nonsticky t))

(defun navi2ch-file-mtime (filename)
  (nth 5 (file-attributes filename)))

(defun navi2ch-file-size (filename)
  (nth 7 (file-attributes filename)))

(run-hooks 'navi2ch-util-load-hook)
;;; navi2ch-util.el ends here
