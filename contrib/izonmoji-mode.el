;;; izonmoji-mode.el --- Visualize Windows and Macintosh izonmoji

;; Copyright (C) 2002-2004 by Navi2ch Project

;; Author: SAITO Takuya <tabmore@users.sourceforge.net>
;; Keywords: 2ch, charset

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

;; izonmoji-mode() and the way to apply izonmoji-{win,mac}-face on GNU Emacs
;; are derived from

;; blank-mode.el
;; Author: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Version: 4.0
;; X-URL: http://www.cpqd.com.br/~vinicius/emacs/

;;; Commentary:

;; $B5!<o0MB8J8;z$rI=<(2DG=$JJ8;zNs$KCV$-49$($FI=<($9$k!#(B
;; $B%G%U%)%k%H$G$OCV49@h$K(B JISX0213 $B$NJ8;z$b;H$C$F$$$^$9$N$G!"(B
;; JISX0213$BMQ$N%U%)%s%H$,I,MW$G$9!#(B
;; $BA4$F$N5!<o0MB8J8;z$rI=<($G$-$k$o$1$G$O$"$j$^$;$s$N$GCm0U!#(B

;; commands:
;;   izonmoji-mode      $B5!<o0MB8J8;zI=<($r%H%0%k(B
;;   izonmoji-mode-on   $B5!<o0MB8J8;z$rI=<((B
;;   izonmoji-mode-off  $B5!<o0MB8J8;zI=<($r$d$a$k(B

;; coding-system:
;;   izonmoji-shift-jis $BFI$_9~$_;~$K!"(BIBM$B3HD%J8;z$rBP1~$9$k(BNEC$BFC<lJ8;z!"(B
;;                      NEC$BA*Dj(BIBM$B3HD%J8;z$KCV49$9$k!#(B

;; izonmoji-mode-on $B$O(B buffer $B$NFbMF$rJQ99$7$^$;$s$,!"(Bizonmoji-shift-jis $B$G(B
;; $B%U%!%$%k$rFI$_9~$s$@;~$H(B shift_jis$B$GFI$_9~$s$@;~$H$G$O(B buffer $B$NFbMF$,(B
;; $B0[$J$k>l9g$b$"$j$^$9$N$G!"0U?^$;$:$K(B file $B$NFbMF$rJQ99$7$J$$$h$&Cm0U(B
;; $B$7$F2<$5$$!#(B

;; Emacs $B$N%P!<%8%g%s(B
;;  Emacs 20 $B0J9_!"(BXEmacs 21.4 $B0J9_$r;H$C$F2<$5$$!#(B
;;  $B$?$@$7!"(BXEmacs 21.1 $B$G$b0lIt$N5!G=$O;H$($^$9!#(B

;; GNU Emacs 20 $B$G$O!"(BMule-UCS$B$,I,MW$G$9!#(B
;; $B$3$N%U%!%$%k$rFI$_9~$`A0$K(B (require 'jisx0213) $B$7$F$/$@$5$$!#(B

;; GNU Emacs 20,21$B$G$O!"(Bbuffer-display-table$B$K$h$C$FI=<($rCV$-BX$($?(B
;; Non-ASCII$B$JJ8;z$N(Bchar-width$B$,$*$+$7$/$J$j$^$9!#(B
;; $BJQ49A08e$G(Bchar-width$B$,JQ$o$i$J$$>l9g$O!"(B
;;  (defadvice char-width (around display-table-hack activate)
;;    (let ((buffer-display-table nil))
;;      ad-do-it))
;; $B$G$4$^$+$;$^$9!#(B string-width$B$bF1MM$G$9!#(B

;; XEmacs 21.4$B0JA0$G$O(Binit-file$B$K0J2<$N$h$&$K=q$$$F$/$@$5$$!#(B
;; (make-charset
;;  'japanese-jisx0213-1
;;  "JIS X 0213:2000 Plain 1"
;;  '(registry "jisx0213\\(\\.2000\\)-1"
;;             dimension 2 chars 94 final ?O graphic 0))
;; (make-charset
;;  'japanese-jisx0213-2
;;  "JIS X 0213:2000 Plain 2"
;;  '(registry "jisx0213\\(\\.2000\\)-2"
;;             dimension 2 chars 94 final ?P graphic 0))

;; $B@_DjNc(B

;; [$B6&DL(B] ~/.emacs $B$X(B
;;  (require 'izonmoji-mode)

;; [navi2ch] ~/.navi2ch/init.el $B$X(B
;;  (add-hook 'navi2ch-bm-mode-hook      'izonmoji-mode-on)
;;  (add-hook 'navi2ch-article-mode-hook 'izonmoji-mode-on)
;;  (add-hook 'navi2ch-popup-article-mode-hook 'izonmoji-mode-on)
;;  ;; IBM$B3HD%J8;z$rI=<((B (XEmacs-21.1 $B$OHsBP1~(B)
;;  (when (memq 'izonmoji-shift-jis (coding-system-list))
;;    (setq navi2ch-coding-system 'izonmoji-shift-jis))

;; [Mew] ~/.mew.el $B$X(B
;;  (add-hook 'mew-message-mode-hook 'izonmoji-mode-on)

;; [Wanderlust] ~/.wl $B$X(B
;;  (add-hook 'wl-message-redisplay-hook 'izonmoji-mode-on)

;; [emacs-w3m] ~/.emacs-w3m.el $B$X(B
;;  (add-hook 'w3m-mode-hook 'izonmoji-mode-on)

;;; Bugs:

;;  1. display-table$B$r$$$8$k(B
;;  2. M-x izonmoji-mode-on
;;  3. 1$B$NJQ99$r85$KLa$9(B
;;  4. M-x izonmoji-mode-off
;;  $B$9$k$H85$K$b$I$i$J$$!#(B
;;  C-u M-x izonmoji-mode-off $B$7$F(Bdisplay-table$B$X$NA4$F$NJQ99$r<h$j>C$9(B
;;  $B$3$H$O$G$-$^$9!#(B


;;; Code:

(eval-when-compile
  (defvar buffer-display-table)
  (defvar current-display-table))

(require 'ccl)

(defvar izonmoji-priority-list '(win mac)
  "*$BI=<($NM%@h=g0L!#(B
'(win mac) $B$J$i!"(BWindows$B$N5!<o0MB8J8;z$rM%@h$7$D$D!"(BMac$B$NJ8;z$bI=<(!#(B
'(win) $B$J$i!"(BWindows$B$N5!<o0MB8J8;z$N$_I=<(!#(B")

(defvar izonmoji-win-face 'izonmoji-win-face
  "*Windows$B$N5!<o0MB8J8;z$NI=<($K;H$&%U%'%$%9L>!#(B
'default $B$K$9$k$H%U%'%$%9$r$D$1$^$;$s!#(B")

(defvar izonmoji-mac-face 'izonmoji-mac-face
  "*Mac$B$N5!<o0MB8J8;z$NI=<($K;H$&%U%'%$%9L>!#(B
'default $B$K$9$k$H%U%'%$%9$r$D$1$^$;$s!#(B")

(defface izonmoji-win-face
  '((((class color) (type tty)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "Aquamarine4"))
    (((class color) (background dark))  (:foreground "Aquamarine3"))
    (t (:underline t)))
  "Windows$B$N5!<o0MB8J8;z$N%U%'%$%9!#(B")

(defface izonmoji-mac-face
  '((((class color) (type tty)) (:foreground "magenta"))
    (((class color) (background light)) (:foreground "pink4"))
    (((class color) (background dark))  (:foreground "pink3"))
    (t (:underline t)))
  "Mac$B$N5!<o0MB8J8;z$N%U%'%$%9!#(B")

(defvar izonmoji-win-display-list
  '("$(O-!(B" "$(O-"(B" "$(O-#(B" "$(O-$(B" "$(O-%(B" "$(O-&(B" "$(O-'(B" "$(O-((B" "$(O-)(B" "$(O-*(B"
    "$(O-+(B" "$(O-,(B" "$(O--(B" "$(O-.(B" "$(O-/(B" "$(O-0(B" "$(O-1(B" "$(O-2(B" "$(O-3(B" "$(O-4(B"
    "$(O-5(B" "$(O-6(B" "$(O-7(B" "$(O-8(B" "$(O-9(B" "$(O-:(B" "$(O-;(B" "$(O-<(B" "$(O-=(B" "$(O->(B"
    "$(O-@(B" "$(O-A(B" "$(O-B(B" "$(O-C(B" "$(O-D(B" "$(O-E(B" "$(O-F(B" "$(O-G(B" "$(O-H(B" "$(O-I(B" "$(O-J(B" "$(O-K(B"
    "$(O-L(B" "$(O-M(B" "$(O-N(B" "$(O-O(B" "$(O-P(B" "$(O-Q(B" "$(O-R(B" "$(O-S(B" "$(O-T(B" "$(O-U(B" "$(O-V(B"
    "$(O-_(B" "$(O-`(B" "$(O-a(B" "$(O-b(B" "$(O-c(B" "$(O-d(B" "$(O-e(B" "$(O-f(B" "$(O-g(B" "$(O-h(B" "$(O-i(B"
    "$(O-j(B" "$(O-k(B" "$(O-l(B" "$(O-m(B" "$(O-n(B" "$(O-o(B"
    "$B"b(B" "$B"a(B" "$B"i(B" "$(O-s(B" "$B&2(B" "$B"e(B" "$B"](B" "$B"\(B" "$(O-x(B" "$(O-y(B" "$B"h(B" "$B"A(B" "$B"@(B"
    "$(Oz7(B" "$(O{n(B" "$(O}9(B" "$(O}.(B" "$(PvU(B" "$(O.9(B" "$(Po`(B" "$(Ou5(B" "$(Oui(B" "$(P{#(B" "$(Ou7(B" "$(Ot:(B" "$(O.$(B" "$(O.((B"
    "$(I"n(B" "$(P!?(B" "$(O.+(B" "$(D0c(B" "$(O.0(B" "$(P!M(B" "$(O.5(B" "$(P!N(B" "$(O.6(B" "$(D1>(B" "$(O.A(B" "$(P!](B" "$(D1H(B" "$(P!\(B"
    "$(P!`(B" "$(P!g(B" "$(P!_(B" "$(O.D(B" "$(IEt(B" "$(D2;(B" "$(O.O(B" "$(D2m(B" "$(O.T(B" "$(D2|(B" "$(I$[(B" "$(O.](B" "$(D3X(B" "$(P#C(B"
    "$(D3e(B" "$(O.g(B" "$(O.k(B" "$(O.l(B" "$(P#N(B" "$(D4@(B" "$(O.r(B" "$(O.t(B" "$(D4^(B" "$B".(B" "$(O.x(B" "$(P#l(B" "$(O/!(B" "$(O/#(B"
    "$(O/*(B" "$(J#a(B" "$(D7B(B" "$(P$l(B" "$(O/O(B" "$(O/N(B" "$(O/K(B" "$(O/W(B" "$(O/](B" "$(D8N(B" "$(P%:(B" "$(D8u(B" "$(P%E(B" "$(O/i(B"
    "$(D8|(B" "$(O/m(B" "$(D97(B" "$(OOV(B" "$(P($(B" "$(OxI(B" "$(OOY(B" "$(OOZ(B" "$(OO\(B" "$(P(C(B" "$(OOi(B" "$(I0b(B" "$(OOq(B" "$(OOu(B"
    "$(OOr(B" "$(P(U(B" "$(OOw(B" "$(OO|(B" "$(P(g(B" "$(D;y(B" "$(D<d(B" "$(Ot6(B" "$(Ot>(B" "$(OtE(B" "$(OtG(B" "$(D=g(B" "$(GUP(B" "$(P,M(B"
    "$(D>0(B" "$(OtU(B" "$Ac3(B" "$(P,X(B" "$(D>=(B" "$(Ot[(B" "$(OtZ(B" "$(P,i(B" "$(D?((B" "$(P-"(B" "$(Ott(B" "$(Otz(B" "$(P-P(B" "$(Ot|(B"
    "$(I66(B" "$(Ou,(B" "$(Ou.(B" "$(I-"(B" "$(Ou-(B" "$(DB+(B" "$(Ou/(B" "$(Ou2(B" "$(Ou>(B" "$(P-~(B" "$(Ou;(B" "$B".(B" "$(Ou?(B" "$(P.-(B"
    "$(OuD(B" "$(OuE(B" "$(P.3(B" "$(OuL(B" "$(DBp(B" "$(OuN(B" "$(OuP(B" "$(OuX(B" "$(Ou^(B" "$(OuZ(B" "$(Ou](B" "$(P._(B" "$(P.}(B" "$(P.y(B"
    "$(Oux(B" "$(Ouz(B" "$(P/6(B" "$(Ov"(B" "$(P/A(B" "$(Ov0(B" "$(P/S(B" "$(Ov1(B" "$(P/Z(B" "$(IZH(B" "$(Ov8(B" "$(OvK(B" "$(Pn5(B" "$(OvR(B"
    "$(OvW(B" "$(DFc(B" "$(PnD(B" "$(Ova(B" "$(Ovk(B" "$(DGC(B" "$(DGK(B" "$(Ovo(B" "$(Pnh(B" "$(I6|(B" "$(H6d(B" "$(Ovv(B" "$(DGn(B" "$(Ovz(B"
    "$(Pnp(B" "$(Pns(B" "$(Po$(B" "$(Ow/(B" "$(PoA(B" "$(Ow;(B" "$(Ow<(B" "$(PoJ(B" "$(Ow>(B" "$(OwE(B" "$(OwG(B" "$(OwL(B" "$(Pok(B" "$(OwW(B"
    "$(OwR(B" "$(OwS(B" "$(OwZ(B" "$(Ow^(B" "$(Owa(B" "$(Pp=(B" "$(Owi(B" "$(DK.(B" "$(Owo(B" "$(PpW(B" "$(DKf(B" "$(Owy(B" "$(Ow{(B" "$(Ow}(B"
    "$(DKn(B" "$(Ox!(B" "$(DKy(B" "$(Ox&(B" "$(Ox((B" "$(DL-(B" "$(Ox+(B" "$(Ox3(B" "$(Ox8(B" "$(Ox;(B" "$(OxG(B" "$(OxJ(B" "$ATm(B" "$(Pqo(B"
    "$(Oxc(B" "$(Oxa(B" "$(Oxd(B" "$B".(B" "$(Oxr(B" "$(P#D(B" "$(PrC(B" "$(Oy"(B" "$(PrF(B" "$(DOm(B" "$(Pr^(B" "$B".(B" "$(Oy<(B" "$(Oy=(B"
    "$(Prg(B" "$(OyA(B" "$(Prj(B" "$(Ps9(B" "$(Ps:(B" "$B".(B" "$(OyX(B" "$(Oyc(B" "$B".(B" "$(Oz"(B" "$(Oz$(B" "$(PtG(B" "$(Oz((B" "$(Oz,(B"
    "$(Oz5(B" "$(Oz9(B" "$(DU.(B" "$B".(B" "$(Ozl(B" "$(Ozm(B" "$(Pv3(B" "$(Oz}(B" "$(DXA(B" "$(O{((B" "$(O{/(B" "$(Pvq(B" "$(O{8(B" "$(DYQ(B"
    "$(O{:(B" "$(O{@(B" "$(Pw8(B" "$(PwE(B" "$(I[)(B" "$(O{m(B" "$(O{}(B" "$(O|"(B" "$(O|((B" "$(O|)(B" "$(Px`(B" "$(O|+(B" "$(O|.(B" "$(O|-(B"
    "$(D^K(B" "$(Pxo(B" "$(Py0(B" "$(O|:(B" "$(O|>(B" "$A8O(B" "$B".(B" "$(D`Y(B" "$(Pyn(B" "$B".(B" "$(Dab(B" "$(O|g(B" "$(O|j(B" "$(O|l(B"
    "$(O|p(B" "$(PzN(B" "$(O||(B" "$(Dc)(B" "$(O}"(B" "$(PzR(B" "$(O|~(B" "$(O}!(B" "$(Dc<(B" "$(O}%(B" "$(PzT(B" "$(O}((B" "$(O}*(B" "$(O})(B"
    "$(Pz\(B" "$(Pz_(B" "$(Pz](B" "$(O}'(B" "$(Pzc(B" "$(Pzo(B" "$(Dcq(B" "$(O}-(B" "$(Pz|(B" "$(Pzy(B" "$(Pzz(B" "$(Pzx(B" "$B".(B" "$(Dd@(B"
    "$(O}3(B" "$(O}2(B" "$(O}8(B" "$(O}7(B" "$(P{$(B" "$B".(B" "$(P{)(B" "$(O}4(B" "$(P{((B" "$(P{%(B" "$(O}?(B" "$(P{0(B" "$(De3(B" "$(O}C(B"
    "$(O}D(B" "$(DeP(B" "$(Deb(B" "$(P{M(B" "$(Dek(B" "$(Gbg(B" "$(O}](B" "$B".(B" "$(O}^(B" "$(P{l(B" "$(O}f(B" "$(O}g(B" "$(Dg.(B" "$(O}h(B"
    "$(O}j(B" "$(I*~(B" "$(P{|(B" "$(O~!(B" "$(O~%(B" "$B".(B" "$B".(B" "$(Dh](B" "$B".(B" "$(O~+(B" "$(O~3(B" "$(J6-(B" "$(P}2(B" "$(O~D(B"
    "$(O~C(B" "$(O~G(B" "$(P}J(B" "$(Djj(B" "$(O~O(B" "$(O~^(B" "$(P~4(B" "$B".(B" "$(P~O(B" "$(O~r(B"
    "$(O,5(B" "$(O,6(B" "$(O,7(B" "$(O,8(B" "$(O,9(B" "$(O,:(B" "$(O,;(B" "$(O,<(B" "$(O,=(B" "$(O,>(B" "$B"L(B" "$(O)%(B" "$(O"/(B" "$(O"0(B")
  "*Windows$B$N5!<o0MB8J8;z$NI=<($K;H$&J8;zNs$N%j%9%H!#(B")

(defvar izonmoji-mac-display-list
  '("$(O-!(B" "$(O-"(B" "$(O-#(B" "$(O-$(B" "$(O-%(B" "$(O-&(B" "$(O-'(B" "$(O-((B" "$(O-)(B" "$(O-*(B"
    "$(O-+(B" "$(O-,(B" "$(O--(B" "$(O-.(B" "$(O-/(B" "$(O-0(B" "$(O-1(B" "$(O-2(B" "$(O-3(B" "$(O-4(B"
    "(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)"
    "(11)" "(12)" "(13)" "(14)" "(15)" "(16)" "(17)" "(18)" "(19)" "(20)"
    "$(O,!(B" "$(O,"(B" "$(O,#(B" "$(O,$(B" "$(O,%(B" "$(O,&(B" "$(O,'(B" "$(O,((B" "$(O,)(B"
    "0." "1." "2." "3." "4." "5." "6." "7." "8." "9." "0." ;0. $B$O$I$3!)(B
    "$(O-5(B" "$(O-6(B" "$(O-7(B" "$(O-8(B" "$(O-9(B" "$(O-:(B" "$(O-;(B" "$(O-<(B" "$(O-=(B" "$(O->(B"
    "$(O-?(B" "$(O-W(B" "X$(O-7(B" "X$(O-8(B" "XV"
    "$(O,5(B" "$(O,6(B" "$(O,7(B" "$(O,8(B" "$(O,9(B" "$(O,:(B" "$(O,;(B" "$(O,<(B" "$(O,=(B" "$(O,>(B"
    "$(O,?(B" "$(O,@(B" "x$(O,7(B" "x$(O,8(B" "xv"
    "(a)" "(b)" "(c)" "(d)" "(e)" "(f)" "(g)" "(h)" "(i)" "(j)" "(k)"
    "(l)" "(m)" "(n)" "(o)" "(p)" "(q)" "(r)" "(s)" "(t)" "(u)" "(v)"
    "(w)" "(x)" "(y)" "(z)"
    "$(O-P(B" "mm$(O),(B" "$(O-Q(B" "$(O-Q),(B" "$(O-Q)-(B" "m" "$(O-V(B" "m$(O)-(B" "$(O-R(B" "k$(O-V(B"
    "$(O-S(B" "g" "$(O-T(B" "$(O-U(B" "m$(O#_(B" "d$(O#_(B" "$(O#_(B" "k$(O#_(B"
    "ms" "$B&L(Bs" "ns" "ps" "$(O!k(BF" "mb" "$(O#^(B" "Hz" "KB" "MB" "GB" "TB"
    "$(O-b(B" "$(O-c(B" "$(O-d(B" "FAX"
    "$(O&9(B" "$(O&?(B" "$(O&=(B" "$(O&;(B" "$(O&:(B" "$(O&@(B" "$(O&>(B" "$(O&<(B"
    "$(O&f(B" "$(O&g(B" "JIS"			;JIS$B%^!<%/(B
    "$B"*(B" "$B"+(B" "$B",(B" "$B"-(B"			;$B;X:9$7Lp0u(B
    "$(O#)(B" "$(O#)(B" "$B","-(B" "$B"-",(B"	     ;$B6v?tHVL\$O!">e2<!":81&$N8~$-$,5U(B
    "$(O#*(B" "$(O#+(B" "$(O#,(B" "$(O#-(B" "$(O#*(B" "$(O#+(B" "$(O#,(B" "$(O#-(B" ;$B8eH>$OEI$jDY$7(B
    "($BF|(B)" "($B7n(B)" "($B2P(B)" "($B?e(B)" "($BLZ(B)" "($B6b(B)" "($BEZ(B)"
    "($B:W(B)" "($B=K(B)" "($B<+(B)" "($B;j(B)" "$(O-l(B" "($B8F(B)" "$(O-j(B" "($B;q(B)" "($BL>(B)"
    "$(O-k(B" "($B3X(B)" "($B:b(B)" "($B<R(B)" "($BFC(B)" "($B4F(B)" "($B4k(B)" "($B6((B)" "($BO+(B)"
    "($BBg(B)" "($B>.(B)" "$(O-e(B" "$(O-f(B" "$(O-g(B" "$(O-h(B" "$(O-i(B"
    "($B0e(B)" "($B:b(B)" "($BM%(B)" "($BO+(B)" "($B0u(B)" "($B95(B)" "($BHk(B)" ;$BK\Ev$O4]IU$-(B
    "$(O-@(B" "$(O-B(B" "$(O-C(B" "$(O-A(B" "$(O-A-C(B" "(I2]A(B" "(IL(0D(B" "(IT0D^(B" "$(O-F(B" "$(O-G(B"
    "$(O-D(B" "$(O-A-D(B" "$(O-E(B" "$(O-H(B" "$(O-N(B" "(IMYB(B" "$(O-I(B" "$(O-J(B" "(IN0](B" "$(O-L(B" "$(O-K(B" "$(O-O(B" "$(O-M(B"
    "(I1J_0D(B" "(I:0N_(B" "(IJ2B(B" "(IK^Y(B" "(IO]<.](B"
    "$(O-m(B" "$(O-n(B" "$(O-o(B" "$(O-_(B"
    "$B3t<02q<R(B" "$BM-8B2q<R(B" "$B:bCDK!?M(B"	;"$(O-j(B" "$(O-k(B"
    "$(O-s(B" "$(O-x(B" "$(O-y(B"
    "$(O-`(B" "$(O-a(B"
    "$(O$t(B" "$(O'r(B" "$(O's(B" "$(O't(B" "$(O'u(B"
    ;; $B=D=q$-(B
    "$B!"(B" "$B!#(B" "$B!1(B" "$B!2(B" "$B!<(B" "$B(!(B" "$B!>(B" "$B!A(B"
    "$B!a(B" "$B(!(B"				;$B2#D>@~(B
    "$B!D(B" "$B!E(B" "$B!J(B" "$B!K(B" "$B!L(B" "$B!M(B" "$B!N(B" "$B!O(B" "$B!P(B" "$B!Q(B" "$B!R(B" "$B!S(B" "$B!T(B" "$B!U(B"
    "$B!V(B" "$B!W(B" "$B!X(B" "$B!Y(B" "$B!Z(B" "$B![(B" "$B!a(B"
    "$B$!(B" "$B$#(B" "$B$%(B" "$B$'(B" "$B$)(B" "$B$C(B" "$B$c(B" "$B$e(B" "$B$g(B" "$B$n(B"
    "$B%!(B" "$B%#(B" "$B%%(B" "$B%'(B" "$B%)(B" "$B%C(B" "$B%c(B" "$B%e(B" "$B%g(B" "$B%n(B" "$B%u(B" "$B%v(B")
  "*Mac$B$N5!<o0MB8J8;z$NI=<($K;H$&J8;zNs$N%j%9%H!#(B")

(defun izonmoji-make-char-list (i js je &optional k)
  (unless k (setq k 1))
  (let ((j js) list)
    (while (<= j je)
      (setq list (cons (make-char 'japanese-jisx0208 i j) list))
      (setq j (+ j k)))
    (nreverse list)))

;; Windows$B$N4]IU$-(B1$B$O!"(B
;; (split-char (decode-sjis-char (hexl-hex-string-to-integer "8740")))
(defvar izonmoji-win-chars-list
  (append
   (izonmoji-make-char-list  45  33  62) ;$B4]IU$-?t;z(B + $B%m!<%^?t;z(B($BBgJ8;z(B)
   (izonmoji-make-char-list  45  64  86) ;$BC10L(B
   (izonmoji-make-char-list  45  95 124) ;$B859f!"?t3X5-9f$J$I(B
   (izonmoji-make-char-list 121  33 126) ;$B4A;z(B
   (izonmoji-make-char-list 122  33 126)
   (izonmoji-make-char-list 123  33 126)
   (izonmoji-make-char-list 124  33 110)
   (izonmoji-make-char-list 124 113 126) ;$B%m!<%^?t;z(B($B>.J8;z(B)
   )
  "*Windows$B$N5!<o0MB8J8;z%j%9%H!#(B")

(defvar izonmoji-mac-chars-list
  (append
   (izonmoji-make-char-list  41  33  52) ;$B4]IU$-?t;z(B
   (izonmoji-make-char-list  41  63  82) ;$B3g8LIU$-?t;z(B
   (izonmoji-make-char-list  41  93 101) ;$B9u4]IU$-?t;z(B
   (izonmoji-make-char-list  41 113 123) ;$BE@IU$-?t;z(B
   (izonmoji-make-char-list  42  33  47) ;$B%m!<%^?t;z(B($BBgJ8;z(B)
   (izonmoji-make-char-list  42  53  67) ;$B%m!<%^?t;z(B($B>.J8;z(B)
   (izonmoji-make-char-list  42  93 118) ;$B3g8LIU$-%"%k%U%!%Y%C%H(B
   (izonmoji-make-char-list  43  33  62) ;$BC10L(B
   (izonmoji-make-char-list  43 123 126) ;$BN,9f(B
   (izonmoji-make-char-list  44  33  40) ;$B%H%i%s%W(B
   (izonmoji-make-char-list  44  53  55) ;$BM9JX(B
   (izonmoji-make-char-list  44  73  88) ;$BLp0u(B
   (izonmoji-make-char-list  45  33  57) ;$BMKF|$J$I(B
   (izonmoji-make-char-list  45 113 126) ;$B4]IU$-J8;z(B
   (izonmoji-make-char-list  46  33  55) ;$B%+%?%+%JC10L(B
   (izonmoji-make-char-list  46  63  67) ;$B%"%Q!<%H(B
   (izonmoji-make-char-list  46 103 106) ;$B859f(B
   (izonmoji-make-char-list  46 124 126) ;$B3t<02q<R(B
   (izonmoji-make-char-list  47  33  35) ;$B?t3X5-9f(B
   (izonmoji-make-char-list  47  53  54) ;""
   (izonmoji-make-char-list  47  73  73) ;$B$&!+(B
   (izonmoji-make-char-list  47  75  78) ;$B%o!+(B
   (izonmoji-make-char-list 117  34  35) ;$B=D=q$-(B
   (izonmoji-make-char-list 117  49  50)
   (izonmoji-make-char-list 117  60  62)
   (izonmoji-make-char-list 117  65  69)
   (izonmoji-make-char-list 117  74  91)
   (izonmoji-make-char-list 117  97  97)
   (izonmoji-make-char-list 120  33  41 2)
   (izonmoji-make-char-list 120  67  67)
   (izonmoji-make-char-list 120  99  99)
   (izonmoji-make-char-list 120 101 103 2)
   (izonmoji-make-char-list 120 110 110)
   (izonmoji-make-char-list 121  33  41 2)
   (izonmoji-make-char-list 121  67  67)
   (izonmoji-make-char-list 121  99  99)
   (izonmoji-make-char-list 121 101 103 2)
   (izonmoji-make-char-list 121 110 110)
   (izonmoji-make-char-list 121 117 118))
  "*Mac$B$N5!<o0MB8J8;z%j%9%H!#(B")

(defvar izonmoji-mode-hook nil "*$B5!<o0MB8J8;z$rI=<($7$?8e$K8F$P$l$k%U%C%/!#(B")

;; Internal variables

(defvar izonmoji-mode nil)
(make-variable-buffer-local 'izonmoji-mode)

(defvar izonmoji-backuped-display-table nil)
(make-variable-buffer-local 'izonmoji-backuped-display-table)

(defun izonmoji-mode (&optional arg)
  "$B5!<o0MB8J8;zI=<($r%H%0%k!#(B
ARG $B$,(B non-nil $B$N>l9g!"(B1$B0J>e$N?t$J$i5!<o0MB8J8;z$rI=<(!#(B
$B$=$l0J30$J$i5!<o0MB8J8;zI=<($r$d$a$k!#(B"
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(not izonmoji-mode))
      (izonmoji-mode-on)
    (izonmoji-mode-off)))

(defun izonmoji-mode-on (&optional reverse win-face mac-face)
  "$B5!<o0MB8J8;z$rI=<((B"
  (interactive "P")
  (let ((priority (reverse izonmoji-priority-list))
	from to table)
    (when reverse
      (setq priority (nreverse priority)))
    (unless izonmoji-mode
      (cond
       ((featurep 'xemacs)
	(let* ((ctable (specifier-instance current-display-table))
	       (len (- (1+ (apply 'max (append izonmoji-win-chars-list
					       izonmoji-mac-chars-list)))
		       (length ctable)))
	       face glyph)
	  (setq izonmoji-backuped-display-table ctable
		table (copy-sequence ctable))
	  (when (> len 0)
	    (setq table (vconcat table (make-vector len nil))))
	  (while priority
	    (cond
	     ((eq (car priority) 'win)
	      (setq from izonmoji-win-chars-list
		    to   izonmoji-win-display-list
		    face (or win-face izonmoji-win-face 'default)))
	     ((eq (car priority) 'mac)
	      (setq from izonmoji-mac-chars-list
		    to   izonmoji-mac-display-list
		    face (or mac-face izonmoji-mac-face 'default))))
	    (setq priority (cdr priority))
	    (while (and from to)
	      (if (or (eq face 'default)
		      ;; XEmacs 21.1 $B$G(B face $B$rIU$1$k$HMn$A$k$N$G!#(B
		      (and (= emacs-major-version 21)
			   (= emacs-minor-version 1)))
		  ;; face $B$r;XDj$7$J$$!#(B
		  (aset table (car from) (car to))
		(setq glyph (make-glyph (car to)))
		(set-glyph-face glyph face)
		(aset table (car from) glyph))
	      (setq from (cdr from) to (cdr to))))
	  (set-specifier current-display-table table (current-buffer))))
       (t				;GNU Emacs
	(let (face-bits)
	  (setq izonmoji-backuped-display-table buffer-display-table
		table (or (copy-sequence (or buffer-display-table
					     standard-display-table))
			  (make-display-table)))
	  (while priority
	    (cond
	     ((eq (car priority) 'win)
	      (setq from izonmoji-win-chars-list
		    to   izonmoji-win-display-list
		    face-bits (ash (face-id
				    (or win-face izonmoji-win-face 'default))
				   19)))
	     ((eq (car priority) 'mac)
	      (setq from izonmoji-mac-chars-list
		    to   izonmoji-mac-display-list
		    face-bits (ash (face-id
				    (or mac-face izonmoji-mac-face 'default))
				   19))))
	    (setq priority (cdr priority))
	    (while (and from to)
	      (aset table (car from)
		    (apply 'vector (mapcar
				    (lambda (ch) (logior ch face-bits))
				    (car to))))
	      (setq from (cdr from) to (cdr to))))
	  (setq buffer-display-table table)))))
    (setq izonmoji-mode t)
    (run-hooks 'izonmoji-mode-hook)))

(defun izonmoji-mode-off (&optional initialize)
  "$B5!<o0MB8J8;zI=<($r$d$a$k(B"
  (interactive "P")
  (when initialize
    (setq izonmoji-mode t
	  izonmoji-backuped-display-table (make-display-table)))
  (when izonmoji-mode
    (if (featurep 'xemacs)
	(set-specifier current-display-table
		       izonmoji-backuped-display-table (current-buffer))
      (setq buffer-display-table izonmoji-backuped-display-table))
    (setq izonmoji-mode nil)))

;; izonmoji-shift-jis
(when (and (fboundp 'ccl-compile-write-multibyte-character)
	   (not (memq 'izonmoji-shift-jis (coding-system-list))))
  (eval-and-compile
    (defun izonmoji-ccl-write-sjis ()
      `((r1 = (r0 de-sjis r1))
	(r0 = (r1 << 7))
	(r0 += r7)
	(r1 = ,(charset-id 'japanese-jisx0208))
	(write-multibyte-character r1 r0)
	(repeat)))

    (defun izonmoji-ccl-ibm-ext (d0 d1)
      `((r0 -= ,d0)
	(r1 -= ,d1)
	,@(izonmoji-ccl-write-sjis))))

  (define-ccl-program izonmoji-shift-jis-decode
    `(2
      (loop
       (read r0)
       (if (r0 < ?\x80)
	   (write-repeat r0))
       ;; if (r0 == 0x80 || r0 == 0xA0 || 0xEF < r0 < 0xFA || r0 > 0xFC)
       (r1 = (r0 == ?\x80))
       (r1 |= (r0 == ?\xA0))
       (r2 = (r0 > ?\xEF))
       (r2 &= (r0 < ?\xFA))
       (r1 |= r2)
       (r1 |= (r0 > ?\xFC))
       (if r1
	   (write-repeat r0))
       (r1 = (r0 <= ?\x9F))
       (r1 |= (r0 >= ?\xE0))
       (if r1
	   ((read r1)
	    (r2 = (r1 < ?\x40))
	    (r2 |= (r1 == ?\x7F))
	    (r2 |= (r1 > ?\xFC))
	    (if r2
		((write r0)
		 (write-repeat r1)))
	    (if (r0 >= ?\xFA)
		((if (r0 == ?\xFA)
		     ((if (r1 <= ?\x49)
			  ,(izonmoji-ccl-ibm-ext 12 -175))
		      (if (r1 <= ?\x53)
			  ,(izonmoji-ccl-ibm-ext 115 -10))
		      (if (r1 <= ?\x57)
			  ,(izonmoji-ccl-ibm-ext 12 -165))
		      (if (r1 == ?\x58)
			  ,(izonmoji-ccl-ibm-ext 115 -50))
		      (if (r1 == ?\x59)
			  ,(izonmoji-ccl-ibm-ext 115 -41))
		      (if (r1 == ?\x5A)
			  ,(izonmoji-ccl-ibm-ext 115 -42))
		      (if (r1 == ?\x5B)
			  ,(izonmoji-ccl-ibm-ext 115 -63))
		      (if (r1 <= ?\x7E)
			  ,(izonmoji-ccl-ibm-ext 13 28))
		      (if (r1 <= ?\x9B)
			  ,(izonmoji-ccl-ibm-ext 13 29))
		      (if (r1 <= ?\xFC)
			  ,(izonmoji-ccl-ibm-ext 13 28))))
		 (if (r0 == ?\xFB)
		     ((if (r1 <= ?\x5B)
			  ,(izonmoji-ccl-ibm-ext 14 -161))
		      (if (r1 <= ?\x7E)
			  ,(izonmoji-ccl-ibm-ext 13 28))
		      (if (r1 <= ?\x9B)
			  ,(izonmoji-ccl-ibm-ext 13 29))
		      (if (r1 <= ?\xFC)
			  ,(izonmoji-ccl-ibm-ext 13 28))))
		 (if (r0 == ?\xFC)
		     ((if (r1 <= ?\x4B)
			  ,(izonmoji-ccl-ibm-ext 14 -161))))
		 (write r0)
		 (write-repeat r1)))
	    ,@(izonmoji-ccl-write-sjis))
	 ((r0 &= ?\x7F)
	  (r1 = ,(charset-id 'katakana-jisx0201))
	  (write-multibyte-character r1 r0)
	  (repeat))))))

  (define-ccl-program izonmoji-shift-jis-encode
    `(1
      (loop
       (read r0)
       (if (r0 == ,(charset-id 'japanese-jisx0208))
	   ((read r0)
	    (read r1)
	    (r0 &= ?\x7F)
	    (r1 &= ?\x7F)
	    (r1 = (r0 en-sjis r1))
	    (write r1 r7)
	    (repeat))
	 ((if (r0 == ,(charset-id 'katakana-jisx0201))
	      (read r0))
	  (write-repeat r0))))))

  (if (featurep 'xemacs)
      (make-coding-system 'izonmoji-shift-jis 'ccl
			  "Shift-JIS for displaying IBM ext characters"
			  (list 'mnemonic "S"
				'decode 'izonmoji-shift-jis-decode
				'encode 'izonmoji-shift-jis-encode))
    (make-coding-system 'izonmoji-shift-jis 4 ?S
			"Shift-JIS for displaying IBM ext characters"
			(cons 'izonmoji-shift-jis-decode
			      'izonmoji-shift-jis-encode)
			(list (cons 'safe-charsets
				    (coding-system-get 'japanese-shift-jis
						       'safe-charsets))))))

(add-to-list 'minor-mode-alist '(izonmoji-mode " Iz"))

(provide 'izonmoji-mode)

;;; izonmoji-mode.el ends here
