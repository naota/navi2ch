;;; navi2ch-spamfilter.el --- Navi2ch interface for spamfilter.el

;; Copyright (C) 2003, 2004 by Navi2ch Project
;; Copyright (C) 2003 http://pc.2ch.net/test/read.cgi/unix/1065246418/38

;; Author: http://pc.2ch.net/test/read.cgi/unix/1065246418/38
;; Keywords: 2ch, network, spam

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

;; 使い方
;; http://www.geocities.co.jp/SiliconValley-PaloAlto/7043/#spamfilter.el
;; から spamfilter パッケージを入手してインストールしておく。
;; ~/.navi2ch/init.el で
;; (require 'navi2ch-spamfilter)
;; とでもして、Navi2ch の実行時に読み込むようにする。
;; まずはコーパスの学習をする。適当なスレを開き、spam を "d" して hide
;; マークを付け、spam 以外は hide モードで "d" して hide マークが付い
;; てない状態にする。この状態で M-x navi2ch-article-register-to-corpus
;; とし、マークに応じてコーパスに登録してやる。
;; ある程度コーパスが育ったら ~/.navi2ch/init.el に
;; (navi2ch-spamf-enable) を加えて自動的にフィルタするようにする。
;; 誤認識が少なくなったら navi2ch-article-auto-spam-register-by-filter
;; を non-nil に設定し、フィルタの結果に応じてコーパスに再登録する。

;;; Code:
(defconst navi2ch-spamfilter-ident
  "$Id$")

(eval-when-compile (require 'cl))
(require 'spamfilter)
(require 'navi2ch)

(defconst navi2ch-spamf-preferred-major 1)
(defconst navi2ch-spamf-preferred-minor 10)

(unless (and (boundp 'spamf-cvs-id)
	     (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" spamf-cvs-id)
	     (let ((major (string-to-number (match-string 1 spamf-cvs-id)))
		   (minor (string-to-number (match-string 2 spamf-cvs-id))))
	       (or (> major navi2ch-spamf-preferred-major)
		   (and (= major navi2ch-spamf-preferred-major)
			(>= minor navi2ch-spamf-preferred-minor)))))
  (error "Use spamfilter.el revision %d.%d or later."
	 navi2ch-spamf-preferred-major
	 navi2ch-spamf-preferred-minor))

(defvar navi2ch-article-bayesian-save-file-name
  (expand-file-name "spamfilter" navi2ch-directory))

(defvar navi2ch-article-auto-spam-register-by-filter nil
  "non-nil の場合、フィルタの結果をコーパスに再登録する。
自動的に隠されたレスの spam っぽさは増加し、隠されなかったレスの spam
っぽさは減少する。
non-nil のままで誤判定を放置すると誤った学習をしてしまうので注意。")

(defvar navi2ch-article-manual-spam-ratio
  (if navi2ch-article-auto-spam-register-by-filter 2 1)
  "手動で spam 登録 / 解除をした際の倍率。
navi2ch-article-message-filter-by-bayesian で自動登録する場合は 2 以上
にしておいた方がいいと思う。")

(defvar navi2ch-article-register-normal-message-as-good t
  "non-nil の場合、マークの付いてないレスも good として登録する。
`navi2ch-article-register-to-corpus' も参照。")

(defvar navi2ch-spamf-good-corpus
  (make-spamf-corpus :name "navi2ch-spamf-good-corpus"
		     :table (make-hash-table :test #'eq)
		     :message-count 0))

(defvar navi2ch-spamf-bad-corpus
  (make-spamf-corpus :name "navi2ch-spamf-bad-corpus"
		     :table (make-hash-table :test #'eq)
		     :message-count 0))

(defvar navi2ch-article-before-save-corpus-hook nil)

(defvar navi2ch-spamf-additional-token-flag nil
  "non-nil の場合、レスの日時、番号等をトークンとして追加する。
経験では、コーパスが大きくなる割に効果は薄いと思われる。")

(dolist (map (list navi2ch-article-mode-map navi2ch-popup-article-mode-map))
  (define-key map "\C-c\C-g"
    'navi2ch-article-add-message-filter-by-bayesian-good)
  (define-key map "\C-c\C-b"
    'navi2ch-article-add-message-filter-by-bayesian-spam)
  (define-key map "\C-c\C-s"
    'navi2ch-article-show-spam-probability))

(defsubst navi2ch-spamf-register-token (corpus token)
  (spamf-increase-word-count corpus (spamf-intern token))
  (incf (spamf-corpus-message-count corpus)))

(defsubst navi2ch-spamf-register-good-token (token)
  (interactive "MToken: ")
  (navi2ch-spamf-register-token navi2ch-spamf-good-corpus token))

(defsubst navi2ch-spamf-register-spam-token (token)
  (interactive "MToken: ")
  (navi2ch-spamf-register-token navi2ch-spamf-bad-corpus token))

(defsubst navi2ch-spamf-register-token-list (corpus list)
  (dolist (token list)
    (spamf-increase-word-count corpus (spamf-intern token)))
  (incf (spamf-corpus-message-count corpus)))

(defsubst navi2ch-spamf-register-good-token-list (list)
  (navi2ch-spamf-register-token-list navi2ch-spamf-good-corpus list))

(defsubst navi2ch-spamf-register-spam-token-list (list)
  (navi2ch-spamf-register-token-list navi2ch-spamf-bad-corpus list))

(defun navi2ch-article-bayesian-tokenizer (alist)
  (nconc
   (funcall spamf-tokenize-string-function
	    (cdr (assq 'data alist)))
   (if navi2ch-spamf-additional-token-flag
       (mapcar (lambda (str)
		 (concat "date:" str))
	       (split-string (cdr (assq 'date alist)) "[ 　]+")))
   (if (string-match "◆[^ ]+" (cdr (assq 'name alist)))
       (list (concat "trip:" (match-string 0 (cdr (assq 'name alist))))))
   (let ((number (or (cdr (assq 'number alist))
		     (navi2ch-article-get-current-number))))
     (when (and navi2ch-spamf-additional-token-flag
		(numberp number))
       (list (concat "num:"  (number-to-string number)))))
   (list
    (concat "mail:" (cdr (assq 'mail alist)))
    (concat "name:" (cdr (assq 'name alist))))))

(defsubst navi2ch-article-tokenize-current-message ()
  (navi2ch-article-bayesian-tokenizer
   (navi2ch-article-get-message
    (navi2ch-article-get-current-number))))

(defun navi2ch-article-add-message-filter-by-bayesian-good ()
  (interactive)
  (dotimes (i navi2ch-article-manual-spam-ratio)
    (navi2ch-spamf-register-good-token-list
     (navi2ch-article-tokenize-current-message))))

(defun navi2ch-article-add-message-filter-by-bayesian-spam ()
  (interactive)
  (dotimes (i navi2ch-article-manual-spam-ratio)
    (navi2ch-spamf-register-spam-token-list
     (navi2ch-article-tokenize-current-message))))

(defsubst navi2ch-article-spam-probability (token)
  (spamf-sum-spam-probability
   (mapcar #'cdr (spamf-cutoff-words token
				     spamf-cutoff-words-limit
				     navi2ch-spamf-good-corpus
				     navi2ch-spamf-bad-corpus))))

(defun navi2ch-article-show-spam-probability (&optional prefix)
  "レスの spam っぽさを表示する。"
  (interactive "P")
  (let* ((token (navi2ch-article-tokenize-current-message))
	 (prob (navi2ch-article-spam-probability token)))
    (if prefix
	(with-output-to-temp-buffer "*spam probability*"
	  (princ (format "Spam probability: %f\n\n" prob))
	  (dolist (pair (spamf-cutoff-words token
					    spamf-cutoff-words-limit
					    navi2ch-spamf-good-corpus
					    navi2ch-spamf-bad-corpus))
	    (prin1 (cons (symbol-name (car pair)) (cdr pair)))
	    (princ "\n")))
      (message "Spam probability: %f" prob))))

(defun navi2ch-article-message-filter-by-bayesian (alist)
  (let ((token (navi2ch-article-bayesian-tokenizer alist)))
    (if (> (navi2ch-article-spam-probability token)
	   spamf-spamness-limit)
	'hide)))

(defun navi2ch-article-save-corpus ()
  (run-hooks 'navi2ch-article-before-save-corpus-hook)
  (message "Saving corpus file...")
  (spamf-save-corpus navi2ch-article-bayesian-save-file-name
		     navi2ch-spamf-good-corpus
		     navi2ch-spamf-bad-corpus)
  (message "Saving corpus file...done"))

(defun navi2ch-article-load-corpus ()
  (message "Loading corpus file...")
  (spamf-load-corpus navi2ch-article-bayesian-save-file-name)
  (message "Loading corpus file...done"))

(defun navi2ch-article-register-to-corpus ()
  "レスのマークに応じて現在のスレをコーパスに登録する。
important マークのレスを good に、hide マークのレスを bad に登録する。
`navi2ch-article-register-normal-message-as-good' が non-nil の場合は
マークの付いてないレスも good に登録する。"
  (interactive)
  (let ((hide (cdr (assq 'hide navi2ch-article-current-article)))
	(imp (cdr (assq 'important navi2ch-article-current-article))))
    (dolist (x navi2ch-article-message-list)
      (let ((num (car x))
	    (alist (cdr x)))
	(if (stringp alist)
	    (setq alist (navi2ch-article-parse-message alist)))
	(message "registering...%d" num)
	(cond ((memq num hide)
	       (navi2ch-spamf-register-spam-token-list
		(navi2ch-article-bayesian-tokenizer alist)))
	      ((or (memq num imp)
		   navi2ch-article-register-normal-message-as-good)
	       (navi2ch-spamf-register-good-token-list
		(navi2ch-article-bayesian-tokenizer alist))))))
    (message "registering...done")))

;; hook and advice
(add-hook 'navi2ch-save-status-hook #'navi2ch-article-save-corpus)
(add-hook 'navi2ch-hook #'navi2ch-article-load-corpus)

(defadvice navi2ch-article-apply-message-filters
  (after navi2ch-article-register-spam-by-filter (alist))
  (when navi2ch-article-auto-spam-register-by-filter
    (let ((token (navi2ch-article-bayesian-tokenizer alist)))
      (if (eq ad-return-value 'hide)
	  (navi2ch-spamf-register-spam-token-list token)
	(navi2ch-spamf-register-good-token-list token))))
  ad-return-value)

(defadvice navi2ch-article-hide-message
  (before navi2ch-article-hide-message-as-spam)
  (navi2ch-article-add-message-filter-by-bayesian-spam))

(defadvice navi2ch-article-cancel-hide-message
  (before navi2ch-article-cancel-hide-message-as-good)
  (navi2ch-article-add-message-filter-by-bayesian-good))

(defadvice navi2ch-article-next-message
  (before navi2ch-article-next-message-as-good)
  (unless navi2ch-article-hide-mode
    (navi2ch-article-add-message-filter-by-bayesian-good)))

(defconst navi2ch-spamf-advice-list
  '((navi2ch-article-apply-message-filters
     after
     navi2ch-article-register-spam-by-filter)
    (navi2ch-article-hide-message
     before
     navi2ch-article-hide-message-as-spam)
    (navi2ch-article-cancel-hide-message
     before
     navi2ch-article-cancel-hide-message-as-good)
    ;; (navi2ch-article-next-message
    ;;  before
    ;;  navi2ch-article-next-message-as-good)
    ))

(defun navi2ch-spamf-enable ()
  "spamfilter を有効にする。
ベイジアンフィルタによる自動フィルタリング、キーによる自動スコアリングが
有効になる。"
  (interactive)
  (dolist (advice navi2ch-spamf-advice-list)
    (apply #'ad-enable-advice advice)
    (ad-activate (car advice)))
  (add-to-list 'navi2ch-article-message-filter-list
	       #'navi2ch-article-message-filter-by-bayesian))

(defun navi2ch-spamf-disable ()
  "spamfilter を無効にする。
ベイジアンフィルタによる自動フィルタリング、キーによる自動スコアリングが
無効になる。"
  (interactive)
  (dolist (advice navi2ch-spamf-advice-list)
    (apply #'ad-disable-advice advice)
    (ad-activate (car advice)))
  (setq navi2ch-article-message-filter-list
	(delq #'navi2ch-article-message-filter-by-bayesian
	      navi2ch-article-message-filter-list)))

(provide 'navi2ch-spamfilter)
;;; navi2ch-spamfilter.el ends here
