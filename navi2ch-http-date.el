;;; navi2ch-http-date.el --- parser / generator of HTTP-date format -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2008 by Navi2ch Project

;; Author: Nanashi San <nanashi@users.sourceforge.net>
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

;; From RFC 2616

;; 3.3.1 Full Date
;;
;;    HTTP applications have historically allowed three different formats
;;    for the representation of date/time stamps:
;;
;;       Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
;;       Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
;;       Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
;;
;;    The first format is preferred as an Internet standard and represents
;;    a fixed-length subset of that defined by RFC 1123 [8] (an update to
;;    RFC 822 [9]). The second format is in common use, but is based on the
;;    obsolete RFC 850 [12] date format and lacks a four-digit year.
;;    HTTP/1.1 clients and servers that parse the date value MUST accept
;;    all three formats (for compatibility with HTTP/1.0), though they MUST
;;    only generate the RFC 1123 format for representing HTTP-date values
;;    in header fields. See section 19.3 for further information.
;;
;;       Note: Recipients of date values are encouraged to be robust in
;;       accepting date values that may have been sent by non-HTTP
;;       applications, as is sometimes the case when retrieving or posting
;;       messages via proxies/gateways to SMTP or NNTP.
;;
;;    All HTTP date/time stamps MUST be represented in Greenwich Mean Time
;;    (GMT), without exception. For the purposes of HTTP, GMT is exactly
;;    equal to UTC (Coordinated Universal Time). This is indicated in the
;;    first two formats by the inclusion of "GMT" as the three-letter
;;    abbreviation for time zone, and MUST be assumed when reading the
;;    asctime format. HTTP-date is case sensitive and MUST NOT include
;;    additional LWS beyond that specifically included as SP in the
;;    grammar.
;;
;;        HTTP-date    = rfc1123-date | rfc850-date | asctime-date
;;        rfc1123-date = wkday "," SP date1 SP time SP "GMT"
;;        rfc850-date  = weekday "," SP date2 SP time SP "GMT"
;;        asctime-date = wkday SP date3 SP time SP 4DIGIT
;;        date1        = 2DIGIT SP month SP 4DIGIT
;;                       ; day month year (e.g., 02 Jun 1982)
;;        date2        = 2DIGIT "-" month "-" 2DIGIT
;;                       ; day-month-year (e.g., 02-Jun-82)
;;        date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
;;                       ; month day (e.g., Jun  2)
;;        time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
;;                       ; 00:00:00 - 23:59:59
;;        wkday        = "Mon" | "Tue" | "Wed"
;;                     | "Thu" | "Fri" | "Sat" | "Sun"
;;        weekday      = "Monday" | "Tuesday" | "Wednesday"
;;                     | "Thursday" | "Friday" | "Saturday" | "Sunday"
;;        month        = "Jan" | "Feb" | "Mar" | "Apr"
;;                     | "May" | "Jun" | "Jul" | "Aug"
;;                     | "Sep" | "Oct" | "Nov" | "Dec"
;;
;;       Note: HTTP requirements for the date/time stamp format apply only
;;       to their usage within the protocol stream. Clients and servers are
;;       not required to use these formats for user presentation, request
;;       logging, etc.

;;; Code:

(provide 'navi2ch-http-date)
(defconst navi2ch-http-date-ident
  "$Id$")

(require 'regexp-opt)
(require 'timezone)

(defun navi2ch-http-date-encode (time)
  "内部形式 TIME を RFC 1123 形式に変換する。"
  (apply (lambda (wday month day time year)
	   (format "%s, %02d %s %s %s GMT"
		   wday (string-to-number day) month year time))
	 (split-string
	  (current-time-string (let ((decoded (decode-time time)))
				 (apply #'encode-time
					(- (car decoded)
					   (car (last decoded)))
					(cdr decoded)))))))

(defun navi2ch-http-date-decode (http-date)
  "HTTP-DATE を内部形式に変換する。"
  ;; XEmacs だと RFC 850 形式の "-" 付きの日付をパースできないので。
  (if (string-match "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)" http-date)
      (setq http-date (replace-match "\\1 \\2 \\3" nil nil http-date)))
  (let ((now (timezone-fix-time http-date "GMT" "GMT")))
    (encode-time (aref now 5) (aref now 4) (aref now 3)
		 (aref now 2) (aref now 1) (aref now 0)
		 (aref now 6))))

;; テスト
(eval-when-compile
  (let ((expected "Sun, 06 Nov 1994 08:49:37 GMT"))
    (assert (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sun, 06 Nov 1994 08:49:37 GMT"))))
    (assert (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sunday, 06-Nov-94 08:49:37 GMT"))))
    (assert (string= expected (navi2ch-http-date-encode
			       (navi2ch-http-date-decode
				"Sun Nov  6 08:49:37 1994"))))))

;;; navi2ch-http-date.el ends here
