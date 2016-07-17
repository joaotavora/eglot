;;; htz.el ---  Timezone-based time and date support for GNU Hyperbole
;;
;; Author:       Masanobu UMEDA             / Bob Weiner
;;
;; Orig-Date:    14-Oct-91 at 07:22:08
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; Adapted from Timezone package for GNU Emacs
;; Copyright(C) 1990 Masanobu UMEDA
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;; All date parsing functions accept the output of any other parsing
;; function as input, so one can convert to a sortable date format, do a
;; compare and the display the result in a user selected format.
;; All date formats use a 4-digit year, so there are no problems around the
;; turn of the century.
;;
;; Hyperbole uses this package to normalize all worldwide date times to
;; Greenwich Mean Time, so that Hyperbole buttons created by users in
;; different timezones will sort by time properly.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'calendar)
(require 'cal-julian)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun htz:date-arpa (&optional date local timezone)
  "Convert optional DATE or current date to an arpanet standard date.
Optional 1st argument LOCAL specifies the default local timezone of the DATE.
Optional 2nd argument TIMEZONE specifies a timezone to be represented in."
  (or (vectorp date)
      (setq date (htz:date-parse (or date (current-time-string)))))
  (let* ((year   (string-to-number (aref date 0)))
	 (month  (string-to-number (aref date 1)))
	 (day    (string-to-number (aref date 2)))
	 (time   (htz:time-parse (aref date 3)))
	 (hour   (string-to-number (aref time 0)))
	 (minute (string-to-number (aref time 1)))
	 (second (string-to-number (aref time 2)))
	 (local  (or (aref date 4) local htz:local)) ;Use original if defined
	 (timezone (or timezone local))
	 (diff   (- (htz:zone-to-hour timezone)
		    (htz:zone-to-hour local)))
	 (new    (htz:time-fix year month day
			       (+ hour diff) minute second)))
    (htz:date-make-arpa (aref new 0) (aref new 1) (aref new 2)
			(htz:time-make-string
			 (aref new 3) (aref new 4) (aref new 5))
			timezone)))

(defun htz:date-parse (date &optional parsed-current-date)
  "Parse DATE string and return a vector [year month day time timezone].
19 is prepended to year if necessary.  Timezone in DATE is optional, it
defaults to the value of `htz:local'.

Recognizes the following styles:
 (1) \"(1 30 1999)\" or \"(1 30 1999)\"  `calendar-julian-date'  requires `parsed-current-date' arg
 (2) \"14 Apr 89 03:20[:12] [GMT]\"
 (3) \"Fri, 17 Mar [19]89 4:01[:33] [GMT]\"
 (4) \"Mon Jan 16 16:12[:37] [GMT] 1989\"
 (5) \"19911014:07:51:08 or 1991101407:51:08\"  `sortable date'
 (6) \"Mar 29 14:00\"    `ls -l date'  requires `parsed-current-date' arg
 (7) \"Mar  7  1994\"    `ls -l date'  requires `parsed-current-date' arg"
  (let ((date (or date ""))
	(year nil)
	(month nil)
	(day nil)
	(time nil)
	(zone nil))			; This may be nil.
    (if (listp date)
	(setq month (nth 0 date)
	      day   (nth 1 date)
	      year  (nth 2 date))
      (cond ((string-match
	      "\\`(\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\))\\'" date)
	     ;; Style (1)
	     (setq year 3 month 1 day 2 time nil zone nil))
	    ((string-match
	      "\\([0-9][0-9][0-9][0-9]\\)\\([0-1][0-9]\\)\\([0-3][0-9]\\):?\\([0-2][0-9]:[0-5][0-9:]+\\)[ ]*\\'" date)
	     ;; Style (4)
	     (setq year 1 month 2 day 3 time 4 zone nil))
	    ((string-match
	      "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9]+:[0-9:]+\\)[ ]*\\'" date)
	     ;; Styles: (1) and (2) without timezone
	     (setq year 3 month 2 day 1 time 4 zone nil))
	    ((string-match
	      "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9]+:[0-9:]+\\)[ ]*\\([-+a-zA-Z0-9]+\\)" date)
	     ;; Styles: (1) and (2) with timezone and buggy timezone
	     (setq year 3 month 2 day 1 time 4 zone 5))
	    ((string-match
	      "\\([^ ,]+\\) +\\([0-9]+\\) \\([0-9]+:[0-9:]+\\) \\([0-9]+\\)" date)
	     ;; Styles: (3) without timezone
	     (setq year 4 month 1 day 2 time 3 zone nil))
	    ((string-match
	      "\\([^ ,]+\\) +\\([0-9]+\\) \\([0-9]+:[0-9:]+\\) \\([-+a-zA-Z0-9]+\\) \\([0-9]+\\)" date)
	     ;; Styles: (3) with timezone
	     (setq year 5 month 1 day 2 time 3 zone 4))
	    ((string-match "^\\([^ ,]+\\) +\\([0-9]+\\) +\\([0-9]+:[0-9:]+\\)$" date)
	     ;; Style: (5)
	     (setq year nil month 1 day 2 time 3 zone nil))
	    ((string-match
	      "^\\([^ ,]+\\) +\\([0-9]+\\) +\\([0-9][0-9][0-9][0-9]\\)$" date)
	     ;; Style: (6)
	     (setq year 3 month 1 day 2 time nil zone nil))
	    (t (error "(htz:date-parse): Invalid date format: `%s'" date)))
      (if year
	  (setq year
		(substring date (match-beginning year) (match-end year))
		year (if (/= (length year) 2) year
		       (let* ((yr (substring (current-time-string) -4))
			      (curr-yr (substring yr 2))
			      (century (substring yr 0 2)))
			 (concat (if (string< curr-yr yr)
				     (format "%02d"
					     (1- (string-to-number century)))
				   century)
				 year))))
	(setq year (if (vectorp parsed-current-date)
		       (aref parsed-current-date 0)
		     "0")))
      (if month
	  (setq month (substring date
				 (match-beginning month) (match-end month))
		month (if (/= (string-to-number month) 0) month
			(int-to-string
			 (cdr (assoc (upcase month) htz:months-assoc)))))
	(setq month (if (vectorp parsed-current-date)
			(aref parsed-current-date 1)
		      "0")))
      (if day
	  (setq day (substring date (match-beginning day) (match-end day)))
	(setq day (if (vectorp parsed-current-date)
		      (aref parsed-current-date 2)
		    "0"))))
    (if time
	(setq time (substring date (match-beginning time) (match-end time)))
      (setq time (if (vectorp parsed-current-date)
		     (aref parsed-current-date 3))))
    (if zone
	(setq zone (substring date (match-beginning zone) (match-end zone)))
      (setq zone (if (vectorp parsed-current-date)
		     (aref parsed-current-date 4)
		   htz:local)))
    ;; Return a vector.
    (vector year month day time zone)))

(defun htz:date-sortable (&optional date local timezone)
  "Convert optional DATE or current date to a sortable date string.
Optional 1st argument LOCAL specifies the local timezone of the DATE.
Optional 2nd argument TIMEZONE specifies an output timezone to use."
  (or (vectorp date)
      (setq date (htz:date-parse (or date (current-time-string)))))
  (let* ((year   (string-to-number (aref date 0)))
	 (month  (string-to-number (aref date 1)))
	 (day    (string-to-number (aref date 2)))
	 (time   (htz:time-parse (aref date 3)))
	 (hour   (string-to-number (aref time 0)))
	 (minute (string-to-number (aref time 1)))
	 (second (string-to-number (aref time 2)))
	 (local  (or (aref date 4) local htz:local)) ;Use original if defined
	 (timezone (or timezone local))
	 (diff   (- (htz:zone-to-hour timezone)
		    (htz:zone-to-hour local)))
	 (new    (htz:time-fix year month day
			       (+ hour diff) minute second)))
    (htz:date-make-sortable (aref new 0) (aref new 1) (aref new 2)
			    (htz:time-make-string
			     (aref new 3) (aref new 4) (aref new 5)))))

;;;
;;; Parsers and Constructors of Date and Time
;;;

(defun htz:date-sortable-gmt (&optional date local)
  "Convert optional DATE or current date  to a sortable date string in Greenwich Mean Time.
Optional argument LOCAL specifies the local timezone of the DATE."
  (htz:date-sortable date local "GMT"))

(defun htz:date-unix (&optional date local timezone)
  "Convert DATE or current date to a unix standard date.
Optional 1st argument LOCAL specifies the local timezone of the DATE (default
is the timezone embedded in the date or if there is none, then the value of
`htz:local').  Optional 2nd argument TIMEZONE specifies the timezone in which
the date is returned; it defaults to the value of `htz:local'."
  (or (vectorp date)
      (setq date (htz:date-parse (or date (current-time-string)))))
  (or local (setq local (or (aref date 4) htz:local)))
  (let* ((year   (string-to-number (aref date 0)))
	 (month  (string-to-number (aref date 1)))
	 (day    (string-to-number (aref date 2)))
	 (time   (htz:time-parse (aref date 3)))
	 (hour   (string-to-number (aref time 0)))
	 (minute (string-to-number (aref time 1)))
	 (second (string-to-number (aref time 2)))
	 (timezone (or timezone local))
	 (diff   (- (htz:zone-to-hour timezone)
		    (htz:zone-to-hour local)))
	 (fixed    (htz:time-fix year month day
				 (+ hour diff) minute second)))
    (htz:date-make-unix
     (aref fixed 0) (aref fixed 1) (aref fixed 2)
     (htz:time-make-string (aref fixed 3) (aref fixed 4) (aref fixed 5))
     timezone)))

(defun htz:span-in-days (start-date end-date)
  "Return span in days between START-DATE and END-DATE strings.
See `htz:date-parse' for a list of acceptable date formats."
  (if (and (listp start-date) (listp end-date))
      (- (calendar-julian-to-absolute end-date)
	 (calendar-julian-to-absolute start-date))
    (let* ((parsed-current-date (htz:date-parse (current-time-string)))
	   (htz-start-date (htz:date-parse start-date parsed-current-date))
	   (htz-end-date   (htz:date-parse end-date parsed-current-date))
	   (cal-start-date
	    (list (string-to-number (aref htz-start-date 1));; month
		  (string-to-number (aref htz-start-date 2));; day
		  (string-to-number (aref htz-start-date 0))));; year
	   (cal-end-date
	    (list (string-to-number (aref htz-end-date 1));; month
		  (string-to-number (aref htz-end-date 2));; day
		  (string-to-number (aref htz-end-date 0))));; year
	   )
      (- (calendar-julian-to-absolute cal-end-date)
	 (calendar-julian-to-absolute cal-start-date)))))

(defun htz:time-parse (time)
  "Parse TIME (HH:MM:SS) and return a vector [hour minute second]."
  (let ((time (or time ""))
	(hour nil)
	(minute nil)
	(second nil))
    (cond ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM:SS
	   (setq hour 1 minute 2 second 3))
	  ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" time)
	   ;; HH:MM
	   (setq hour 1 minute 2 second nil)))
    ;; Return [hour minute second]
    (vector
     (if hour
	 (substring time (match-beginning hour) (match-end hour)) "0")
     (if minute
	 (substring time (match-beginning minute) (match-end minute)) "0")
     (if second
	 (substring time (match-beginning second) (match-end second)) "0"))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun htz:date-make-arpa (year month day time &optional timezone)
  "Make arpanet standard date string from YEAR, MONTH, DAY, and TIME.
Optional argument TIMEZONE specifies a time zone."
  (format "%02d %s %02d %s%s"
	  day
	  (capitalize (car (rassq month htz:months-assoc)))
	  (- year (* (/ year 100) 100))	;1990 -> 90
	  time
	  (if timezone (concat " " timezone) "")
	  ))

(defun htz:date-make-unix (year month day time &optional timezone)
  "Approximate Unix date format from YEAR, MONTH, DAY, and TIME.
Optional argument TIMEZONE specifies a time zone."
  (format "%s %02d %s%s %04d"
	  (capitalize (car (rassq month htz:months-assoc)))
	  day time (if timezone (concat " " timezone) "") year))

(defun htz:date-make-sortable (year month day time)
  "Make sortable date string from YEAR, MONTH, DAY, and TIME."
  (format "%04d%02d%02d:%s" year month day time))

(defun htz:last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (htz:leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defun htz:leap-year-p (year)
  "Returns t if YEAR is a Gregorian leap year."
  (or (and (zerop  (% year 4))
	   (not (zerop (% year 100))))
      (zerop (% year 400))))

(defun htz:time-fix (year month day hour minute second)
  "Fix date and time."
  (cond ((<= 24 hour)			; 24 -> 00
	 (setq hour (- hour 24))
	 (setq day  (1+ day))
	 (if (< (htz:last-day-of-month month year) day)
	     (progn
	       (setq month (1+ month))
	       (setq day 1)
	       (if (< 12 month)
		   (progn
		     (setq month 1)
		     (setq year (1+ year)))))))
	((> 0 hour)
	 (setq hour (+ hour 24))
	 (setq day  (1- day))
	 (if (> 1 day)
	     (progn
	       (setq month (1- month))
	       (if (> 1 month)
		   (progn
		     (setq month 12)
		     (setq year (1- year))))
	       (setq day (htz:last-day-of-month month year))))))
  (vector year month day hour minute second))

;; Partly copied from Calendar program by Edward M. Reingold.
(defun htz:time-make-string (hour minute second)
  "Make time string from HOUR, MINUTE, and SECOND."
  (format "%02d:%02d:%02d" hour minute second))

(defun htz:zone-to-hour (timezone)
  "Translate TIMEZONE (in zone name or integer) to integer hour."
  (if timezone
      (progn
	(setq timezone
	      (or (cdr (assoc (upcase timezone)
			      htz:world-timezones))
		  (and (fboundp 'current-time-zone)
		       (if (listp (current-time-zone))
			   (car (current-time-zone))
			 (current-time-zone)))
		  timezone))
	(if (stringp timezone)
	    (setq timezone (string-to-number timezone)))
	(/ timezone 100))
    (error "(htz:zone-to-hour): Nil timezone sent as argument")))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar htz:local
  (let ((local-tz (or (getenv "TZ") (getenv "TIMEZONE")
		      (if (fboundp 'current-time-zone)
			  (car (cdr (current-time-zone))))
		      (progn 
			(require 'hypb)
			(hypb:call-process-p
			 "date" nil '(if (re-search-backward
					  " \\([-+a-zA-Z0-9]+\\) [0-9]+$" nil t)
					 (buffer-substring (match-beginning 1)
							   (match-end 1))))))))
    (if (and (stringp local-tz) (string-match " " local-tz))
	;; Windows returns things like "Eastern Daylight Time", so
	;; abbreviate to the first letter of each word.
	(concat (mapcar (lambda (s) (aref s 0)) (split-string local-tz)))
      local-tz))
  "Holds string giving the timezone for the local machine.")

(defvar htz:world-timezones
  '(("PST" .  -800)
    ("PDT" .  -700)
    ("MST" .  -700)
    ("MDT" .  -600)
    ("CST" .  -600)
    ("CDT" .  -500)
    ("EST" .  -500)
    ("EDT" .  -400)
    ("AST" .  -400)
    ("NST" .  -330)
    ("GMT" .  +000)
    ("BST" .  +100)
    ("MET" .  +100)
    ("EET" .  +200)
    ("JST" .  +900)
    ("GMT+1"  .  +100) ("GMT+2"  .  +200) ("GMT+3"  .  +300)
    ("GMT+4"  .  +400) ("GMT+5"  .  +500) ("GMT+6"  .  +600)
    ("GMT+7"  .  +700) ("GMT+8"  .  +800) ("GMT+9"  .  +900)
    ("GMT+10" . +1000) ("GMT+11" . +1100) ("GMT+12" . +1200) ("GMT+13" . +1300)
    ("GMT-1"  .  -100) ("GMT-2"  .  -200) ("GMT-3"  .  -300)
    ("GMT-4"  .  -400) ("GMT-5"  .  -500) ("GMT-6"  .  -600)
    ("GMT-7"  .  -700) ("GMT-8"  .  -800) ("GMT-9"  .  -900)
    ("GMT-10" . -1000) ("GMT-11" . -1100) ("GMT-12" . -1200))
  "Time differentials of timezone from GMT in +-HHMM form.")

(defvar htz:months-assoc
  '(("JAN" .  1)("FEB" .  2)("MAR" .  3)
    ("APR" .  4)("MAY" .  5)("JUN" .  6)
    ("JUL" .  7)("AUG" .  8)("SEP" .  9)
    ("OCT" . 10)("NOV" . 11)("DEC" . 12))
  "Alist of first three letters of a month and its numerical representation.")


(provide 'htz)

;;; htz.el ends here
