;;; ical2org.el --- Download an ical file and import into org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Hans Fredrik Furholt
;;
;; Author: Hans Fredrik Furholt <http://github/hansffu>
;; Created: September 17, 2020
;; Modified: September 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/hansffu/ical2org
;; Package-Requires: ((cl-lib "0.5") (dash "2.17.0") (s "1.12.0") (ts "0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Download an ical file and import into org
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'url)
(require 's)
(require 'org)
(require 'ts)
(require 'parse-time)
(require 'icalendar)

;;;;;;;;;Configuration;;;;;;;;;;;

(defvar ical2org/calendars '()
  "List of calendars to be imported.")

;;;;;;;;;Implementation;;;;;;;;;;

(defun ical2org/parse-buffer ()
  "Extract ical data from buffer."
  (with-current-buffer
      (current-buffer)
    (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (icalendar--read-element nil nil)
    ))

(defun ical2org/get-next-by-dow (current dow)
  "Find next date after CURRENT wher day of week is DOW."
  (if (or (s-contains? "-" dow) (s-contains? "+" dow) )
      (progn
        (message "WARNING: Relative days (+WE, -MO etc) is not yet supported")
        nil)
    (let* ((cur-dow (ts-dow current))
           (dow-num (pcase dow
                      ("MO" 1) ("TU" 2) ("WE" 3) ("TH" 4)
                      ("FR" 5) ("SA" 6) ("SU" 7))))
      (ts-inc 'day
              (if (> cur-dow dow-num)
                  (+ 7 (- dow-num cur-dow))
                (- dow-num cur-dow))
              current))))

(defun ical2org/same-date-p (d1 d2)
  "Check if D1 and D2 is the same date."
  (and
   (eq (ts-day d1) (ts-day d2))
   (eq (ts-month d1) (ts-month d2))
   (eq (ts-year d1) (ts-year d2))))

(defun ical2org/format-event-timestamp (start end repeat-frequency)
  "Format event schedule from START to END with with REPEAT-FREQUENCY."
  (cond ((not end)
         (format "<%s%s>"
                 (ts-format "%Y-%m-%d %a" start)
                 ;; (ts-format "%H:%M" start)
                 (or repeat-frequency "")))

        ((ical2org/same-date-p start end)
         (format "<%s%s>"
                 (ts-format "%Y-%m-%d %a" start)
                 ;; (ts-format "%H:%M" start)
                 ;; (ts-format "%H:%M" end)
                 (or repeat-frequency "")))

        (t (format "%s--%s"
                   (ts-format (cdr org-time-stamp-formats) start)
                   (ts-format (cdr org-time-stamp-formats) end)
                   ) )))

(defsubst ical2org/decode-timestamp (datetime time-zone)
  "Return new `ts' struct, parsing DATETIME with the given TIME-ZONE."
  (let ((parsed (icalendar--decode-isodatetime datetime nil time-zone)))

    ;; Fill nil values
    (cl-loop for i from 0 to 5
             when (null (nth i parsed))
             do (setf (nth i parsed) 0))
    (->> parsed
         (apply #'encode-time)
         float-time
         (make-ts :unix)))
  )

(defun ical2org/dayname-to-diary-num (dayname)
  (pcase dayname
    ("MO" 1) ("TU" 2) ("WE" 3) ("TH" 4)
    ("FR" 5) ("SA" 6) ("SU" 0)
    ))

(defun ical2org/format-timestamp (start end rrule)
  "Format as org time range. START and END specifies start and end times. RRULE specifies repeat rules."
  (let* (
         (rules-alist (when rrule (--map (s-split "=" it) (s-split ";" rrule)) ))

         (frequency (when rules-alist (assoc "FREQ" rules-alist)))
         (interval (when rules-alist (assoc "INTERVAL" rules-alist)))
         (repeat-frequency (if  frequency
                               (format " +%s%s"
                                       (if interval (cadr interval) 1 )
                                       (pcase (cadr frequency)
                                         ("WEEKLY" "w")
                                         ("DAILY" "d")
                                         ))
                             ""))

         (byday (when rules-alist (assoc "BYDAY" rules-alist)))
         (days (when byday  (--map (if (s-contains-p "-" it)
                                       (cons (s-left 2 it) (s-right 2 it))
                                     (cons (ical2org/get-next-by-dow start it)
                                           (ical2org/get-next-by-dow end it)) )
                                   (s-split "," (cadr byday) ) )))
         )

    (cond
     ;; ((and byday (s-contains? "-" (cadr byday)) ) "<%%(diary-float t 3 -1)>")
     (days (--reduce-from
            (if (stringp (car it))
                (format "<%%%%(diary-float t %s %s)>" (ical2org/dayname-to-diary-num (cdr it)) (car it))
              (format "%s%s" acc
                      (ical2org/format-event-timestamp
                       (car it)
                       (cdr it)
                       repeat-frequency)) )
            "" days))
     (t (ical2org/format-event-timestamp start end repeat-frequency) )
     )

    )
  )

(defun ical2org/write-event (event zone-map)
  "Write EVENT in org format. Times are adjusted according to ZONE-MAP."
  (let* (
         (description (icalendar--convert-string-for-import
                       (or (icalendar--get-event-property event 'DESCRIPTION)
                           "No description")))
         (summary (icalendar--convert-string-for-import
                   (or (icalendar--get-event-property event 'SUMMARY)
                       "No summary")))

         (dtstart (icalendar--get-event-property event 'DTSTART))
         (dtstart-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes event 'DTSTART)
                        zone-map))
         (dtstart-dec (when (and dtstart) (ical2org/decode-timestamp dtstart dtstart-zone) ))
         (dtend (icalendar--get-event-property event 'DTEND))
         (dtend-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes event 'DTEND)
                      zone-map))
         (dtend-dec (when (and dtend) (ical2org/decode-timestamp dtend dtend-zone) ))
         (rrule (icalendar--get-event-property event 'RRULE))
         (date (format "%s\n" (ical2org/format-timestamp dtstart-dec dtend-dec rrule) ))
         (time (format "%s-%s" (ts-format "%H:%M" dtstart-dec) (ts-format "%H:%M" dtend-dec)))

         (status (or (icalendar--get-event-property event 'STATUS) "UNKNOWN"))

         (attendees (icalendar--get-event-properties event 'ATTENDEE))
         (attendees-formatted (if attendees
                                  (--reduce (format "%s [[%s][%s]]" acc it (s-chop-prefix "mailto:" it))
                                            attendees)
                                ""))

         (organizer  (icalendar--get-event-property event 'ORGANIZER) )
         (organizer-props (icalendar--get-event-property-attributes event 'ORGANIZER))
         (organizer-name (when organizer-props (cadr organizer-props)))
         (organizer-formatted (if organizer
                                  (format "[[%s][%s]]" organizer
                                          (or organizer-name (s-chop-prefix "mailto:" organizer)))
                                ""))
         (location (or (icalendar--get-event-property event 'LOCATION) "" ))
         )
    (insert "** " time ?\s summary ?\n
            ":PROPERTIES:" ?\n
            ":STATUS:" ?\s status ?\n
            ":ORGANIZER:" ?\s organizer-formatted ?\n
            ":ATTENDEES:" ?\s attendees-formatted ?\n
            ":LOCATION:" ?\s location ?\n
            ":WILD_NOTIFIER_NOTIFY_BEFORE: 1" ?\n
            ":END:"?\n
            date ?\n
            description ?\n)
    )

  )

(defun ical2org/write-events (calendar ical-list)
  "Write parsed events in ICAL-LIST to org-file given in CALENDAR."
  (with-current-buffer
      (or (find-buffer-visiting (plist-get calendar :org-file))
          (create-file-buffer (plist-get calendar :org-file)))
    (erase-buffer)

    (org-mode)

    (let ((events (icalendar--all-events ical-list))
          (zone-map (icalendar--convert-all-timezones ical-list)))
      (insert "*" ?\s
              (plist-get calendar :name)
              ?\s
              ":" (plist-get calendar :name) ":"
              ?\n)
      (dolist (event events) (ical2org/write-event event zone-map)))

    (write-file (plist-get calendar :org-file))
    ))

(defun ical2org/import-calendar (calendar)
  "Fetch calendars defined in CALENDAR."
  (let* ((events (with-temp-buffer
                   (url-insert-file-contents
                    (plist-get calendar :url))
                   (ical2org/parse-buffer))
                 ))
    (ical2org/write-events calendar events)))

(defun ical2org/import ()
  "Fetch calendars defined in `ical2org/calendars'."
  (interactive)

  (-map #'ical2org/import-calendar ical2org/calendars)
  )


(provide 'ical2org)

;;; ical2org.el ends here
