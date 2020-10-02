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

;;;;;;;;;Configuration;;;;;;;;;;;
(cl-defstruct ical2org/calendar
  name
  url
  org-file
  )

(defgroup ical2org nil
  "Settings for `ical2org'."
  :group 'org)

(defcustom ical2org/calendars nil
  "List of calendars to be imported."
  :type 'list
  :group 'ical2org)

;;;;;;;;;Implementation;;;;;;;;;;
(cl-defstruct ical2org/event
  summary
  description
  date
  status
  attendees
  organizer
  )


(defun ical2org/read-line ()
  "Return entire line as string."
  (buffer-substring (line-beginning-position) (line-end-position)) )

(defun ical2org/read-line-after (SEPARATOR)
  "Read what comes after SEPARATOR on current line."
  (save-excursion
    (line-beginning-position)
    (search-forward SEPARATOR)
    (buffer-substring (point)(line-end-position))
    )
  )

(defun ical2org/get-event-entry-value-location (KEY START END)
  "Search within START and END locations for the specified KEY and return points for the value."
  (save-excursion
    (goto-char START)
    (condition-case nil
        (cons
         (progn (search-forward KEY END)
                (forward-char)
                (point))
         (progn (while (progn (forward-line) (beginning-of-line) (eq (char-after) 32) )
                  (end-of-line))
                (point)))
      (error nil))
    ))

(defun printContent (RANGE)
  (message (buffer-substring (car RANGE) (cdr RANGE))))

(defun ical2org/parse-event (START END)
  "Parse event entries between START and END points. Return struct `ical2org/event'."
  (save-excursion
    (let ((description (ical2org/get-event-entry-value-location "DESCRIPTION" START END))
          (summary (ical2org/get-event-entry-value-location "SUMMARY" START END))
          (start-time (ical2org/get-event-entry-value-location "DTSTART" START END))
          (end-time (ical2org/get-event-entry-value-location "DTEND" START END))
          )

      (message (printContent summary))
      (when start-time
        (message "start: %s" (buffer-substring (car start-time) (cdr start-time))) )
      (when end-time
        (message "end: %s" (buffer-substring (car end-time) (cdr end-time))) )

      (make-ical2org/event
       :summary (buffer-substring (car summary) (cdr summary))
       :description (buffer-substring (car description) (cdr description))))))

(defun ical2org/next-vevent ()
  "Returt start and end points for the next event in buffer."
  (save-excursion
    (condition-case nil
        (cons (progn (search-forward "BEGIN:VEVENT") (beginning-of-line) (point) )
              (progn (search-forward "END:VEVENT") (end-of-line) (point)))
      (error nil)
      )))

(defun ical2org/find-vevents ()
  "Returt start and end points for all events in buffer."
  (save-excursion
    (let ((events '())
          (last-event (ical2org/next-vevent)))
      (while last-event
        (push last-event events)
        (goto-char (cdr last-event))
        (setq last-event (ical2org/next-vevent)))
      events)))

(defun ical2org/parse-buffer ()
  "Extract ical data from buffer."
  (with-current-buffer
      (current-buffer)
    ;; (switch-to-buffer (current-buffer))
    (message "Preparing iCalendar...")
    (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (message "Preparing iCalendar...done")
    (icalendar--read-element nil nil)
    ))

(defun ical2org/clean-text (text)
  "Clean TEXT from escape characters and multiline strings."
  (s-replace-all '(("\n " . "")
                   ("\\n" . "\n")
                   ("\\," . ","))
                 text))

(defun ical2org/get-next-by-dow (current dow)
  "Find next date after CURRENT wher day of week is DOW."
  (let* ((cur-dow (ts-dow current))
         (dow-num (pcase dow
                    ("MO" 1) ("TU" 2) ("WE" 3) ("TH" 4)
                    ("FR" 5) ("SA" 6) ("SU" 7))))
    (ts-inc 'day
            (if (> cur-dow dow-num)
                (+ 7 (- dow-num cur-dow))
              (- dow-num cur-dow))
            current)))

(defun ical2org/same-date-p (d1 d2)
  "Check if D1 and D2 is the same date."
  (and
   (eq (ts-day d1) (ts-day d2))
   (eq (ts-month d1) (ts-month d2))
   (eq (ts-year d1) (ts-year d2))))

(defun ical2org/format-event-timestamp (start end repeat-frequency)
  "Format event schedule from START to END with with REPEAT-FREQUENCY."
  (if (ical2org/same-date-p start end)
      (format "<%s %s-%s%s>"
              (ts-format "%Y-%m-%d %a" start)
              (ts-format "%H:%M" start)
              (ts-format "%H:%M" end)
              repeat-frequency)
    (format "%s--%s"
            (ts-format (cdr org-time-stamp-formats) start)
            (ts-format (cdr org-time-stamp-formats) end)
            )))

(defsubst ical2org/parse-timestamp (string)
  "Return new `ts' struct, parsing STRING with `iso8601-parse'."
  (let ((parsed (iso8601-parse string)))
    ;; Fill nil values
    (cl-loop for i from 0 to 5
             when (null (nth i parsed))
             do (setf (nth i parsed) 0))
    (->> parsed
         (apply #'encode-time)
         float-time
         (make-ts :unix)))
  )

(defsubst ical2org/decode-timestamp (datetime time-zone)
  "Return new `ts' struct, parsing STRING with `iso8601-parse'."
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

(defun ical2org/format-timestamp (start end rrule)
  "Format as org time range. DTSTART and DTEND specifies start and end times. RRULE specifies repeat rules."
  (let* (

         ;; (start (ical2org/parse-timestamp dtstart))
         ;; (end (ical2org/parse-timestamp dtend))

         (rules-alist (when rrule (--map (s-split "=" it) (s-split ";" rrule)) ))

         (frequency (when rules-alist (assoc "FREQ" rules-alist)))
         (repeat-frequency (if (s-equals? "WEEKLY" (cadr frequency)) " +1w" ""))

         (byday (when rules-alist (assoc "BYDAY" rules-alist)))
         (days (when byday  (--map (cons (ical2org/get-next-by-dow start it)
                                         (ical2org/get-next-by-dow end it))
                                   (s-split "," (cadr byday) ) )))
         )

    (if days
        (--reduce-from
         (format "%s%s" acc
                 (ical2org/format-event-timestamp
                  (car it)
                  (cdr it)
                  repeat-frequency))
         "" days)
      (ical2org/format-event-timestamp start end repeat-frequency)
      )

    )
  )

(defun ical2org/write-event (event zone-map)
  "Write EVENT in org format."
  (let* (
         (content (caddr event))
         (keyword "TODO")
         ;; (header (ical2org/clean-text (ical2org/event-summary event)))
         ;; (description (ical2org/clean-text (ical2org/event-description event)))
         ;; (summary (cdr (assoc 'SUMMARY content) ))

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
         (dtstart-dec (ical2org/decode-timestamp dtstart dtstart-zone))
         ;; (dtstart-dec (icalendar--decode-isodatetime dtstart nil dtstart-zone))


         (dtend (icalendar--get-event-property event 'DTEND))
         (dtend-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes event 'DTEND)
                      zone-map))
         (dtend-dec (ical2org/decode-timestamp dtend dtend-zone))

         )

    (insert (format "\n* TODO %s\n" summary))
    (insert (format "%s\n" description))

    (insert (ical2org/format-timestamp dtstart-dec dtend-dec ""))
    ;; (insert (format "dtstart %s\n" dtstart))
    ;; (insert (format "cfstart-zone  %s\n" dtstart-zone))
    ;; (insert (format "dtstart-dec) %s\n" (ts-format dtstart-dec )))
    ;; (insert (format "start-d %s\n" start-d))


    )

  )

(defun ical2org/write-events (calendar ical-list)
  "Write parsed EVENTS to org-file given in CALENDAR."
  (with-current-buffer
      (create-file-buffer (ical2org/calendar-org-file calendar))
    (org-mode)
    (let ((events (icalendar--all-events ical-list))

          (zone-map (icalendar--convert-all-timezones ical-list))
          )

      (message "'%s" events)
      (--map (ical2org/write-event it zone-map) events)
      )


    (switch-to-buffer (current-buffer))
    ))

(defun ical2org/import-calendar (calendar)
  "Fetch calendars defined in CALENDAR."
  (let* ((events (with-temp-buffer
                   (url-insert-file-contents
                    (ical2org/calendar-url calendar))
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
