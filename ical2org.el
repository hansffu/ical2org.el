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
    (goto-char (point-min))
    (--map (ical2org/parse-event (car it) (cdr it))
           (ical2org/find-vevents))))

(defun ical2org/clean-text (text)
  "Clean TEXT from escape characters and multiline strings."
  (s-replace-all '(("\n " . "")
                   ("\\n" . "\n")
                   ("\\," . ","))
                 text))


(defun ical2org/format-timestamp (dtstart dtend rrule)
  "Format as org time range. DTSTART and DTEND specifies start and end times. RRULE specifies repeat rules."
  (let* (
         (same-day-p (lambda (d1 d2) (and
                                      (eq (ts-day d1) (ts-day d2))
                                      (eq (ts-month d1) (ts-month d2))
                                      (eq (ts-year d1) (ts-year d2)))))
         (start (ts-parse dtstart))
         (end (ts-parse dtend))
         (rules-alist (when rrule (--map (s-split "=" it) (s-split ";" rrule)) ))
         (frequency (when rules-alist (assoc "FREQ" rules-alist)))
         (repeat-frequency (when (s-equals? "WEEKLY" (cadr frequency)) "+1w"))
         )

    (cond ((funcall same-day-p start end)
           (format "<%s %s-%s>"
                   (ts-format "%Y-%m-%d %a" start)
                   (ts-format "%H:%M" start)
                   (ts-format "%H:%M" end)))
          (t (format "%s--%s"
                   (ts-format (cdr org-time-stamp-formats) start)
                   (ts-format (cdr org-time-stamp-formats) end)))
          )

    ;; (message rules-alist)
    ;; (message (cadr (assoc "FREQ" rules-alist) ) )
    ;; (message "SHEDULED <%s--%s %s>"
    ;;          (format-time-string (cdr org-time-stamp-formats) (parse-iso8601-time-string dtstart))
    ;;          (format-time-string (cdr org-time-stamp-formats) (parse-iso8601-time-string dtend))
    ;;          repeat-frequency
    ;;          )
    ;; (message (format-time-string (cdr org-time-stamp-formats) parsed) )
    ;; (message (org-time-stamp-format parsed) )
    )
  )

(defun ical2org/write-event (event)
  "Write EVENT in org format."
  (insert (format "* TODO %s\n" (ical2org/clean-text (ical2org/event-summary event) )))
  (insert (format (ical2org/clean-text (ical2org/event-description event) )))
  )

(defun ical2org/write-events (calendar events)
  "Write parsed EVENTS to org-file given in CALENDAR."
  (with-current-buffer
      (create-file-buffer (ical2org/calendar-org-file calendar))
    (org-mode)
    (-map #'ical2org/write-event events)

    (switch-to-buffer (current-buffer))))

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
  (ical2org/format-timestamp
   "20200924T150000Z"
   "20200924T150000Z"
   nil
   ;; "FREQ=WEEKLY;UNTIL=20210319T080000Z;INTERVAL=1;BYDAY=MO,TU,WE,TH,FR;WKST=SU"
   )
  (message (ts-format (ts-parse "TZID=Romance Standard Time:20200622T090000") ))
  ;; (-map #'ical2org/import-calendar ical2org/calendars)
  )

(provide 'ical2org)

;;; ical2org.el ends here
