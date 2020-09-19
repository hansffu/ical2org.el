;;; ical2org.el --- Download an ical file and import into org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Hans Fredrik Furholt
;;
;; Author: Hans Fredrik Furholt <http://github/hansffu>
;; Maintainer: Hans Fredrik Furholt <hansff@gmail.com>
;; Created: September 17, 2020
;; Modified: September 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/hansffu/ical2org
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
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



(defun ical2org/import-calendar (calendar)
  "Fetch calendars defined in CALENDAR."
  (url-retrieve (ical2org/calendar-url calendar)
                (apply-partially #'ical2org/callback calendar))
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

(defun ical2org/get-event-entry-location-value (KEY START END)
  "Search within START and END locations for the specified KEY and return points for the value."
  (save-excursion
    (goto-char START)
    (cons
     (progn (search-forward KEY END)
            (forward-char)
            (point))
     (progn (while (progn (forward-line) (beginning-of-line) (eq (char-after) 32) )
              (end-of-line))
            (point))))
  )

(defun printContent (RANGE)
  (message (buffer-substring (car RANGE) (cdr RANGE))))

(defun ical2org/parse-event (START END)
  "Parse event entries between START and END points."
  (save-excursion
    (let ((description (ical2org/get-event-entry-location-value "DESCRIPTION" START END))
          (summary (ical2org/get-event-entry-location-value "SUMMARY" START END)))

      (printContent summary)
      )
    "test"
    )
  )

(defun ical2org/next-vevent ()
  "Returt start and end points for the next event in buffer."
  (save-excursion
    (condition-case nil
        (cons (progn (search-forward "BEGIN:VEVENT" ) (beginning-of-line) (point) )
              (progn (search-forward "END:VEVENT") (end-of-line) (point)))
      (error nil)
      )))

(defun ical2org/callback (calendar callback)
  (with-current-buffer
      (current-buffer)

    ;; (switch-to-buffer (current-buffer))

    (beginning-of-buffer)

    (let ((events '())
          (last-event (ical2org/next-vevent)))
      (while last-event
        (push last-event events)
        ;; (message last-event)
        (message "event %d %d" (car last-event) (cdr last-event))
        (goto-char (cdr last-event))
        (setq last-event (ical2org/next-vevent))
        )
      (--map (ical2org/parse-event (car it) (cdr it)) events)
      )

    )
  )

(defun ical2org/import ()
  "Fetch calendars defined in `ical2org/calendars'."
  (interactive)
  (-map #'ical2org/import-calendar ical2org/calendars))

(provide 'ical2org)
;;; ical2org.el ends here
