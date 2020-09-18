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
  ;; (save-excursion
  (line-beginning-position)
  (search-forward SEPARATOR)
  (buffer-substring (point)(line-end-position))
  ;; )
  )

(defun ical2org/parse-event-entry (KEY END)
  (save-excursion
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
    ;; (message (buffer-substring START END))
    (let ((description (ical2org/parse-event-entry "DESCRIPTION" END))
          (summary (ical2org/parse-event-entry "SUMMARY" END)))

      ;; (message (buffer-substring (car summary) (cdr summary)))
      ;; (message (buffer-substring (car description) (cdr description)))
      (printContent summary)
      (printContent description)
      )
    "test"
    )
  )

(defun ical2org/next-vevent ()
  (save-excursion
    (let ((start  (progn (search-forward "BEGIN:VEVENT" ) (beginning-of-line) (point) ) )
          (end (progn (search-forward "END:VEVENT" nil t) (end-of-line) (point))))
      (if (and start end)
          (list start end)
        (message "hkkk"))
      )))

(defun ical2org/callback (calendar callback)
  ;; (message "callback %s" (ical2org/calendar-name calendar))
  ;; (let ((ical-buffer (current-buffer)))
  ;;   (switch-to-buffer ical-buffer)
  ;;   )
  (with-current-buffer
      (current-buffer)
    ;; (switch-to-buffer (current-buffer))
    (beginning-of-buffer)

    ;; (message (car (ical2org/next-vevent) ))
    ;; (message "pos %d" (search-forward "BEGIN:VEVENT" ) )
    ;; (beginning-of-line)
    ;; (message (ical2org/read-line))
    ;; (message "start")
    ;; (message (ical2org/next-vevent))
    ;; (message "end")
    (apply 'ical2org/parse-event (ical2org/next-vevent))
    ;; (search-forward "BEGIN:VEVENT")
    ;; (forward-line)

    ;; (message (buffer-substring (point) (line-end-position)))
    ;; (let ((entry (ical2org/parse-event)))
    ;;   (message "hello")
    ;;   (message (cdr (assoc 'summary entry)))
    ;;   (message (cdr (assoc 'start-time entry))))
    )
  ;; (switch-to-buffer (current-buffer))
  )

(defun ical2org/import ()
  "Fetch calendars defined in `ical2org/calendars'."
  (interactive)
  (-map #'ical2org/import-calendar ical2org/calendars))

(provide 'ical2org)
;;; ical2org.el ends here
