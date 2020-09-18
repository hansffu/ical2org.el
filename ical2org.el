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
  (buffer-substring (line-beginning-position) (line-end-position)) )

(defun ical2org/read-value ()
  "Read what comes after '=' on current line"
  (save-excursion
    (line-beginning-position)
    (search-forward "=")
    (buffer-substring (point)(line-end-position))
    )
  )

(defun ical2org/parse-event ()
  "Parse event until END:CALENDAR."
  (let* ((lines  '())
         (current-line (ical2org/read-line)))
    (while (not (string-prefix-p "END" current-line))
      (setq current-line (ical2org/read-line))

      (when (string-prefix-p "SUMMARY" current-line)
        (add-to-list 'lines (cons 'summary (ical2org/read-value))))

      (forward-line)
      )
    (cdr (assoc 'summary lines))
    )
  )

(defun ical2org/callback (calendar callback)
  (message "callback %s" (ical2org/calendar-name calendar))
  ;; (let ((ical-buffer (current-buffer)))
  ;;   (switch-to-buffer ical-buffer)
  ;;   )
  (with-current-buffer
      (current-buffer)
    (search-forward "BEGIN:VEVENT")
    (forward-line)

    ;; (message (buffer-substring (point) (line-end-position)))
    (message "hello")
    (message (ical2org/parse-event))
    (message (ical2org/parse-event))
    (message (ical2org/parse-event))
    )
  ;; (switch-to-buffer (current-buffer))
  )

(defun ical2org/import ()
  "Fetch calendars defined in `ical2org/calendars'."
  (interactive)
  (-map #'ical2org/import-calendar ical2org/calendars))

(provide 'ical2org)
;;; ical2org.el ends here
