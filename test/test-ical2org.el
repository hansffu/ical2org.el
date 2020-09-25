;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-

(require 'buttercup)
(require 'ical2org)

(describe "format-timestamp"
  (it "It should return time range when start and end date is same day"
    (expect
     (ical2org/format-timestamp
      "20200924T140000Z"
      "20200924T141500Z"
      nil
      )
     :to-equal "<2020-09-24 Thu 16:00-16:15>")
    )
  (it "It should return date range when start and end date is different days"
    (expect
     (ical2org/format-timestamp
      "20200924T140000Z"
      "20200925T141500Z"
      nil
      )
     :to-equal "<2020-09-24 Thu 16:00>--<2020-09-25 Fri 16:15>")
    )
  (it "It should add +1w if frequency is weekly"
    (expect
     (ical2org/format-timestamp
      "20200924T140000Z"
      "20200924T141500Z"
      ";FREQ=WEEKLY"
      )
     :to-equal "<2020-09-24 Thu 16:00-16:15 +1w>")
    )
  (it "should repeat every day defined in byday"
    ;; (expect
     (ical2org/format-timestamp
      "20200924T140000Z"
      "20200924T141500Z"
      ";FREQ=WEEKLY;BYDAY=MO,WE,TH,SU"
      )
     ;; :to-equal
     ;; "<2020-09-24 Thu 16:00-16:15 +1w><2020-09-27 Sun 16:00-16:15 +1w><2020-09-28 Mon 16:00-16:15 +1w><2020-09-29 Tue 16:00-16:15 +1w>")
    )
  )
