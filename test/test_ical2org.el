;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
(require 'buttercup)
(require 'ical2org)

(describe "format-timestamp"
  (it "It should parse event"
    (expect
     (ical2org/format-timestamp
      "20200924T150000Z"
      "20200924T151500Z"
      nil
      )
     :to-be "<2020-09-24 Wed 15:00-15:15>")
    )
  )
