(load "check.scm")
(check-set-mode! 'report-failed)

(check (fizzbuzz 1) => 1)

(check-report)
