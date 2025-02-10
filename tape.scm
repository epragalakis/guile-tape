(define-module (tape)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 format)
  #:export (describe
            it
            expect
            listToContain 
            stringToMatch
            toBe
            toBeCloseTo
            toBeFalsy
            toBeGreaterThan
            toBeLessThan
            toBeTruthy
            toEqual
            toHaveLength
            toThrow))

; ANSI color codes
(define green "\x1b[32m")
(define red "\x1b[31m")
(define yellow "\x1b[33m")
(define reset "\x1b[0m")

(define-syntax-rule (describe name body ...)
  (begin
    (test-begin name)
    body ...
    (test-end name)))

(define current-test-name #f) ; TODO avoid defining a global var

(define-syntax-rule (it description test-body)
  (let ((old-test-name current-test-name))
    (set! current-test-name description)
    (let ((result test-body))
      (set! current-test-name old-test-name)
      result)))

(define-syntax-rule (expect actual matcher) 
  (matcher actual (string-append "it" " " current-test-name)))

; matchers 
(define-syntax-rule (toBe actual)
  (lambda (expected test-name)
    (test-eqv test-name expected actual)))

(define-syntax-rule (toEqual actual)
  (lambda (expected test-name)
    (test-equal test-name expected actual)))

(define-syntax-rule (toBeTruthy)
  (lambda (value test-name)
    (test-assert test-name value)))

(define-syntax-rule (toBeFalsy)
  (lambda (value test-name)
    (test-assert test-name (not value))))

(define-syntax-rule (toBeGreaterThan actual)
  (lambda (expected test-name)
    (test-assert test-name (> expected actual))))

(define-syntax-rule (toBeLessThan actual)
  (lambda (expected test-name)
    (test-assert test-name (< expected actual))))

(define-syntax-rule (toBeCloseTo actual delta)
  (lambda (expected test-name)
    (test-approximate test-name expected actual delta)))

(define-syntax-rule (stringToMatch actual)
  ;TODO: regex support
  (lambda (expected test-name)
    (test-assert test-name (string-contains actual expected))))

(define-syntax-rule (listToContain actual)
  ;TODO: find a better name
  (lambda (expected test-name)
    (test-assert test-name
      (member expected actual))))

(define-syntax-rule (toHaveLength actual)
  (lambda (item test-name)
    (let ((actual-length 
      (if (string? item) 
        (string-length item)
        (length item))))
      (test-eqv test-name actual actual-length))))

(define-syntax-rule (toThrow)
  ;TODO completely broken
  (lambda (expression test-name)
    (test-error test-name #t expression)))

(define (test-result runner)
  (let* ((test-name (test-runner-test-name runner))
         (expected-error (test-result-ref runner 'expected-error))
         (result-kind (test-result-kind runner))
         (pass? (eq? result-kind 'pass))
         (color (if pass? green red))
         (status (if pass? "PASS" "FAIL")))
    (format #t "~a~a: ~a~a\n" color status test-name reset)
    (unless pass?
        (let ((expected (or (assq-ref (test-result-alist runner) 'expected-value) #t))
            (actual (or (assq-ref (test-result-alist runner) 'actual-value) #f)))
        (format #t "  ~aExpected: ~s~a\n" green expected reset)
        (format #t "  ~aReceived: ~s~a\n" red actual reset)))))


(define (tests-summary runner)
  (let* ((passes (test-runner-pass-count runner))
         (failures (test-runner-fail-count runner))
         (total (+ passes failures)))
    (format #t "\n~a# of expected passes      ~a~a\n" green passes reset)
    (format #t "~a# of unexpected failures  ~a~a\n" red failures reset)
    (format #t "~a# of total tests          ~a~a\n" yellow total reset)))

; disable log files generation
(set! test-log-to-file #f)

;TODO: the test runner factory should be focused on test running, not timing, find a more idiomatic solution

(test-runner-factory 
 (lambda ()
   (let ((runner (test-runner-simple))
         (start-time (current-time time-monotonic)))
     (test-runner-on-test-end! runner
       (lambda (runner)
         (when (test-runner-test-name runner)
           (test-result runner))))
     (test-runner-on-final! runner
        (lambda (runner)
          (let* ((end-time (current-time time-monotonic))
                 (duration (time-difference end-time start-time))
                 (seconds (+ (time-second duration)
                             (/ (time-nanosecond duration) 1e9))))
            (tests-summary runner)
            (format #t "Total time: ~,6f seconds\n~%" seconds))))
      runner)))

