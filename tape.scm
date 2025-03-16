(define-module (tape)
  #:use-module (srfi srfi-64)
  #:use-module ((srfi srfi-64)
    #:select (test-skip)
    #:renamer (symbol-prefix-proc 'srfi:))
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 format)
  #:export (describe
            tests
            test
            test-fail
            test-skip
            test-todo
            it
            it-fail
            it-skip
            it-todo
            expect
            beforeAll
            beforeEach
            afterAll
            afterEach
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

;; ANSI color codes
(define green "\x1b[32m")
(define green-bg "\x1b[48;5;28m")
(define red "\x1b[31m")
(define red-bg "\x1b[48;5;88m")
(define yellow "\x1b[33m")
(define yellow-bg "\x1b[48;5;100m")
(define reset "\x1b[0m")

(define before-all-hook #f)
(define after-all-hook #f)
(define before-each-hook #f)
(define after-each-hook #f)

(define-syntax-rule (beforeAll body ...)
  (set! before-all-hook (lambda () body ...)))

(define-syntax-rule (afterAll body ...)
  (set! after-all-hook (lambda () body ...)))

(define-syntax-rule (beforeEach body ...)
  (set! before-each-hook (lambda () body ...)))

(define-syntax-rule (afterEach body ...)
  (set! after-each-hook (lambda () body ...)))

;; TODO any alternative from having global vars? (alists?)
(define current-test-name #f)
(define current-before-each (make-parameter (lambda () #f)))
(define current-after-each (make-parameter (lambda () #f)))

;;; Define an individual test case
(define-syntax it
  (syntax-rules ()
    ((_ description body ...)
     (begin
       ((current-before-each))
       (let ((old-test-name current-test-name))
         (set! current-test-name description)
         body ...
         (set! current-test-name old-test-name))
       ((current-after-each))))))

;;; Alias of "it"
(define-syntax test
  (identifier-syntax it))

;;; Define an individual test case that is expected to fail
(define-syntax it-fail
  (syntax-rules ()
    ((_ description body ...)
     (begin
      (let ((old-test-name current-test-name))
        (set! current-test-name description)
        (test-expect-fail 1)
        body ...
        (set! current-test-name old-test-name))
       ))))

;;; Alias of "it-fail"
(define-syntax test-fail
  (identifier-syntax it-fail))

;;; Define an individual test case that is going to be skipped
;;; TODO skip tests are still evaluated? check the actual and expected value
(define-syntax it-skip
  (syntax-rules ()
    ((_ description body ...)
     (begin
      (let ((old-test-name current-test-name))
        (set! current-test-name description)
        (srfi:test-skip 1) ;; skips the next test
        body ... ;; runs the next test
        (set! current-test-name old-test-name))
       ))))

;;; Alias of "it-skip"
(define-syntax test-skip
  (identifier-syntax it-skip))

;;; Define an individual test case that is in a "TODO" state and it is going to be skipped
(define-syntax it-todo
  (syntax-rules ()
    ((_ description )
     (begin
        (test-eqv (string-append "it " description) "&&TODO&&" "&&TODO&&"))))) ;; I know..

;;; Alias of "it-todo"
(define-syntax test-todo
  (identifier-syntax it-todo))

;;; Group a set of tests
(define-syntax describe
  (syntax-rules (beforeAll afterAll beforeEach afterEach)
    ((_ name (beforeAll before-all) (afterAll after-all) (beforeEach before-each) (afterEach after-each) body ...)
     (let ((run-before-each before-each)
           (run-after-each after-each))
       (begin
         (test-begin name)
         (before-all)
         (parameterize ((current-before-each run-before-each)
                        (current-after-each run-after-each))
           body ...)
         (after-all)
         (test-end name))))

    ;; case with only beforeAll and afterAll hooks
    ((_ name (beforeAll before-all) (afterAll after-all) body ...)
     (begin
       (test-begin name)
       (before-all)
       body ...
       (after-all)
       (test-end name)))

    ;; case with only beforeAll hook
    ((_ name (beforeAll before-all) body ...)
     (begin
       (test-begin name)
       (before-all)
       body ...
       (test-end name)))

    ;; case with only afterAll hook
    ((_ name (afterAll after-all) body ...)
     (begin
       (test-begin name)
       body ...
       (after-all)
       (test-end name)))

    ;; Case with only beforeEach and afterEach hooks
    ((_ name (beforeEach before-each) (afterEach after-each) body ...)
     (let ((run-before-each before-each)
           (run-after-each after-each))
       (test-begin name)
       (parameterize ((current-before-each run-before-each)
                      (current-after-each run-after-each))
         body ...)
       (test-end name)))

    ;; Case with only beforeEach
    ((_ name (beforeEach before-each) body ...)
     (let ((run-before-each before-each))
       (test-begin name)
       (parameterize ((current-before-each run-before-each))
         body ...)
       (test-end name)))

    ;; Case with only afterEach
    ((_ name (afterEach after-each) body ...)
     (let ((run-after-each after-each))
       (test-begin name)
       (parameterize ((current-after-each run-after-each))
         body ...)
       (test-end name)))

    ;; Case without any hooks at all
    ((_ name body ...)
     (begin
       (test-begin name)
       body ...
       (test-end name)))))

;;; Alias of "describe"
(define-syntax tests
  (identifier-syntax describe))

;;; Assert that ACTUAL satisfies a given MATCHER
(define-syntax-rule (expect actual matcher) 
  ;; TODO account also for 'test' instead of 'it'
  (matcher actual (string-append "it" " " current-test-name)))

;;; Matchers

;;; Check if ACTUAL is equivalent to an expected value using "test-eqv"
(define-syntax-rule (toBe actual)
  (lambda (expected test-name)
    (test-eqv test-name expected actual)))

;;; Check if ACTUAL is equal to an expected  value using "test-equal"
(define-syntax-rule (toEqual actual)
  (lambda (expected test-name)
    (test-equal test-name expected actual)))

;;; Check if a value is truthy using "test-assert"
(define-syntax-rule (toBeTruthy)
  (lambda (value test-name)
    (test-assert test-name value)))

;;; Check if a value is falsy using "test-assert"
(define-syntax-rule (toBeFalsy)
  (lambda (value test-name)
    (test-assert test-name (not value))))

;;; Check if ACTUAL is greater than expected value, using "test-assert"
(define-syntax-rule (toBeGreaterThan actual)
  (lambda (expected test-name)
    (test-assert test-name (> expected actual))))

;;; Check if ACTUAL is less than an expected value, using "test-assert"
(define-syntax-rule (toBeLessThan actual)
  (lambda (expected test-name)
    (test-assert test-name (< expected actual))))

;;; Check if ACTUAL is approximately equal to an expected value within DELTA, using "test-approximate"
(define-syntax-rule (toBeCloseTo actual delta)
  (lambda (expected test-name)
    (test-approximate test-name expected actual delta)))

;;; Check if ACTUAL string contains an expected substring, using "test-assert"
;;; TODO: regex support
(define-syntax-rule (stringToMatch actual)
  (lambda (expected test-name)
    (test-assert test-name (string-contains actual expected))))

;;; Check if Actual list contains an expected element, using "test-assert"
(define-syntax-rule (listToContain actual)
  ;;TODO: find a better name
  (lambda (expected test-name)
    (test-assert test-name
      (member expected actual))))

;;; Check if the length of ACTUAL (string or list) equals to expected length, using "test-eqv"
(define-syntax-rule (toHaveLength actual)
  (lambda (item test-name)
    (let ((actual-length 
      (if (string? item) 
        (string-length item)
        (length item))))
      (test-eqv test-name actual actual-length))))

;;; Check if EXPRESSION throws an error during evaluation, using "test-error"
;;; TODO: currently it accepts a lambda that returns a throwing error (need to change it so it is more flexible)
(define-syntax-rule (toThrow)
  (lambda (expression test-name)
    (test-error test-name #t ((expression)))))

;;; Test result
(define todo-count 0)
(define (test-result runner)
  (let* ((test-name (test-runner-test-name runner))
         (expected-error (test-result-ref runner 'expected-error))
         (result-kind (test-result-kind runner))
         (pass? (eq? result-kind 'pass))
         (skip? (eq? result-kind 'skip))
         (xpass? (eq? result-kind 'xpass))
         (xfail? (eq? result-kind 'xfail))
         (expected (or (assq-ref (test-result-alist runner) 'expected-value) #t))
         (actual (or (assq-ref (test-result-alist runner) 'actual-value) #f))
         (todo? (and (string? actual) (string=? actual "&&TODO&&"))) ;; I know..
         (color (cond
          (todo? yellow)
          (skip? yellow)
          (pass? green)
          (xpass? red)
          (xfail? green)
          (else red)))
         (color-bg (cond
          (todo? yellow-bg)
          (skip? yellow-bg)
          (pass? green-bg)
          (xpass? red-bg)
          (xfail? green-bg)
          (else red-bg)))
         (status (cond
          (todo? "TODO")
          (skip? "SKIP")
          (pass? "PASS")
          (xfail? "PASS")
          (xpass? "FAIL")
          (else "FAIL"))))

    (when todo? (set! todo-count (+ todo-count 1))) ;; TODO there should be a better way to do that in the tests-summary

    (format #t "  ~a ~a ~a~a~a\n" color-bg status reset (string-append " " test-name) reset)
    (when (equal? status "FAIL")
      (format #t "    ~aExpected: ~s~a\n" green expected reset)
      (format #t "    ~aReceived: ~s~a\n" red actual reset))))

;;; Test summary
(define (tests-summary runner)
  (let* ((passes (test-runner-pass-count runner))
         (failures (test-runner-fail-count runner))
         (skips (test-runner-skip-count runner))
         (expected-failures (test-runner-xfail-count runner))
         (unexpected-failures (test-runner-xpass-count runner))
         (total (+ passes failures todo-count skips expected-failures unexpected-failures)))
    (newline)
    (display "Summary:")
    (format #t "\n  ~aTests run:               ~a~a\n" yellow (- total todo-count) reset) ;; remove "TODO" tests them from the count as they are part of the "passes" count
    (format #t "  ~aTODO tests:              ~a~a\n" yellow todo-count reset)
    (format #t "  ~aSkipped tests:           ~a~a\n" yellow skips reset)
    (format #t "  ~aExpected tests passed:   ~a~a\n" green (+ (- passes todo-count)) reset) ;; currently "TODO" tests count as "PASS"
    (format #t "  ~aExpected tests failed:   ~a~a\n" green expected-failures reset)
    (format #t "  ~aUnexpected tests failed: ~a~a\n" red (+ failures unexpected-failures) reset))
    (set! todo-count 0)
  )

;; disable log files generation
;; I dont see a point of having log files as in all the cases I've seen the same information is already in the terminal.
;; If I'm missing something I can add a toggle here.
(set! test-log-to-file #f)

;;TODO: the test runner factory should be focused on test running, not timing, find a more idiomatic solution
(test-runner-factory
 (lambda ()
   (let ((runner (test-runner-simple))
         (start-time (current-time time-monotonic)))
     (test-runner-on-group-begin! runner
       (lambda (runner name count)
         (display name)
         (newline)))
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
            (format #t "  Total time: ~,6f seconds\n~%" seconds))))
      runner)))
