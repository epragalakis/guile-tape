(add-to-load-path (getcwd))
(use-modules (tape))

(describe "Outer describe block"
  (it "should pass" (expect #t (toBeTruthy)))
  (describe "Inner describe block"
   (it "should pass" (expect #t (toBeTruthy)))
   (it "should pass" (expect #t (toBeTruthy)))))

(describe "calls a test in an 'it' or a 'test' block"
  (it "should call a test in an 'it' block"
      (expect #t (toBeTruthy)))

  (test "should call a test in an 'test' block"
      (expect #t (toBeTruthy))))

(tests "'tests' as an alternative syntax to 'describe'"
  (test "should pass" (expect #t (toBeTruthy))))

(describe "toBe matcher"
  (it "should compare equal numbers"
      (let ((number4 4))
      (expect (+ 2 2) (toBe number4))))

  (it "should match boolean values"
      (expect #t (toBe #t)))

  (it "should compare strings"
      (expect "hello" (toBe "hello")))

  (it-fail "should fail comparing unequal numbers"
      (expect (+ 2 2) (toBe 5)))

  (it-fail "should fail comparing mismatched boolean values"
      (expect #t (toBe #f))))

(describe "toEqual matcher"
  (it "should correctly compare equivalent lists"
      (expect '(1 2 3) (toEqual (list 1 2 3))))

  (it "should correctly compare nested structures"
      (expect '((a 1) (b 2)) (toEqual '((a 1) (b 2)))))

  (it "should correctly compare identical strings"
      (expect "hello world" (toEqual "hello world")))

  (it-fail "should fail comparing different lists"
      (expect '(1 2 3) (toEqual (list 1 2 4))))

  (it-fail "should fail comparing different strings"
      (expect "hello" (toEqual "world"))))

(describe "toBeTruthy matcher"
  (it "should identify True boolean as true"
      (expect #t (toBeTruthy)))

  (it "should identify a list as true"
      (expect '(1 2 3) (toBeTruthy)))

  (it-fail "should fail identifying False boolean as true"
      (expect #f (toBeTruthy))))

(describe "toBeFalsy matcher"
  (it "should identify False boolean as false"
      (expect #f (toBeFalsy)))

  (it-fail "should fail identifying True boolean as false"
      (expect #t (toBeFalsy))))

(describe "toBeGreaterThan matcher"
  (it "should pass when the expected value is greater than the actual"
      (expect 15 (toBeGreaterThan 10)))

  (it-fail "should fail when the values are equal"
      (expect 5 (toBeGreaterThan 5)))

  (it-fail "should fail when the actual value is greater than the expected"
      (expect 5 (toBeGreaterThan 10))))

(describe "toBeLessThan matcher"
  (it "should pass when the expected value is lower than the actual"
      (expect 5 (toBeLessThan 10)))

  (it-fail "should fail when the values are equal"
      (expect 5 (toBeLessThan 5)))

  (it-fail "should fail when the expected value is greater than the actual"
      (expect 10 (toBeLessThan 5))))

(describe "toBeCloseTo matcher"
  (it "should pass if the number is kinda close to the expected one"
    (let ((someNumber (/ 22 7)))
      (expect someNumber (toBeCloseTo 3.14 0.01))))

  (it "should pass if the number is exactly the same"
      (expect 3.14 (toBeCloseTo 3.14 0.001)))

  (it-fail "should fail if the number is just too far off"
      (expect 3.14 (toBeCloseTo 3.15 0.001))))

(describe "stringToMatch matcher"
  (it "should test that the string matches"
      (expect "hello world" (stringToMatch "hello world")))

  (it "should test when the string contains the expected substring"
      (expect "world" (stringToMatch "hello world")))
 
  (it-fail "should fail when strings do not match"
      (expect "goodbye" (stringToMatch "hello world"))))

(describe "listToContain matcher"
  (it "should pass when the item is in the list"
      (expect 'b (listToContain '(a b c))))

  (it-fail "should fail when the item is not in the list"
      (expect 'd (listToContain '(a b c)))))

(describe "toHaveLength matcher"
  (it "should pass when the list has the expected length"
      (expect '(1 2 3) (toHaveLength 3)))

  (it "should pass when the string has the expected number of characters"
      (expect "hello" (toHaveLength 5)))

  (it-fail "should fail when the list is shorter or longer than expected"
      (expect '(1 2) (toHaveLength 4)))

  (it-fail "should fail when the string length doesn't match the expected"
      (expect "hello" (toHaveLength 1))))

(describe "should expect for the test to fail"
  (it-fail "should fail using the it-fail syntax (this test will pass)" (expect #f (toBeTruthy)))
  (test-fail "should fail using the test-fail syntax (this test will fail)" (expect #t (toBeTruthy))))

(describe "toThrow (currently buggy)"
  (it-fail "should test a throwing error" (expect #f (toThrow))))
           ;(expect (lambda () (car '())))

  ;(it-fail "should fail when there isnt a throwing error"
      ;(expect (lambda () (+ 1 2)))
        ;(toThrow)))

(describe "skips tests that are marked as 'skip'"
  (it-skip "should skip this test (it-skip syntax)" (expect #f (toBeTruthy)))
  (test-skip "should skip this test (test-skip syntax)" (expect #t (toBeTruthy)))
  (it "should not skip this test" (expect #t (toBeTruthy))))

(describe "skips tests that are marked as 'todo'"
  (it-todo "should skip this test (it-todo syntax)")
  (it "should not skip this test" (expect #t (toBeTruthy)))
  (test-todo "should skip this test (test-todo syntax)")
  (it-todo "should skip this test")
  (it "should not skip this test" (expect #t (toBeTruthy))))

(define expectedValue 0)
(describe "only beforeAll"
          (beforeAll (lambda ()
                       (display "  --Before all tests\n")
                       (set! expectedValue "only beforeAll")))

          (it "should match expected value" (expect expectedValue (toBe "only beforeAll")))
          (it "should match boolean values" (expect #t (toBe #t))))

(describe "beforeAll and afterAll"
  (beforeAll (lambda ()
              (display "  --Before all tests\n")
              (set! expectedValue "beforeAll and afterAll")))

  (afterAll (lambda ()
              (display "  --After all tests\n")
              (set! expectedValue 0)))

  (it "should match expected value" (expect expectedValue (toBe "beforeAll and afterAll")))
  (it "should match boolean values" (expect #t (toBe #t))))

(set! expectedValue "only afterAll")
(describe "only afterAll"
  (afterAll (lambda () (display "  --After all tests\n")))

  (it "should match expected value" (expect expectedValue (toBe "only afterAll")))
  (it "should match boolean values" (expect #t (toBe #t))))

(describe "beforeEach and afterEach hooks"
  (beforeEach (lambda ()
    (display "  --Before each test\n")
    (set! expectedValue "beforeEach and afterEach hooks")))
  (afterEach (lambda ()
    (display "  --After each test\n")
    (set! expectedValue 0)))

  (it "should match expected value (1)"
    (expect expectedValue (toBe "beforeEach and afterEach hooks")))
  (it "should match expected value (2)"
    (expect expectedValue (toBe "beforeEach and afterEach hooks"))))

(describe "only beforeEach"
  (beforeEach (lambda ()
    (display "  --Before each test\n")
    (set! expectedValue "only beforeEach")))

  (it "should match expected value (1)"
    (expect expectedValue (toBe "only beforeEach")))
  (it "should match expected value (2)"
    (expect expectedValue (toBe "only beforeEach"))))

(describe "only afterEach"
  (beforeEach (lambda ()
    (display "  --After each test\n")
    (set! expectedValue "only afterEach")))

  (it "should match expected value (1)"
    (expect expectedValue (toBe "only afterEach")))
  (it "should match expected value (2)"
    (expect expectedValue (toBe "only afterEach"))))

(describe "no hooks"
  ;; This test is needed to ensure that the state from the previous hooks is not persisted
  (it "should match expected value"
    (expect expectedValue (toBe "only afterEach"))))
