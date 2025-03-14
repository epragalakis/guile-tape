(add-to-load-path (getcwd))
(use-modules (tape))

(describe "calls a test in an 'it' or a 'test' block"
  (it "should call a test in an 'it' block"
      (expect #t (toBeTruthy)))

  (test "should call a test in an 'test' block"
      (expect #t (toBeTruthy))))

(tests "'tests' as an alternative syntax to 'describe'"
  (test "should pass" (expect #t (toBeTruthy))))

(describe "toBe matcher"
  (it "should compare equal numbers (pass)"
      (let ((number4 4))
      (expect (+ 2 2) (toBe number4))))

  (it "should match boolean values (pass)"
      (expect #t (toBe #t)))

  (it "should compare strings (pass)" 
      (expect "hello" (toBe "hello")))

  (it "should compare unequal numbers (fail)"
      (expect (+ 2 2) (toBe 5)))

  (it "should compare mismatched boolean values (fail)" 
      (expect #t (toBe #f))))

(describe "toEqual matcher"
  (it "should correctly compare equivalent lists (pass)" 
      (expect '(1 2 3) (toEqual (list 1 2 3))))

  (it "should correctly compare nested structures (pass)" 
      (expect '((a 1) (b 2)) (toEqual '((a 1) (b 2)))))

  (it "should correctly compare identical strings (pass)"
      (expect "hello world" (toEqual "hello world")))

  (it "should compare different lists (fail)" 
      (expect '(1 2 3) (toEqual (list 1 2 4))))

  (it "should compare different strings (fail)"
      (expect "hello" (toEqual "world"))))

(describe "toBeTruthy matcher"
  (it "should identify True boolean as true (pass)"
      (expect #t (toBeTruthy)))

  (it "should identify a list as true (pass)"
      (expect '(1 2 3) (toBeTruthy)))

  (it "should identify False boolean as true (fail)"
      (expect #f (toBeTruthy))))

(describe "toBeFalsy matcher"
  (it "should identify False boolean as false (pass)"
      (expect #f (toBeFalsy)))

  (it "should identify True boolean as false (fail)" 
      (expect #t (toBeFalsy))))

(describe "toBeGreaterThan matcher"
  (it "should pass when the expected value is greater than the actual (pass)"
      (expect 15 (toBeGreaterThan 10)))

  (it "should fail when the values are equal (fail)"
      (expect 5 (toBeGreaterThan 5)))

  (it "should fail when the actual value is greater than the expected (fail)"
      (expect 5 (toBeGreaterThan 10))))

(describe "toBeLessThan matcher"
  (it "should pass when the expected value is lower than the actual (pass)"
      (expect 5 (toBeLessThan 10)))

  (it "should fail when the values are equal (fail)"
      (expect 5 (toBeLessThan 5)))

  (it "should fail when the expected value is greater than the actual (fail)"
      (expect 10 (toBeLessThan 5))))

(describe "toBeCloseTo matcher"
  (it "should pass if the number is kinda close to the expected one (pass)" 
    (let ((someNumber (/ 22 7)))
      (expect someNumber (toBeCloseTo 3.14 0.01))))

  (it "should pass if the number is exactly the same (pass)"
      (expect 3.14 (toBeCloseTo 3.14 0.001)))

  (it "should fail if the number is just too far off (fail)" 
      (expect 3.14 (toBeCloseTo 3.15 0.001))))

(describe "stringToMatch matcher"
  (it "should test that the string matches (pass)" 
      (expect "hello world" (stringToMatch "hello world")))

  (it "should test when the string contains the expected substring (pass)" 
      (expect "world" (stringToMatch "hello world")))
 
  (it "should test when strings do not match (fail)" 
      (expect "goodbye" (stringToMatch "hello world"))))

(describe "listToContain matcher"
  (it "should pass when the item is in the list (pass)" 
      (expect 'b (listToContain '(a b c))))

  (it "should fail when the item is not in the list (fail)" 
      (expect 'd (listToContain '(a b c)))))

(describe "toHaveLength matcher"
  (it "should pass when the list has the expected length (pass)" 
      (expect '(1 2 3) (toHaveLength 3)))

  (it "should pass when the string has the expected number of characters (pass)" 
      (expect "hello" (toHaveLength 5)))

  (it "should fail when the list is shorter or longer than expected (fail)"
      (expect '(1 2) (toHaveLength 4)))

  (it "should fail when the string length doesn't match the expected (fail)" 
      (expect "hello" (toHaveLength 1))))

(describe "toThrow (currently buggy)"
  (it "should test a throwing error (pass)"
      ;(expect (lambda () (car '())))
      (expect #f
        (toThrow))))

  ;(it "should fail when there isnt a throwing error (fail)" 
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
                       (display "--Before all tests\n")
                       (set! expectedValue "only beforeAll")))

          (it "should match expected value (pass)" (expect expectedValue (toBe "only beforeAll")))
          (it "should match boolean values (pass)" (expect #t (toBe #t))))

(describe "beforeAll and afterAll"
  (beforeAll (lambda ()
              (display "--Before all tests\n")
              (set! expectedValue "beforeAll and afterAll")))

  (afterAll (lambda ()
              (display "--After all tests\n")
              (set! expectedValue 0)))

  (it "should match expected value (pass)" (expect expectedValue (toBe "beforeAll and afterAll")))
  (it "should match boolean values (pass)" (expect #t (toBe #t))))

(set! expectedValue "only afterAll")
(describe "only afterAll"
  (afterAll (lambda () (display "--After all tests\n")))

  (it "should match expected value (pass)" (expect expectedValue (toBe "only afterAll")))
  (it "should match boolean values (pass)" (expect #t (toBe #t))))

(describe "beforeEach and afterEach hooks"
  (beforeEach (lambda ()
    (display "--Before each test\n")
    (set! expectedValue "beforeEach and afterEach hooks")))
  (afterEach (lambda ()
    (display "--After each test\n")
    (set! expectedValue 0)))

  (it "should match expected value (1) (pass)"
    (expect expectedValue (toBe "beforeEach and afterEach hooks")))
  (it "should match expected value (2) (pass)"
    (expect expectedValue (toBe "beforeEach and afterEach hooks"))))

(describe "only beforeEach"
  (beforeEach (lambda ()
    (display "--Before each test\n")
    (set! expectedValue "only beforeEach")))

  (it "should match expected value (1) (pass)"
    (expect expectedValue (toBe "only beforeEach")))
  (it "should match expected value (2) (pass)"
    (expect expectedValue (toBe "only beforeEach"))))

(describe "only afterEach"
  (beforeEach (lambda ()
    (display "--After each test\n")
    (set! expectedValue "only afterEach")))

  (it "should match expected value (1) (pass)"
    (expect expectedValue (toBe "only afterEach")))
  (it "should match expected value (2) (pass)"
    (expect expectedValue (toBe "only afterEach"))))

(describe "no hooks"
  ;; This test is needed to ensure that the state from the previous hooks is not persisted
  (it "should match expected value (1) (pass)"
    (expect expectedValue (toBe "only afterEach"))))
