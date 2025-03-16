# Tape

Producing test harness for Guile

![tape](https://web.archive.org/web/20170612184731if_/http://substack.net/images/tape_drive.png)

_The above image and the name came from the popular [Tape javascript library](https://github.com/tape-testing/tape)_

Disclaimer:

This was written as a practice of learning Guile.

The code is messy and there are bugs.

Curently, Tape is not following the [TAP](https://testanything.org/) specification (but it probably will in the future).

## Example

```scheme
(use-modules (tape))

(describe "toBe"
  (it "tests that a number is equal to four"
      (let ((number4 4))
      (expect (+ 2 2) (toBe number4))))

  (it "tests that a string is equal to 'world'"
      (expect "hello" (toBe "world"))))
```

_the following output is actually coloured_

```bash
toBe
  PASS: tests that a number is equal to four
  FAIL: tests that a string is equal to 'world'
    Expected: "world"
    Received: "hello"

Summary:
  Tests run:               2
  TODO tests:              0
  Skipped tests:           0
  Expected tests passed:   1
  Expected tests failed:   1
  Unexpected tests failed: 0
  Total time: 0.000071 seconds
```

## Install

It is not yet published anywhere.

## Usage

- Import tape in your test file: `use-modules ('tape')`
- Run the test: `$ guile tests/my-test.scm`

### Test wrappers

All tests need to be wrapped in a `describe` block (or in a `tests` block as an alias of `describe`).

You can also nest multiple `describe` blocks.

### Test callers

You can use:

- `it` to define an individual test case
- `it-fail` to define an individual test case that is expected to fail
- `it-todo` to define a todo test case
- `it-skip` to define a test case that you want to be skipped
- `test` as an alias to `it`
- `test-fail` as an alias to `test-fail`
- `test-skip` as an alias to `test-skip`
- `test-todo` as an alias to `test-todo`

### Hooks

The following combinations of hooks can be used in the given order, after the `describe` block:

- beforeAll, afterAll, beforeEach, afterEach
- beforeAll, afterAll
- beforeAll
- afterAll
- beforeEach afterEach
- beforeEach
- afterEach
- no hooks

Example:

```
(describe "A describe block"
  (beforeEach (lambda () (display "This will run before each test.\n")))
   ...
)
```

_due to pattern matching I decided not to implement all the 64 possible combinations (64 because all the possible combinations + the differences in the call order) with the hope that there is an easier solution to be found_

### Matchers

# Matchers Documentation

This document outlines the matchers available in the testing suite, their functionality, and usage.

---

## Matchers Overview

Matchers are used to compare actual values against expected values in tests.

_ check the `./test.scm` file, to see examples on how to use them_

Here is all the supported matchers:

#### toBe

Checks if `actual` is equivalent to an `expected` value using `test-eqv`.

#### toEqual

Checks if `actual` is equal to an `expected` value using `test-equal`.

#### toBeTruthy

Checks if a value is truthy using `test-assert`.

#### toBeFalsy

Checks if a value is falsy using `test-assert`.

#### toBeGreaterThan

Checks if `actual` is greater than an `expected` value using `test-assert`.

#### toBeLessThan

Checks if `actual` is less than an `expected` value using `test-assert`.

#### toBeCloseTo

Checks if `actual` is approximately equal to an `expected` value within a given delta using `test-approximate`.

#### stringToMatch

Checks if the `actual` string contains an expected substring using `test-assert`.

#### listToContain

Checks if the `actual` list contains an expected element using `test-assert`.

#### toHaveLength

Checks if the length of the `actual` (string or list) equals the expected length using `test-eqv`.

#### toThrow

Checks if an expression throws an error during evaluation using `test-error`.

> **Note:** Currently accepts a lambda that returns a throwing error.

## License

This project is licensed under the [LGPL License](LICENSE.md).
