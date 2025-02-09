# Tape

Producing test harness for Guile

![tape](https://web.archive.org/web/20170612184731if_/http://substack.net/images/tape_drive.png)

_The above image and the name came from the popular [Tape javascript library](https://github.com/tape-testing/tape)_

Disclaimer:

This was written as a practice of learning Guile.

The code is messy and there are bugs.

Also, the module's name is **Tape** it is not following the [TAP](https://testanything.org/) spec.

In the future it might evolve further, to better serve my TDD needs.

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
%%%% Starting test toBe
PASS: tests that a number is equal to four
FAIL: tests that a string is equal to 'world'
  Expected: "world"
  Received: "hello"

# of expected passes      1
# of unexpected failures  1
# of total tests          2
Total time: 0.000080 seconds
```

## Install

TODO as its not yet published anywhere

## Usage

- Import tape in your test file: `use-modules ('tape')`
- Run the test: `$ guile tests/my-test.scm`

- For all supported matchers check: `./tape.scm`
- For more use cases check: `./test.scm`

## License

This project is licensed under the [LGPL License](LICENSE.md).
