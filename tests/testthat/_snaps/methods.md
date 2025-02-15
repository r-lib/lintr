# print.lint, print.lints support optional message wrapping : width = 10

    Code
      print(lints, width = width)
    Output
      <text>:1:1:
          warning:
          [test_linter]
          The
          quick
          brown
          fox
          jumps
          over
          the
          lazy
          dog.
      a
      ^

---

    Code
      print(lints)
    Output
      <text>:1:1:
          warning:
          [test_linter]
          The
          quick
          brown
          fox
          jumps
          over
          the
          lazy
          dog.
      a
      ^

# print.lint, print.lints support optional message wrapping : width = 20

    Code
      print(lints, width = width)
    Output
      <text>:1:1:
          warning:
          [test_linter]
          The quick brown
          fox jumps over
          the lazy dog.
      a
      ^

---

    Code
      print(lints)
    Output
      <text>:1:1:
          warning:
          [test_linter]
          The quick brown
          fox jumps over
          the lazy dog.
      a
      ^

# print.lint, print.lints support optional message wrapping : width = 40

    Code
      print(lints, width = width)
    Output
      <text>:1:1: warning: [test_linter] The
          quick brown fox jumps over the lazy
          dog.
      a
      ^

---

    Code
      print(lints)
    Output
      <text>:1:1: warning: [test_linter] The
          quick brown fox jumps over the lazy
          dog.
      a
      ^

# print.lint, print.lints support optional message wrapping : width = 80

    Code
      print(lints, width = width)
    Output
      <text>:1:1: warning: [test_linter] The quick brown fox jumps over the lazy dog.
      a
      ^

---

    Code
      print(lints)
    Output
      <text>:1:1: warning: [test_linter] The quick brown fox jumps over the lazy dog.
      a
      ^

