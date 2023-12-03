# print.lint, print.lints support optional message wrapping : width = 10

    Code
      print(lint)
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
      print(lint)
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
      print(lint)
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
      print(lint)
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

# format.lint, format.lints support optional message wrapping : width = 10

    Code
      format(lint)
    Output
      [1] "<text>:1:1:\n    warning:\n    [test_linter]\n    The\n    quick\n    brown\n    fox\n    jumps\n    over\n    the\n    lazy\n    dog.\na\n^\n"

---

    Code
      format(lints)
    Output
      [1] "<text>:1:1:\n    warning:\n    [test_linter]\n    The\n    quick\n    brown\n    fox\n    jumps\n    over\n    the\n    lazy\n    dog.\na\n^\n"

# format.lint, format.lints support optional message wrapping : width = 20

    Code
      format(lint)
    Output
      [1] "<text>:1:1:\n    warning:\n    [test_linter]\n    The quick brown\n    fox jumps over\n    the lazy dog.\na\n^\n"

---

    Code
      format(lints)
    Output
      [1] "<text>:1:1:\n    warning:\n    [test_linter]\n    The quick brown\n    fox jumps over\n    the lazy dog.\na\n^\n"

# format.lint, format.lints support optional message wrapping : width = 40

    Code
      format(lint)
    Output
      [1] "<text>:1:1: warning: [test_linter] The\n    quick brown fox jumps over the lazy\n    dog.\na\n^\n"

---

    Code
      format(lints)
    Output
      [1] "<text>:1:1: warning: [test_linter] The\n    quick brown fox jumps over the lazy\n    dog.\na\n^\n"

# format.lint, format.lints support optional message wrapping : width = 80

    Code
      format(lint)
    Output
      [1] "<text>:1:1: warning: [test_linter] The quick brown fox jumps over the lazy dog.\na\n^\n"

---

    Code
      format(lints)
    Output
      [1] "<text>:1:1: warning: [test_linter] The quick brown fox jumps over the lazy dog.\na\n^\n"

