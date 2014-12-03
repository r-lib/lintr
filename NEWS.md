# lintr 0.2.0.9000 #

* use `globalenv()` instead of `baseenv()` for default parent environment so
  that `methods` will be included.
* do not check object usage if eval fails.  Fixes (#24, reported by @fabian-s)
* trailing_whitespace_linter was reporting the incorrect line number

# lintr 0.2.0 #

* Initial release
