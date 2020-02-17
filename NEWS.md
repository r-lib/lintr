# lintr 2.0.1

## New features

* lintr now supports GitHub Actions and will print the lints as warning messages if lints are printed during an action.

## Minor fixes and features

* `single_quote_linter()` no longer causes a print issue when open quote
  appears at a column > than close quote (#457, @jamieRowen)
* `absolute_path_linter()` and `nonportable_path_linter()` now handle
  file-paths that are wrapped with double-quotes (#433, #437, @russHyde).
* `get_source_expressions()` has been changed to handle `expr_or_assign_or_help`
  tokens arising when parsing code containing equals-assignments in R-devel
  (#403, #456, @russHyde).
* `object_usage_linter` has been changed to ensure lint-position is indicated
  relative to the start of the file, rather than the start of a defining
  function (#432, @russHyde).

# lintr 2.0.0

lintr 2.0.0 is a major release, and incorporates development changes since the last major release (1.0.0) in 2016-04-16.

## Deprecated functions
* Deprecated `camel_case_linter()`, `snake_case_linter()` and `multiple_dots_linter()`
  in favor of `object_name_linter()` which enforce the given style: snake_case,
  dotted.case, lowerCamelCalse, UpperCamelCase, alllowercase or ALLUPPERCASE
  (#59, @fangly).
* Deprecated absolute_paths_linter() in favor of the new `absolute_path_linter()`,
  with a lax mode for fewer false positive lints (#199, fangly).

## New linters
* New `cyclocomp_linter()` identifies overly complex functions (#361, @fabian-s)
* New `equals_na_linter()` (#143, #326, @jabranham)
* New `extraction_operator_linter()` checks that the `[[` operator is used when
  extracting a single element from an object, not `[` (subsetting) nor `$`
  (interactive use) (@fangly).
* New `function_left_parentheses_linter()` to check that there is no space between
  a function name and its left parentheses (#204, @jrnold).
* New `implicit_integer_linter()` detects round numbers not declared as integers,
  i.e. 1 instead of 1L (@fangly).
* New `nonportable_path_linter()` identifies paths constructed without file.path()
  (@fangly).
* New `paren_brace_linter()` checks that there is a space between right
  parenthesis and an opening curly brace (@bfgray3, #242).
* New `pipe_continuation_linter()` to ensure there is a space before %>% and newline afterwards (#216).
* New `semicolon_terminator_linter()` reports semicolons at the end a line (#147,
  @gaborcsardi) and between expressions (#181, @fangly).
* New `seq_linter()`, finds `1:length(...)` (and similar) expressions (#155, @gaborcsardi)
* New `todo_comment_linter()` lints TODOs (@fangly).
* New `T_and_F_symbol_linter()` warns when using T and F instead of TRUE and FALSE
  (@fangly).
* New `undesirable_operator_linter()` and `undesirable_function_linter()` lint uses of
  user-specified functions and operators (#48, #149, @fangly).
* New `unneeded_concatenation_linter()` lints uses of c() with a constant or no
  arguments (@fangly).

## New functions for writing linters
* Export `expect_lint()` (#178, #210)
* Export `ids_with_token()` and `with_id()` (#297 @stufield)
* linters can use the XML parse tree as well now, via the
  https://github.com/MangoTheCat/xmlparsedata package (#154, @gaborcsardi)

## New functions for users
* New `lint_dir()` function to lint files under a given directory (@arekbee, #360)
* New `summary.lints()` function to summarize the linter results (#260, #262, @wlandau).
* New `checkstyle_output()` function to output lints to checkstyle XML output (#156, @joshkgold)

## Linter fixes
* `closed_curly_linter()` now allows closing parenthesis or comma after closing curly brace (#167, @Enchufa2)
* `commas_linter()` now handles missing arguments calls properly (#145)
* `commented_code_linter()` now relaxed, it no longer lints comments within roxygen blocks
  and does not consider "-" an R operator to avoid too many false positives.
* `function_left_parentheses_linter()` now allows spaces if a function starts with a left parenthesis (#311)
* `no_tab_linter()` now reports proper line in all cases (#134, @fangly)
* `object_length_linter()` argument `length` now defaults to 30 for consistency (#325 @DragosMG)
* `object_name_linter()` now works when passed multiple styles (#341, @infotroph)
* `object_usage_linter()` has been changed to better detect lexical scoping of global variables (#27, #336, #91, #382)
* `object_usage_linter()` now respects `utils::globalVariables()`, so it can be used to avoid false positive warnings due to non-standard evaluation (#352)
* `object_usage_linter()` now ignores top level calls that contain function definitions (#26).
* `object_linter*()`s now only lint objects declared in the current file
  (#76, #108, #136, #191, #194, #201, @fangly).
* `open_curly_linter()` and `closed_curly_linter()` now do not lint double curly syntax (#388)
* `open_curly_linter()` now allows comments after the curly braces (#188)
* `pipe_continuation_linter()` now behaves better in nested expressions, functions etc. (#366 @russHyde)
* `space_inside_linter()` now reports proper line and column numbers (#203, @fangly)

## General improvements and fixes
* `expect_lint()` now no longer shows Rstudio markers and error messages are correctly preserved (#180, #211, @fangly)
* `Lint()` / `as.data.frame()` error now fixed (#179, @fangly).
* `lint()` no longer errors with inline `\\Sexpr` (#127).
* `lint()` no longer errors with '<% %>' constructs (#185).
* `lint_package()` now works with the cache, as intended (#146, @schloerke)
* `lint_package()` now excludes `R/RcppExports.R` by default (#282)
* `lint_package()` now removes fully excluded files as soon as possible to
* lintr now looks up its configuration in any parent directories as well as the package directory (#238, #345)
* `seq_linter` is now one of the default linters (#316).
* Fix issue in lintr's compatibility with R-devel, due to to a new version of the PCRE library (#411.)
* `read_settings()` now has a better error message when the config file does
  not end with a newline (#160, #189)
* `expect_lint_free()` is now automatically skipped when run on covr (#287)
* Now lintr only tries to generate comments if running in wercker or travis CI (#166)
* Add support for overriding GitHub API Token via `GITHUB_TOKEN` environment
  variable (#63, @mattyb)
* Config files are now also searched for in the users' home directory (#266, @randy3k)
* Fixed crash caused by ambiguous cache file paths (#212, @fangly).
* RStudio addins to lint current source and project (fixes #264, @JhossePaul)
* Added proper handling of tab characters (fixes #44, @fangly)
* lintr does not need the igraph package any more (#152, @gaborcsardi)
* Fixed cache not saved in a directory other than requested (#213, @fangly)
  avoid reading and pre-processing of ignored files (@mwaldstein)
* Allow for any number of `#` to start a comment. Useful in ESS (#299, @prosoitos)
* R Markdown files that do not contain chunks are no longer treated as code (#370).
* Fixed plain-code-block bug in Rmarkdown (#252, @russHyde)
* Fixed bug where non-R chunks using {lang} `engine format` were parsed from R-markdown (#322, @russHyde)
* Ensured `lintr` runs / installs / tests on R-3.6: pinned to github
  `xmlparsedata`; ensure vectors are length-1 when compared using `&&` and `||`
  (#363 #377 #384 #391, @russHyde).

# lintr 1.0.3 #
* Fix tests to work with changes in the parser in R 3.6

# lintr 1.0.2 #
* Fix tests to work with upcoming testthat release.

# lintr 1.0.1 #
* bugfix to work with knitr 1.16.7
* `expect_lint_free()` now is always skipped on CRAN. This is necessary because
  the non-binary R source may not be available when running tests on CRAN, and
  those tests may not be run in the package directory.

# lintr 1.0.0 #
* bugfix to work with testthat 1.0.0

# lintr 0.3.3 #
* infix_spaces_linter now properly checks `=` in named arguments. (#130, @saurfang).
* commas_linter now properly recognizes lints when preceded by a blank line and
  points to the missing space rather than the comma (#111, #129, @saurfang).
* Make spaces_left_parentheses_linter more robust when determining `(` type (#128, @saurfang)
* commented_code_linter (#83, @jackwasey)
* Now trims long comments (#55, reported by @paulstaab)
* Automatic commenting of Github commits and pull requests when linting on Travis-CI
* expect_lint_free expectation can be added to testthat unit tests.
* Robust configuration system and exclusion logic
* Emacs and Sublime Text 3 plugins now available from their respective package repositories.
* add `names.lints`, `split.lints` (#49, @ttriche)
* Fixed bug that caused vim syntatic plugin not to work properly in windows (#46, @abossenbroek)
* allow lintr customization per project using `.lintr` config files.
* use `globalenv()` instead of `baseenv()` for default parent environment so
  that `methods` will be included.
* do not check object usage if eval fails.  Fixes (#24, reported by @fabian-s)
* `trailing_whitespace_linter` was reporting the incorrect line number
* Use RStudio source marker API to display lints (#37, @jjallaire)
* Permit single quotes if they quote literal double quotes (#28, @jackwasey)
* # nolint comments are respected with caching (#68, @krlmlr)
* Properly handle all knitr document formats
* Allow for (( when linting (#259, @nathaneastwood)
* Remove ^ from infix spaces to conform with tidyverse. (#302, @nathaneastwood)

# lintr 0.2.0 #

* Initial release
