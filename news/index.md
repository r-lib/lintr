# Changelog

## lintr 3.3.0-1

CRAN release: 2025-11-27

### Deprecations & breaking changes

- The default for
  [`pipe_consistency_linter()`](https://lintr.r-lib.org/reference/pipe_consistency_linter.md)
  is changed from `"auto"` (require one pipe style, either magrittr or
  native) to `"|>"` (R native pipe required) to coincide with the same
  change in the Tidyverse Style Guide
  ([\#2707](https://github.com/r-lib/lintr/issues/2707),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`lint()`](https://lintr.r-lib.org/reference/lint.md) no longer picks
  up settings automatically in *ad hoc* invocations like
  `lint("text\n")` or `lint(text = "str")`. You should set
  `parse_settings=TRUE` to force settings to be read. Emacs ESS users
  may need to update to a recent version, e.g. `ESS>20251003`.
- Arguments `allow_cascading_assign=`, `allow_right_assign=`, and
  `allow_pipe_assign=` to
  [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  are now defunct.
- Six linters marked as deprecated with warning in the previous release
  are now fully deprecated:
  [`consecutive_stopifnot_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  [`extraction_operator_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  [`no_tab_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  [`single_quotes_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  [`unnecessary_nested_if_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  and
  [`unneeded_concatenation_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md).
  They will be removed in the next release.
- As previously announced, the following fully-deprecated items are now
  removed from the package:
  - `source_file=` argument to
    [`ids_with_token()`](https://lintr.r-lib.org/reference/ids_with_token.md)
    and
    [`with_id()`](https://lintr.r-lib.org/reference/ids_with_token.md).
  - Passing linters by name or as non-`"linter"`-classed functions.
  - `linter=` argument of
    [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md).
  - `with_defaults()`.
  - Linters `closed_curly_linter()`, `open_curly_linter()`,
    `paren_brace_linter()`, and `semicolon_terminator_linter()`.
- Argument `interpret_glue` to
  [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  is deprecated in favor of the more general `interpret_extensions`, in
  which `"glue"` is present by default
  ([\#1472](https://github.com/r-lib/lintr/issues/1472),
  [@MichaelChirico](https://github.com/MichaelChirico)). See the
  description below under ‘New and improved features’.
- [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md), and thus all
  linters, require that the returned object’s `message` attribute is
  consistently a simple character string (and not, for example, an
  object of class `"glue"`;
  [\#2740](https://github.com/r-lib/lintr/issues/2740),
  [@MichaelChirico](https://github.com/MichaelChirico)). In general it
  is good to avoid slower string builders like `glue()` inside a loop (a
  linter might be run on every expression in your pakcage). Classed
  character strings return a warning in this release, which will be
  upgraded to an error subsequently.

### Bug fixes

- Files with encoding inferred from settings read more robustly under
  `lint(parse_settings = TRUE)`
  ([\#2803](https://github.com/r-lib/lintr/issues/2803),
  [@MichaelChirico](https://github.com/MichaelChirico)). Thanks also to
  [@bastistician](https://github.com/bastistician) for detecting a
  regression caused by the initial change for users of Emacs
  ([\#2847](https://github.com/r-lib/lintr/issues/2847)).
- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  no longer errors if `"%<>%"` is an allowed operator
  ([\#2850](https://github.com/r-lib/lintr/issues/2850),
  [@AshesITR](https://github.com/AshesITR)).
- [`expect_lint()`](https://lintr.r-lib.org/reference/expect_lint.md)
  conforms to {testthat} v3.3.0+ rules for custom expectations, namely
  that they produce either exactly one success or exactly one failure
  ([\#2937](https://github.com/r-lib/lintr/issues/2937),
  [@hadley](https://github.com/hadley)).

### Changes to default linters

- [`pipe_consistency_linter()`](https://lintr.r-lib.org/reference/pipe_consistency_linter.md),
  with its new default to enforce the native pipe `|>`, is now a default
  linter, since it corresponds directly to a rule in the Tidyverse Style
  Guide ([\#2707](https://github.com/r-lib/lintr/issues/2707),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### New and improved features

#### New linters

- [`all_equal_linter()`](https://lintr.r-lib.org/reference/all_equal_linter.md)
  warns about incorrect use of
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) in `if` clauses
  or preceded by `!`
  ([\#2885](https://github.com/r-lib/lintr/issues/2885),
  [@Bisaloo](https://github.com/Bisaloo)). Such usages should wrap
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) with
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html), for example.
- [`download_file_linter()`](https://lintr.r-lib.org/reference/download_file_linter.md)
  encourages the use of `mode = "wb"` (or `mode = "ab"`) when using
  [`download.file()`](https://rdrr.io/r/utils/download.file.html),
  rather than `mode = "w"` or `mode = "a"`, as the latter can produce
  broken files in Windows
  ([\#2882](https://github.com/r-lib/lintr/issues/2882),
  [@Bisaloo](https://github.com/Bisaloo)).
- [`list2df_linter()`](https://lintr.r-lib.org/reference/list2df_linter.md)
  encourages the use of the
  [`list2DF()`](https://rdrr.io/r/base/list2DF.html) function, or the
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) function when
  recycling is required, over the slower and less readable
  `do.call(cbind.data.frame, )` alternative
  ([\#2834](https://github.com/r-lib/lintr/issues/2834),
  [@Bisaloo](https://github.com/Bisaloo)).
- [`coalesce_linter()`](https://lintr.r-lib.org/reference/coalesce_linter.md)
  encourages the use of the infix operator `x %||% y`, which is
  equivalent to `if (is.null(x)) y else x`
  ([\#2246](https://github.com/r-lib/lintr/issues/2246),
  [@MichaelChirico](https://github.com/MichaelChirico)). While this has
  long been used in many tidyverse packages (it was added to {ggplot2}
  in 2008), it became part of every R installation from R 4.4.0. Thanks
  also to [@emmanuel-ferdman](https://github.com/emmanuel-ferdman) for
  fixing a false positive before release.

#### Linter improvements

- [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  has a new argument `function_bodies` (default `"multi_line"`) which
  controls when to require function bodies to be wrapped in curly
  braces, with the options `"always"`, `"multi_line"` (only require
  curly braces when a function body spans multiple lines),
  `"not_inline"` (only require curly braces when a function body starts
  on a new line) and `"never"`
  ([\#1807](https://github.com/r-lib/lintr/issues/1807),
  [\#2240](https://github.com/r-lib/lintr/issues/2240),
  [@salim-b](https://github.com/salim-b)).
- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md):
  - recommends using `seq_along(x)` instead of `seq_len(length(x))`
    ([\#2577](https://github.com/r-lib/lintr/issues/2577),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - recommends using
    [`sequence()`](https://rdrr.io/r/base/sequence.html) instead of
    `unlist(lapply(ints, seq))`
    ([\#2618](https://github.com/r-lib/lintr/issues/2618),
    [@Bisaloo](https://github.com/Bisaloo)).
- [`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md):
  - Lints operators in prefix form, e.g. `` `%%`(x, 2) ``
    ([\#1910](https://github.com/r-lib/lintr/issues/1910),
    [@MichaelChirico](https://github.com/MichaelChirico)). Disable this
    by setting `call_is_undesirable=FALSE`.
  - Accepts unnamed entries, treating them as undesirable operators,
    e.g. `undesirable_operator_linter("%%")`
    ([\#2536](https://github.com/r-lib/lintr/issues/2536),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md)
  accepts unnamed entries, treating them as undesirable functions,
  e.g. `undesirable_function_linter("sum")`
  ([\#2536](https://github.com/r-lib/lintr/issues/2536),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  handles un-braced `for` loops correctly
  ([\#2564](https://github.com/r-lib/lintr/issues/2564),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- Setting `exclusions` supports globs like `knitr*` to exclude
  files/directories with a pattern
  ([\#1554](https://github.com/r-lib/lintr/issues/1554),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  and
  [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  apply to objects assigned with
  [`assign()`](https://rdrr.io/r/base/assign.html) or generics created
  with `setGeneric()`
  ([\#1665](https://github.com/r-lib/lintr/issues/1665),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  gains argument `interpret_extensions` to govern which false
  positive-prone common syntaxes should be checked for used objects
  ([\#1472](https://github.com/r-lib/lintr/issues/1472),
  [@MichaelChirico](https://github.com/MichaelChirico)). Currently
  `"glue"` (renamed from earlier argument `interpret_glue`) and
  `"rlang"` are supported. The latter newly covers usage of the `.env`
  pronoun like `.env$key`, where `key` was previously missed as being a
  used variable.
- [`boolean_arithmetic_linter()`](https://lintr.r-lib.org/reference/boolean_arithmetic_linter.md)
  finds many more cases like `sum(x | y) == 0` where the total of a
  known-logical vector is compared to 0
  ([\#1580](https://github.com/r-lib/lintr/issues/1580),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`any_duplicated_linter()`](https://lintr.r-lib.org/reference/any_duplicated_linter.md)
  is extended to recognize some usages from {dplyr} and {data.table}
  that could be replaced by
  [`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html),
  e.g. `n_distinct(col) == n()` or `uniqueN(col) == .N`
  ([\#2482](https://github.com/r-lib/lintr/issues/2482),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`fixed_regex_linter()`](https://lintr.r-lib.org/reference/fixed_regex_linter.md)
  recognizes usage of the new (R 4.5.0)
  [`grepv()`](https://rdrr.io/r/base/grep.html) wrapper of
  [`grep()`](https://rdrr.io/r/base/grep.html);
  [`regex_subset_linter()`](https://lintr.r-lib.org/reference/regex_subset_linter.md)
  also recommends [`grepv()`](https://rdrr.io/r/base/grep.html)
  alternatives ([\#2855](https://github.com/r-lib/lintr/issues/2855),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  lints missing packages that may cause false positives
  ([\#2872](https://github.com/r-lib/lintr/issues/2872),
  [@AshesITR](https://github.com/AshesITR))
- [`sprintf_linter()`](https://lintr.r-lib.org/reference/sprintf_linter.md)
  lints [`sprintf()`](https://rdrr.io/r/base/sprintf.html) and
  [`gettextf()`](https://rdrr.io/r/base/sprintf.html) calls when a
  constant string is passed to `fmt`
  ([\#2894](https://github.com/r-lib/lintr/issues/2894),
  [@Bisaloo](https://github.com/Bisaloo)).
- [`length_test_linter()`](https://lintr.r-lib.org/reference/length_test_linter.md)
  is extended to check incorrect usage of
  [`nrow()`](https://rdrr.io/r/base/nrow.html),
  [`ncol()`](https://rdrr.io/r/base/nrow.html),
  [`NROW()`](https://rdrr.io/r/base/nrow.html),
  [`NCOL()`](https://rdrr.io/r/base/nrow.html)
  ([\#2933](https://github.com/r-lib/lintr/issues/2933),
  [@mcol](https://github.com/mcol)).
- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  gains argument `allow_paren_print` to disable lints for the use of `(`
  for auto-printing
  ([\#2962](https://github.com/r-lib/lintr/issues/2962),
  [@TimTaylor](https://github.com/TimTaylor)).
- [`line_length_linter()`](https://lintr.r-lib.org/reference/line_length_linter.md)
  has a new argument `ignore_string_bodies` (defaulting to `FALSE`)
  which governs whether the contents of multi-line string bodies should
  be linted ([\#856](https://github.com/r-lib/lintr/issues/856),
  [@MichaelChirico](https://github.com/MichaelChirico)). We think the
  biggest use case for this is writing SQL in R strings, especially in
  cases where the recommended string width for SQL & R differ.
- [`package_hooks_linter()`](https://lintr.r-lib.org/reference/package_hooks_linter.md)
  now validates `.onUnload()` hook signatures, requiring exactly one
  argument starting with ‘lib’
  ([\#2940](https://github.com/r-lib/lintr/issues/2940),
  [@emmanuel-ferdman](https://github.com/emmanuel-ferdman)).

#### Lint accuracy fixes: removing false positives

- [`unnecessary_nesting_linter()`](https://lintr.r-lib.org/reference/unnecessary_nesting_linter.md):
  - Treats function bodies under the shorthand lambda (`\()`) the same
    as normal function bodies
    ([\#2748](https://github.com/r-lib/lintr/issues/2748),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Treats `=` assignment the same as `<-` when deciding to combine
    consecutive `if()` clauses
    ([\#2245](https://github.com/r-lib/lintr/issues/2245),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`string_boundary_linter()`](https://lintr.r-lib.org/reference/string_boundary_linter.md)
  omits lints of patterns like `\\^` which have an anchor but are not
  regular expressions
  ([\#2636](https://github.com/r-lib/lintr/issues/2636),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- `implicit_integer_linter(allow_colon = TRUE)` is OK with negative
  literals, e.g. `-1:1` or `1:-1`
  ([\#2673](https://github.com/r-lib/lintr/issues/2673),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`missing_argument_linter()`](https://lintr.r-lib.org/reference/missing_argument_linter.md)
  allows empty calls like `foo()` even if there are comments between `(`
  and `)` ([\#2741](https://github.com/r-lib/lintr/issues/2741),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`return_linter()`](https://lintr.r-lib.org/reference/return_linter.md)
  works on functions that happen to use braced expressions in their
  formals ([\#2616](https://github.com/r-lib/lintr/issues/2616),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  and
  [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  account for S3 class correctly when the generic is assigned with `=`
  ([\#2507](https://github.com/r-lib/lintr/issues/2507),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  with `operator = "="` does a better job of skipping implicit
  assignments, which are intended to be governed by
  [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  ([\#2765](https://github.com/r-lib/lintr/issues/2765),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  with `allow_scoped=TRUE` doesn’t lint for `if (a <- 1) print(a)`
  ([\#2913](https://github.com/r-lib/lintr/issues/2913),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`expect_true_false_linter()`](https://lintr.r-lib.org/reference/expect_true_false_linter.md)
  is pipe-aware, so that `42 |> expect_identical(x, ignore_attr = TRUE)`
  no longer lints ([\#1520](https://github.com/r-lib/lintr/issues/1520),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md)
  ignores `T` and `F`:
  - When used as symbols in formulas (`y ~ T + F`), which can represent
    variables in data not controlled by the author
    ([\#2637](https://github.com/r-lib/lintr/issues/2637),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - If followed by `[` or `[[`
    ([\#2944](https://github.com/r-lib/lintr/issues/2944),
    [@mcol](https://github.com/mcol)).

#### Lint accuracy fixes: removing false negatives

- [`todo_comment_linter()`](https://lintr.r-lib.org/reference/todo_comment_linter.md)
  finds comments inside {roxygen2} markup comments
  ([\#2447](https://github.com/r-lib/lintr/issues/2447),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- Linters with logic around function declarations consistently include
  the R 4.0.0 shorthand `\()`
  ([\#2818](https://github.com/r-lib/lintr/issues/2818), continuation of
  earlier [\#2190](https://github.com/r-lib/lintr/issues/2190),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - [`library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.md)
  - [`terminal_close_linter()`](https://lintr.r-lib.org/reference/terminal_close_linter.md)
  - [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md)
- More consistency on handling `@` extractions to match how similar `$`
  extractions would be linted
  ([\#2820](https://github.com/r-lib/lintr/issues/2820),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)
  - [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  - [`library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.md)
  - [`missing_argument_linter()`](https://lintr.r-lib.org/reference/missing_argument_linter.md)
- [`condition_call_linter()`](https://lintr.r-lib.org/reference/condition_call_linter.md)
  no longer covers cases where the object type in the ellipsis cannot be
  determined with certainty
  ([\#2888](https://github.com/r-lib/lintr/issues/2888),
  [\#2890](https://github.com/r-lib/lintr/issues/2890),
  [@Bisaloo](https://github.com/Bisaloo)). In particular, this fixes the
  known false positive of custom conditions created via
  [`errorCondition()`](https://rdrr.io/r/base/conditions.html) or
  [`warningCondition()`](https://rdrr.io/r/base/conditions.html) not
  being compatible with the `call.` argument in
  [`stop()`](https://rdrr.io/r/base/stop.html) or
  [`warning()`](https://rdrr.io/r/base/warning.html).

#### Other improvements

- `get_source_expression()` captures warnings emitted by the R parser
  (currently always for mis-specified literal integers like `1.1L`) and
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) returns them as
  lints ([\#2065](https://github.com/r-lib/lintr/issues/2065),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`expect_lint()`](https://lintr.r-lib.org/reference/expect_lint.md)
  has a new argument `ignore_order` (default `FALSE`), which, if `TRUE`,
  allows the `checks=` to be provided in arbitary order vs. how
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) produces them
  ([@MichaelChirico](https://github.com/MichaelChirico)).
- New
  [`gitlab_output()`](https://lintr.r-lib.org/reference/gitlab_output.md)
  function to output lints to GitLab format
  ([\#2858](https://github.com/r-lib/lintr/issues/2858),
  [@lschneiderbauer](https://github.com/lschneiderbauer)).
- New argument `include_s4_slots` for the `xml_find_function_calls()`
  entry in the
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  to govern whether calls of the form `s4Obj@fun()` are included in the
  result ([\#2820](https://github.com/r-lib/lintr/issues/2820),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`use_lintr()`](https://lintr.r-lib.org/reference/use_lintr.md) adds
  the created `.lintr` file to the `.Rbuildignore` if run in a package
  ([\#2926](https://github.com/r-lib/lintr/issues/2926), initial work by
  [@MEO265](https://github.com/MEO265), finalized by
  [@Bisaloo](https://github.com/Bisaloo)).

### Notes

- [lintr](https://lintr.r-lib.org) now has an associated paper at the
  [Journal of Open Source Software](https://doi.org/10.21105/joss.07240)
  that you can use to cite the package if you use it in a paper - see
  citation(“lintr”) for details.
- [`expect_lint_free()`](https://lintr.r-lib.org/reference/expect_lint_free.md)
  and other functions that rely on the {testthat} framework now have a
  consistent error message.
  ([\#2585](https://github.com/r-lib/lintr/issues/2585),
  [@F-Noelle](https://github.com/F-Noelle)).
- [`unnecessary_nesting_linter()`](https://lintr.r-lib.org/reference/unnecessary_nesting_linter.md)
  gives a more specific lint message identifying:
  - the unmatched “exit call” that prompts the recommendation to reduce
    nesting ([\#2316](https://github.com/r-lib/lintr/issues/2316),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - the specific `if()` statement that can be combined with the linted
    one ([\#1891](https://github.com/r-lib/lintr/issues/1891),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- The description in
  [`?paste_linter`](https://lintr.r-lib.org/reference/paste_linter.md)
  of `allow_file_path=` has been corrected
  ([\#2675](https://github.com/r-lib/lintr/issues/2675),
  [@MichaelChirico](https://github.com/MichaelChirico)). In particular,
  `allow_file_path="never"` is the most strict form,
  `allow_file_path="always"` is the most lax form.
- `comment_token` is removed from settings. This was a vestige of the
  now-defunct support for posting GitHub comments.

## lintr 3.2.0

CRAN release: 2025-02-12

### Deprecations & breaking changes

- Various things marked deprecated since {lintr} 3.0.0 have been fully
  deprecated. They will be completely removed in the subsequent release.
  See previous NEWS for advice on how to replace them.
  - `source_file=` argument to
    [`ids_with_token()`](https://lintr.r-lib.org/reference/ids_with_token.md)
    and
    [`with_id()`](https://lintr.r-lib.org/reference/ids_with_token.md).
  - Passing linters by name or as non-`"linter"`-classed functions.
  - `linter=` argument of
    [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md).
  - `with_defaults()`.
  - Linters `closed_curly_linter()`, `open_curly_linter()`,
    `paren_brace_linter()`, and `semicolon_terminator_linter()`.
  - Helper `with_defaults()`.
- [`all_linters()`](https://lintr.r-lib.org/reference/all_linters.md)
  has signature `all_linters(..., packages)` rather than
  `all_linters(packages, ...)`
  ([\#2332](https://github.com/r-lib/lintr/issues/2332),
  [@MichaelChirico](https://github.com/MichaelChirico)). This forces
  `packages=` to be supplied by name and will break users who rely on
  supplying `packages=` positionally, of which we found none searching
  GitHub.
- Adjusted various lint messages for consistency and readability
  ([\#1330](https://github.com/r-lib/lintr/issues/1330),
  [@MichaelChirico](https://github.com/MichaelChirico)). In general, we
  favor lint messages to be phrased like “Action, reason” to put the
  “what” piece of the message front-and-center. This may be a breaking
  change for code that tests the specific phrasing of lints.
- [`extraction_operator_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  is deprecated. Although switching from `$` to `[[` has some robustness
  benefits for package code, it can lead to non-idiomatic code in many
  contexts (e.g. R6 classes, Shiny applications, etc.)
  ([\#2409](https://github.com/r-lib/lintr/issues/2409),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)). One reason to
  avoid `$` is that it allows partial matching where `[[` does not. Use
  `options(warnPartialMatchDollar = TRUE)` to disable this feature and
  restore some parity to using `$` vs. `[[`.
- [`unnecessary_nested_if_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  is deprecated and subsumed into the new/more general
  [`unnecessary_nesting_linter()`](https://lintr.r-lib.org/reference/unnecessary_nesting_linter.md).
- Dropped support for posting GitHub comments from inside GitHub comment
  bot, Travis, Wercker, and Jenkins CI tools (spurred by
  [\#2148](https://github.com/r-lib/lintr/issues/2148),
  [@MichaelChirico](https://github.com/MichaelChirico)). We rely on
  GitHub Actions for linting in CI, and don’t see any active users
  relying on these alternatives. We welcome and encourage community
  contributions to get support for different CI systems going again.
- [`cyclocomp_linter()`](https://lintr.r-lib.org/reference/cyclocomp_linter.md)
  is no longer part of the default linters
  ([\#2555](https://github.com/r-lib/lintr/issues/2555),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)) because the
  tidyverse style guide doesn’t contain any guidelines on meeting
  certain complexity requirements. With this, we also downgrade
  {cyclocomp} from `Imports:` to `Suggests:`. Note that users with
  [`cyclocomp_linter()`](https://lintr.r-lib.org/reference/cyclocomp_linter.md)
  in their configs may now need to install {cyclocomp} intentionally, in
  particular in CI/CD pipelines.
- [`scalar_in_linter()`](https://lintr.r-lib.org/reference/scalar_in_linter.md)
  is now configurable to allow other `%in%`-like operators to be linted.
  The data.table operator `%chin%` is no longer linted by default; use
  `in_operators = "%chin%"` to continue linting it.
  ([@F-Noelle](https://github.com/F-Noelle))
- [`lint()`](https://lintr.r-lib.org/reference/lint.md) and friends now
  normalize paths to forward slashes on Windows
  ([@olivroy](https://github.com/olivroy),
  [\#2613](https://github.com/r-lib/lintr/issues/2613)).
- [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md),
  [`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md),
  and
  [`list_comparison_linter()`](https://lintr.r-lib.org/reference/list_comparison_linter.md)
  were removed from the tag `efficiency`
  ([@IndrajeetPatil](https://github.com/IndrajeetPatil),
  [\#2655](https://github.com/r-lib/lintr/issues/2655)). If you use
  `linters_with_tags("efficiency")` to include these linters, you’ll
  need to adjust your config to keep linting your code against them. We
  did not find any such users on GitHub.
- Arguments `allow_cascading_assign=`, `allow_right_assign=`, and
  `allow_pipe_assign=` to
  [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  are all deprecated in favor of the new `operator=` argument. Usage of
  a positional first argument like `assignment_linter(TRUE)`, of which
  we found zero cases on GitHub, is totally deprecated to allow
  `operator=` to be positionally first. See below about the new
  argument.

### Bug fixes

- [`expect_identical_linter()`](https://lintr.r-lib.org/reference/expect_identical_linter.md)
  also skips `expect_equal()` comparison to *negative* non-integers like
  `-1.034` ([\#2411](https://github.com/r-lib/lintr/issues/2411),
  [@Bisaloo](https://github.com/Bisaloo)). This is a parity fix since
  *positive* reals have always been skipped because “high-precision”
  comparisons are typically done to get tests within `tolerance`, so
  `expect_identical()` is not a great substitution.
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  no longer errors when user-supplied `regexes=` have capture groups
  ([\#2188](https://github.com/r-lib/lintr/issues/2188),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- `.lintr` config validation correctly accepts regular expressions which
  only compile under `perl = TRUE`
  ([\#2375](https://github.com/r-lib/lintr/issues/2375),
  [@MichaelChirico](https://github.com/MichaelChirico)). These have
  always been valid (since
  [`rex::re_matches()`](https://rdrr.io/pkg/rex/man/re_matches.html),
  which powers the lint exclusion logic, also uses this setting), but
  the new up-front validation in v3.1.1 incorrectly used `perl = FALSE`.
- `.lintr` configs set by option `lintr.linter_file` or environment
  variable `R_LINTR_LINTER_FILE` can point to subdirectories
  ([\#2512](https://github.com/r-lib/lintr/issues/2512),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  returns lints with `ranges[1L]==1L` when the offending line has 0
  spaces ([\#2550](https://github.com/r-lib/lintr/issues/2550),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`literal_coercion_linter()`](https://lintr.r-lib.org/reference/literal_coercion_linter.md)
  doesn’t surface a warning about `NA`s during coercion for code like
  `as.integer("a")`
  ([\#2566](https://github.com/r-lib/lintr/issues/2566),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Changes to default linters

- New default linter
  [`return_linter()`](https://lintr.r-lib.org/reference/return_linter.md)
  for the style guide rule that terminal returns should be left implicit
  ([\#1100](https://github.com/r-lib/lintr/issues/1100),
  [\#2343](https://github.com/r-lib/lintr/issues/2343),
  [\#2354](https://github.com/r-lib/lintr/issues/2354), and
  [\#2356](https://github.com/r-lib/lintr/issues/2356),
  [@MEO265](https://github.com/MEO265) and
  [@MichaelChirico](https://github.com/MichaelChirico)).

### New and improved features

- New function node caching for big efficiency gains to most linters
  (e.g. overall
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md)
  improvement of 14-27% and core linting improvement up to 30%;
  [\#2357](https://github.com/r-lib/lintr/issues/2357),
  [@AshesITR](https://github.com/AshesITR)). Most linters are written
  around function usage, and XPath performance searching for many
  functions is poor. The new `xml_find_function_calls()` entry in the
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  output caches all function call nodes instead. See the vignette on
  creating linters for more details on how to use it.
- [`Linter()`](https://lintr.r-lib.org/reference/Linter.md) has a new
  argument `linter_level=` (default `NA`). This is used by
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) to more
  efficiently check for expression levels than the idiom
  `if (!is_lint_level(...)) { return(list()) }`
  ([\#2351](https://github.com/r-lib/lintr/issues/2351),
  [@AshesITR](https://github.com/AshesITR)).
- New
  [`return_linter()`](https://lintr.r-lib.org/reference/return_linter.md)
  also has arguments for fine-tuning which functions get linted:
  - `return_style=` (`"implicit"` by default) which checks that all
    functions confirm to the specified return style of `"implicit"` or
    `"explicit"` ([\#2271](https://github.com/r-lib/lintr/issues/2271)
    and part of [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico),
    [@AshesITR](https://github.com/AshesITR) and
    [@MEO265](https://github.com/MEO265)).
  - `allow_implicit_else=` (default `TRUE`) which, when `FALSE`, checks
    that all terminal `if` statements are paired with a corresponding
    `else` statement (part of
    [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - `return_functions=` to customize which functions are equivalent to
    [`return()`](https://rdrr.io/r/base/function.html) as “exit”
    clauses,
    e.g. [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)
    can be considered in addition to the default functions like
    [`stop()`](https://rdrr.io/r/base/stop.html) and
    [`q()`](https://rdrr.io/r/base/quit.html) from base
    ([\#2271](https://github.com/r-lib/lintr/issues/2271) and part of
    [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico) and
    [@MEO265](https://github.com/MEO265)).
  - `except=` to customize which functions are ignored entirely (i.e.,
    whether they have a return of the specified style is not checked;
    [\#2271](https://github.com/r-lib/lintr/issues/2271) and part of
    [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico) and
    [@MEO265](https://github.com/MEO265)). Namespace hooks like
    `.onAttach()` and `.onLoad()` are always ignored.
  - `except_regex=`, the same purpose as `except=`, but filters
    functions by pattern. This is motivated by {RUnit}, where test
    suites are based on unit test functions matched by pattern,
    e.g. `^Test`, and where explicit return may be awkward
    ([\#2335](https://github.com/r-lib/lintr/issues/2335),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  can be fully customized with the new `operator=` argument to specify
  an exact vector of assignment operators to allow
  ([\#2441](https://github.com/r-lib/lintr/issues/2441),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@J-Moravec](https://github.com/J-Moravec)). The default is `<-` and
  `<<-`; authors wishing to use `=` (only) for assignment in their
  codebase can use `operator = "="`. This supersedes several old
  arguments: to accomplish `allow_cascading_assign=TRUE`, add `"<<-"`
  (and/or `"->>"`) to `operator=`; for `allow_right_assign=TRUE`, add
  `"->"` (and/or `"->>"`) to `operator=`; for `allow_pipe_assign=TRUE`,
  add `"%<>%"` to `operator=`. Use `operator = "any"` to denote “ignore
  all assignment operators”; in this case, only the value of
  `allow_trailing=` matters. Implicit assignments with `<-` are always
  ignored by
  [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md);
  use
  [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  to handle linting these.
- More helpful errors for invalid configs
  ([\#2253](https://github.com/r-lib/lintr/issues/2253),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.md)
  is extended
  - to encourage all packages to be attached with
    [`library(symbol)`](https://rdrr.io/r/base/library.html), not
    [`library("symbol", character.only = TRUE)`](https://rdrr.io/r/base/library.html)
    or “vectorized” approaches looping over package names (part of
    [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - to discourage many consecutive calls to
    [`suppressMessages()`](https://rdrr.io/r/base/message.html) or
    [`suppressPackageStartupMessages()`](https://rdrr.io/r/base/message.html)
    (part of [\#884](https://github.com/r-lib/lintr/issues/884),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md)
  is extended to encourage vectorized comparisons where possible,
  e.g. `sapply(x, sum) > 0` instead of
  `sapply(x, function(x) sum(x) > 0)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)). Toggle this
  behavior with argument `allow_comparison=`.
- [`backport_linter()`](https://lintr.r-lib.org/reference/backport_linter.md)
  is slightly faster by moving expensive computations outside the
  linting function
  ([\#2339](https://github.com/r-lib/lintr/issues/2339),
  [\#2348](https://github.com/r-lib/lintr/issues/2348),
  [@AshesITR](https://github.com/AshesITR) and
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`string_boundary_linter()`](https://lintr.r-lib.org/reference/string_boundary_linter.md)
  recognizes regular expression calls like `grepl("^abc$", x)` that can
  be replaced by using `==` instead
  ([\#1613](https://github.com/r-lib/lintr/issues/1613),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  has an argument `allow_comment_regex=` for customizing which
  “terminal” comments to exclude
  ([\#2327](https://github.com/r-lib/lintr/issues/2327),
  [@MichaelChirico](https://github.com/MichaelChirico)). Exclusion
  comments from {lintr} and {covr} (e.g. `# nocov end`) are always
  excluded.
- [`format()`](https://rdrr.io/r/base/format.html) and
  [`print()`](https://rdrr.io/r/base/print.html) methods for `lint` and
  `lints` classes get a new option `width=` to control the printing
  width of lint messages
  ([\#1884](https://github.com/r-lib/lintr/issues/1884),
  [@MichaelChirico](https://github.com/MichaelChirico)). The default is
  controlled by a new option `lintr.format_width`; if unset, no wrapping
  occurs (matching earlier behavior).
- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  gets a custom message for the case of using `(` to induce printing
  like `(x <- foo())`; use an explicit call to
  [`print()`](https://rdrr.io/r/base/print.html) for clarity
  ([\#2257](https://github.com/r-lib/lintr/issues/2257),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`todo_comment_linter()`](https://lintr.r-lib.org/reference/todo_comment_linter.md)
  has a new argument `except_regex=` for setting *valid* TODO comments,
  e.g. for forcing TODO comments to be linked to GitHub issues like
  `TODO(`[`#154`](https://github.com/r-lib/lintr/issues/154)`)`
  ([\#2047](https://github.com/r-lib/lintr/issues/2047),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`vector_logic_linter()`](https://lintr.r-lib.org/reference/vector_logic_linter.md)
  is extended to recognize incorrect usage of scalar operators `&&` and
  `||` inside subsetting expressions like `dplyr::filter(x, A && B)`
  ([\#2166](https://github.com/r-lib/lintr/issues/2166),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`any_is_na_linter()`](https://lintr.r-lib.org/reference/any_is_na_linter.md)
  is extended to catch the unusual usage `NA %in% x`
  ([\#2113](https://github.com/r-lib/lintr/issues/2113),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`make_linter_from_xpath()`](https://lintr.r-lib.org/reference/make_linter_from_xpath.md)
  errors up front when `lint_message=` is missing (instead of delaying
  this error until the linter is used,
  [\#2541](https://github.com/r-lib/lintr/issues/2541),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`paste_linter()`](https://lintr.r-lib.org/reference/paste_linter.md)
  is extended to recommend using
  [`paste()`](https://rdrr.io/r/base/paste.html) instead of
  [`paste0()`](https://rdrr.io/r/base/paste.html) for simply aggregating
  a character vector with `collapse=`, i.e., when `sep=` is irrelevant
  ([\#1108](https://github.com/r-lib/lintr/issues/1108),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`expect_no_lint()`](https://lintr.r-lib.org/reference/expect_lint.md)
  was added as new function to cover the typical use case of expecting
  no lint message, akin to the recent {testthat} functions like
  `expect_no_warning()`
  ([\#2580](https://github.com/r-lib/lintr/issues/2580),
  [@F-Noelle](https://github.com/F-Noelle)).
- [`lint()`](https://lintr.r-lib.org/reference/lint.md) and friends emit
  a message if no lints are found
  ([\#2643](https://github.com/r-lib/lintr/issues/2643),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
- [`commented_code_linter()`](https://lintr.r-lib.org/reference/commented_code_linter.md)
  can detect commented code that ends with a pipe
  ([\#2671](https://github.com/r-lib/lintr/issues/2671),
  [@jcken95](https://github.com/jcken95))

#### New linters

- [`condition_call_linter()`](https://lintr.r-lib.org/reference/condition_call_linter.md)
  for ensuring consistent use of `call.` in
  [`warning()`](https://rdrr.io/r/base/warning.html) and
  [`stop()`](https://rdrr.io/r/base/stop.html). The default
  `call. = FALSE` follows the tidyverse guidance of not displaying the
  call ([\#2226](https://github.com/r-lib/lintr/issues/2226),
  [@Bisaloo](https://github.com/Bisaloo))
- [`sample_int_linter()`](https://lintr.r-lib.org/reference/sample_int_linter.md)
  for encouraging `sample.int(n, ...)` over equivalents like
  `sample(1:n, ...)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`stopifnot_all_linter()`](https://lintr.r-lib.org/reference/stopifnot_all_linter.md)
  discourages tests with [`all()`](https://rdrr.io/r/base/all.html) like
  `stopifnot(all(x > 0))`;
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) runs
  [`all()`](https://rdrr.io/r/base/all.html) itself, and signals a
  better error message (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`comparison_negation_linter()`](https://lintr.r-lib.org/reference/comparison_negation_linter.md)
  for discouraging negated comparisons when a direct negation is
  preferable, e.g. `!(x == y)` could be `x != y` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`nzchar_linter()`](https://lintr.r-lib.org/reference/nzchar_linter.md)
  for encouraging [`nzchar()`](https://rdrr.io/r/base/nchar.html) to
  test for empty strings, e.g. `nchar(x) > 0` can be `nzchar(x)` (part
  of [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`terminal_close_linter()`](https://lintr.r-lib.org/reference/terminal_close_linter.md)
  for discouraging using
  [`close()`](https://rdrr.io/r/base/connections.html) to end functions
  (part of [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)). Such usages are
  not robust to errors, where
  [`close()`](https://rdrr.io/r/base/connections.html) will not be run
  as intended. Put [`close()`](https://rdrr.io/r/base/connections.html)
  in an [`on.exit()`](https://rdrr.io/r/base/on.exit.html) hook, or use
  {withr} to manage connections with proper cleanup.
- [`rep_len_linter()`](https://lintr.r-lib.org/reference/rep_len_linter.md)
  for encouraging use of [`rep_len()`](https://rdrr.io/r/base/rep.html)
  directly instead of `rep(x, length.out = n)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)). Note that in
  older versions of R (e.g. pre-4.0),
  [`rep_len()`](https://rdrr.io/r/base/rep.html) may not copy attributes
  as expected.
- [`which_grepl_linter()`](https://lintr.r-lib.org/reference/which_grepl_linter.md)
  for discouraging `which(grepl(ptn, x))` in favor of directly using
  `grep(ptn, x)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`list_comparison_linter()`](https://lintr.r-lib.org/reference/list_comparison_linter.md)
  for discouraging comparisons on the output of
  [`lapply()`](https://rdrr.io/r/base/lapply.html),
  e.g. `lapply(x, sum) > 10` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`print_linter()`](https://lintr.r-lib.org/reference/print_linter.md)
  for discouraging usage of
  [`print()`](https://rdrr.io/r/base/print.html) on string literals like
  `print("Reached here")` or `print(paste("Found", nrow(DF), "rows."))`
  ([\#1894](https://github.com/r-lib/lintr/issues/1894),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unnecessary_nesting_linter()`](https://lintr.r-lib.org/reference/unnecessary_nesting_linter.md)
  for discouraging overly-nested code where an early return or
  eliminated sub-expression (inside `{`) is preferable
  ([\#2317](https://github.com/r-lib/lintr/issues/2317),
  [\#2334](https://github.com/r-lib/lintr/issues/2334) and part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`consecutive_mutate_linter()`](https://lintr.r-lib.org/reference/consecutive_mutate_linter.md)
  for encouraging consecutive calls to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  to be combined (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`if_switch_linter()`](https://lintr.r-lib.org/reference/if_switch_linter.md)
  for encouraging [`switch()`](https://rdrr.io/r/base/switch.html) over
  repeated `if`/`else` tests
  ([\#2322](https://github.com/r-lib/lintr/issues/2322) and part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`nested_pipe_linter()`](https://lintr.r-lib.org/reference/nested_pipe_linter.md)
  for discouraging pipes within pipes,
  e.g. `df1 %>% inner_join(df2 %>% select(a, b))` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`nrow_subset_linter()`](https://lintr.r-lib.org/reference/nrow_subset_linter.md)
  for discouraging usage like `nrow(subset(x, conditions))` in favor of
  something like `with(x, sum(conditions))` which doesn’t require a full
  subset of `x` ([\#2313](https://github.com/r-lib/lintr/issues/2313),
  [\#2314](https://github.com/r-lib/lintr/issues/2314) and part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`pipe_return_linter()`](https://lintr.r-lib.org/reference/pipe_return_linter.md)
  for discouraging usage of
  [`return()`](https://rdrr.io/r/base/function.html) inside a {magrittr}
  pipeline (part of [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`one_call_pipe_linter()`](https://lintr.r-lib.org/reference/one_call_pipe_linter.md)
  for discouraging one-step pipelines like `x |> as.character()`
  ([\#2330](https://github.com/r-lib/lintr/issues/2330) and part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_overwrite_linter()`](https://lintr.r-lib.org/reference/object_overwrite_linter.md)
  for discouraging re-use of upstream package exports as local variables
  ([\#2344](https://github.com/r-lib/lintr/issues/2344),
  [\#2346](https://github.com/r-lib/lintr/issues/2346) and part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@AshesITR](https://github.com/AshesITR)).

#### Lint accuracy fixes: removing false positives

- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  and
  [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  ignore {rlang} name injection like
  `x |> mutate("{new_name}" := foo(col))`
  ([\#1926](https://github.com/r-lib/lintr/issues/1926),
  [@MichaelChirico](https://github.com/MichaelChirico)). No checking is
  applied in such cases. {data.table} in-place assignments like
  `DT[, "sPoNGeBob" := "friend"]` are still eligible for lints.
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  finds global variables assigned with `=` or `->`, which avoids some
  issues around “undefined global variables” in scripts
  ([\#2654](https://github.com/r-lib/lintr/issues/2654),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Notes

- [lintr](https://lintr.r-lib.org) now has a hex sticker
  (<https://github.com/rstudio/hex-stickers/pull/110>). Thank you,
  [@gregswinehart](https://github.com/gregswinehart)!
- All user-facing messages (including progress bars) are now prepared
  using the [cli](https://cli.r-lib.org) package
  ([\#2418](https://github.com/r-lib/lintr/issues/2418) and
  [\#2641](https://github.com/r-lib/lintr/issues/2641),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)). As noted above,
  all messages have been reviewed and updated to be more informative and
  consistent.
- File locations in lints and error messages contain clickable
  hyperlinks to improve code navigation
  ([\#2645](https://github.com/r-lib/lintr/issues/2645),
  [\#2588](https://github.com/r-lib/lintr/issues/2588),
  [@olivroy](https://github.com/olivroy)).
- {lintr} now depends on R version 4.0.0. It already does so implicitly
  due to recursive upstream dependencies requiring this version; we’ve
  simply made that dependency explicit and up-front
  ([\#2569](https://github.com/r-lib/lintr/issues/2569),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- Some code with parameters accepting regular expressions is less strict
  about whether there are capture groups
  ([\#2678](https://github.com/r-lib/lintr/issues/2678),
  [@MichaelChirico](https://github.com/MichaelChirico)). In particular,
  this affects `unreachable_code_linter(allow_comment_regex=)` and
  `expect_lint(checks=)`.

## lintr 3.1.2

CRAN release: 2024-03-25

### New and improved features

#### Lint accuracy fixes: removing false positives

- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  ignores reachable code in inline functions like
  `function(x) if (x > 2) stop() else x`
  ([\#2259](https://github.com/r-lib/lintr/issues/2259),
  [@MEO265](https://github.com/MEO265)).
- [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md)
  - ignores extractions with explicit returns like
    `lapply(l, function(x) foo(x)$bar)`
    ([\#2258](https://github.com/r-lib/lintr/issues/2258),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - ignores calls on the RHS of operators like
    `lapply(l, function(x) "a" %in% names(x))`
    ([\#2310](https://github.com/r-lib/lintr/issues/2310),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`vector_logic_linter()`](https://lintr.r-lib.org/reference/vector_logic_linter.md)
  recognizes some cases where bitwise `&`/`|` are used correctly
  ([\#1453](https://github.com/r-lib/lintr/issues/1453),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`expect_comparison_linter()`](https://lintr.r-lib.org/reference/expect_comparison_linter.md)
  ignores faulty usage like `expect_true(x, y > z)`
  ([\#2083](https://github.com/r-lib/lintr/issues/2083),
  [@MichaelChirico](https://github.com/MichaelChirico)). Note that
  `y > z` is being passed to the `info=` argument, so this is likely a
  mistake.
- [`consecutive_assertion_linter()`](https://lintr.r-lib.org/reference/consecutive_assertion_linter.md)
  ignores cases where a second assertion follows an intervening
  assignment with `=`
  ([\#2444](https://github.com/r-lib/lintr/issues/2444),
  [@MichaelChirico](https://github.com/MichaelChirico)).

#### Lint accuracy fixes: removing false negatives

- [`missing_argument_linter()`](https://lintr.r-lib.org/reference/missing_argument_linter.md)
  catches all missing arguments in calls with several, e.g. `foo(,,)`
  gives 3 lints instead of 2
  ([\#2399](https://github.com/r-lib/lintr/issues/2399),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`duplicate_argument_linter()`](https://lintr.r-lib.org/reference/duplicate_argument_linter.md)
  no longer misses cases with duplicate arguments where a comment comes
  between the argument name and `=`
  ([\#2402](https://github.com/r-lib/lintr/issues/2402),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Notes

- Fixed a test assuming a specific parser error message that recently
  changed in r-devel
  ([\#2527](https://github.com/r-lib/lintr/issues/2527),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
- @MichaelChirico has taken over CRAN maintainer duties for the package.
  Many thanks to [@jimhester](https://github.com/jimhester) for more
  than 10 years and 15 releases wearing that hat!!

## lintr 3.1.1

CRAN release: 2023-11-07

### Breaking changes

- [`infix_spaces_linter()`](https://lintr.r-lib.org/reference/infix_spaces_linter.md)
  distinguishes `<-`, `:=`, `<<-` and `->`, `->>`,
  i.e. `infix_spaces_linter(exclude_operators = "->")` will no longer
  exclude `->>` ([\#2115](https://github.com/r-lib/lintr/issues/2115),
  [@MichaelChirico](https://github.com/MichaelChirico)). This change is
  breaking for users relying on manually-supplied `exclude_operators`
  containing `"<-"` to also exclude `:=` and `<<-`. The fix is to
  manually supply `":="` and `"<<-"` as well. We don’t expect this
  change to affect many users, the fix is simple, and the new behavior
  is much more transparent, so we are including this breakage in a minor
  release.
- Removed `find_line()` and `find_column()` entries from
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  expression-level objects. These have been marked deprecated since
  version 3.0.0. No users were found on GitHub.
- There is experimental support for writing config in plain R scripts
  (as opposed to DCF files;
  [\#1210](https://github.com/r-lib/lintr/issues/1210),
  [@MichaelChirico](https://github.com/MichaelChirico)). The script is
  run in a new environment and variables matching settings
  ([`?default_settings`](https://lintr.r-lib.org/reference/default_settings.md))
  are copied over. In particular, this removes the need to write R code
  in a DCF-friendly way, and allows normal R syntax highlighting in the
  saved file. We may eventually deprecate the DCF approach in favor of
  this one; user feedback is welcome on strong preferences for either
  approach, or for a different approach like YAML. Generally you should
  be able to convert your existing `.lintr` file to an equivalent R
  config by replacing the `:` key-value separators with assignments
  (`<-`). By default, such a config is searched for in a file named
  `.lintr.R`. This is a mildly breaking change if you happened to be
  keeping a file `.lintr.R` around since that file is given precedence
  over `.lintr`.
  - We also validate config files up-front make it clearer when invalid
    configs are present
    ([\#2195](https://github.com/r-lib/lintr/issues/2195),
    [@MichaelChirico](https://github.com/MichaelChirico)). There is a
    warning for “invalid” settings, i.e., settings not part of
    [`?default_settings`](https://lintr.r-lib.org/reference/default_settings.md).
    We think this is more likely to affect users declaring settings in
    R, since any variable defined in the config that’s not a setting
    must be removed to make it clearer which variables are settings
    vs. ancillary.

### Bug fixes

- [`sprintf_linter()`](https://lintr.r-lib.org/reference/sprintf_linter.md)
  doesn’t error in cases where whitespace in `...` arguments is
  significant, e.g. `sprintf("%s", if (A) "" else y)`, which won’t parse
  if whitespace is removed
  ([\#2131](https://github.com/r-lib/lintr/issues/2131),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Changes to default linters

- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  lints the {magrittr} assignment pipe `%<>%`
  ([\#2008](https://github.com/r-lib/lintr/issues/2008),
  [@MichaelChirico](https://github.com/MichaelChirico)). This can be
  deactivated by setting the new argument `allow_pipe_assign` to `TRUE`.
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md):
  - assumes `glue()` is
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    when `interpret_glue=TRUE`
    ([\#2032](https://github.com/r-lib/lintr/issues/2032),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - finds function usages, including infix usage, inside `glue()` calls
    to avoid false positives for “unused objects”
    ([\#2029](https://github.com/r-lib/lintr/issues/2029) and
    [\#2069](https://github.com/r-lib/lintr/issues/2069),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  no longer attempts to lint strings in function calls on the LHS of
  assignments ([\#1466](https://github.com/r-lib/lintr/issues/1466),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`infix_spaces_linter()`](https://lintr.r-lib.org/reference/infix_spaces_linter.md)
  allows finer control for linting `=` in different scenarios using
  parse tags `EQ_ASSIGN`, `EQ_SUB`, and `EQ_FORMALS`
  ([\#1977](https://github.com/r-lib/lintr/issues/1977),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`equals_na_linter()`](https://lintr.r-lib.org/reference/equals_na_linter.md)
  checks for `x %in% NA`, which is a more convoluted form of `is.na(x)`
  ([\#2088](https://github.com/r-lib/lintr/issues/2088),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### New and improved features

- New exclusion sentinel `# nolint next` to signify the next line should
  skip linting ([\#1791](https://github.com/r-lib/lintr/issues/1791),
  [@MichaelChirico](https://github.com/MichaelChirico)). The usual rules
  apply for excluding specific linters,
  e.g. `# nolint next: assignment_linter.`. The exact string used to
  match a subsequent-line exclusion is controlled by the `exclude_next`
  config entry or R option `"lintr.exclude_next"`.
- New
  [`xp_call_name()`](https://lintr.r-lib.org/reference/xp_call_name.md)
  helper to facilitate writing custom linters
  ([\#2023](https://github.com/r-lib/lintr/issues/2023),
  [@MichaelChirico](https://github.com/MichaelChirico)). This helper
  converts a matched XPath to the R function to which it corresponds.
  This is useful for including the “offending” function in the lint’s
  message.
- New
  [`make_linter_from_xpath()`](https://lintr.r-lib.org/reference/make_linter_from_xpath.md)
  to facilitate making simple linters directly from a single XPath
  ([\#2064](https://github.com/r-lib/lintr/issues/2064),
  [@MichaelChirico](https://github.com/MichaelChirico)). This is
  especially helpful for making on-the-fly/exploratory linters, but also
  extends to any case where the linter can be fully defined from a
  static lint message and single XPath.
- Toggle lint progress indicators with argument `show_progress` to
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) and
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md)
  ([\#972](https://github.com/r-lib/lintr/issues/972),
  [@MichaelChirico](https://github.com/MichaelChirico)). The default is
  still to show progress in
  [`interactive()`](https://rdrr.io/r/base/interactive.html) sessions.
  Progress is also now shown with a “proper” progress bar
  ([`utils::txtProgressBar()`](https://rdrr.io/r/utils/txtProgressBar.html)),
  which in particular solves the issue of progress `.` spilling well
  past the width of the screen in large directories.
- [`lint()`](https://lintr.r-lib.org/reference/lint.md),
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md), and
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) fail
  more gracefully when the user mis-spells an argument name
  ([\#2134](https://github.com/r-lib/lintr/issues/2134),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- Quarto files (.qmd) are included by
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) by default
  ([\#2150](https://github.com/r-lib/lintr/issues/2150),
  [@dave-lovell](https://github.com/dave-lovell)).

#### New linters

- [`library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.md)
  can detect if all library/require calls are not at the top of your
  script ([\#2027](https://github.com/r-lib/lintr/issues/2027),
  [\#2043](https://github.com/r-lib/lintr/issues/2043),
  [\#2163](https://github.com/r-lib/lintr/issues/2163), and
  [\#2170](https://github.com/r-lib/lintr/issues/2170),
  [@nicholas-masel](https://github.com/nicholas-masel) and
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`keyword_quote_linter()`](https://lintr.r-lib.org/reference/keyword_quote_linter.md)
  for finding unnecessary or discouraged quoting of symbols in
  assignment, function arguments, or extraction (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)). Quoting is
  unnecessary when the target is a valid R name, e.g. `c("a" = 1)` can
  be `c(a = 1)`. The same goes to assignment (`"a" <- 1`) and extraction
  (`x$"a"`). Where quoting is necessary, the linter encourages doing so
  with backticks (e.g. `` x$`a b` `` instead of `x$"a b"`).
- [`length_levels_linter()`](https://lintr.r-lib.org/reference/length_levels_linter.md)
  for using the specific function
  [`nlevels()`](https://rdrr.io/r/base/nlevels.html) instead of checking
  `length(levels(x))` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`scalar_in_linter()`](https://lintr.r-lib.org/reference/scalar_in_linter.md)
  for discouraging `%in%` when the right-hand side is a scalar,
  e.g. `x %in% 1` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`if_not_else_linter()`](https://lintr.r-lib.org/reference/if_not_else_linter.md)
  for encouraging `if` statements to be structured as `if (A) x else y`
  instead of `if (!A) y else x` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`repeat_linter()`](https://lintr.r-lib.org/reference/repeat_linter.md)
  for encouraging `repeat` for infinite loops instead of `while (TRUE)`
  ([\#2106](https://github.com/r-lib/lintr/issues/2106),
  [@MEO265](https://github.com/MEO265)).
- [`length_test_linter()`](https://lintr.r-lib.org/reference/length_test_linter.md)
  detects the common mistake `length(x == 0)` which is meant to be
  `length(x) == 0`
  ([\#1991](https://github.com/r-lib/lintr/issues/1991),
  [@MichaelChirico](https://github.com/MichaelChirico)).

#### Extensions to existing linters

- [`fixed_regex_linter()`](https://lintr.r-lib.org/reference/fixed_regex_linter.md)
  gains an option `allow_unescaped` (default `FALSE`) to toggle linting
  regexes not requiring any escapes or character classes
  ([\#1689](https://github.com/r-lib/lintr/issues/1689),
  [@MichaelChirico](https://github.com/MichaelChirico)). Thus
  `fixed_regex_linter(allow_unescaped = TRUE)` would lint on
  `grepl("[$]", x)` but not on `grepl("a", x)` since the latter does not
  use any regex special characters.
- [`line_length_linter()`](https://lintr.r-lib.org/reference/line_length_linter.md)
  helpfully includes the line length in the lint message
  ([\#2057](https://github.com/r-lib/lintr/issues/2057),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`conjunct_test_linter()`](https://lintr.r-lib.org/reference/conjunct_test_linter.md)
  also lints usage like `dplyr::filter(x, A & B)` in favor of using
  `dplyr::filter(x, A, B)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884);
  [\#2110](https://github.com/r-lib/lintr/issues/2110) and
  [\#2078](https://github.com/r-lib/lintr/issues/2078),
  [@salim-b](https://github.com/salim-b) and
  [@MichaelChirico](https://github.com/MichaelChirico)). Option
  `allow_filter` toggles when this applies. `allow_filter = "always"`
  drops such lints entirely, while `"not_dplyr"` only lints calls
  explicitly qualified as
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
  The default, `"never"`, assumes all unqualified calls to
  [`filter()`](https://rdrr.io/r/stats/filter.html) are
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
- [`sort_linter()`](https://lintr.r-lib.org/reference/sort_linter.md)
  checks for code like `x == sort(x)` which is better served by using
  the function
  [`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html) (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`paste_linter()`](https://lintr.r-lib.org/reference/paste_linter.md)
  gains detection for file paths that are better constructed with
  [`file.path()`](https://rdrr.io/r/base/file.path.html),
  e.g. `paste0(dir, "/", file)` would be better as
  `file.path(dir, file)` (part of
  [\#884](https://github.com/r-lib/lintr/issues/884),
  [\#2082](https://github.com/r-lib/lintr/issues/2082),
  [@MichaelChirico](https://github.com/MichaelChirico)). What exactly
  gets linted here can be fine-tuned with the `allow_file_path` option
  (`"double_slash"` by default, with alternatives `"never"` and
  `"always"`). When `"always"`, these rules are ignored. When
  `"double_slash"`, paths appearing to construct a URL that have
  consecutive forward slashes (`/`) are skipped. When `"never"`, even
  URLs should be constructed with
  [`file.path()`](https://rdrr.io/r/base/file.path.html).
- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md)
  recommends [`rev()`](https://rdrr.io/r/base/rev.html) in the lint
  message for lints like `nrow(x):1`
  ([\#1542](https://github.com/r-lib/lintr/issues/1542),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`function_argument_linter()`](https://lintr.r-lib.org/reference/function_argument_linter.md)
  detects usage of [`missing()`](https://rdrr.io/r/base/missing.html)
  for the linted argument
  ([\#1546](https://github.com/r-lib/lintr/issues/1546),
  [@MichaelChirico](https://github.com/MichaelChirico)). The simplest
  fix for
  [`function_argument_linter()`](https://lintr.r-lib.org/reference/function_argument_linter.md)
  lints is typically to set that argument to `NULL` by default, in which
  case it’s usually preferable to update function logic checking
  [`missing()`](https://rdrr.io/r/base/missing.html) to check
  [`is.null()`](https://rdrr.io/r/base/NULL.html) instead.
- [`commas_linter()`](https://lintr.r-lib.org/reference/commas_linter.md)
  gains an option `allow_trailing` (default `FALSE`) to allow trailing
  commas while indexing.
  ([\#2104](https://github.com/r-lib/lintr/issues/2104),
  [@MEO265](https://github.com/MEO265))
- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  - checks for code inside `if (FALSE)` and other conditional loops with
    deterministically false conditions
    ([\#1428](https://github.com/r-lib/lintr/issues/1428),
    [@ME0265](https://github.com/ME0265)).
  - checks for unreachable code inside `if`, `else`, `for`, `while`, and
    `repeat` blocks, including combinations with
    [`break`](https://rdrr.io/r/base/Control.html) and
    [`next`](https://rdrr.io/r/base/Control.html) statements.
    ([\#2105](https://github.com/r-lib/lintr/issues/2105),
    [@ME0265](https://github.com/ME0265)).
- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  gains an argument `allow_lazy` (default `FALSE`) that allows
  optionally skipping lazy assignments like `A && (B <- foo(A))`
  ([\#2016](https://github.com/r-lib/lintr/issues/2016),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unused_import_linter()`](https://lintr.r-lib.org/reference/unused_import_linter.md)
  gains an argument `interpret_glue` (default `TRUE`) paralleling that
  in
  [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  to toggle whether
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  expressions should be inspected for exported object usage
  ([\#2042](https://github.com/r-lib/lintr/issues/2042),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- `default_undesirable_functions` is updated to also include
  [`Sys.unsetenv()`](https://rdrr.io/r/base/Sys.setenv.html) and
  [`structure()`](https://rdrr.io/r/base/structure.html)
  ([\#2192](https://github.com/r-lib/lintr/issues/2192) and
  [\#2228](https://github.com/r-lib/lintr/issues/2228),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil) and
  [@MichaelChirico](https://github.com/MichaelChirico)).
- Linters with logic around the magrittr pipe `%>%` consistently apply
  it to the other pipes `%!>%`, `%T>%`, `%<>%` (and possibly `%$%`)
  where appropriate
  ([\#2008](https://github.com/r-lib/lintr/issues/2008),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  - [`pipe_call_linter()`](https://lintr.r-lib.org/reference/pipe_call_linter.md)
  - [`pipe_continuation_linter()`](https://lintr.r-lib.org/reference/pipe_continuation_linter.md)
  - [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md)
  - [`unnecessary_placeholder_linter()`](https://lintr.r-lib.org/reference/unnecessary_placeholder_linter.md)
- Linters with logic around function declarations consistently include
  the R 4.0.0 shorthand `\()`
  ([\#2190](https://github.com/r-lib/lintr/issues/2190),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  - [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)
  - [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  - [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  - [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  - [`package_hooks_linter()`](https://lintr.r-lib.org/reference/package_hooks_linter.md)
  - [`paren_body_linter()`](https://lintr.r-lib.org/reference/paren_body_linter.md)
  - [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md)
  - [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)

#### Lint accuracy fixes: removing false positives

- [`fixed_regex_linter()`](https://lintr.r-lib.org/reference/fixed_regex_linter.md)
  - Is pipe-aware, in particular removing false positives around piping
    into {stringr} functions like `x |> str_replace(fixed("a"), "b")`
    ([\#1811](https://github.com/r-lib/lintr/issues/1811),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Ignores non-string inputs to `pattern=` as a keyword argument
    ([\#2159](https://github.com/r-lib/lintr/issues/2159),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- Several linters avoiding false positives in `$` extractions get the
  same exceptions for `@` extractions, e.g. `S4@T` will no longer throw
  a
  [`T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md)
  hit ([\#2039](https://github.com/r-lib/lintr/issues/2039),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - [`T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md)
  - [`for_loop_index_linter()`](https://lintr.r-lib.org/reference/for_loop_index_linter.md)
  - [`literal_coercion_linter()`](https://lintr.r-lib.org/reference/literal_coercion_linter.md)
  - [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  - [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md)
  - [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  - [`yoda_test_linter()`](https://lintr.r-lib.org/reference/yoda_test_linter.md)
- [`sprintf_linter()`](https://lintr.r-lib.org/reference/sprintf_linter.md)
  is pipe-aware, so that `x %>% sprintf(fmt = "%s")` no longer lints
  ([\#1943](https://github.com/r-lib/lintr/issues/1943),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`condition_message_linter()`](https://lintr.r-lib.org/reference/condition_message_linter.md)
  ignores usages of extracted calls like `env$stop(paste(a, b))`
  ([\#1455](https://github.com/r-lib/lintr/issues/1455),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`inner_combine_linter()`](https://lintr.r-lib.org/reference/inner_combine_linter.md)
  no longer throws on length-1 calls to
  [`c()`](https://rdrr.io/r/base/c.html) like `c(exp(2))` or `c(log(3))`
  ([\#2017](https://github.com/r-lib/lintr/issues/2017),
  [@MichaelChirico](https://github.com/MichaelChirico)). Such usage is
  discouraged by
  [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md),
  but
  [`inner_combine_linter()`](https://lintr.r-lib.org/reference/inner_combine_linter.md)
  *per se* does not apply.
- [`sort_linter()`](https://lintr.r-lib.org/reference/sort_linter.md)
  only lints on [`order()`](https://rdrr.io/r/base/order.html) of a
  single vector, excluding e.g. `x[order(x, y)]` and `x[order(y, x)]`
  ([\#2156](https://github.com/r-lib/lintr/issues/2156),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`redundant_ifelse_linter()`](https://lintr.r-lib.org/reference/redundant_ifelse_linter.md)
  is aware of
  [`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)’s
  `missing=` argument, so that
  `if_else(A, TRUE, FALSE, missing = FALSE)` doesn’t lint, but
  `if_else(A, TRUE, FALSE, NA)` does
  ([\#1941](https://github.com/r-lib/lintr/issues/1941),
  [@MichaelChirico](https://github.com/MichaelChirico)). Note that
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)
  or `tidyr::replace_na()` may still be preferable.

#### Lint accuracy fixes: removing false negatives

- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  finds unreachable code even in the presence of a comment or semicolon
  after [`return()`](https://rdrr.io/r/base/function.html) or
  [`stop()`](https://rdrr.io/r/base/stop.html)
  ([\#2127](https://github.com/r-lib/lintr/issues/2127),
  [@MEO265](https://github.com/MEO265)).
- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  - finds assignments in call arguments besides the first one
    ([\#2136](https://github.com/r-lib/lintr/issues/2136),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - finds assignments in parenthetical expressions like
    `if (A && (B <- foo(A))) { }`
    ([\#2138](https://github.com/r-lib/lintr/issues/2138),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md)
  checks for cases using explicit returns,
  e.g. `lapply(x, \(xi) return(sum(xi)))`
  ([\#1567](https://github.com/r-lib/lintr/issues/1567),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - thanks to [@Bisaloo](https://github.com/Bisaloo) and
    [@strengejacke](https://github.com/strengejacke) for detecting a
    regression in the original fix
    ([\#2231](https://github.com/r-lib/lintr/issues/2231),
    [\#2247](https://github.com/r-lib/lintr/issues/2247)).

## lintr 3.1.0

CRAN release: 2023-07-19

### Deprecations & Breaking Changes

- `.lintr` files can now be kept in the directory `.github/linters` for
  better compatibility with Super-Linter. Note that this may be a
  breaking change if you already have a config in `.github/linters`
  inside a subdirectory as well as in your R project’s root, since the
  former will now be discovered first where it was ignored before.
  Please see
  [`vignette("lintr")`](https://lintr.r-lib.org/articles/lintr.md) for
  details on how configs are discovered
  ([\#1746](https://github.com/r-lib/lintr/issues/1746),
  [@tonyk7440](https://github.com/tonyk7440) and
  [@klmr](https://github.com/klmr)).
- [`single_quotes_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  is deprecated in favor of the more generalizable
  [`quotes_linter()`](https://lintr.r-lib.org/reference/quotes_linter.md)
  ([\#1729](https://github.com/r-lib/lintr/issues/1729),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- `unneeded_concatentation_linter()` is deprecated in favor of
  [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md)
  for naming consistency
  ([\#1707](https://github.com/r-lib/lintr/issues/1707),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
- [`consecutive_stopifnot_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  is deprecated in favor of the more general (see below)
  [`consecutive_assertion_linter()`](https://lintr.r-lib.org/reference/consecutive_assertion_linter.md)
  ([\#1604](https://github.com/r-lib/lintr/issues/1604),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`no_tab_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  is deprecated in favor of
  [`whitespace_linter()`](https://lintr.r-lib.org/reference/whitespace_linter.md)
  for naming consistency and future generalization
  ([\#1954](https://github.com/r-lib/lintr/issues/1954),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`available_linters()`](https://lintr.r-lib.org/reference/available_linters.md)
  prioritizes `tags` over `exclude_tags` in the case of overlap, i.e.,
  tags listed in both arguments are included, not excluded. We don’t
  expect many people to be affected by this, and the old behavior was
  not made explicit in the documentation, but make note of it here since
  it required changing a test in lintr’s own suite where
  [`linters_with_tags()`](https://lintr.r-lib.org/reference/linters_with_tags.md)
  implicitly assumed this behavior.
- [`lint()`](https://lintr.r-lib.org/reference/lint.md),
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md), and
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) no
  longer accept certain arguments (`cache=` for
  [`lint()`](https://lintr.r-lib.org/reference/lint.md),
  `relative_path=` for the latter two) positionally. The
  [`warning()`](https://rdrr.io/r/base/warning.html) since 3.0.0 has
  been upgraded to an error.

### Bug fixes

- [`linters_with_tags()`](https://lintr.r-lib.org/reference/linters_with_tags.md)
  now includes the previously missing spaces around “and” when listing
  missing linters advertised by
  [`available_linters()`](https://lintr.r-lib.org/reference/available_linters.md).
  This error message may appear e.g. when you update lintr to a version
  with new linters but don’t restart your R session
  ([\#1946](https://github.com/r-lib/lintr/issues/1946),
  [@Bisaloo](https://github.com/Bisaloo))

- [`fixed_regex_linter()`](https://lintr.r-lib.org/reference/fixed_regex_linter.md)
  is more robust to errors stemming from unrecognized escapes
  ([\#1545](https://github.com/r-lib/lintr/issues/1545),
  [\#1845](https://github.com/r-lib/lintr/issues/1845),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  can handle Sweave/Rmarkdown documents with reference chunks like
  `<<ref_file>>` ([\#779](https://github.com/r-lib/lintr/issues/779),
  [@MichaelChirico](https://github.com/MichaelChirico)). Note that these
  are simply skipped, rather than attempting to retrieve the reference
  and also lint it.

- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  no longer lints assignments in braces that include comments when
  `allow_trailing = FALSE`
  ([\#1701](https://github.com/r-lib/lintr/issues/1701),
  [@ashbaldry](https://github.com/ashbaldry))

- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)

  - No longer silently ignores usage warnings that don’t contain a
    quoted name ([\#1714](https://github.com/r-lib/lintr/issues/1714),
    [@AshesITR](https://github.com/AshesITR))
  - No longer fails on code with comments inside a multi-line call to
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    ([\#1919](https://github.com/r-lib/lintr/issues/1919),
    [@MichaelChirico](https://github.com/MichaelChirico))

- [`namespace_linter()`](https://lintr.r-lib.org/reference/namespace_linter.md)
  correctly recognizes backticked operators to be exported from
  respective namespaces (like
  [`` rlang::`%||%` ``](https://rlang.r-lib.org/reference/op-null-default.html))
  ([\#1752](https://github.com/r-lib/lintr/issues/1752),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil))

- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md)
  correctly finds a package from within a subdir if the `path` points to
  anywhere within the package
  ([\#1759](https://github.com/r-lib/lintr/issues/1759),
  [@AshesITR](https://github.com/AshesITR))

- Improved error behavior in
  [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md),
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) and
  [`xml_nodes_to_lints()`](https://lintr.r-lib.org/reference/xml_nodes_to_lints.md)
  ([\#1427](https://github.com/r-lib/lintr/issues/1427),
  [\#763](https://github.com/r-lib/lintr/issues/763),
  [@AshesITR](https://github.com/AshesITR))

  - [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md) validates
    its inputs more thoroughly, preventing errors during `print.Lints`
    like “Error in rep.int(character, length) : invalid ‘times’ value:”.
  - [`lint()`](https://lintr.r-lib.org/reference/lint.md) no longer
    tries to create an expression tree with unexpected end of input
    errors, because they can be broken.
  - [`xml_nodes_to_lints()`](https://lintr.r-lib.org/reference/xml_nodes_to_lints.md)
    warns if it can’t find lint locations and uses dummy locations as a
    fallback.

- [`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md)
  no longer erroneously marks linter factories as linters
  ([\#1725](https://github.com/r-lib/lintr/issues/1725),
  [@AshesITR](https://github.com/AshesITR)).

- Row names for
  [`available_linters()`](https://lintr.r-lib.org/reference/available_linters.md)
  data frame are now contiguous
  ([\#1781](https://github.com/r-lib/lintr/issues/1781),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  allows all S3 group Generics (see
  [`?base::groupGeneric`](https://rdrr.io/r/base/groupGeneric.html)) and
  S3 generics defined in a different file in the same package
  ([\#1808](https://github.com/r-lib/lintr/issues/1808),
  [\#1841](https://github.com/r-lib/lintr/issues/1841),
  [@AshesITR](https://github.com/AshesITR))

- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  improves identification of the exact source of a lint

  - for undefined variables in expressions with where the variable is
    used as a symbol in a usual way, for example in a formula or in an
    extraction with `$`
    ([\#1914](https://github.com/r-lib/lintr/issues/1914),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - for general usage warnings without location info
    ([\#1986](https://github.com/r-lib/lintr/issues/1986) and
    [\#1917](https://github.com/r-lib/lintr/issues/1917),
    [@AshesITR](https://github.com/AshesITR))

- [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)
  produces a more specific lint (and no longer fails) when the opening
  parenthesis is on a different line than `function` or the call name
  ([\#1953](https://github.com/r-lib/lintr/issues/1953),
  [@MichaelChirico](https://github.com/MichaelChirico)). Thanks also to
  [@IndrajeetPatil](https://github.com/IndrajeetPatil) and
  [@lorenzwalthert](https://github.com/lorenzwalthert) for identifying a
  regression in the initial fix,
  [\#1963](https://github.com/r-lib/lintr/issues/1963).

### Changes to defaults

- Set the default for the `except` argument in
  [`duplicate_argument_linter()`](https://lintr.r-lib.org/reference/duplicate_argument_linter.md)
  to `c("mutate", "transmute")`. This allows sequential updates like
  `x |> mutate(a = b + 1, a = log(a))`
  ([\#1345](https://github.com/r-lib/lintr/issues/1345),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)

  - gains `skip_with` argument to skip code in
    [`with()`](https://rdrr.io/r/base/with.html) expressions. To be
    consistent with `R CMD check`, it defaults to `TRUE`
    ([\#941](https://github.com/r-lib/lintr/issues/941),
    [\#1458](https://github.com/r-lib/lintr/issues/1458),
    [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
  - Handles backticked symbols inside {glue} expressions correctly,
    e.g. `` glue("{`x`}") `` correctly determines `x` was used
    ([\#1619](https://github.com/r-lib/lintr/issues/1619),
    [@MichaelChirico](https://github.com/MichaelChirico))
  - Detects problems inside R4.1.0+ lambda functions (`\(...)`)
    ([\#1933](https://github.com/r-lib/lintr/issues/1933),
    [@MichaelChirico](https://github.com/MichaelChirico))

- [`spaces_inside_linter()`](https://lintr.r-lib.org/reference/spaces_inside_linter.md)
  allows terminal missing keyword arguments (e.g. `alist(arg = )`;
  [\#540](https://github.com/r-lib/lintr/issues/540),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  allows empty braced expression on the same line
  (e.g. `while (updating_condition()) { }`) regardless of
  `allow_single_line` to match the corresponding behavior in {styler}.
  This is an expedient while the style guide on handling this case
  awaits clarification: <https://github.com/tidyverse/style/issues/191>.
  ([\#1346](https://github.com/r-lib/lintr/issues/1346),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md)
  and
  [`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md)
  now produce an error if empty vector of undesirable functions or
  operators is provided
  ([\#1867](https://github.com/r-lib/lintr/issues/1867),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- New linters which are also included as defaults (see “New linters” for
  more details):

  - [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  - [`quotes_linter()`](https://lintr.r-lib.org/reference/quotes_linter.md)
  - [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md)
  - [`whitespace_linter()`](https://lintr.r-lib.org/reference/whitespace_linter.md)

- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) also
  looks for files in `exec/`
  ([\#1950](https://github.com/r-lib/lintr/issues/1950),
  [@jmaspons](https://github.com/jmaspons)).

### New and improved features

- New
  [`get_r_string()`](https://lintr.r-lib.org/reference/get_r_string.md)
  helper to get the R-equivalent value of a string, especially useful
  for R-4-style raw strings. Previously an internal `lintr` helper, now
  exported to facilitate writing custom linters
  ([\#1493](https://github.com/r-lib/lintr/issues/1493),
  [@MichaelChirico](https://github.com/MichaelChirico)).

- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  improves lint metadata when detecting undefined infix operators,
  e.g. `%>%` or `:=`
  ([\#1497](https://github.com/r-lib/lintr/issues/1497),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`unused_import_linter()`](https://lintr.r-lib.org/reference/unused_import_linter.md)
  can detect datasets from imported packages and no longer warns when a
  package is imported only for its datasets
  ([\#1545](https://github.com/r-lib/lintr/issues/1545),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- When a linter triggers an error,
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) will provide a
  more actionable summary of where the error occurred, particularly
  useful for cases like
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) where
  both the responsible file and the responsible linter would be unknown
  ([@MichaelChirico](https://github.com/MichaelChirico)).

  Typically, linters should not themselves cause R to stop – syntax
  errors lead to error lints, for example. Please report such failures
  as they are likely bugs.

- [`pipe_continuation_linter()`](https://lintr.r-lib.org/reference/pipe_continuation_linter.md)
  recognizes violations involving the native R pipe `|>`
  ([\#1609](https://github.com/r-lib/lintr/issues/1609),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`paste_linter()`](https://lintr.r-lib.org/reference/paste_linter.md)
  also catches usages like `paste(rep("*", 10L), collapse = "")` that
  can be written more concisely as `strrep("*", 10L)`
  ([\#1108](https://github.com/r-lib/lintr/issues/1108),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`spaces_inside_linter()`](https://lintr.r-lib.org/reference/spaces_inside_linter.md)
  produces lints for spaces inside `[[`
  ([\#1673](https://github.com/r-lib/lintr/issues/1673),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`sprintf_linter()`](https://lintr.r-lib.org/reference/sprintf_linter.md)
  also applies to [`gettextf()`](https://rdrr.io/r/base/sprintf.html)
  ([\#1677](https://github.com/r-lib/lintr/issues/1677),
  [@MichaelChirico](https://github.com/MichaelChirico))

- Documentation for all linters contains examples of code that does and
  does not produce lints
  ([\#1492](https://github.com/r-lib/lintr/issues/1492),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`implicit_integer_linter()`](https://lintr.r-lib.org/reference/implicit_integer_linter.md)
  gains parameter `allow_colon` to skip lints on expressions like `1:10`
  ([\#1155](https://github.com/r-lib/lintr/issues/1155),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`infix_spaces_linter()`](https://lintr.r-lib.org/reference/infix_spaces_linter.md)
  supports the native R pipe `|>`
  ([\#1793](https://github.com/r-lib/lintr/issues/1793),
  [@AshesITR](https://github.com/AshesITR))

- [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md)
  (f.k.a.
  [`unneeded_concatenation_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md))
  no longer lints on `c(...)` (i.e., passing `...` in a function call)
  when `allow_single_expression = FALSE`
  ([\#1696](https://github.com/r-lib/lintr/issues/1696),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  gains parameter `regexes` to allow custom naming conventions
  ([\#822](https://github.com/r-lib/lintr/issues/822),
  [\#1421](https://github.com/r-lib/lintr/issues/1421),
  [@AshesITR](https://github.com/AshesITR))

- [`literal_coercion_linter()`](https://lintr.r-lib.org/reference/literal_coercion_linter.md)
  reports a replacement in the lint message, e.g. code like
  `as.integer(1)` will suggest using `1L` instead, and code like
  `as.numeric(NA)` will suggest using `NA_real_` instead
  ([\#1439](https://github.com/r-lib/lintr/issues/1439),
  [@MichaelChirico](https://github.com/MichaelChirico))

- Added [`format()`](https://rdrr.io/r/base/format.html) functions for
  `lint` and `lints`
  ([\#1784](https://github.com/r-lib/lintr/issues/1784),
  [@AshesITR](https://github.com/AshesITR))

- [`all_linters()`](https://lintr.r-lib.org/reference/all_linters.md)
  function provides an easy way to access all available linters
  ([\#1843](https://github.com/r-lib/lintr/issues/1843),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil))

- [`missing_argument_linter()`](https://lintr.r-lib.org/reference/missing_argument_linter.md)
  allows missing arguments in
  [`quote()`](https://rdrr.io/r/base/substitute.html) calls
  ([\#1889](https://github.com/r-lib/lintr/issues/1889),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  correctly extracts indented code chunks from R Markdown documents,
  which helps avoid spurious lints related to whitespace
  ([\#1945](https://github.com/r-lib/lintr/issues/1945),
  [@MichaelChirico](https://github.com/MichaelChirico)). The convention
  taken is that, within each chunk, all code is anchored relative to the
  leftmost non-whitespace column.

- [`available_linters()`](https://lintr.r-lib.org/reference/available_linters.md)
  gives priority to `tags` over `exclude_tags` in the case of overlap.
  In particular, this means that
  `available_linters(tags = "deprecated")` will work to return
  deprecated linters without needing to specify `exclude_tags`
  ([\#1959](https://github.com/r-lib/lintr/issues/1959),
  [@MichaelChirico](https://github.com/MichaelChirico)).

- The {lintr} configuration file is now searched in the system’s user
  configuration path; the lintr config filename can also be configured
  explicitly by setting the environment variable `R_LINTR_LINTER_FILE`
  ([\#460](https://github.com/r-lib/lintr/issues/460),
  [@klmr](https://github.com/klmr))

- Errors in the {lintr} configuration file now produce more informative
  error messages ([\#886](https://github.com/r-lib/lintr/issues/886),
  [@AshesITR](https://github.com/AshesITR))

#### New linters

- [`matrix_apply_linter()`](https://lintr.r-lib.org/reference/matrix_apply_linter.md)
  recommends use of dedicated
  [`rowSums()`](https://rdrr.io/r/base/colSums.html),
  [`colSums()`](https://rdrr.io/r/base/colSums.html),
  [`colMeans()`](https://rdrr.io/r/base/colSums.html),
  [`rowMeans()`](https://rdrr.io/r/base/colSums.html) over
  `apply(., MARGIN, sum)` or `apply(., MARGIN, mean)`. The recommended
  alternative is much more efficient and more readable
  ([\#1869](https://github.com/r-lib/lintr/issues/1869),
  [@Bisaloo](https://github.com/Bisaloo)).

- [`unnecessary_lambda_linter()`](https://lintr.r-lib.org/reference/unnecessary_lambda_linter.md):
  detect unnecessary lambdas (anonymous functions), e.g.
  `lapply(x, function(xi) sum(xi))` can be `lapply(x, sum)` and
  `purrr::map(x, ~quantile(.x, 0.75, na.rm = TRUE))` can be
  `purrr::map(x, quantile, 0.75, na.rm = TRUE)`. Naming `probs = 0.75`
  can further improve readability
  ([\#1531](https://github.com/r-lib/lintr/issues/1531),
  [\#1866](https://github.com/r-lib/lintr/issues/1866),
  [@MichaelChirico](https://github.com/MichaelChirico),
  [@Bisaloo](https://github.com/Bisaloo)).

- [`redundant_equals_linter()`](https://lintr.r-lib.org/reference/redundant_equals_linter.md)
  for redundant comparisons to `TRUE` or `FALSE` like
  `is_treatment == TRUE`
  ([\#1500](https://github.com/r-lib/lintr/issues/1500),
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`lengths_linter()`](https://lintr.r-lib.org/reference/lengths_linter.md)
  for encouraging usage of `lengths(x)` instead of `sapply(x, length)`
  (and similar)

- [`function_return_linter()`](https://lintr.r-lib.org/reference/function_return_linter.md)
  for handling issues in function
  [`return()`](https://rdrr.io/r/base/function.html) statements.
  Currently handles assignments within the
  [`return()`](https://rdrr.io/r/base/function.html) clause,
  e.g. `return(x <- foo())`
  ([@MichaelChirico](https://github.com/MichaelChirico))

- [`boolean_arithmetic_linter()`](https://lintr.r-lib.org/reference/boolean_arithmetic_linter.md)
  for identifying places where logical aggregations are more
  appropriate, e.g. `length(which(x == y)) == 0` is the same as
  `!any(x == y)` or even `all(x != y)`
  ([@MichaelChirico](https://github.com/MichaelChirico))

- [`for_loop_index_linter()`](https://lintr.r-lib.org/reference/for_loop_index_linter.md)
  to prevent overwriting local variables in a `for` loop declared like
  `for (x in x) { ... }`
  ([@MichaelChirico](https://github.com/MichaelChirico))

- [`is_numeric_linter()`](https://lintr.r-lib.org/reference/is_numeric_linter.md)
  for redundant checks equivalent to `is.numeric(x)` such as
  `is.numeric(x) || is.integer(x)` or
  `class(x) %in% c("numeric", "integer")`
  ([@MichaelChirico](https://github.com/MichaelChirico))

- [`empty_assignment_linter()`](https://lintr.r-lib.org/reference/empty_assignment_linter.md)
  for identifying empty assignments like `x = {}` that are more clearly
  written as `x = NULL`
  ([@MichaelChirico](https://github.com/MichaelChirico))

- [`unnecessary_placeholder_linter()`](https://lintr.r-lib.org/reference/unnecessary_placeholder_linter.md)
  for identifying where usage of the {magrittr} placeholder `.` could be
  omitted ([@MichaelChirico](https://github.com/MichaelChirico))

- [`routine_registration_linter()`](https://lintr.r-lib.org/reference/routine_registration_linter.md)
  for identifying native routines that don’t use registration
  (`useDynLib` in the `NAMESPACE`;
  [@MichaelChirico](https://github.com/MichaelChirico))

- [`indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.md)
  for checking that the indentation conforms to 2-space Tidyverse-style
  ([@AshesITR](https://github.com/AshesITR) and
  [@dgkf](https://github.com/dgkf),
  [\#1411](https://github.com/r-lib/lintr/issues/1411),
  [\#1792](https://github.com/r-lib/lintr/issues/1792),
  [\#1898](https://github.com/r-lib/lintr/issues/1898)).

- [`unnecessary_nested_if_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  for checking unnecessary nested `if` statements where a single `if`
  statement with appropriate conditional expression would suffice
  ([@IndrajeetPatil](https://github.com/IndrajeetPatil) and
  [@AshesITR](https://github.com/AshesITR),
  [\#1778](https://github.com/r-lib/lintr/issues/1778)).

- [`implicit_assignment_linter()`](https://lintr.r-lib.org/reference/implicit_assignment_linter.md)
  for checking implicit assignments in function calls
  ([@IndrajeetPatil](https://github.com/IndrajeetPatil) and
  [@AshesITR](https://github.com/AshesITR),
  [\#1777](https://github.com/r-lib/lintr/issues/1777)).

- [`quotes_linter()`](https://lintr.r-lib.org/reference/quotes_linter.md)
  is a generalized version of (now deprecated)
  [`single_quotes_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md).
  It accepts an argument `delimiter` to specify whether `"` or `'`
  should be the accepted method for delimiting character literals. The
  default, `"`, reflects the Tidyverse style guide recommendation and
  matches the behavior of
  [`single_quotes_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md).

- [`unnecessary_concatenation_linter()`](https://lintr.r-lib.org/reference/unnecessary_concatenation_linter.md)
  is simply
  [`unneeded_concatenation_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  renamed.

- [`consecutive_assertion_linter()`](https://lintr.r-lib.org/reference/consecutive_assertion_linter.md)
  (f.k.a.
  [`consecutive_stopifnot_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md))
  now lints for consecutive calls to `assertthat::assert_that()` (as
  long as the `msg=` argument is not used;
  [\#1604](https://github.com/r-lib/lintr/issues/1604),
  [@MichaelChirico](https://github.com/MichaelChirico)).

- [`whitespace_linter()`](https://lintr.r-lib.org/reference/whitespace_linter.md)
  is simply
  [`no_tab_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md),
  renamed. In the future, we plan to extend it to work for different
  whitespace preferences.

### Notes

- {lintr} now depends on R version 3.5.0, in line with the tidyverse
  policy for R version compatibility.

- [`lint()`](https://lintr.r-lib.org/reference/lint.md) continues to
  support Rmarkdown documents. For users of custom .Rmd engines, e.g.
  `marginformat` from {tufte} or `theorem` from {bookdown}, note that
  those engines must be registered in {knitr} prior to running
  [`lint()`](https://lintr.r-lib.org/reference/lint.md) in order for
  {lintr} to behave as expected, i.e., they should be shown as part of
  `knitr::knit_engines$get()`.

  For {tufte} and {bookdown} in particular, one only needs to load the
  package namespace to accomplish this (i.e., minimally
  `loadNamespace("tufte")` or `loadNamespace("bookdown")`, respectively,
  will register those packages’ custom engines; since
  [`library()`](https://rdrr.io/r/base/library.html) also runs
  [`loadNamespace()`](https://rdrr.io/r/base/ns-load.html), running
  [`library()`](https://rdrr.io/r/base/library.html) will also work).
  Note further that {tufte} only added this code to their `.onLoad()`
  recently after our request to do so (see
  <https://github.com/rstudio/tufte/issues/117>). Therefore, ensure
  you’re using a more recent version to get the behavior described here
  for {tufte}.

  More generally, there is no requirement that
  [`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) will register
  a package’s custom {knitr} engines, so you may need to work with other
  package authors to figure out a solution for other engines.

  Thanks to Yihui and other developers for their helpful discussions
  around this issue ([\#797](https://github.com/r-lib/lintr/issues/797),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- The output of [`lint()`](https://lintr.r-lib.org/reference/lint.md)
  and [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md) gain S3
  class `"list"` to assist with S3 dispatch
  ([\#1494](https://github.com/r-lib/lintr/issues/1494),
  [@MichaelChirico](https://github.com/MichaelChirico))

  - As a corollary, we now register an `as_tibble` method for class
    `lints`, conditional on {tibble} availability, to avoid dispatching
    to the `list` method which does not work with
    [`lint()`](https://lintr.r-lib.org/reference/lint.md) output
    ([\#1997](https://github.com/r-lib/lintr/issues/1997),
    [@MichaelChirico](https://github.com/MichaelChirico))

- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  gives a more helpful warning when a `glue()` expression fails to
  evaluate ([\#1985](https://github.com/r-lib/lintr/issues/1985),
  [@MichaelChirico](https://github.com/MichaelChirico))

- The documentation of
  [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  now describes how `"symbols"` works when passed to the `styles`
  parameter ([\#1924](https://github.com/r-lib/lintr/issues/1924),
  [@hedsnz](https://github.com/hedsnz)).

## lintr 3.0.2

CRAN release: 2022-10-19

- Fix test to avoid leaving behind cache files in the global cache
  directory.

## lintr 3.0.1

CRAN release: 2022-09-13

- Skip multi-byte tests in non UTF-8 locales
  ([\#1504](https://github.com/r-lib/lintr/issues/1504))

- [`modify_defaults()`](https://lintr.r-lib.org/reference/modify_defaults.md)
  no longer uses the mistaken `"lintr_function"` S3 class, instead
  applying the `"linter"` class also common to
  [`Linter()`](https://lintr.r-lib.org/reference/Linter.md).
  [`Linter()`](https://lintr.r-lib.org/reference/Linter.md) also
  includes `"function"` in the S3 class of its output to facilitate S3
  dispatch to `function` methods where appropriate
  ([\#1392](https://github.com/r-lib/lintr/issues/1392),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Changes to defaults

- [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  allows opening curly braces on a new line when there is a comment
  ending the preceding line
  ([\#1433](https://github.com/r-lib/lintr/issues/1433) and
  [\#1434](https://github.com/r-lib/lintr/issues/1434),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md)
  produces lint for `seq(...)`, since it also cannot properly handle
  empty edge cases
  ([\#1468](https://github.com/r-lib/lintr/issues/1468),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md)
  additionally lints on `1:n()` (from {dplyr}) and `1:.N` (from
  {data.table}) ([\#1396](https://github.com/r-lib/lintr/issues/1396),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`literal_coercion_linter()`](https://lintr.r-lib.org/reference/literal_coercion_linter.md)
  lints {rlang}’s atomic vector constructors (i.e., `int()`, `chr()`,
  `lgl()`, and `dbl()`) if the argument is a scalar
  ([\#1437](https://github.com/r-lib/lintr/issues/1437),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`redundant_ifelse_linter()`](https://lintr.r-lib.org/reference/redundant_ifelse_linter.md)’s
  lint message correctly suggests negation when the `yes` condition is
  `0` ([\#1432](https://github.com/r-lib/lintr/issues/1432),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md)
  provides more specific replacement code in lint message
  ([\#1475](https://github.com/r-lib/lintr/issues/1475),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

### New and improved features

- New
  [`sort_linter()`](https://lintr.r-lib.org/reference/sort_linter.md) to
  detect `x[order(x)]` and recommend the faster and clearer alternative:
  `sort(x)` ([\#1528](https://github.com/r-lib/lintr/issues/1528),
  [@Bisaloo](https://github.com/Bisaloo))

- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  ignores trailing comments if they match a closing nolint block
  ([\#1347](https://github.com/r-lib/lintr/issues/1347),
  [@AshesITR](https://github.com/AshesITR)).

- New
  [`function_argument_linter()`](https://lintr.r-lib.org/reference/function_argument_linter.md)
  to enforce that arguments with defaults appear last in function
  declarations, see the [Tidyverse design
  guide](https://design.tidyverse.org/required-no-defaults.html)
  ([\#450](https://github.com/r-lib/lintr/issues/450),
  [@AshesITR](https://github.com/AshesITR)).

- New `allow_trailing` argument added to
  [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  to check when assignment operators are at the end of a line, and the
  value is on the following line
  ([\#1491](https://github.com/r-lib/lintr/issues/1491),
  [@ashbaldry](https://github.com/ashbaldry))

- New
  [`sarif_output()`](https://lintr.r-lib.org/reference/sarif_output.md)
  function to output lints to SARIF output
  ([\#1424](https://github.com/r-lib/lintr/issues/1424),
  [@shaopeng-gh](https://github.com/shaopeng-gh))

- [`commented_code_linter()`](https://lintr.r-lib.org/reference/commented_code_linter.md)
  now lints commented argument code, containing a trailing comma, as
  well ([\#386](https://github.com/r-lib/lintr/issues/386),
  [@AshesITR](https://github.com/AshesITR)). For example a comment
  containing `# na.rm = TRUE,` now triggers a lint.

### Bug fixes

- [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  does not fail in case there are dependencies with no exports
  (e.g. data-only packages)
  ([\#1424](https://github.com/r-lib/lintr/issues/1424),
  [\#1509](https://github.com/r-lib/lintr/issues/1509),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  no longer fails on R files that match a knitr pattern
  ([\#743](https://github.com/r-lib/lintr/issues/743),
  [\#879](https://github.com/r-lib/lintr/issues/879),
  [\#1406](https://github.com/r-lib/lintr/issues/1406),
  [@AshesITR](https://github.com/AshesITR)).
- Parse error lints now appear with the linter name `"error"` instead of
  `NA` ([\#1405](https://github.com/r-lib/lintr/issues/1405),
  [@AshesITR](https://github.com/AshesITR)). Also, linting no longer
  runs if the `source_expressions` contain invalid string data that
  would cause error messages in other linters. in other linters.
- Prevent [`lint()`](https://lintr.r-lib.org/reference/lint.md) from
  hanging on Rmd files with some syntax errors
  ([\#1443](https://github.com/r-lib/lintr/issues/1443),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  no longer omits trailing non-code lines from knitr files
  ([\#1400](https://github.com/r-lib/lintr/issues/1400),
  [\#1415](https://github.com/r-lib/lintr/issues/1415),
  [@AshesITR](https://github.com/AshesITR)). This fixes the location
  information for
  [`trailing_blank_lines_linter()`](https://lintr.r-lib.org/reference/trailing_blank_lines_linter.md)
  in RMarkdown documents without terminal newlines.
- The [`vignette("lintr")`](https://lintr.r-lib.org/articles/lintr.md)
  incorrectly cited `exclude` as the key for setting file exclusions in
  `.lintr` when it is actually `exclusions`.
  ([\#1401](https://github.com/r-lib/lintr/issues/1401),
  [@AshesITR](https://github.com/AshesITR))
- Fixed file exclusion detection in
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) so it no
  longer errors if there are multiple exclusions or no global exclusions
  configured for a single file
  ([\#1413](https://github.com/r-lib/lintr/issues/1413),
  [\#1442](https://github.com/r-lib/lintr/issues/1442),
  [@AshesITR](https://github.com/AshesITR)).

### Other changes

- The minimum needed version for soft dependency
  [withr](https://withr.r-lib.org) has been bumped to `2.5.0`
  ([\#1404](https://github.com/r-lib/lintr/issues/1404),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).
- Changed the deprecation warning for `with_defaults()` to also mention
  [`modify_defaults()`](https://lintr.r-lib.org/reference/modify_defaults.md)
  ([\#1438](https://github.com/r-lib/lintr/issues/1438),
  [@AshesITR](https://github.com/AshesITR)).
- Quarto files (`.qmd`) were supported out of the box. The documentation
  and the testing infrastructure are updated to reflect this
  ([\#1486](https://github.com/r-lib/lintr/issues/1486),
  [@IndrajeetPatil](https://github.com/IndrajeetPatil)).

## lintr 3.0.0

CRAN release: 2022-06-13

### Breaking changes

- All linters are now function factories (i.e., functions that return
  functions) for consistency. Previously, only linters with customizable
  parameters were factories
  ([\#245](https://github.com/r-lib/lintr/issues/245),
  [@fangly](https://github.com/fangly),
  [@AshesITR](https://github.com/AshesITR), and
  [@MichaelChirico](https://github.com/MichaelChirico)).

  This means that usage such as `lint("file.R", seq_linter)` should be
  updated to `lint("file.R", seq_linter())`, and the following update
  for custom linters:

  ``` r
  my_custom_linter <- function(source_expression) { ... }

  # becomes
  my_custom_linter <- function() Linter(function(source_expression) { ... })
  ```

- Exclusions specified in the `.lintr` file are now relative to the
  location of that file and support excluding entire directories
  ([\#158](https://github.com/r-lib/lintr/issues/158),
  [\#438](https://github.com/r-lib/lintr/issues/438),
  [@AshesITR](https://github.com/AshesITR)).

- Removed long-deprecated linters (they’ve been marked as deprecated
  since v1.0.1 in 2017):

  - `absolute_paths_linter()`
  - `camel_case_linter()`
  - `multiple_dots_linter()`
  - `snake_case_linter()`
  - `trailing_semicolons_linter()`

- Removed [`return()`](https://rdrr.io/r/base/function.html) from
  `all_undesirable_functions` because early returns (which often improve
  readability and reduce code complexity) require explicit use of
  [`return()`](https://rdrr.io/r/base/function.html). Follow
  [\#1100](https://github.com/r-lib/lintr/issues/1100) for an upcoming
  [`return_linter()`](https://lintr.r-lib.org/reference/return_linter.md)
  to lint unnecessary [`return()`](https://rdrr.io/r/base/function.html)
  statements ([\#1146](https://github.com/r-lib/lintr/issues/1146),
  [@AshesITR](https://github.com/AshesITR)).

  Note that you can replicate old behavior by supplying `return` as a
  custom undesirable function:
  `undesirable_function_linter(c(all_undesirable_functions, list(return = NA)))`

### Deprecations

- Lints are now marked with the name of the `linter` that caused them
  instead of the name of their implementation function. Deprecated the
  obsolete `linter` argument of
  [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md)
  ([\#664](https://github.com/r-lib/lintr/issues/664),
  [\#673](https://github.com/r-lib/lintr/issues/673),
  [\#746](https://github.com/r-lib/lintr/issues/746),
  [@AshesITR](https://github.com/AshesITR)). Downstream custom linters
  should follow suit.
- Renamed `semicolon_terminator_linter()` to
  [`semicolon_linter()`](https://lintr.r-lib.org/reference/semicolon_linter.md)
  for better consistency. `semicolon_terminator_linter()` survives but
  is marked for deprecation. The new linter also has a new signature,
  taking arguments `allow_compound` and `allow_trailing` to replace the
  old single argument `semicolon`, again for signature consistency with
  other linters.
- The following linters were subsumed into
  [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  and are now deprecated; see the item on
  [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  below:
  - `closed_curly_linter()`
  - `open_curly_linter()`
  - `paren_brace_linter()`
- The `...` argument for
  [`lint()`](https://lintr.r-lib.org/reference/lint.md),
  [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md), and
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) has been
  promoted to an earlier position to better match the [Tidyverse design
  principle](https://design.tidyverse.org/required-no-defaults.html) of
  data-\>descriptor-\>details. This change enables passing objects to
  `...` without needing to specify non-required arguments,
  e.g. `lint_dir("/path/to/dir", linter())` now works without the need
  to specify `relative_path`. This affects some code that uses
  positional arguments
  ([\#935](https://github.com/r-lib/lintr/issues/935),
  [@MichaelChirico](https://github.com/MichaelChirico)).
  - For [`lint()`](https://lintr.r-lib.org/reference/lint.md), `...` is
    now the 3rd argument, where earlier this was `cache`.
  - For [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) and
    [`lint_package()`](https://lintr.r-lib.org/reference/lint.md), `...`
    is now the 2nd argument, where earlier this was `relative_path`.
- Deprecated argument `source_file` to exported functions
  [`with_id()`](https://lintr.r-lib.org/reference/ids_with_token.md) and
  [`ids_with_token()`](https://lintr.r-lib.org/reference/ids_with_token.md).
  It has been renamed to `source_expression` to better reflect that this
  argument is typically the output of
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md).
  For now, the old argument `source_file` can still be used (with
  warning). The now-private functional versions of many linters also
  underwent the same renaming (`source_file` -\> `source_expression`).
  This has no direct effect on packages importing lintr, but is
  mentioned in case custom linters imitating `lintr` style had also
  adopted the `source_file` naming and want to adapt to keep in sync.
- Deprecated `with_defaults()` in favor of
  [`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md),
  and add
  [`modify_defaults()`](https://lintr.r-lib.org/reference/modify_defaults.md)
  which is intended to be used more generally to modify (i.e., extend,
  trim, and/or update) a list of defaults. Note that the argument
  corresponding to `with_defaults()`’s `default=` is called `defaults=`
  (i.e., pluralized) in both of these, and that usage like
  `with_defaults(default = NULL, ...)` should be converted to
  `linters_with_defaults(defaults = list(), ...)`
  ([\#1029](https://github.com/r-lib/lintr/issues/1029),
  [\#1336](https://github.com/r-lib/lintr/issues/1336),
  [\#1361](https://github.com/r-lib/lintr/issues/1361),
  [@AshesITR](https://github.com/AshesITR) and
  [@michaelchirico](https://github.com/michaelchirico)).
- Deprecated the `find_line()` and `find_column()` helpers from the
  item-level `expressions` returned with
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md).
  These helpers were typically associated with regex-based logic for
  building linters, which is rarely needed and prone to false positives;
  now that lintr almost exclusively uses XPath-based logic for linters,
  these are no longer necessary
  ([\#1373](https://github.com/r-lib/lintr/issues/1373),
  [@MichaelChirico](https://github.com/MichaelChirico)).

### Other changes to defaults

#### Updates to `default_linters`

- New
  [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
  which combines several curly brace related linters, deprecating the
  following predecessors
  ([\#1041](https://github.com/r-lib/lintr/issues/1041),
  [@AshesITR](https://github.com/AshesITR)):
  - `closed_curly_linter()`; both now also allow `}]` in addition to
    `})` and `},` as exceptions, i.e., `}` doesn’t need to be on its own
    line if paired with a closing square bracket, a closing parenthesis,
    or a comma. Also improved lint metadata so that source markers land
    at the closing brace instead of the closing parenthesis to improve
    the experience of fixing the lint
    ([\#583](https://github.com/r-lib/lintr/issues/583),
    [@AshesITR](https://github.com/AshesITR)).
  - `open_curly_linter()`; both also no longer lint unnecessary trailing
    whitespace (use
    [`trailing_whitespace_linter()`](https://lintr.r-lib.org/reference/trailing_whitespace_linter.md)
    for this) and also allow `(`, `,`, and `%>%` on preceding lines as
    exceptions, i.e., `{` can be alone on a line if the previous line is
    terminated with an opening parenthesis, a comma, or a pipe (`%>%`)
    ([\#487](https://github.com/r-lib/lintr/issues/487),
    [\#1028](https://github.com/r-lib/lintr/issues/1028),
    [@AshesITR](https://github.com/AshesITR)).
  - `paren_brace_linter()`;
    [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
    also lints `if`/`else` and `repeat` with missing whitespace.
  - [`brace_linter()`](https://lintr.r-lib.org/reference/brace_linter.md)
    also newly enforces the following rules surrounding curly braces
    (originally Google linters, see below):
    - Require `else` to come on the same line as the preceding `}`, if
      present ([\#884](https://github.com/r-lib/lintr/issues/884),
      [@MichaelChirico](https://github.com/MichaelChirico)).
    - Require functions spanning multiple lines to use curly braces
      ([\#987](https://github.com/r-lib/lintr/issues/987),
      [@MichaelChirico](https://github.com/MichaelChirico)).
    - Require balanced usage of
      [`{}`](https://rdrr.io/r/base/Paren.html) in `if`/`else`
      conditions, i.e., if the `if` branch uses braces, then so must the
      `else` branch, and *vice versa*
      ([\#983](https://github.com/r-lib/lintr/issues/983),
      [@MichaelChirico](https://github.com/MichaelChirico)).
- New
  [`paren_body_linter()`](https://lintr.r-lib.org/reference/paren_body_linter.md)
  checks that there is a space between a right parenthesis and a body
  expression ([\#809](https://github.com/r-lib/lintr/issues/809),
  [@kpagacz](https://github.com/kpagacz)).
- Added
  [`semicolon_linter()`](https://lintr.r-lib.org/reference/semicolon_linter.md)
  as a default because it enforces a tidyverse style guide rule
  ([\#683](https://github.com/r-lib/lintr/issues/683),
  [@AshesITR](https://github.com/AshesITR)).
- [`assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.md)
  ([\#915](https://github.com/r-lib/lintr/issues/915),
  [@MichaelChirico](https://github.com/MichaelChirico)):
  - Right assignments are now linted by default (`->` and `->>`).
  - New argument `allow_cascading_assign` (`TRUE` by default) toggles
    whether to lint `<<-` and `->>`.
  - New argument `allow_right_assign` (`FALSE` by default) toggles
    whether to lint `->` and `->>`.
- [`commented_code_linter()`](https://lintr.r-lib.org/reference/commented_code_linter.md):
  use the parse tree to find comments, eliminating some false positives
  ([\#451](https://github.com/r-lib/lintr/issues/451),
  [@AshesITR](https://github.com/AshesITR)).
- [`equals_na_linter()`](https://lintr.r-lib.org/reference/equals_na_linter.md)
  ([\#545](https://github.com/r-lib/lintr/issues/545),
  [@MichaelChirico](https://github.com/MichaelChirico)):
  - Extended to lint `x != NA` (before, only `==` was caught) and
    `NA == x` (before, only `NA` on RHS was caught).
  - Extended to skip usages in comments like
    `is.na(x) # use is.na(x), not x == NA`.
- [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md):
  improved location information
  ([\#1266](https://github.com/r-lib/lintr/issues/1266),
  [\#1267](https://github.com/r-lib/lintr/issues/1267),
  [@AshesITR](https://github.com/AshesITR)).
- [`infix_spaces_linter()`](https://lintr.r-lib.org/reference/infix_spaces_linter.md):
  - Added argument `allow_multiple_spaces` (`TRUE` by default) which
    toggles whether to generate a lint for operators used with multiple
    spaces, e.g. `x + 2`. The default setting allows extra spacing to be
    used to increase line-to-line alignment
    ([\#940](https://github.com/r-lib/lintr/issues/940),
    [@f-ritter](https://github.com/f-ritter) and
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Extended so that usages like `a~b` and `function(a=1) { ... }` are
    linted ([\#930](https://github.com/r-lib/lintr/issues/930),
    \#michaelchirico).
  - Added argument `exclude_operators` to disable lints on selected
    infix operators. By default, all “low-precedence” operators throw
    lints; see
    [`?infix_spaces_linter`](https://lintr.r-lib.org/reference/infix_spaces_linter.md)
    for an enumeration of these.
    ([\#914](https://github.com/r-lib/lintr/issues/914),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Add an exception for `/` usage in `box::use()` declarations
    ([\#1087](https://github.com/r-lib/lintr/issues/1087),
    [@klmr](https://github.com/klmr)).
- [`line_length_linter()`](https://lintr.r-lib.org/reference/line_length_linter.md):
  place the source marker at the margin of the affected line to improve
  user experience during de-linting – just press Return
  ([\#735](https://github.com/r-lib/lintr/issues/735),
  [@AshesITR](https://github.com/AshesITR)).\*
- [`no_tab_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md):
  use more reliable matching (e.g., excluding matches found in comments;
  [\#441](https://github.com/r-lib/lintr/issues/441),
  [@russHyde](https://github.com/russHyde)).
- [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md):
  correctly detect generics and only count the implementation class
  towards the length. This prevents false positive lints in the case of
  long generic names, e.g.
  `very_very_very_long_generic_name.short_class` no longer produces a
  lint ([\#871](https://github.com/r-lib/lintr/issues/871),
  [@AshesITR](https://github.com/AshesITR)).
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md):
  - Improved generic detection – in user-defined method
    `my_method.upstream.class`, `upstream.class` no longer throws a lint
    because the generic (`my_method`) properly uses `snake_case`
    ([\#737](https://github.com/r-lib/lintr/issues/737),
    [@AshesITR](https://github.com/AshesITR)).
  - Exclude special R namespace hook functions such as `.onLoad()`
    ([\#500](https://github.com/r-lib/lintr/issues/500),
    [\#614](https://github.com/r-lib/lintr/issues/614),
    [@AshesITR](https://github.com/AshesITR) and
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Correctly detect imported functions when linting packages
    ([\#642](https://github.com/r-lib/lintr/issues/642),
    [@AshesITR](https://github.com/AshesITR)).
  - Correctly detect assignment generics like `names<-.class_name`
    ([\#843](https://github.com/r-lib/lintr/issues/843),
    [@jonkeane](https://github.com/jonkeane)).
  - Added new styles `"symbols"` and `"SNAKE_CASE"`
    ([\#494](https://github.com/r-lib/lintr/issues/494),
    [\#495](https://github.com/r-lib/lintr/issues/495),
    [\#615](https://github.com/r-lib/lintr/issues/615),
    [\#670](https://github.com/r-lib/lintr/issues/670),
    [@MichaelChirico](https://github.com/MichaelChirico) and
    [@AshesITR](https://github.com/AshesITR)).
    - `"symbols"` is a new default style which won’t lint all-symbol
      object names. In particular, that means operator names like `%+%`
      are allowed.
  - No longer lints names used in `$` extractions
    ([\#582](https://github.com/r-lib/lintr/issues/582),
    [@AshesITR](https://github.com/AshesITR)).
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md):
  - Detect global variables if there are top-level dollar-assignments
    ([\#666](https://github.com/r-lib/lintr/issues/666),
    [@AshesITR](https://github.com/AshesITR)).
  - Report usage warnings spanning multiple lines
    ([\#507](https://github.com/r-lib/lintr/issues/507),
    [@AshesITR](https://github.com/AshesITR)).
  - Detect usages inside
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    constructs ([\#942](https://github.com/r-lib/lintr/issues/942),
    [@AshesITR](https://github.com/AshesITR)).
  - Extended to include functions assigned with `=` instead of `<-`
    ([\#1081](https://github.com/r-lib/lintr/issues/1081),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Detect functions exported by packages that are explicitly attached
    using [`library()`](https://rdrr.io/r/base/library.html) or
    [`require()`](https://rdrr.io/r/base/library.html) calls
    ([\#1127](https://github.com/r-lib/lintr/issues/1127),
    [@AshesITR](https://github.com/AshesITR)).
  - Improved location information in some cases where the previous
    regex-based approach didn’t work, e.g. unicode characters in
    variable names
    ([\#1285](https://github.com/r-lib/lintr/issues/1285),
    [@AshesITR](https://github.com/AshesITR)).
  - Correctly detect functions declared within
    [`assign()`](https://rdrr.io/r/base/assign.html) and `setMethod()`
    ([\#1322](https://github.com/r-lib/lintr/issues/1322),
    [@AshesITR](https://github.com/AshesITR)).
- [`spaces_inside_linter()`](https://lintr.r-lib.org/reference/spaces_inside_linter.md):
  ignore spaces preceding trailing comments
  ([\#636](https://github.com/r-lib/lintr/issues/636),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md):
  - Added as a default because it enforces a tidyverse style guide rule
    ([\#517](https://github.com/r-lib/lintr/issues/517),
    [@AshesITR](https://github.com/AshesITR)).
  - No longer lint occurrences of `T` and `F` when used for subsetting,
    and give a better message when used as variable names
    ([\#657](https://github.com/r-lib/lintr/issues/657),
    [@AshesITR](https://github.com/AshesITR)).
- [`trailing_blank_lines_linter()`](https://lintr.r-lib.org/reference/trailing_blank_lines_linter.md):
  - Extended to lint files without a terminal newline
    ([\#675](https://github.com/r-lib/lintr/issues/675),
    [@AshesITR](https://github.com/AshesITR)).
  - Also, running [`lint()`](https://lintr.r-lib.org/reference/lint.md)
    on a file without a terminal newline no longer throws a
    [`warning()`](https://rdrr.io/r/base/warning.html).
- [`trailing_whitespace_linter()`](https://lintr.r-lib.org/reference/trailing_whitespace_linter.md):
  - Extended to also lint completely blank lines by default
    ([\#1044](https://github.com/r-lib/lintr/issues/1044),
    [@AshesITR](https://github.com/AshesITR)).
  - Added argument `allow_empty_lines` (`FALSE` by default) to toggle
    this behavior.
  - Improved so that trailing whitespace inside string literals does not
    trigger a lint
    ([\#1045](https://github.com/r-lib/lintr/issues/1045),
    [@AshesITR](https://github.com/AshesITR)).
  - Added argument `allow_in_strings` (`TRUE` by default) to toggle this
    behavior.
- [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md):
  - Added new functions to `default_undesirable_functions` related to
    debugging ([\#876](https://github.com/r-lib/lintr/issues/876),
    [@MichaelChirico](https://github.com/MichaelChirico)):
    - [`browser()`](https://rdrr.io/r/base/browser.html)
    - [`debug()`](https://rdrr.io/r/base/debug.html)
    - [`debugcall()`](https://rdrr.io/r/utils/debugcall.html)
    - [`debugonce()`](https://rdrr.io/r/base/debug.html)
    - [`trace()`](https://rdrr.io/r/base/trace.html)
    - [`untrace()`](https://rdrr.io/r/base/trace.html)
  - No longer lints [`library()`](https://rdrr.io/r/base/library.html)
    and [`require()`](https://rdrr.io/r/base/library.html) calls
    attaching a package with an undesired name,
    e.g. [`library(foo)`](https://rdrr.io/r/base/library.html)
    ([\#814](https://github.com/r-lib/lintr/issues/814),
    [@kpagacz](https://github.com/kpagacz) and
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - No longer lints undesirable symbols if they are used as names in `$`
    extractions ([\#1050](https://github.com/r-lib/lintr/issues/1050),
    [@AshesITR](https://github.com/AshesITR)).
  - Added more explanation why certain functions might be undesirable
    and what alternatives to use; ditto for
    [`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md)
    ([\#1133](https://github.com/r-lib/lintr/issues/1133),
    [\#1146](https://github.com/r-lib/lintr/issues/1146),
    [\#1159](https://github.com/r-lib/lintr/issues/1159),
    [@AshesITR](https://github.com/AshesITR)).

#### Other noteworthy changes

- [`cyclocomp_linter()`](https://lintr.r-lib.org/reference/cyclocomp_linter.md):
  set the default `complexity_limit` to 15. This brings the default into
  sync with what is enforced via `default_linters`
  ([\#693](https://github.com/r-lib/lintr/issues/693),
  [@AshesITR](https://github.com/AshesITR)).
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) now
  lints files in the `demo` directory by default
  ([\#703](https://github.com/r-lib/lintr/issues/703),
  [@dmurdoch](https://github.com/dmurdoch)).
- Moved the default lintr cache directory from `~/.R/lintr_cache` (which
  was a violation of CRAN policy) to `R_user_dir("lintr", "cache")`.
  Note that 3.0.0 is a major version update and invalidates the old
  cache anyway, so it can be safely deleted
  ([\#1062](https://github.com/r-lib/lintr/issues/1062),
  [@AshesITR](https://github.com/AshesITR)).

### New and improved features

#### New linters

- [`backport_linter()`](https://lintr.r-lib.org/reference/backport_linter.md)
  for detecting mismatched R version dependencies
  ([\#506](https://github.com/r-lib/lintr/issues/506),
  [\#1316](https://github.com/r-lib/lintr/issues/1316),
  [\#1318](https://github.com/r-lib/lintr/issues/1318),
  [\#1319](https://github.com/r-lib/lintr/issues/1319),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@AshesITR](https://github.com/AshesITR)).
- [`duplicate_argument_linter()`](https://lintr.r-lib.org/reference/duplicate_argument_linter.md)
  similarly checks that there are no duplicate arguments supplied to
  function calls ([\#850](https://github.com/r-lib/lintr/issues/850),
  [@renkun-ken](https://github.com/renkun-ken)).
- [`missing_argument_linter()`](https://lintr.r-lib.org/reference/missing_argument_linter.md)
  to check for empty (missing) arguments in function calls
  ([\#563](https://github.com/r-lib/lintr/issues/563),
  [\#1152](https://github.com/r-lib/lintr/issues/1152),
  [@renkun-ken](https://github.com/renkun-ken) and
  [@AshesITR](https://github.com/AshesITR)).
- [`missing_package_linter()`](https://lintr.r-lib.org/reference/missing_package_linter.md)
  to check if packages in calls to
  [`library()`](https://rdrr.io/r/base/library.html) and friends are
  missing ([\#536](https://github.com/r-lib/lintr/issues/536),
  [\#1037](https://github.com/r-lib/lintr/issues/1037),
  [@renkun-ken](https://github.com/renkun-ken) and
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`namespace_linter()`](https://lintr.r-lib.org/reference/namespace_linter.md)
  to check for common mistakes in `pkg::symbol` usages
  ([\#548](https://github.com/r-lib/lintr/issues/548),
  [@renkun-ken](https://github.com/renkun-ken)).
- [`package_hooks_linter()`](https://lintr.r-lib.org/reference/package_hooks_linter.md)
  to run a series of checks also done by `R CMD check` on the
  `.onLoad()`, `.onAttach()`, `.Last.lib()` and `.onDetach()` hooks
  ([\#882](https://github.com/r-lib/lintr/issues/882),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`pipe_call_linter()`](https://lintr.r-lib.org/reference/pipe_call_linter.md)
  to enforce that all steps of `magrittr` pipelines use explicit calls
  instead of symbols, e.g. `x %>% mean()` instead of `x %>% mean`
  ([\#801](https://github.com/r-lib/lintr/issues/801),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`sprintf_linter()`](https://lintr.r-lib.org/reference/sprintf_linter.md)
  to check for common mistakes in
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) usage
  ([\#544](https://github.com/r-lib/lintr/issues/544),
  [\#624](https://github.com/r-lib/lintr/issues/624),
  [@renkun-ken](https://github.com/renkun-ken) and
  [@AshesITR](https://github.com/AshesITR)).
- [`unused_import_linter()`](https://lintr.r-lib.org/reference/unused_import_linter.md)
  to detect unnecessary
  [`library()`](https://rdrr.io/r/base/library.html) calls in R scripts
  ([\#239](https://github.com/r-lib/lintr/issues/239),
  [@jimhester](https://github.com/jimhester),
  [@AshesITR](https://github.com/AshesITR)).

##### Google linters

Google is a heavy user of lintr internally, and has developed a large
set of linters improving code consistency and correcting common R usage
mistakes. This release includes many of these linters that are of
general interest to the broader R community. More will be included in
future releases. See, e.g.
[\#884](https://github.com/r-lib/lintr/issues/884),
[\#979](https://github.com/r-lib/lintr/issues/979),
[\#998](https://github.com/r-lib/lintr/issues/998),
[\#1011](https://github.com/r-lib/lintr/issues/1011),
[\#1016](https://github.com/r-lib/lintr/issues/1016),
[\#1036](https://github.com/r-lib/lintr/issues/1036),
[\#1051](https://github.com/r-lib/lintr/issues/1051),
[\#1066](https://github.com/r-lib/lintr/issues/1066), and
[\#1067](https://github.com/r-lib/lintr/issues/1067); special thanks to
[@MichaelChirico](https://github.com/MichaelChirico) and
[@michaelquinn32](https://github.com/michaelquinn32).

- [`any_duplicated_linter()`](https://lintr.r-lib.org/reference/any_duplicated_linter.md)
  Require usage of `anyDuplicated(x) > 0L` over `any(duplicated(x))` and
  similar.
- [`any_is_na_linter()`](https://lintr.r-lib.org/reference/any_is_na_linter.md)
  Require usage of `anyNA(x)` over `any(is.na(x))`.
- [`class_equals_linter()`](https://lintr.r-lib.org/reference/class_equals_linter.md)
  Prevent comparing `class(x)` with `==`, `!=`, or `%in%`, where
  [`inherits()`](https://rdrr.io/r/base/class.html) is typically
  preferred.
- [`condition_message_linter()`](https://lintr.r-lib.org/reference/condition_message_linter.md)
  Prevent condition messages from being constructed like
  `stop(paste(...))` (where just `stop(...)` is preferable).
- [`conjunct_test_linter()`](https://lintr.r-lib.org/reference/conjunct_test_linter.md)
  Require usage of `expect_true(x); expect_true(y)` over
  `expect_true(x && y)` and similar.
- [`consecutive_stopifnot_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  Require consecutive calls to
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) to be unified
  into one.
- [`expect_comparison_linter()`](https://lintr.r-lib.org/reference/expect_comparison_linter.md)
  Require usage of `expect_gt(x, y)` over `expect_true(x > y)` and
  similar.
- [`expect_identical_linter()`](https://lintr.r-lib.org/reference/expect_identical_linter.md)
  Require usage of `expect_identical()` by default, and `expect_equal()`
  only by exception.
- [`expect_length_linter()`](https://lintr.r-lib.org/reference/expect_length_linter.md)
  Require usage of `expect_length(x, n)` over
  `expect_equal(length(x), n)` and similar.
- [`expect_named_linter()`](https://lintr.r-lib.org/reference/expect_named_linter.md)
  Require usage of `expect_named(x, n)` over `expect_equal(names(x), n)`
  and similar.
- [`expect_not_linter()`](https://lintr.r-lib.org/reference/expect_not_linter.md)
  Require usage of `expect_false(x)` over `expect_true(!x)`, and *vice
  versa*.
- [`expect_null_linter()`](https://lintr.r-lib.org/reference/expect_null_linter.md)
  Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and
  similar.
- [`expect_s3_class_linter()`](https://lintr.r-lib.org/reference/expect_s3_class_linter.md)
  Require usage of `expect_s3_class(x, k)` over
  `expect_equal(class(x), k)` and similar.
- [`expect_s4_class_linter()`](https://lintr.r-lib.org/reference/expect_s4_class_linter.md)
  Require usage of `expect_s4_class(x, k)` over
  `expect_true(methods::is(x, k))`.
- [`expect_true_false_linter()`](https://lintr.r-lib.org/reference/expect_true_false_linter.md)
  Require usage of `expect_true(x)` over `expect_equal(x, TRUE)` and
  similar.
- [`expect_type_linter()`](https://lintr.r-lib.org/reference/expect_type_linter.md)
  Require usage of `expect_type(x, t)` over `expect_equal(typeof(x), t)`
  and similar.
- [`fixed_regex_linter()`](https://lintr.r-lib.org/reference/fixed_regex_linter.md)
  Require `fixed = TRUE` or
  [`stringr::fixed()`](https://stringr.tidyverse.org/reference/modifiers.html)
  for regular expressions that can be expressed statically,
  e.g. `strsplit(x, "[.]")` can be `strsplit(x, ".", fixed = TRUE)`.
  - Added parameter `allow_grepl` (default `FALSE`) to toggle whether
    [`grepl()`](https://rdrr.io/r/base/grep.html) usages should be
    linted. These might be treated separately because `grepl("^x", NA)`
    is `FALSE`; the
    [`startsWith()`](https://rdrr.io/r/base/startsWith.html) equivalent
    to get `FALSE` for missing input is clunkier, but more explicit:
    `!is.na(x) & startsWith(x, string)`
    ([\#1376](https://github.com/r-lib/lintr/issues/1376),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`ifelse_censor_linter()`](https://lintr.r-lib.org/reference/ifelse_censor_linter.md)
  Require usage of [`pmax()`](https://rdrr.io/r/base/Extremes.html) /
  [`pmin()`](https://rdrr.io/r/base/Extremes.html) where appropriate,
  e.g. `ifelse(x > y, x, y)` is `pmax(x, y)`.
- [`inner_combine_linter()`](https://lintr.r-lib.org/reference/inner_combine_linter.md)
  Require inputs to known-vectorized functions to be combined first
  rather than later, e.g. `as.Date(c(x, y))` over
  `c(as.Date(x), as.Date(y))`.
- [`literal_coercion_linter()`](https://lintr.r-lib.org/reference/literal_coercion_linter.md)
  Require using correctly-typed literals instead of direct coercion,
  e.g. `1L` instead of `as.numeric(1)`.
- [`nested_ifelse_linter()`](https://lintr.r-lib.org/reference/nested_ifelse_linter.md)
  Prevent nested calls to
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) like
  `ifelse(A, x, ifelse(B, y, z))`, and similar.
- [`numeric_leading_zero_linter()`](https://lintr.r-lib.org/reference/numeric_leading_zero_linter.md)
  Require a leading `0` in fractional numeric constants, e.g. `0.1`
  instead of `.1`.
- [`outer_negation_linter()`](https://lintr.r-lib.org/reference/outer_negation_linter.md)
  Require usage of `!any(x)` over `all(!x)` and `!all(x)` over
  `any(!x)`.
- [`paste_linter()`](https://lintr.r-lib.org/reference/paste_linter.md)
  lint for common mis-use of
  [`paste()`](https://rdrr.io/r/base/paste.html) and
  [`paste0()`](https://rdrr.io/r/base/paste.html):
  - [`paste0()`](https://rdrr.io/r/base/paste.html) encouraged instead
    of `paste(sep = "")`.
  - [`toString()`](https://rdrr.io/r/base/toString.html) or
    [`glue::glue_collapse()`](https://glue.tidyverse.org/reference/glue_collapse.html)
    encouraged instead of `paste(x, collapse = ", ")`.
  - Lint `sep=` passed to
    [`paste0()`](https://rdrr.io/r/base/paste.html) – typically a
    mistake.
- [`redundant_ifelse_linter()`](https://lintr.r-lib.org/reference/redundant_ifelse_linter.md)
  Prevent usage like `ifelse(A & B, TRUE, FALSE)` or `ifelse(C, 0, 1)`
  (the latter is `as.numeric(!C)`).
- [`regex_subset_linter()`](https://lintr.r-lib.org/reference/regex_subset_linter.md)
  Require usage of `grep(ptn, x, value = TRUE)` over `x[grep(ptn, x)]`
  and similar.
- [`string_boundary_linter()`](https://lintr.r-lib.org/reference/string_boundary_linter.md)
  Require usage of `startsWith(x, ptn)` over `grepl("^ptn", x)` or
  `substr(x, 1, 3) == ptn` and similar.
- [`strings_as_factors_linter()`](https://lintr.r-lib.org/reference/strings_as_factors_linter.md)
  Check for code designed to work before and after the
  `stringsAsFactors = FALSE` default change in R 4.0 by examining code
  for [`data.frame()`](https://rdrr.io/r/base/data.frame.html) usages
  susceptible to assumptions about the default value of
  `stringsAsFactors=`.
- [`system_file_linter()`](https://lintr.r-lib.org/reference/system_file_linter.md)
  Prevent usage like `file.path(system.file("A", package = "pkg"), "B")`
  where simply `system.file("A", "B", package = "pkg")` is more concise
  and readable.
- [`unreachable_code_linter()`](https://lintr.r-lib.org/reference/unreachable_code_linter.md)
  Prevent code after [`return()`](https://rdrr.io/r/base/function.html)
  and [`stop()`](https://rdrr.io/r/base/stop.html) statements that will
  never be reached (extended for
  [\#1051](https://github.com/r-lib/lintr/issues/1051) thanks to early
  user testing, thanks
  [@bersbersbers](https://github.com/bersbersbers)!).
- [`vector_logic_linter()`](https://lintr.r-lib.org/reference/vector_logic_linter.md)
  Require use of scalar logical operators (`&&` and `||`) inside `if()`
  conditions and similar.
- [`yoda_test_linter()`](https://lintr.r-lib.org/reference/yoda_test_linter.md)
  Require usage of `expect_identical(x, 1L)` over `expect_equal(1L, x)`
  and similar.

#### Other features and improvements

- **Documentation**: Reorganize linter documentation into new tag-based
  Rd pages ([\#888](https://github.com/r-lib/lintr/issues/888),
  [\#1015](https://github.com/r-lib/lintr/issues/1015),
  [@AshesITR](https://github.com/AshesITR)).
  - Each linter has its own help page.
  - [`?linters`](https://lintr.r-lib.org/reference/linters.md) also
    links to tag help pages, collecting linters with a similar goal.
  - Each linter can have multiple tags.
  - [`available_linters()`](https://lintr.r-lib.org/reference/available_linters.md):
    new function to list available linters and their tags. This feature
    is extensible by package authors providing add-on linters for
    {lintr}.
  - [`available_tags()`](https://lintr.r-lib.org/reference/available_linters.md):
    new function to list available tags.
  - [`linters_with_tags()`](https://lintr.r-lib.org/reference/linters_with_tags.md):
    new function to help build a list of linters using tags.
- **Encodings**: lintr now supports non-system character Encodings. The
  correct encoding is auto-detected from .Rproj or DESCRIPTION files in
  your project. Override the default in the `encoding` setting of lintr
  ([\#752](https://github.com/r-lib/lintr/issues/752),
  [\#782](https://github.com/r-lib/lintr/issues/782),
  [@AshesITR](https://github.com/AshesITR)).
- **Jenkins CI**: Support for writing comments to GitHub repo when
  running in Jenkins CI
  ([\#488](https://github.com/r-lib/lintr/issues/488),
  [@fdlk](https://github.com/fdlk)).
- **Performance**: Optimized performance-critical functions in lintr,
  such as
  [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  resulting in about 2x speedup in our test suite and even more for
  complex files ([\#1169](https://github.com/r-lib/lintr/issues/1169),
  [\#1197](https://github.com/r-lib/lintr/issues/1197),
  [\#1200](https://github.com/r-lib/lintr/issues/1200),
  [\#1201](https://github.com/r-lib/lintr/issues/1201),
  [\#1214](https://github.com/r-lib/lintr/issues/1214),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@AshesITR](https://github.com/AshesITR)). Average
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md)
  execution time is down about 30% and the median package sees about 40%
  improvement.
- **Raw strings**: Several linters tightened internal logic to allow for
  raw strings like `R"( a\string )"`
  ([\#1034](https://github.com/r-lib/lintr/issues/1034),
  [\#1285](https://github.com/r-lib/lintr/issues/1285),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@AshesITR](https://github.com/AshesITR)).
- **Selective exclusion syntax**: New syntax to exclude only selected
  linters from certain lines or passages. Use
  `# nolint: linter_name, linter2_name.` or
  `# nolint start: linter_name, linter2_name.` in source files or named
  lists of line numbers in `.lintr`. Note the terminal `.` is required.
  Also allows for partial matching as long as the supplied prefix is
  unique, e.g. `# nolint: infix_spaces.` works to exclude
  `infix_spaces_linter`
  ([\#605](https://github.com/r-lib/lintr/issues/605),
  [\#872](https://github.com/r-lib/lintr/issues/872),
  [@AshesITR](https://github.com/AshesITR)).
  - Added the linter name to lintrs output to facilitate discovery of
    the correct name
    ([\#1357](https://github.com/r-lib/lintr/issues/1357),
    [@AshesITR](https://github.com/AshesITR)).
- Improved S3 generic detection for non-standard S3 generics where
  [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html) is called after
  several preceding expressions
  ([\#846](https://github.com/r-lib/lintr/issues/846),
  [@jonkeane](https://github.com/jonkeane)).
- [`extraction_operator_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md):
  no longer lint `x[NULL]`
  ([\#1273](https://github.com/r-lib/lintr/issues/1273),
  [@AshesITR](https://github.com/AshesITR)).
- [`is_lint_level()`](https://lintr.r-lib.org/reference/is_lint_level.md):
  new exported helper for readably explaining which type of expression
  is required for a custom linter. Some linters are written to require
  the full file’s parse tree (for example,
  [`single_quotes_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)).
  Others only need single expressions, which is more cache-friendly
  (most linters are written this way to leverage caching)
  ([\#921](https://github.com/r-lib/lintr/issues/921),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) excludes the
  `renv` and `packrat` directories by default
  ([\#697](https://github.com/r-lib/lintr/issues/697),
  [@AshesITR](https://github.com/AshesITR)).
- [`lint()`](https://lintr.r-lib.org/reference/lint.md): new optional
  argument `text` for supplying a line or lines directly, e.g. if the
  file is already in memory or linting is being done *ad hoc*
  ([\#503](https://github.com/r-lib/lintr/issues/503),
  [@renkun-ken](https://github.com/renkun-ken)).
- [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md):
  improve lint message to be clearer about the reason for linting
  ([\#522](https://github.com/r-lib/lintr/issues/522),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- [`unneeded_concatenation_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md):
  - Correctly considers arguments in pipelines (`%>%` or `|>`;
    [\#573](https://github.com/r-lib/lintr/issues/573),
    [\#1270](https://github.com/r-lib/lintr/issues/1270),
    [@michaelquinn32](https://github.com/michaelquinn32) and
    [@AshesITR](https://github.com/AshesITR)).
  - New argument `allow_single_expression`, default `TRUE`, toggling
    whether `c(x)` should be linted, i.e., a call to
    [`c()`](https://rdrr.io/r/base/c.html) with only one entry which is
    not a constant. In some such cases,
    [`c()`](https://rdrr.io/r/base/c.html) can simply be dropped,
    e.g. `c(a:b)`; in others, the parentheses are still needed,
    e.g. `-c(a:b)` should be `-(a:b)`; and in still others,
    [`c()`](https://rdrr.io/r/base/c.html) is used for the side-effect
    of stripping attributes, e.g. `c(factor(letters))` or
    `c(matrix(1:10, 5, 2))`. In this last case,
    [`c()`](https://rdrr.io/r/base/c.html) can (and should) in most
    cases be replaced by
    [`as.vector()`](https://rdrr.io/r/base/vector.html) or
    [`as.integer()`](https://rdrr.io/r/base/integer.html) for
    readability. In fact, we suspect it is *always* preferable to do so,
    and may change the default to `allow_single_expression = FALSE` in
    the future. Please report your use case if
    [`as.vector()`](https://rdrr.io/r/base/vector.html) does not suit
    your needs ([\#1344](https://github.com/r-lib/lintr/issues/1344),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`use_lintr()`](https://lintr.r-lib.org/reference/use_lintr.md): new
  exported helper for creating a minimal `.lintr` configuration
  ([\#902](https://github.com/r-lib/lintr/issues/902),
  [@AshesITR](https://github.com/AshesITR)).
- [`xml_nodes_to_lints()`](https://lintr.r-lib.org/reference/xml_nodes_to_lints.md):
  new exported helper for converting `xml_node` objects obtained using
  linter logic expressed in XPath into `Lint` objects
  ([\#1124](https://github.com/r-lib/lintr/issues/1124),
  [\#1216](https://github.com/r-lib/lintr/issues/1216),
  [\#1234](https://github.com/r-lib/lintr/issues/1234),
  [@MichaelChirico](https://github.com/MichaelChirico) and
  [@AshesITR](https://github.com/AshesITR)).

### Bug fixes

- **RStudio**: Source markers are cleared when there are no lints
  ([\#520](https://github.com/r-lib/lintr/issues/520),
  [@AshesITR](https://github.com/AshesITR)).
- Error message for mismatched starts and ends of exclusion ranges is
  now more helpful. ([\#571](https://github.com/r-lib/lintr/issues/571),
  [\#860](https://github.com/r-lib/lintr/issues/860),
  [@AshesITR](https://github.com/AshesITR) and
  [@danielinteractive](https://github.com/danielinteractive)).
- Improved location information for R parse errors
  ([\#894](https://github.com/r-lib/lintr/issues/894),
  [\#892](https://github.com/r-lib/lintr/issues/892),
  [@renkun-ken](https://github.com/renkun-ken) and
  [@AshesITR](https://github.com/AshesITR)).
- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md):
  - Fix possible error on invalid XML produced by
    [`xmlparsedata::xml_parse_data()`](https://rdrr.io/pkg/xmlparsedata/man/xml_parse_data.html)
    ([\#559](https://github.com/r-lib/lintr/issues/559),
    [@renkun-ken](https://github.com/renkun-ken)).
  - Fix handling zero-length variable name error
    ([\#566](https://github.com/r-lib/lintr/issues/566),
    [@renkun-ken](https://github.com/renkun-ken)).
  - Malformed Rmd files now cause a lint instead of an error
    ([\#571](https://github.com/r-lib/lintr/issues/571),
    [@AshesITR](https://github.com/AshesITR)).
  - No longer fails if
    [`getParseData()`](https://rdrr.io/r/utils/getParseData.html)
    returns a truncated (invalid) Unicode character as parsed text
    ([\#815](https://github.com/r-lib/lintr/issues/815),
    [@leogama](https://github.com/leogama)).
  - Fixes the `text` value for `STR_CONST` nodes involving 1- or 2-width
    octal escapes (e.g. `"\1"`) to account for an R parser bug
    (<https://bugs.r-project.org/show_bug.cgi?id=18323>;
    [\#1056](https://github.com/r-lib/lintr/issues/1056),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Handle Rmd inputs containing unevaluated code blocks with named
    format specifiers
    ([\#472](https://github.com/r-lib/lintr/issues/472),
    [@russHyde](https://github.com/russHyde)).
- [`line_length_linter()`](https://lintr.r-lib.org/reference/line_length_linter.md):
  fix a bug causing duplicate lints for lines containing multiple
  expressions ([\#681](https://github.com/r-lib/lintr/issues/681),
  [@AshesITR](https://github.com/AshesITR)).
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md):
  - Warns and returns `NULL` if no package is found (instead of giving a
    peculiar error message;
    [\#776](https://github.com/r-lib/lintr/issues/776),
    [@MichaelChirico](https://github.com/MichaelChirico)).
  - Stricter about what is considered to be a package – folders named
    `DESCRIPTION` are ignored
    ([\#702](https://github.com/r-lib/lintr/issues/702),
    [@MichaelChirico](https://github.com/MichaelChirico)).
- [`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md)
  (formerly `with_defaults()`):
  - No longer duplicates the `lintr_function` class when it is already
    present ([\#511](https://github.com/r-lib/lintr/issues/511),
    [@AshesITR](https://github.com/AshesITR)).
  - Warns if a named argument is `NULL` but its name is not in
    `defaults` ([\#1049](https://github.com/r-lib/lintr/issues/1049),
    [@AshesITR](https://github.com/AshesITR)).
- [`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md)
  handles automatic naming of very long arguments correctly
  ([\#774](https://github.com/r-lib/lintr/issues/774),
  [@MichaelChirico](https://github.com/MichaelChirico)).
- `save_cache()` will now recursively create the cache directory; this
  avoids errors that could arise if any parent directories do not exist
  ([\#60](https://github.com/r-lib/lintr/issues/60),
  [@dankessler](https://github.com/dankessler)).
- [`spaces_left_parentheses_linter()`](https://lintr.r-lib.org/reference/spaces_left_parentheses_linter.md):
  fix a bug causing warnings like “In
  `parent == parent[before_operator_idx]` longer object length is not a
  multiple of shorter object length” in nested expressions
  ([\#654](https://github.com/r-lib/lintr/issues/654),
  [@AshesITR](https://github.com/AshesITR)).

### Internals

- Added a new, more restrictive test workflow - `test-package` - that
  fails on warnings emitted by tests
  ([\#1263](https://github.com/r-lib/lintr/issues/1263),
  [\#1272](https://github.com/r-lib/lintr/issues/1272),
  [@AshesITR](https://github.com/AshesITR)).
- Added a secondary, more restrictive lint workflow -
  `lint-changed-files` - for newly written / modified code
  ([\#641](https://github.com/r-lib/lintr/issues/641),
  [@dragosmg](https://github.com/dragosmg)).
- Several optional `Imported` packages have become `Suggested`
  dependencies: `httr`, `testthat`, and `rstudioapi`. This should allow
  snappier CI builds for usages not relying on some more “peripheral”
  features of the package.
- Special thanks to [@bersbersbers](https://github.com/bersbersbers) for
  early testing on the 3.0.0 changes.
- Switched CI from Travis to GitHub Actions, using the full tidyverse
  recommended `R CMD check`. Code coverage and linting are implemented
  using separate GitHub Actions workflows
  ([\#572](https://github.com/r-lib/lintr/issues/572),
  [@dragosmg](https://github.com/dragosmg)).
- Updated R CMD GitHub Actions workflow to check for R 3.6 on Ubuntu,
  instead of R 3.3, and for R 4.0 on Windows, instead of R 3.6
  ([\#803](https://github.com/r-lib/lintr/issues/803), @ dragosmg).
- `lintr` now uses the 3rd edition of `testthat`
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [@AshesITR](https://github.com/AshesITR),
  [\#910](https://github.com/r-lib/lintr/issues/910),
  [\#967](https://github.com/r-lib/lintr/issues/967)).

## lintr 2.0.1

CRAN release: 2020-02-19

### New features

- lintr now supports GitHub Actions and will print the lints as warning
  messages if lints are printed during an action.
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) will now
  lint vignettes and data-raw by default
  ([\#447](https://github.com/r-lib/lintr/issues/447),
  [@AshesITR](https://github.com/AshesITR)).
- [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) will now
  include Rmd and Rnw files by default
  ([@AshesITR](https://github.com/AshesITR)).

### Minor fixes and features

- `single_quote_linter()` no longer causes a print issue when open quote
  appears at a column \> than close quote
  ([\#457](https://github.com/r-lib/lintr/issues/457),
  [@jamieRowen](https://github.com/jamieRowen))
- [`absolute_path_linter()`](https://lintr.r-lib.org/reference/absolute_path_linter.md)
  and
  [`nonportable_path_linter()`](https://lintr.r-lib.org/reference/nonportable_path_linter.md)
  now handle file-paths that are wrapped with double-quotes
  ([\#433](https://github.com/r-lib/lintr/issues/433),
  [\#437](https://github.com/r-lib/lintr/issues/437),
  [@russHyde](https://github.com/russHyde)).
- [`get_source_expressions()`](https://lintr.r-lib.org/reference/get_source_expressions.md)
  has been changed to handle `expr_or_assign_or_help` tokens arising
  when parsing code containing equals-assignments in R-devel
  ([\#403](https://github.com/r-lib/lintr/issues/403),
  [\#456](https://github.com/r-lib/lintr/issues/456),
  [@russHyde](https://github.com/russHyde)).
- `object_usage_linter` has been changed to ensure lint-position is
  indicated relative to the start of the file, rather than the start of
  a defining function
  ([\#432](https://github.com/r-lib/lintr/issues/432),
  [@russHyde](https://github.com/russHyde)).
- `commas_linter` now allows spaces to come before a comma when used to
  denote a fall-through in a switch statement
  ([\#499](https://github.com/r-lib/lintr/issues/499),
  [@MrMallIronmaker](https://github.com/MrMallIronmaker))

## lintr 2.0.0

CRAN release: 2019-10-01

lintr 2.0.0 is a major release, and incorporates development changes
since the last major release (1.0.0) in 2016-04-16.

### Deprecated functions

- Deprecated `camel_case_linter()`, `snake_case_linter()` and
  `multiple_dots_linter()` in favor of
  [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  which enforce the given style: snake_case, dotted.case,
  lowerCamelCalse, UpperCamelCase, alllowercase or ALLUPPERCASE
  ([\#59](https://github.com/r-lib/lintr/issues/59),
  [@fangly](https://github.com/fangly)).
- Deprecated absolute_paths_linter() in favor of the new
  [`absolute_path_linter()`](https://lintr.r-lib.org/reference/absolute_path_linter.md),
  with a lax mode for fewer false positive lints
  ([\#199](https://github.com/r-lib/lintr/issues/199),
  [@fangly](https://github.com/fangly)).

### New linters

- New
  [`cyclocomp_linter()`](https://lintr.r-lib.org/reference/cyclocomp_linter.md)
  identifies overly complex functions
  ([\#361](https://github.com/r-lib/lintr/issues/361),
  [@fabian-s](https://github.com/fabian-s))
- New
  [`equals_na_linter()`](https://lintr.r-lib.org/reference/equals_na_linter.md)
  ([\#143](https://github.com/r-lib/lintr/issues/143),
  [\#326](https://github.com/r-lib/lintr/issues/326),
  [@jabranham](https://github.com/jabranham))
- New
  [`extraction_operator_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  checks that the `[[` operator is used when extracting a single element
  from an object, not `[` (subsetting) nor `$` (interactive use)
  ([@fangly](https://github.com/fangly)).
- New
  [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)
  to check that there is no space between a function name and its left
  parentheses ([\#204](https://github.com/r-lib/lintr/issues/204),
  [@jrnold](https://github.com/jrnold)).
- New
  [`implicit_integer_linter()`](https://lintr.r-lib.org/reference/implicit_integer_linter.md)
  detects round numbers not declared as integers, i.e. 1 instead of 1L
  ([@fangly](https://github.com/fangly)).
- New
  [`nonportable_path_linter()`](https://lintr.r-lib.org/reference/nonportable_path_linter.md)
  identifies paths constructed without file.path()
  ([@fangly](https://github.com/fangly)).
- New `paren_brace_linter()` checks that there is a space between right
  parenthesis and an opening curly brace
  ([@bfgray3](https://github.com/bfgray3),
  [\#242](https://github.com/r-lib/lintr/issues/242)).
- New
  [`pipe_continuation_linter()`](https://lintr.r-lib.org/reference/pipe_continuation_linter.md)
  to ensure there is a space before %\>% and newline afterwards
  ([\#216](https://github.com/r-lib/lintr/issues/216)).
- New `semicolon_terminator_linter()` reports semicolons at the end of a
  line ([\#147](https://github.com/r-lib/lintr/issues/147),
  1.  and between expressions
      ([\#181](https://github.com/r-lib/lintr/issues/181),
      [@fangly](https://github.com/fangly)).
- New [`seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.md),
  finds `1:length(...)` (and similar) expressions
  ([\#155](https://github.com/r-lib/lintr/issues/155), 1)
- New
  [`todo_comment_linter()`](https://lintr.r-lib.org/reference/todo_comment_linter.md)
  lints TODOs ([@fangly](https://github.com/fangly)).
- New
  [`T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md)
  warns when using T and F instead of TRUE and FALSE
  ([@fangly](https://github.com/fangly)).
- New
  [`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md)
  and
  [`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md)
  lint uses of user-specified functions and operators
  ([\#48](https://github.com/r-lib/lintr/issues/48),
  [\#149](https://github.com/r-lib/lintr/issues/149),
  [@fangly](https://github.com/fangly)).
- New
  [`unneeded_concatenation_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  lints uses of c() with a constant or no arguments
  ([@fangly](https://github.com/fangly)).

### New functions for writing linters

- Export
  [`expect_lint()`](https://lintr.r-lib.org/reference/expect_lint.md)
  ([\#178](https://github.com/r-lib/lintr/issues/178),
  [\#210](https://github.com/r-lib/lintr/issues/210))
- Export
  [`ids_with_token()`](https://lintr.r-lib.org/reference/ids_with_token.md)
  and [`with_id()`](https://lintr.r-lib.org/reference/ids_with_token.md)
  ([\#297](https://github.com/r-lib/lintr/issues/297)
  [@stufield](https://github.com/stufield))
- linters can use the XML parse tree as well now, via the
  <https://github.com/MangoTheCat/xmlparsedata> package
  ([\#154](https://github.com/r-lib/lintr/issues/154), 1)

### New functions for users

- New [`lint_dir()`](https://lintr.r-lib.org/reference/lint.md) function
  to lint files under a given directory
  ([@arekbee](https://github.com/arekbee),
  [\#360](https://github.com/r-lib/lintr/issues/360))
- New `summary.lints()` function to summarize the linter results
  ([\#260](https://github.com/r-lib/lintr/issues/260),
  [\#262](https://github.com/r-lib/lintr/issues/262),
  [@wlandau](https://github.com/wlandau)).
- New
  [`checkstyle_output()`](https://lintr.r-lib.org/reference/checkstyle_output.md)
  function to output lints to checkstyle XML output
  ([\#156](https://github.com/r-lib/lintr/issues/156),
  [@joshkgold](https://github.com/joshkgold))

### Linter fixes

- `closed_curly_linter()` now allows closing parenthesis or comma after
  closing curly brace
  ([\#167](https://github.com/r-lib/lintr/issues/167),
  [@Enchufa2](https://github.com/Enchufa2))
- [`commas_linter()`](https://lintr.r-lib.org/reference/commas_linter.md)
  now handles missing arguments calls properly
  ([\#145](https://github.com/r-lib/lintr/issues/145))
- [`commented_code_linter()`](https://lintr.r-lib.org/reference/commented_code_linter.md)
  now relaxed, it no longer lints comments within roxygen blocks and
  does not consider “-” an R operator to avoid too many false positives.
- [`function_left_parentheses_linter()`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)
  now allows spaces if a function starts with a left parenthesis
  ([\#311](https://github.com/r-lib/lintr/issues/311))
- [`no_tab_linter()`](https://lintr.r-lib.org/reference/lintr-deprecated.md)
  now reports proper line in all cases
  ([\#134](https://github.com/r-lib/lintr/issues/134),
  [@fangly](https://github.com/fangly))
- [`object_length_linter()`](https://lintr.r-lib.org/reference/object_length_linter.md)
  argument `length` now defaults to 30 for consistency
  ([\#325](https://github.com/r-lib/lintr/issues/325)
  [@DragosMG](https://github.com/DragosMG))
- [`object_name_linter()`](https://lintr.r-lib.org/reference/object_name_linter.md)
  now works when passed multiple styles
  ([\#341](https://github.com/r-lib/lintr/issues/341),
  [@infotroph](https://github.com/infotroph))
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  has been changed to better detect lexical scoping of global variables
  ([\#27](https://github.com/r-lib/lintr/issues/27),
  [\#336](https://github.com/r-lib/lintr/issues/336),
  [\#91](https://github.com/r-lib/lintr/issues/91),
  [\#382](https://github.com/r-lib/lintr/issues/382))
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  now respects
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html),
  so it can be used to avoid false positive warnings due to non-standard
  evaluation ([\#352](https://github.com/r-lib/lintr/issues/352))
- [`object_usage_linter()`](https://lintr.r-lib.org/reference/object_usage_linter.md)
  now ignores top level calls that contain function definitions
  ([\#26](https://github.com/r-lib/lintr/issues/26)).
- `object_linter*()`s now only lint objects declared in the current file
  ([\#76](https://github.com/r-lib/lintr/issues/76),
  [\#108](https://github.com/r-lib/lintr/issues/108),
  [\#136](https://github.com/r-lib/lintr/issues/136),
  [\#191](https://github.com/r-lib/lintr/issues/191),
  [\#194](https://github.com/r-lib/lintr/issues/194),
  [\#201](https://github.com/r-lib/lintr/issues/201),
  [@fangly](https://github.com/fangly)).
- `open_curly_linter()` and `closed_curly_linter()` now do not lint
  double curly syntax
  ([\#388](https://github.com/r-lib/lintr/issues/388))
- `open_curly_linter()` now allows comments after the curly braces
  ([\#188](https://github.com/r-lib/lintr/issues/188))
- [`pipe_continuation_linter()`](https://lintr.r-lib.org/reference/pipe_continuation_linter.md)
  now behaves better in nested expressions, functions etc.
  ([\#366](https://github.com/r-lib/lintr/issues/366)
  [@russHyde](https://github.com/russHyde))
- `space_inside_linter()` now reports proper line and column numbers
  ([\#203](https://github.com/r-lib/lintr/issues/203),
  [@fangly](https://github.com/fangly))

### General improvements and fixes

- [`expect_lint()`](https://lintr.r-lib.org/reference/expect_lint.md)
  now no longer shows Rstudio markers and error messages are correctly
  preserved ([\#180](https://github.com/r-lib/lintr/issues/180),
  [\#211](https://github.com/r-lib/lintr/issues/211),
  [@fangly](https://github.com/fangly))
- [`Lint()`](https://lintr.r-lib.org/reference/lint-s3.md) /
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) error
  now fixed ([\#179](https://github.com/r-lib/lintr/issues/179),
  [@fangly](https://github.com/fangly)).
- [`lint()`](https://lintr.r-lib.org/reference/lint.md) no longer errors
  with inline `\\Sexpr`
  ([\#127](https://github.com/r-lib/lintr/issues/127)).
- [`lint()`](https://lintr.r-lib.org/reference/lint.md) no longer errors
  with ‘\<% %\>’ constructs
  ([\#185](https://github.com/r-lib/lintr/issues/185)).
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) now
  works with the cache, as intended
  ([\#146](https://github.com/r-lib/lintr/issues/146),
  [@schloerke](https://github.com/schloerke))
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) now
  excludes `R/RcppExports.R` by default
  ([\#282](https://github.com/r-lib/lintr/issues/282))
- [`lint_package()`](https://lintr.r-lib.org/reference/lint.md) now
  removes fully excluded files as soon as possible
- lintr now looks up its configuration in any parent directories as well
  as the package directory
  ([\#238](https://github.com/r-lib/lintr/issues/238),
  [\#345](https://github.com/r-lib/lintr/issues/345))
- `seq_linter` is now one of the default linters
  ([\#316](https://github.com/r-lib/lintr/issues/316)).
- Fix issue in lintr’s compatibility with R-devel, due to to a new
  version of the PCRE library
  ([\#411](https://github.com/r-lib/lintr/issues/411).)
- [`read_settings()`](https://lintr.r-lib.org/reference/read_settings.md)
  now has a better error message when the config file does not end with
  a newline ([\#160](https://github.com/r-lib/lintr/issues/160),
  [\#189](https://github.com/r-lib/lintr/issues/189))
- [`expect_lint_free()`](https://lintr.r-lib.org/reference/expect_lint_free.md)
  is now automatically skipped when run on covr
  ([\#287](https://github.com/r-lib/lintr/issues/287))
- Now lintr only tries to generate comments if running in wercker or
  travis CI ([\#166](https://github.com/r-lib/lintr/issues/166))
- Add support for overriding GitHub API Token via `GITHUB_TOKEN`
  environment variable
  ([\#63](https://github.com/r-lib/lintr/issues/63),
  [@mattyb](https://github.com/mattyb))
- Config files are now also searched for in the users’ home directory
  ([\#266](https://github.com/r-lib/lintr/issues/266),
  [@randy3k](https://github.com/randy3k))
- Fixed crash caused by ambiguous cache file paths
  ([\#212](https://github.com/r-lib/lintr/issues/212),
  [@fangly](https://github.com/fangly)).
- RStudio addins to lint current source and project (fixes
  [\#264](https://github.com/r-lib/lintr/issues/264),
  [@JhossePaul](https://github.com/JhossePaul))
- Added proper handling of tab characters (fixes
  [\#44](https://github.com/r-lib/lintr/issues/44),
  [@fangly](https://github.com/fangly))
- lintr does not need the igraph package any more
  ([\#152](https://github.com/r-lib/lintr/issues/152), 1)
- Fixed cache not saved in a directory other than requested
  ([\#213](https://github.com/r-lib/lintr/issues/213),
  [@fangly](https://github.com/fangly)) avoid reading and pre-processing
  of ignored files ([@mwaldstein](https://github.com/mwaldstein))
- Allow for any number of `#` to start a comment. Useful in ESS
  ([\#299](https://github.com/r-lib/lintr/issues/299),
  [@prosoitos](https://github.com/prosoitos))
- R Markdown files that do not contain chunks are no longer treated as
  code ([\#370](https://github.com/r-lib/lintr/issues/370)).
- Fixed plain-code-block bug in Rmarkdown
  ([\#252](https://github.com/r-lib/lintr/issues/252),
  [@russHyde](https://github.com/russHyde))
- Fixed bug where non-R chunks using {lang} `engine format` were parsed
  from R-markdown ([\#322](https://github.com/r-lib/lintr/issues/322),
  [@russHyde](https://github.com/russHyde))
- Ensured `lintr` runs / installs / tests on R-3.6: pinned to github
  `xmlparsedata`; ensure vectors are length-1 when compared using `&&`
  and `||` ([\#363](https://github.com/r-lib/lintr/issues/363)
  [\#377](https://github.com/r-lib/lintr/issues/377)
  [\#384](https://github.com/r-lib/lintr/issues/384)
  [\#391](https://github.com/r-lib/lintr/issues/391),
  [@russHyde](https://github.com/russHyde)).

## lintr 1.0.3

CRAN release: 2018-11-08

- Fix tests to work with changes in the parser in R 3.6

## lintr 1.0.2

CRAN release: 2017-11-08

- Fix tests to work with upcoming testthat release.

## lintr 1.0.1

CRAN release: 2017-08-10

- bugfix to work with knitr 1.16.7
- [`expect_lint_free()`](https://lintr.r-lib.org/reference/expect_lint_free.md)
  now is always skipped on CRAN. This is necessary because the
  non-binary R source may not be available when running tests on CRAN,
  and those tests may not be run in the package directory.

## lintr 1.0.0

CRAN release: 2016-04-16

- bugfix to work with testthat 1.0.0

## lintr 0.3.3

CRAN release: 2015-09-15

- infix_spaces_linter now properly checks `=` in named arguments.
  ([\#130](https://github.com/r-lib/lintr/issues/130),
  [@saurfang](https://github.com/saurfang)).
- commas_linter now properly recognizes lints when preceded by a blank
  line and points to the missing space rather than the comma
  ([\#111](https://github.com/r-lib/lintr/issues/111),
  [\#129](https://github.com/r-lib/lintr/issues/129),
  [@saurfang](https://github.com/saurfang)).
- Make spaces_left_parentheses_linter more robust when determining `(`
  type ([\#128](https://github.com/r-lib/lintr/issues/128),
  [@saurfang](https://github.com/saurfang))
- commented_code_linter
  ([\#83](https://github.com/r-lib/lintr/issues/83),
  [@jackwasey](https://github.com/jackwasey))
- Now trims long comments
  ([\#55](https://github.com/r-lib/lintr/issues/55), reported by
  [@paulstaab](https://github.com/paulstaab))
- Automatic commenting of GitHub commits and pull requests when linting
  on Travis-CI
- expect_lint_free expectation can be added to testthat unit tests.
- Robust configuration system and exclusion logic
- Emacs and Sublime Text 3 plugins now available from their respective
  package repositories.
- add `names.lints`, `split.lints`
  ([\#49](https://github.com/r-lib/lintr/issues/49),
  [@ttriche](https://github.com/ttriche))
- Fixed bug that caused vim syntastic plugin not to work properly in
  windows ([\#46](https://github.com/r-lib/lintr/issues/46),
  [@abossenbroek](https://github.com/abossenbroek))
- allow lintr customization per project using `.lintr` config files.
- use [`globalenv()`](https://rdrr.io/r/base/environment.html) instead
  of [`baseenv()`](https://rdrr.io/r/base/environment.html) for default
  parent environment so that `methods` will be included.
- do not check object usage if eval fails. Fixes
  ([\#24](https://github.com/r-lib/lintr/issues/24), reported by
  [@fabian-s](https://github.com/fabian-s))
- `trailing_whitespace_linter` was reporting the incorrect line number
- Use RStudio source marker API to display lints
  ([\#37](https://github.com/r-lib/lintr/issues/37),
  [@jjallaire](https://github.com/jjallaire))
- Permit single quotes if they quote literal double quotes
  ([\#28](https://github.com/r-lib/lintr/issues/28),
  [@jackwasey](https://github.com/jackwasey))
- `# nolint` comments are respected with caching
  ([\#68](https://github.com/r-lib/lintr/issues/68),
  [@krlmlr](https://github.com/krlmlr))
- Properly handle all knitr document formats
- Allow for (( when linting
  ([\#259](https://github.com/r-lib/lintr/issues/259),
  [@nathaneastwood](https://github.com/nathaneastwood))
- Remove ^ from infix spaces to conform with tidyverse.
  ([\#302](https://github.com/r-lib/lintr/issues/302),
  [@nathaneastwood](https://github.com/nathaneastwood))

## lintr 0.2.0

CRAN release: 2014-12-01

- Initial release
