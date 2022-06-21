# lintr (development version)

## Changes to defaults

* `seq_linter()` additionally lints on `1:n()` (from dplyr) 
  and `1:.N` (from data.table) (#1396, @IndrajeetPatil).

## Bug fixes

* `get_source_expressions()` no longer fails on R files that match a knitr pattern (#743, #879, #1406, @AshesITR).
* Parse error lints now appear with the linter name `"error"` instead of `NA` (#1405, @AshesITR).  
  Also, linting no longer runs if the `source_expressions` contain invalid string data that would cause error messages
  in other linters. 
* `get_source_expressions()` no longer omits trailing non-code lines from knitr files (#1400, #1415, @AshesITR).  
  This fixes the location information for `trailing_blank_lines_linter()` in RMarkdown documents without terminal
  newlines.
* The `vignette("lintr")` incorrectly cited `exclude` as the key for setting file exclusions in `.lintr` when it is 
  actually `exclusions`. (#1401, @AshesITR)
* `lint_dir()` no longer errors if there are multiple configured exclusions for a single file (#1413, @AshesITR).

## Other changes

* The minimum needed version for soft dependency `{withr}` has been bumped to `2.5.0`
  (#1404, @IndrajeetPatil).

# lintr 3.0.0

## Breaking changes

* All linters are now function factories (i.e., functions that return functions) for consistency. Previously, only
  linters with customizable parameters were factories (#245, @fangly, @AshesITR, and @MichaelChirico).

  This means that usage such as `lint("file.R", seq_linter)` should be updated to `lint("file.R", seq_linter())`, and
  the following update for custom linters:

  ```r
  my_custom_linter <- function(source_expression) { ... }

  # becomes
  my_custom_linter <- function() Linter(function(source_expression) { ... })
  ```
* Exclusions specified in the `.lintr` file are now relative to the location of that file
  and support excluding entire directories (#158, #438, @AshesITR).
* Removed long-deprecated linters (they've been marked as deprecated since v1.0.1 in 2017):
   + `absolute_paths_linter()`
   + `camel_case_linter()`
   + `multiple_dots_linter()`
   + `snake_case_linter()`
   + `trailing_semicolons_linter()`
* Removed `return()` from `all_undesirable_functions` because early returns (which often improve
  readability and reduce code complexity) require explicit use of `return()`. Follow #1100 for
  an upcoming `return_linter()` to lint unnecessary `return()` statements (#1146, @AshesITR).  
  Note that you can replicate old behavior by supplying `return` as a custom undesirable function:
  `undesirable_function_linter(c(all_undesirable_functions, list(return = NA)))`

## Deprecations

* Lints are now marked with the name of the `linter` that caused them instead of the name of their implementation
  function. Deprecated the obsolete `linter` argument of `Lint()` (#664, #673, #746, @AshesITR). Downstream custom
  linters should follow suit.
* Renamed `semicolon_terminator_linter()` to `semicolon_linter()` for better consistency.
  `semicolon_terminator_linter()` survives but is marked for deprecation. The new linter also has a new signature,
  taking arguments `allow_compound` and `allow_trailing` to replace the old single argument `semicolon`, again for
  signature consistency with other linters.
* The following linters were subsumed into `brace_linter()` and are now deprecated; see the item on `brace_linter()`
  below:
   + `closed_curly_linter()`
   + `open_curly_linter()`
   + `paren_brace_linter()`
* The `...` argument for `lint()`, `lint_dir()`, and `lint_package()` has been promoted to an earlier position to
  better match the [Tidyverse design principle](https://design.tidyverse.org/args-data-details.html) of
  data->descriptor->details. This change enables passing objects to `...` without needing to specify non-required
  arguments, e.g. `lint_dir("/path/to/dir", linter())` now works without the need to specify `relative_path`.
  This affects some code that uses positional arguments (#935, @MichaelChirico).
   + For `lint()`, `...` is now the 3rd argument, where earlier this was `cache`.
   + For `lint_dir()` and `lint_package()`, `...` is now the 2nd argument, where earlier this was `relative_path`.
* Deprecated argument `source_file` to exported functions `with_id()` and `ids_with_token()`. It has been renamed to
  `source_expression` to better reflect that this argument is typically the output of `get_source_expressions()`.
  For now, the old argument `source_file` can still be used (with warning). The now-private functional versions of many
  linters also underwent the same renaming (`source_file` -> `source_expression`). This has no direct effect on
  packages importing lintr, but is mentioned in case custom linters imitating `lintr` style had also adopted the
  `source_file` naming and want to adapt to keep in sync.
* Deprecated `with_defaults()` in favor of `linters_with_defaults()`, and add `modify_defaults()` which is intended to
  be used more generally to modify (i.e., extend, trim, and/or update) a list of defaults. Note that the argument
  corresponding to `with_defaults()`'s `default=` is called `defaults=` (i.e., pluralized) in both of these, and that
  usage like `with_defaults(default = NULL, ...)` should be converted to `linters_with_defaults(defaults = list(), ...)`
  (#1029, #1336, #1361, @AshesITR and @michaelchirico).
* Deprecated the `find_line()` and `find_column()` helpers from the item-level `expressions` returned with
  `get_source_expressions()`. These helpers were typically associated with regex-based logic for building linters,
  which is rarely needed and prone to false positives; now that lintr almost exclusively uses XPath-based
  logic for linters, these are no longer necessary (#1373, @MichaelChirico).

## Other changes to defaults

### Updates to `default_linters`

* New `brace_linter()` which combines several curly brace related linters, deprecating the following predecessors
  (#1041, @AshesITR):
   + `closed_curly_linter()`; both now also allow `}]` in addition to `})` and `},` as exceptions, i.e., `}` doesn't
     need to be on its own line if paired with a closing square bracket, a closing parenthesis, or a comma. Also
     improved lint metadata so that source markers land at the closing brace instead of the closing parenthesis to
     improve the experience of fixing the lint (#583, @AshesITR).
   + `open_curly_linter()`; both also no longer lint unnecessary trailing whitespace (use `trailing_whitespace_linter()`
     for this) and also allow `(`, `,`, and `%>%` on preceding lines as exceptions, i.e., `{` can be alone on a line if
     the previous line is terminated with an opening parenthesis, a comma, or a pipe (`%>%`) (#487, #1028, @AshesITR).
   + `paren_brace_linter()`; `brace_linter()` also lints `if`/`else` and `repeat` with missing whitespace.
   + `brace_linter()` also newly enforces the following rules surrounding curly braces (originally Google linters, see
     below):
      - Require `else` to come on the same line as the preceding `}`, if present (#884, @MichaelChirico).
      - Require functions spanning multiple lines to use curly braces (#987, @MichaelChirico).
      - Require balanced usage of `{}` in `if`/`else` conditions, i.e., if the `if` branch uses braces,
       then so must the `else` branch, and _vice versa_ (#983, @MichaelChirico).
* New `paren_body_linter()` checks that there is a space between a right parenthesis and a body expression (#809,
  @kpagacz).
* Added `semicolon_linter()` as a default because it enforces a tidyverse style guide rule (#683, @AshesITR).
* `assignment_linter()` (#915, @MichaelChirico):
  + Right assignments are now linted by default (`->` and `->>`).
  + New argument `allow_cascading_assign` (`TRUE` by default) toggles whether to lint `<<-` and `->>`.
  + New argument `allow_right_assign` (`FALSE` by default) toggles whether to lint `->` and `->>`.
* `commented_code_linter()`: use the parse tree to find comments, eliminating some false positives (#451, @AshesITR).
* `equals_na_linter()` (#545, @MichaelChirico):
   + Extended to lint `x != NA` (before, only `==` was caught) and `NA == x` (before, only `NA` on RHS was caught).
   + Extended to skip usages in comments like `is.na(x) # use is.na(x), not x == NA`.
* `function_left_parentheses_linter()`: improved location information (#1266, #1267, @AshesITR).
* `infix_spaces_linter()`:
   + Added argument `allow_multiple_spaces` (`TRUE` by default) which toggles
     whether to generate a lint for operators used with multiple spaces, e.g. `x   +   2`.
     The default setting allows extra spacing to be used to increase
     line-to-line alignment (#940, @f-ritter and @MichaelChirico).
   + Extended so that usages like `a~b` and `function(a=1) { ... }` are linted (#930, #michaelchirico).
   + Added argument `exclude_operators` to disable lints on selected infix operators.
     By default, all "low-precedence" operators throw lints; see `?infix_spaces_linter` for an enumeration of these.
     (#914, @MichaelChirico).
   + Add an exception for `/` usage in `box::use()` declarations (#1087, @klmr).
* `line_length_linter()`: place the source marker at the margin of the affected line to improve user experience
  during de-linting -- just press <kbd>Return</kbd> (#735, @AshesITR).* 
* `no_tab_linter()`: use more reliable matching (e.g., excluding matches found in comments; #441, @russHyde).
* `object_length_linter()`: correctly detect generics and only count the implementation class towards the length.
  This prevents false positive lints in the case of long generic names, e.g.
  `very_very_very_long_generic_name.short_class` no longer produces a lint (#871, @AshesITR).
* `object_name_linter()`:
   + Improved generic detection -- in user-defined method `my_method.upstream.class`,
     `upstream.class` no longer throws a lint because the generic (`my_method`)
     properly uses `snake_case` (#737, @AshesITR).
   + Exclude special R namespace hook functions such as `.onLoad()` (#500, #614, @AshesITR and @MichaelChirico).
   + Correctly detect imported functions when linting packages (#642, @AshesITR).
   + Correctly detect assignment generics like `names<-.class_name` (#843, @jonkeane).
   + Added new styles `"symbols"` and `"SNAKE_CASE"` (#494, #495, #615, #670, @MichaelChirico and @AshesITR).
      - `"symbols"` is a new default style which won't lint all-symbol object names. In particular, that means
        operator names like `%+%` are allowed.
   + No longer lints names used in `$` extractions (#582, @AshesITR).
* `object_usage_linter()`:
   + Detect global variables if there are top-level dollar-assignments (#666, @AshesITR).
   + Report usage warnings spanning multiple lines (#507, @AshesITR).
   + Detect usages inside `glue::glue()` constructs (#942, @AshesITR).
   + Extended to include functions assigned with `=` instead of `<-` (#1081, @MichaelChirico).
   + Detect functions exported by packages that are explicitly attached using `library()` or
     `require()` calls (#1127, @AshesITR).
   + Improved location information in some cases where the previous regex-based approach didn't work, e.g. unicode
     characters in variable names (#1285, @AshesITR).
   + Correctly detect functions declared within `assign()` and `setMethod()` (#1322, @AshesITR).
* `spaces_inside_linter()`: ignore spaces preceding trailing comments (#636, @MichaelChirico).
* `T_and_F_symbol_linter()`:
   + Added as a default because it enforces a tidyverse style guide rule (#517, @AshesITR).
   + No longer lint occurrences of `T` and `F` when used for subsetting, and give a better
     message when used as variable names (#657, @AshesITR).
* `trailing_blank_lines_linter()`:
   + Extended to lint files without a terminal newline (#675, @AshesITR).
   + Also, running `lint()` on a file without a terminal newline no longer throws a `warning()`.
* `trailing_whitespace_linter()`:
   + Extended to also lint completely blank lines by default (#1044, @AshesITR).
   + Added argument `allow_empty_lines` (`FALSE` by default) to toggle this behavior.
   + Improved so that trailing whitespace inside string literals does not trigger a lint (#1045, @AshesITR).
   + Added argument `allow_in_strings` (`TRUE` by default) to toggle this behavior.
* `undesirable_function_linter()`:
   + Added new functions to `default_undesirable_functions` related to debugging (#876, @MichaelChirico):
      - `browser()`
      - `debug()`
      - `debugcall()`
      - `debugonce()`
      - `trace()`
      - `untrace()`
   + No longer lints `library()` and `require()` calls attaching a package with an undesired name,
     e.g. `library(foo)` (#814, @kpagacz and @MichaelChirico).
   + No longer lints undesirable symbols if they are used as names in `$` extractions (#1050, @AshesITR).
   + Added more explanation why certain functions might be undesirable and what alternatives to use;
     ditto for `undesirable_operator_linter()` (#1133, #1146, #1159, @AshesITR).

### Other noteworthy changes

* `cyclocomp_linter()`: set the default `complexity_limit` to 15. This brings the default into sync with what
  is enforced via `default_linters` (#693, @AshesITR).
* `lint_package()` now lints files in the `demo` directory by default (#703, @dmurdoch).
* Moved the default lintr cache directory from `~/.R/lintr_cache` (which was a violation of
  CRAN policy) to `R_user_dir("lintr", "cache")`. Note that 3.0.0 is a major version update and invalidates
  the old cache anyway, so it can be safely deleted (#1062, @AshesITR).

## New and improved features

### New linters

* `backport_linter()` for detecting mismatched R version dependencies (#506, #1316, #1318, #1319, @MichaelChirico and
  @AshesITR).
* `duplicate_argument_linter()` similarly checks that there are no duplicate arguments supplied to function calls (#850,
  @renkun-ken).
* `missing_argument_linter()` to check for empty (missing) arguments in function calls (#563, #1152, @renkun-ken and 
  @AshesITR).
* `missing_package_linter()` to check if packages in calls to `library()` and friends
  are missing (#536, #1037, @renkun-ken and @MichaelChirico).
* `namespace_linter()` to check for common mistakes in `pkg::symbol` usages (#548, @renkun-ken).
* `package_hooks_linter()` to run a series of checks also done by `R CMD check` on the `.onLoad()`, `.onAttach()`,
  `.Last.lib()` and `.onDetach()` hooks (#882, @MichaelChirico).
* `pipe_call_linter()` to enforce that all steps of `magrittr` pipelines use explicit calls instead of symbols,
  e.g. `x %>% mean()` instead of `x %>% mean` (#801, @MichaelChirico).
* `sprintf_linter()` to check for common mistakes in `sprintf()` usage (#544, #624, @renkun-ken and @AshesITR).
* `unused_import_linter()` to detect unnecessary `library()` calls in R scripts (#239, @jimhester, @AshesITR).

#### Google linters

Google is a heavy user of lintr internally, and has developed a large set of linters improving code consistency
and correcting common R usage mistakes. This release includes many of these linters that are
of general interest to the broader R community. More will be included in future releases. See, e.g.
#884, #979, #998, #1011, #1016, #1036, #1051, #1066, and #1067; special thanks to @MichaelChirico and @michaelquinn32.

* `any_duplicated_linter()` Require usage of `anyDuplicated(x) > 0L` over `any(duplicated(x))` and similar.
* `any_is_na_linter()` Require usage of `anyNA(x)` over `any(is.na(x))`.
* `class_equals_linter()` Prevent comparing `class(x)` with `==`, `!=`, or `%in%`, where `inherits()` is typically
  preferred.
* `condition_message_linter()` Prevent condition messages from being constructed like `stop(paste(...))`
  (where just `stop(...)` is preferable).
* `conjunct_test_linter()` Require usage of `expect_true(x); expect_true(y)` over `expect_true(x && y)` and similar.
* `consecutive_stopifnot_linter()` Require consecutive calls to `stopifnot()` to be unified into one.
* `expect_comparison_linter()` Require usage of `expect_gt(x, y)` over `expect_true(x > y)` and similar.
* `expect_identical_linter()` Require usage of `expect_identical()` by default, and `expect_equal()` only by exception.
* `expect_length_linter()` Require usage of `expect_length(x, n)` over `expect_equal(length(x), n)` and similar.
* `expect_named_linter()` Require usage of `expect_named(x, n)` over `expect_equal(names(x), n)` and similar.
* `expect_not_linter()` Require usage of `expect_false(x)` over `expect_true(!x)`, and _vice versa_.
* `expect_null_linter()` Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and similar.
* `expect_s3_class_linter()` Require usage of `expect_s3_class(x, k)` over `expect_equal(class(x), k)` and similar.
* `expect_s4_class_linter()` Require usage of `expect_s4_class(x, k)` over `expect_true(methods::is(x, k))`.
* `expect_true_false_linter()` Require usage of `expect_true(x)` over `expect_equal(x, TRUE)` and similar.
* `expect_type_linter()` Require usage of `expect_type(x, t)` over `expect_equal(typeof(x), t)` and similar.
* `fixed_regex_linter()` Require `fixed = TRUE` or `stringr::fixed()` for regular expressions that can be
  expressed statically, e.g. `strsplit(x, "[.]")` can be `strsplit(x, ".", fixed = TRUE)`.
   + Added parameter `allow_grepl` (default `FALSE`) to toggle whether `grepl()` usages should be linted.
     These might be treated separately because `grepl("^x", NA)` is `FALSE`; the `startsWith()` equivalent to
     get `FALSE` for missing input is clunkier, but more explicit: `!is.na(x) & startsWith(x, string)` (#1376, @MichaelChirico).
* `ifelse_censor_linter()` Require usage of `pmax()` / `pmin()` where appropriate, e.g. `ifelse(x > y, x, y)` is
  `pmax(x, y)`.
* `inner_combine_linter()` Require inputs to known-vectorized functions to be combined first rather than later,
  e.g. `as.Date(c(x, y))` over `c(as.Date(x), as.Date(y))`.
* `literal_coercion_linter()` Require using correctly-typed literals instead of direct coercion, e.g. `1L` instead of
  `as.numeric(1)`.
* `nested_ifelse_linter()` Prevent nested calls to `ifelse()` like `ifelse(A, x, ifelse(B, y, z))`, and similar.
* `numeric_leading_zero_linter()` Require a leading `0` in fractional numeric constants, e.g. `0.1` instead of `.1`.
* `outer_negation_linter()` Require usage of `!any(x)` over `all(!x)` and `!all(x)` over `any(!x)`.
* `paste_linter()` lint for common mis-use of `paste()` and `paste0()`:
   + `paste0()` encouraged instead of `paste(sep = "")`.
   + `toString()` or `glue::glue_collapse()` encouraged instead of `paste(x, collapse = ", ")`.
   + Lint `sep=` passed to `paste0()` -- typically a mistake.
* `redundant_ifelse_linter()` Prevent usage like `ifelse(A & B, TRUE, FALSE)` or `ifelse(C, 0, 1)`
  (the latter is `as.numeric(!C)`).
* `regex_subset_linter()` Require usage of `grep(ptn, x, value = TRUE)` over `x[grep(ptn, x)]` and similar.
* `string_boundary_linter()` Require usage of `startsWith(x, ptn)` over `grepl("^ptn", x)` or `substr(x, 1, 3) == ptn`
  and similar.
* `strings_as_factors_linter()` Check for code designed to work before and after the `stringsAsFactors = FALSE` default
  change in R 4.0 by examining code for `data.frame()` usages susceptible to assumptions about the default value
  of `stringsAsFactors=`.
* `system_file_linter()` Prevent usage like `file.path(system.file("A", package = "pkg"), "B")` where simply
  `system.file("A", "B", package = "pkg")` is more concise and readable.
* `unreachable_code_linter()` Prevent code after `return()` and `stop()` statements that will never be reached
  (extended for #1051 thanks to early user testing, thanks @bersbersbers!).
* `vector_logic_linter()` Require use of scalar logical operators (`&&` and `||`) inside `if()` conditions and similar.
* `yoda_test_linter()` Require usage of `expect_identical(x, 1L)` over `expect_equal(1L, x)` and similar.

### Other features and improvements

* **Documentation**: Reorganize linter documentation into new tag-based Rd pages (#888, #1015, @AshesITR).
   + Each linter has its own help page.
   + `?linters` also links to tag help pages, collecting linters with a similar goal.
   + Each linter can have multiple tags.
   + `available_linters()`: new function to list available linters and their tags.
     This feature is extensible by package authors providing add-on linters for {lintr}.
   + `available_tags()`: new function to list available tags.
   + `linters_with_tags()`: new function to help build a list of linters using tags.
* **Encodings**: lintr now supports non-system character Encodings. The correct the correct encoding
  is auto-detected from .Rproj or DESCRIPTION files in your project.
  Override the default in the `encoding` setting of lintr (#752, #782, @AshesITR).
* **Jenkins CI**: Support for writing comments to GitHub repo when running in Jenkins CI (#488, @fdlk).
* **Performance**: Optimized performance-critical functions in lintr, such as `get_source_expressions()` resulting in
  about 2x speedup in our test suite and even more for complex files (#1169, #1197, #1200, #1201, #1214, @MichaelChirico
  and @AshesITR). Average `lint_package()` execution time is down about 30% and the median package sees about 40%
  improvement.
* **Raw strings**: Several linters tightened internal logic to allow for raw strings like `R"( a\string )"`
  (#1034, #1285, @MichaelChirico and @AshesITR).
* **Selective exclusion syntax**: New syntax to exclude only selected linters from certain lines or passages.
  Use `# nolint: linter_name, linter2_name.` or `# nolint start: linter_name, linter2_name.`
  in source files or named lists of line numbers in `.lintr`. Note the terminal `.` is required.
  Also allows for partial matching as long as the supplied prefix is unique, e.g.
  `# nolint: infix_spaces.` works to exclude `infix_spaces_linter` (#605, #872, @AshesITR).
   + Added the linter name to lintrs output to facilitate discovery of the correct name (#1357, @AshesITR).
* Improved S3 generic detection for non-standard S3 generics where `UseMethod()` is called after several
  preceding expressions (#846, @jonkeane).
* New `sarif_output()` function to output lints to SARIF output (#1424, @shaopeng-gh)
* `extraction_operator_linter()`: no longer lint `x[NULL]` (#1273, @AshesITR).
* `is_lint_level()`: new exported helper for readably explaining which type of expression is required for a custom
  linter. Some linters are written to require the full file's parse tree (for example, `single_quotes_linter()`).
  Others only need single expressions, which is more cache-friendly (most linters are written this way to leverage
  caching) (#921, @MichaelChirico).
* `lint_dir()` excludes the `renv` and `packrat` directories by default (#697, @AshesITR).
* `lint()`: new optional argument `text` for supplying a line or lines directly, e.g. if the file is already
  in memory or linting is being done _ad hoc_ (#503, @renkun-ken).
* `seq_linter()`: improve lint message to be clearer about the reason for linting (#522, @MichaelChirico).
* `unneeded_concatenation_linter()`:
   + Correctly considers arguments in pipelines (`%>%` or `|>`; #573, #1270, @michaelquinn32 and @AshesITR).
   + New argument `allow_single_expression`, default `TRUE`, toggling whether `c(x)` should be linted, i.e.,
     a call to `c()` with only one entry which is not a constant. In some such cases, `c()` can simply be dropped,
     e.g. `c(a:b)`; in others, the parentheses are still needed, e.g. `-c(a:b)` should be `-(a:b)`;
     and in still others, `c()` is used for the side-effect of stripping attributes, e.g.
     `c(factor(letters))` or `c(matrix(1:10, 5, 2))`. In this last case, `c()` can (and should) in most cases
     be replaced by `as.vector()` or `as.integer()` for readability. In fact, we suspect it is _always_
     preferable to do so, and may change the default to `allow_single_expression = FALSE` in the future. Please
     report your use case if `as.vector()` does not suit your needs (#1344, @MichaelChirico).
* `use_lintr()`: new exported helper for creating a minimal `.lintr` configuration (#902, @AshesITR).
* `xml_nodes_to_lints()`: new exported helper for converting `xml_node` objects obtained using linter logic
  expressed in XPath into `Lint` objects (#1124, #1216, #1234, @MichaelChirico and @AshesITR).

## Bug fixes

* **RStudio**: Source markers are cleared when there are no lints (#520, @AshesITR).
* Error message for mismatched starts and ends of exclusion ranges is now more helpful.
  (#571, #860, @AshesITR and @danielinteractive).
* Improved location information for R parse errors (#894, #892, @renkun-ken and @AshesITR).
* `get_source_expressions()`:
   + Fix possible error on invalid XML produced by `xmlparsedata::xml_parse_data()` (#559, @renkun-ken).
   + Fix handling zero-length variable name error (#566, @renkun-ken).
   + Malformed Rmd files now cause a lint instead of an error (#571, @AshesITR).
   + No longer fails if `getParseData()` returns a truncated (invalid) Unicode character as parsed text (#815,
     @leogama).
   + Fixes the `text` value for `STR_CONST` nodes involving 1- or 2-width octal escapes
     (e.g. `"\1"`) to account for an R parser bug (https://bugs.r-project.org/show_bug.cgi?id=18323; #1056,
     @MichaelChirico).
   + Handle Rmd inputs containing unevaluated code blocks with named format specifiers (#472, @russHyde).
* `line_length_linter()`: fix a bug causing duplicate lints for lines containing multiple expressions (#681, @AshesITR).
* `lint_package()`:
   + Warns and returns `NULL` if no package is found (instead of giving a peculiar error message; #776,
     @MichaelChirico).
   + Stricter about what is considered to be a package -- folders named `DESCRIPTION` are ignored (#702,
     @MichaelChirico).
* `linters_with_defaults()` (formerly `with_defaults()`):
   + No longer duplicates the `lintr_function` class when it is already present (#511, @AshesITR).
   + Warns if a named argument is `NULL` but its name is not in `defaults` (#1049, @AshesITR).
* `linters_with_defaults()` handles automatic naming of very long arguments correctly (#774, @MichaelChirico).
* `save_cache()` will now recursively create the cache directory; this avoids errors that could arise if any parent
  directories do not exist (#60, @dankessler).
* `spaces_left_parentheses_linter()`: fix a bug causing warnings like "In `parent == parent[before_operator_idx]`
  longer object length is not a multiple of shorter object length" in nested expressions (#654, @AshesITR).

## Internals

* Added a new, more restrictive test workflow - `test-package` - that fails on warnings emitted by tests
  (#1263, #1272, @AshesITR).
* Added a secondary, more restrictive lint workflow - `lint-changed-files` - for newly written / modified code
  (#641, @dragosmg).
* Several optional `Imported` packages have become `Suggested` dependencies: `httr`, `testthat`, and `rstudioapi`.
  This should allow snappier CI builds for usages not relying on some more "peripheral" features of the package.
* Special thanks to @bersbersbers for early testing on the 3.0.0 changes.
* Switched CI from Travis to GitHub Actions, using the full tidyverse recommended `R CMD check`. Code coverage and
  linting are implemented using separate GitHub Actions workflows (#572, @dragosmg).
* Updated R CMD GitHub Actions workflow to check for R 3.6 on Ubuntu, instead of R 3.3, and for R 4.0 on Windows,
  instead of R 3.6 (#803, @ dragosmg).
* `lintr` now uses the 3rd edition of `testthat` (@MichaelChirico, @AshesITR, #910, #967).

# lintr 2.0.1

## New features

* lintr now supports GitHub Actions and will print the lints as warning messages if lints are printed during an action.
* `lint_package()` will now lint vignettes and data-raw by default (#447, @AshesITR).
* `lint_dir()` will now include Rmd and Rnw files by default (@AshesITR).

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
* `commas_linter` now allows spaces to come before a comma when used to denote a
  fall-through in a switch statement (#499, @MrMallIronmaker)

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
