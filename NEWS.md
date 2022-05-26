# lintr (development version)

## Breaking changes

* Exclusions specified in the `.lintr` file are now relative to the location of that file 
  and support excluding entire directories (#158, #438, @AshesITR)
* All linters are now function factories (i.e., functions that return functions) for consistency. Previously, only linters with customizable parameters were factories (#245, @fangly, @AshesITR, and @MichaelChirico)

  This means that usage such as `lint("file.R", seq_linter)` should be updated to `lint("file.R", seq_linter())`, and
  the following update for custom linters:

  ```r
  my_custom_linter <- function(source_expression) { ... }
  
  # becomes
  my_custom_linter <- function() Linter(function(source_expression) { ... })
  ```
* Removed long-deprecated linters (they've been marked as deprecated since v1.0.1 in 2017):
   + `absolute_paths_linter()`
   + `camel_case_linter()`
   + `multiple_dots_linter()`
   + `snake_case_linter()`
   + `trailing_semicolons_linter()as calls`
* Removed `"return"` from `all_undesirable_functions`. Follow #1100 for an upcoming `return_linter()` to lint 
  unnecessary `return()` statements (#1146, @AshesITR). Note that you can replicate old behavior by supplying
  `return` as a custom undesirable function:
  `undesirable_function_linter(c(all_undesirable_functions, list(return = NA)))`

## Deprecations

* Lints are now marked with the name of the `linter` that caused them instead of the name of their implementation
  function.
  Deprecated the obsolete `linter` argument of `Lint()`. (#664, #673, #746, @AshesITR)
  Downstream custom linters should follow suit.
* Rename `semicolon_terminator_linter()` to `semicolon_linter()` for better consistency.
  `semicolon_terminator_linter()` survives but is marked for deprecation. The new linter also has a new signature, 
  taking arguments `allow_compound` and `allow_trailing` to replace the old single argument `semicolon`, again for
  signature consistency with other linters.
* The following linters were subsumed into `brace_linter()` and are now deprecated; see the item on `brace_linter()` below:
   + `closed_curly_linter()`
   + `open_curly_linter()`
   + `paren_brace_linter()`
* The `...` argument for `lint()`, `lint_dir()`, and `lint_package()` has been promoted to an earlier position to better
  match the [Tidyverse design principle](https://design.tidyverse.org/args-data-details.html) of
  data->descriptor->details. This change enables passing objects to `...` without needing to specify non-required
  arguments, e.g. `lint_dir("/path/to/dir", linter())` now works without the need to specify `relative_path`.
  This affects some code that uses positional arguments. (#935, @michaelchirico)
  + For `lint()`, `...` is now the 3rd argument, where earlier this was `cache`
  + For `lint_dir()` and `lint_package()`, `...` is now the 2nd argument, where earlier this was `relative_path`
* Argument `source_file` to exported functions `with_id()` and `ids_with_token()` has been renamed to
  `source_expression` to better reflect that this argument is typically the output of `get_source_expressions()`.
  It has also been renamed as the argument of the now-private functional versions of many linters, which has no direct
  effect on packages importing lintr, but is mentioned in case custom linters imitating `lintr` style have also
  adopted the `source_file` naming and want to adapt to keep in sync.
* Deprecated `with_defaults()` in favor of `linters_with_defaults()` (#1029, @AshesITR)

## Other changes to defaults

### Updates to `default_linters`

* New `brace_linter()` which combines several curly brace related linters, deprecating the following predecessors (#1041, @AshesITR):
   + `closed_curly_linter()`; both now also allow `}]` in addition to `})` and `},` as exceptions.
   + `open_curly_linter()`; both also no longer lint unnecessary trailing whitespace (use `trailing_whitespace_linter()` for this)
     and also allow `(`, `,`, and `%>%` on preceding lines as exceptions. (#487, #1028)
   + `paren_brace_linter()`; `brace_linter()` also lints `if`/`else` and `repeat` with missing whitespace
   + Improved lint metadata so that source markers land at the opening brace instead of the closing parenthesis
     to improve the experience of fixing the lint (#583, @AshesITR)
* `brace_linter()` now marks lints at the opening brace instead of the closing parenthesis, making fixing the lints
  by jumping to source markers easier (#583, @AshesITR)
  `brace_linter()` also newly enforces the following rules surrounding curly braces (originally Google linters, see below):
   + require `else` to come on the same line as the preceding `}`, if present (#884, @michaelchirico)
   + require functions spanning multiple lines to use curly braces (@michaelchirico)
   + require balanced usage of `{}` in `if`/`else` conditions, i.e., if the `if` branch uses braces,
     then so must the `else` branch, and _vice versa_ (@michaelchirico)
* New `paren_body_linter()` checks that there is a space between a right parenthesis and a body expression. (#809, @kpagacz)
* Added `T_and_F_symbol_linter()` (#517, @AshesITR)
* Added `semicolon_linter()` (#683, @AshesITR)
* `undesirable_function_linter()`
   + Added new functions to the defaults related to debugging (#876, @michaelchirico):
     - `browser()`
     - `debug()`
     - `debugcall()`
     - `debugonce()`
     - `trace()`
     - `untrace()`
   + No longer lints `library()` and `require()` calls attaching a package with an undesired name,
     e.g. `library(foo)` (#814, @kpagacz and @michaelchirico)
   + No longer lints undesirable symbols if they are used as names in `$` extractions (#1050, @AshesITR)
   + Added more explanation why certain functions might be undesirable and what alternatives to use;
     ditto for `undesirable_operator_linter()` (#1133, #1146, #1159, @AshesITR)
* `assignment_linter()`: extended and add arguments (#915, @michaelchirico)
   + right assignments are now linted by default (`->` and `->>`)
   + new argument `allow_cascading_assign` (`TRUE` by default) toggles whether to lint `<<-` and `->>`
   + new argument `allow_right_assign` (`FALSE` by default) toggles whether to lint `->` and `->>`
* `infix_spaces_linter()`
   + added argument `allow_multiple_spaces` (`TRUE` by default) which toggles
     whether to generate a lint for operators used with multiple spaces, e.g. `x   +   2`.
     The default setting allows extra spacing to be used to increase
     line-to-line alignment (#940, @f-ritter and @michaelchirico)
   + extended so that usages like `a~b` and `function(a=1) { ... }` are linted (#930, #michaelchirico)
   + added argument `exclude_operators` to disable lints on selected infix operators.
     By default, all "low-precedence" operators throw lints; see `?infix_spaces_linter` for an enumeration of these.
     (#914, @michaelchirico)
   + add exception for `box::use()` declarations (#1087, @klmr)
* `trailing_whitespace_linter()`
   + extended to also lint completely blank lines by default (#1044, @AshesITR)
   + added argument `allow_empty_lines` (`FALSE` by default) to toggle this behavior
   + improved so that trailing whitespace inside string literals does not trigger a lint (#1045, @AshesITR)
   * added argument `allow_in_strings` (`TRUE` by default) to toggle this behavior
* `trailing_blank_lines_linter()`: extend to lint files without a terminal newline (#675, @AshesITR)
* `object_name_linter()`
   + improved generic detection -- in user-defined method `my_method.upstream.class`,
     `upstream.class` no longer throws a lint because the generic (`my_method`)
     properly uses `snake_case` (#737, @AshesITR)
   + extended to exclude special R namespace hook functions such as `.onLoad` (#500, #614, @AshesITR and @michaelchirico)
   + extended to correctly detect imported functions when linting packages (#642, @AshesITR)
   + correctly detect assignment generics like `names<-.class_name` (#843, @jonkeane)
* `object_usage_linter()`
   + detect global variables if there are top-level dollar-assignments (#666, #709, @AshesITR)
   + report usage warnings spanning multiple lines (#507, @AshesITR)
   + detect usages inside `glue::glue()` constructs (#942, @AshesITR)
   + detect within functions assigned with `=` instead of `<-` (#1081, @michaelchirico)
   + detect functions exported by packages that are explicitly attached using `library()` or
     `require()` calls (#1127, @AshesITR)
* `object_length_linter()`: correctly detect generics and only counts the implementation class towards the length.
  This prevents false positive lints in the case of long generic names, e.g.
  `very_very_very_long_generic_name.short_class` no longer produces a lint (#871, @AshesITR)
* `equals_na_linter()` (#545, @michaelchirico)
   + extended to lint `x != NA` (before, only `==` was caught) and `NA == x`(before, only `NA` on RHS was caught)
   + extended to skip usages in comments like `is.na(x) # use is.na(x), not x == NA`
* `spaces_inside_linter()`: ignore spaces preceding trailing comments (#636, @michaelchirico)
* `no_tab_linter()`: use more reliable matching (e.g., excluding matches found in comments; #441, @russHyde)
* `line_length_linter()`: place the source marker at the margin of the affected line to improve user experience
  during de-linting -- just press <kbd>Return</kbd> (#735, @AshesITR)
* `commented_code_linter()`: use the parse tree to find comments, eliminating some false positives (#451, @AshesITR)
* `T_and_F_symbol_linter()`: no longer lint occurrences of `T` and `F` when used for subsetting and gives a better
  message when used as variable names (#657, @AshesITR)

### Other noteworthy changes

* `object_name_linter()` gains a new default style, `"symbols"`, which won't lint all-symbol object names.
  In particular, that means operator names like `%+%` are skipped (#495, #615, #670, @michaelchirico and @AshesITR)
* Set the default `complexity_limit` in `cyclocomp_linter()` to 15. This is the same complexity limit that is enforced
  via `default_linters` (#693, #695, @AshesITR).
* `lint_package()` now lints files in the `demo` directory by default (#703, @dmurdoch).
* Moved the default lintr cache directory from `~/.R/lintr_cache` to `R_user_dir("lintr", "cache")`.
  Note that this major version update invalidates the old cache anyway, so it can be safely deleted. (#1062, @AshesITR)

## New and improved features

### New linters

* `missing_package_linter()` to check if packags in calls to `library()` and friends are missing (#536, #547, #1037, @renkun-ken and @michaelchirico)
* `namespace_linter()` to check for common mistakes in `pkg::symbol` usages (#548, #551, @renkun-ken)
* `missing_argument_linter()` to check for empty (missing) arguments in function calls (#563, #565, @renkun-ken)
* `duplicate_argument_linter()` similarly checks that there are no duplicate arguments supplied to function calls (#850, @renkun-ken)
* `sprintf_linter()` to check for common mistakes in `sprintf()` usage (#544, #578, #624, #625, @renkun-ken, @AshesITR)
* `backport_linter()` for detecting mismatched R version dependencies (#506, @MichaelChirico)
* `pipe_call_linter()` to enforce that all steps of `magrittr` pipelines use explicit calls instead of symbols,
  e.g. `x %>% mean()` instead of `x %>% mean` (@michaelchirico)
* `package_hooks_linter()` to run a series of checks also done by `R CMD check` on the `.onLoad()`, `.onAttach()`,
  `.Last.lib()` and `.onDetach()` hooks (#882, @MichaelChirico)
* `unused_import_linter()` to detect unnecessary `library()` calls in R scripts (#239, @jimhester, @AshesITR)

#### Google linters

Google uses lintr heavily internally, and has developed a large set of linters improving code consistency
and correcting common R usage mistakes. This release includes many of these linters that are
of general interest to the broader R community. More will be included in future releases. See, e.g.
#884, #979, #998, #1011, #1016, #1036, #1051, #1066, and #1067; special thanks to @michaelchirico and @michaelquinn32.

* `expect_null_linter()` Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and similar
* `expect_type_linter()` Require usage of `expect_type(x, t)` over `expect_equal(typeof(x), t)` and similar
* `expect_s3_class_linter()` Require usage of `expect_s3_class(x, k)` over `expect_equal(class(x), k)` and similar
* `expect_s4_class_linter()` Require usage of `expect_s4_class(x, k)` over `expect_true(methods::is(x, k))`
* `conjunct_test_linter()` Require usage of `expect_true(x); expect_true(y)` over `expect_true(x && y)` and similar
* `expect_not_linter()` Require usage of `expect_false(x)` over `expect_true(!x)`, and _vice versa_.
* `expect_true_false_linter()` Require usage of `expect_true(x)` over `expect_equal(x, TRUE)` and similar
* `expect_named_linter()` Require usage of `expect_named(x, n)` over `expect_equal(names(x), n)` and similar
* `expect_length_linter()` Require usage of `expect_length(x, n)` over `expect_equal(length(x), n)` and similar
* `yoda_test_linter()` Require usage of `expect_identical(x, 1L)` over `expect_equal(1L, x)` and similar
* `expect_identical_linter()` Require usage of `expect_identical()` by default, and `expect_equal()` only by exception
* `expect_comparison_linter()` Require usage of `expect_gt(x, y)` over `expect_true(x > y)` and similar
* `vector_logic_linter()` Require use of scalar logical operators (`&&` and `||`) inside `if()` conditions and similar
* `any_is_na_linter()` Require usage of `anyNA(x)` over `any(is.na(x))`
* `class_equals_linter()` Prevent comparing `class(x)` with `==`, `!=`, or `%in%`, where `inherits()` is typically preferred
* `outer_negation_linter()` Require usage of `!any(x)` over `all(!x)` and `!all(x)` over `any(!x)`
* `numeric_leading_zero_linter()` Require a leading `0` in fractional numeric constants, e.g. `0.1` instead of `.1`
* `literal_coercion_linter()` Require using correctly-typed literals instead of direct coercion, e.g. `1L` instead of `as.numeric(1)`
* `paste_linter()` lint for common mis-use of `paste()` and `paste0()`:
   + `paste0()` encouraged instead of `paste(sep = "")`
   + `toString()` or `glue::glue_collapse()` encouraged instead of `paste(x, collapse = ", ")`
   + `sep=` passed to `paste0()` -- typically a mistake
* `nested_ifelse_linter()` Prevent nested calls to `ifelse()` like `ifelse(A, x, ifelse(B, y, z))`, and similar
* `condition_message_linter()` Prevent condition messages from being constructed like `stop(paste(...))`
  (where just `stop(...)` is preferable)
* `redundant_ifelse_linter()` Prevent usage like `ifelse(A & B, TRUE, FALSE)` or `ifelse(C, 0, 1)`
  (the latter is `as.numeric(!C)`)
* `unreachable_code_linter()` Prevent code after `return()` and `stop()` statements that will never be reached
  (extended for #1051 thanks to early user testing, thanks @bersbersbers!)
* `regex_subset_linter()` Require usage of `grep(ptn, x, value = TRUE)` over `x[grep(ptn, x)]` and similar
* `consecutive_stopifnot_linter()` Require consecutive calls to `stopifnot()` to be unified into one
* `ifelse_censor_linter()` Require usage of `pmax()` / `pmin()` where appropriate, e.g. `ifelse(x > y, x, y)` is `pmax(x, y)`
* `system_file_linter()` Require file paths to be constructed by `system.file()` instead of calling `file.path()` directly
* `strings_as_factors_linter()` Check for code designed to work before and after the new `stringsAsFactors = FALSE` default
* `inner_combine_linter()` Require inputs to vectorized functions to be combined first rather than later,
  e.g. `as.Date(c(x, y))` over `c(as.Date(x), as.Date(y))`

### Other features and improvements

* **Selective exclusion syntax**: New syntax to exclude only selected linters from linting lines or passages.
  Use `# nolint: linter_name, linter2_name.` or `# nolint start: linter_name, linter2_name.`
  in source files or named lists of line numbers in `.lintr`.
  Also allows for partial matching as long as the supplied prefix is unique, e.g.
  `# nolint: infix_spaces` works to exclude `infix_spaces_linter` (#660, #872, @AshesITR)
* `object_name_linter()`: new styles `"symbols"` and `"SNAKE_CASE"`
  (#494, #495, #615, #670, @michaelchirico and @AshesITR)
* `lint()`: new optional argument `text` for supplying a string or lines directly, e.g. if the file is already 
  in memory or linting is being done _ad hoc_. (#503, @renkun-ken)
* `lint_dir()` excludes the `renv` and `packrat` directories by default (#697, @AshesITR)
* **Encodings**: lintr now supports non-system character Encodings. The correct the correct encoding
  is auto-detected from .Rproj or DESCRIPTION files in your project.
  Override the default in the `encoding` setting of lintr. (#752, #782, @AshesITR)
* Reorganize linter documentation into new tag-based Rd pages (#888, #1015, @AshesITR)
  + Each linter has its own help page
  + `?linters` also links to tag help pages, collecting linters with a similar goal
  + Each linter can have multiple tags
  + `available_linters()`: new function to list available linters and their tags 
    This feature is extensible by package authors providing add-on linters for {lintr}.
  + `available_tags()`: new function to list available tags
  + `linters_with_tags()`: new function to help build a list of linters using tags
* `is_lint_level()`: new exported helper for readably explaining which type of expression is required for a custom linter. 
  Some linters are written to require the full file's parse tree (for example, `single_quotes_linter()`).
  Others only need single expressions, which is more cache-friendly (most linters are written this way to leverage 
  caching). (#921, @michaelchirico)
* `xml_nodes_to_lints()`: new exported helper for converting `xml_node` objects obtained using linter logic
  expressed in XPath into `Lint` objects (#1124, #1216, #1234, @michaelchirico and @AshesITR)
* `use_lintr()`: new exported helper for creating a minimal `.lintr` configuration (#902, @AshesITR)
* **Performance**: Optimized performance-critical functions in lintr, such as `get_source_expressions()` resulting in about 2x speedup 
  in our test suite and even more for complex files (#1169, #1197, #1200, #1201, #1214, @MichaelChirico and @AshesITR) 
* **Jenkins CI**: Support for writing comments to GitHub repo when running in Jenkins CI (#488, @fdlk)
* `seq_linter()`: improve lint message to be clearer about the reason for linting. (#522, @michaelchirico)
* `unneeded_concatenation_linter()`: correctly considers arguments piped in via magrittr `%>%` (#573, #585, @michaelquinn32)
* `unneeded_concatenation_linter()`: added support for the native pipe `|>` (#1270, #1271, @AshesITR)
* `function_left_parentheses_linter()`: improved location information (#1266, #1267, @AshesITR)
* `object_usage_linter()`: improved location information in some cases (#1285, @AshesITR)
* **Raw strings**: Several linters tightened internal logic to allow for raw strings like `R"( a\string )"` 
  (#1034, #1285, @michaelchirico and @AshesITR)
* Improved S3 generic detection for non-standard S3 generics where `UseMethod()` is called after several
  preceding expressions (#846, @jonkeane)

## Bug fixes

* `save_cache()` will now recursively create the cache directory; this avoids errors that could arise if any parent 
  directories do not exist (#60, @dankessler).
* `extract_r_source()` handles Rmd containing unevaluated code blocks with named format specifiers (#472, @russHyde)
* **RStudio**: Source markers are cleared when there are no lints (#520, @AshesITR)
* `get_source_expressions()`
   + Fix possible error on invalid XML produced by `xmlparsedata::xml_parse_data()` (#559, #560, @renkun-ken)
   + Fix handling zero-length variable name error (#566, #567, @renkun-ken)
   + Malformed Rmd files now cause a lint instead of an error (#571, #575, @AshesITR)
   + No longer fails if `getParseData()` returns a truncated (invalid) Unicode character as parsed text (#815, #816, @leogama)
   + Fixes the `text` value for `STR_CONST` nodes involving 1- or 2-width octal escapes
    (e.g. `"\1"`) to account for an R parser bug (https://bugs.r-project.org/show_bug.cgi?id=18323; #1056, @michaelchirico)
* `linters_with_defaults()` (formerly `with_defaults()`)
   + No longer duplicates the `lintr_function` class when it is already present (#511, #612, @AshesITR)
   + Warns if a named argument is `NULL` but its name is not in `default` (#1049, @AshesITR)
* `spaces_left_parentheses_linter()`: fix a bug causing warnings in nested expressions like
  "In `parent == parent[before_operator_idx]` longer object length is not a multiple of shorter object length" (#654, @AshesITR)
* `line_length_linter()`: fix a bug causing duplicate lints for lines containing multiple expressions (#681, #682, @AshesITR)
* `linters_with_defaults()` handles automatic naming of very long arguments correctly (#774, @michaelchirico)
* `object_name_linter()` no longer lints names used for subsetting (#582, @AshesITR)
* `lint_package()`
   + warns and returns `NULL` if no package is found (instead of giving a peculiar error message; #776, @michaelchirico)
   + stricter about what is considered to be a package -- folders named `DESCRIPTION` are ignored (#702, @michaelchirico)
* Error message for mismatched starts and ends of exclusion ranges is now more helpful.
  (#571, #860, @AshesITR and @danielinteractive)
* Improved location information for R parse errors (#894, #892, @renkun-ken and @AshesITR)
* `extraction_operator_linter()` no longer lints `x[NULL]` (#1273, #1286, @AshesITR)

## Internals

* Updated R CMD GitHub Actions workflow to check for R 3.6 on Ubuntu, instead of R 3.3, and for R 4.0 on Windows,
  instead of R 3.6 (#803, @ dragosmg)
* Added a secondary, more restrictive lint workflow - `lint-changed-files` - for newly written / modified code
  (#641, @dragosmg) 
* Added a new, more restrictive test workflow - `test-package` - that fails on warnings emitted by tests 
  (#1263, #1272, @AshesITR)
* Switched CI from Travis to GitHub Actions, using the full tidyverse recommended R CMD check. Code coverage and linting 
  are implemented using separate GitHub Actions workflows (#572, @dragosmg)
* Several optional `Imported` packages have become `Suggested` dependencies: `httr`, `testthat`, and `rstudioapi`.
  This should allow snappier CI builds for usages not relying on some more "peripheral" features of the package.
* `lintr` now uses the 3rd edition of `testthat` (@MichaelChirico, @AshesITR, #910, #967)
* Special thanks to @bersbersbers for early testing on the 3.0.0 changes

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
