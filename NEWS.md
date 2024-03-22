# lintr 3.1.2

## New and improved features

### Lint accuracy fixes: removing false positives

* `unreachable_code_linter()` ignores reachable code in inline functions like `function(x) if (x > 2) stop() else x` (#2259, @MEO265).
* `unnecessary_lambda_linter()`
  + ignores extractions with explicit returns like `lapply(l, function(x) foo(x)$bar)` (#2258, @MichaelChirico).
  + ignores calls on the RHS of operators like `lapply(l, function(x) "a" %in% names(x))` (#2310, @MichaelChirico).
* `vector_logic_linter()` recognizes some cases where bitwise `&`/`|` are used correctly (#1453, @MichaelChirico).
* `expect_comparison_linter()` ignores faulty usage like `expect_true(x, y > z)` (#2083, @MichaelChirico). Note that `y > z` is being passed to the `info=` argument, so this is likely a mistake.
* `consecutive_assertion_linter()` ignores cases where a second assertion follows an intervening assignment with `=` (#2444, @MichaelChirico).

### Lint accuracy fixes: removing false negatives

* `missing_argument_linter()` catches all missing arguments in calls with several, e.g. `foo(,,)` gives 3 lints instead of 2 (#2399, @MichaelChirico).
* `duplicate_argument_linter()` no longer misses cases with duplicate arguments where a comment comes between the argument name and `=` (#2402, @MichaelChirico).

## Notes

* Fixed a test assuming a specific parser error message that recently changed in r-devel (#2527, @IndrajeetPatil).
* @MichaelChirico has taken over CRAN maintainer duties for the package. Many thanks to @jimhester for more than 10 years and 15 releases wearing that hat!!

# lintr 3.1.1

## Breaking changes

* `infix_spaces_linter()` distinguishes `<-`, `:=`, `<<-` and `->`, `->>`, i.e. `infix_spaces_linter(exclude_operators = "->")` will no longer exclude `->>` (#2115, @MichaelChirico). This change is breaking for users relying on manually-supplied `exclude_operators` containing `"<-"` to also exclude `:=` and `<<-`. The fix is to manually supply `":="` and `"<<-"` as well. We don't expect this change to affect many users, the fix is simple, and the new behavior is much more transparent, so we are including this breakage in a minor release.
* Removed `find_line()` and `find_column()` entries from `get_source_expressions()` expression-level objects. These have been marked deprecated since version 3.0.0. No users were found on GitHub.
* There is experimental support for writing config in plain R scripts (as opposed to DCF files; #1210, @MichaelChirico). The script is run in a new environment and variables matching settings (`?default_settings`) are copied over. In particular, this removes the need to write R code in a DCF-friendly way, and allows normal R syntax highlighting in the saved file. We may eventually deprecate the DCF approach in favor of this one; user feedback is welcome on strong preferences for either approach, or for a different approach like YAML. Generally you should be able to convert your existing `.lintr` file to an equivalent R config by replacing the `:` key-value separators with assignments (`<-`). By default, such a config is searched for in a file named '.lintr.R'. This is a mildly breaking change if you happened to be keeping a file '.lintr.R' around since that file is given precedence over '.lintr'.
  + We also validate config files up-front make it clearer when invalid configs are present (#2195, @MichaelChirico). There is a warning for "invalid" settings, i.e., settings not part of `?default_settings`. We think this is more likely to affect users declaring settings in R, since any variable defined in the config that's not a setting must be removed to make it clearer which variables are settings vs. ancillary.

## Bug fixes

* `sprintf_linter()` doesn't error in cases where whitespace in `...` arguments is significant, e.g. `sprintf("%s", if (A) "" else y)`, which won't parse if whitespace is removed (#2131, @MichaelChirico).

## Changes to default linters

* `assignment_linter()` lints the {magrittr} assignment pipe `%<>%` (#2008, @MichaelChirico). This can be deactivated by setting the new argument `allow_pipe_assign` to `TRUE`.
* `object_usage_linter()`:
  + assumes `glue()` is `glue::glue()` when `interpret_glue=TRUE` (#2032, @MichaelChirico).
  + finds function usages, including infix usage, inside `glue()` calls to avoid false positives for "unused objects" (#2029 and #2069, @MichaelChirico).
* `object_name_linter()` no longer attempts to lint strings in function calls on the LHS of assignments (#1466, @MichaelChirico).
* `infix_spaces_linter()` allows finer control for linting `=` in different scenarios using parse tags `EQ_ASSIGN`, `EQ_SUB`, and `EQ_FORMALS` (#1977, @MichaelChirico).
* `equals_na_linter()` checks for `x %in% NA`, which is a more convoluted form of `is.na(x)` (#2088, @MichaelChirico).

## New and improved features

* New exclusion sentinel `# nolint next` to signify the next line should skip linting (#1791, @MichaelChirico). The usual rules apply for excluding specific linters, e.g. `# nolint next: assignment_linter.`. The exact string used to match a subsequent-line exclusion is controlled by the `exclude_next` config entry or R option `"lintr.exclude_next"`.
* New `xp_call_name()` helper to facilitate writing custom linters (#2023, @MichaelChirico). This helper converts a matched XPath to the R function to which it corresponds. This is useful for including the "offending" function in the lint's message.
* New `make_linter_from_xpath()` to facilitate making simple linters directly from a single XPath (#2064, @MichaelChirico). This is especially helpful for making on-the-fly/exploratory linters, but also extends to any case where the linter can be fully defined from a static lint message and single XPath.
* Toggle lint progress indicators with argument `show_progress` to `lint_dir()` and `lint_package()` (#972, @MichaelChirico). The default is still to show progress in `interactive()` sessions. Progress is also now shown with a "proper" progress bar (`utils::txtProgressBar()`), which in particular solves the issue of progress `.` spilling well past the width of the screen in large directories.
* `lint()`, `lint_dir()`, and `lint_package()` fail more gracefully when the user mis-spells an argument name (#2134, @MichaelChirico).
* Quarto files (.qmd) are included by `lint_dir()` by default (#2150, @dave-lovell).

### New linters

* `library_call_linter()` can detect if all library/require calls are not at the top of your script (#2027, #2043, #2163, and #2170, @nicholas-masel and @MichaelChirico).
* `keyword_quote_linter()` for finding unnecessary or discouraged quoting of symbols in assignment, function arguments, or extraction (part of #884, @MichaelChirico). Quoting is unnecessary when the target is a valid R name, e.g. `c("a" = 1)` can be `c(a = 1)`. The same goes to assignment (`"a" <- 1`) and extraction (`x$"a"`). Where quoting is necessary, the linter encourages doing so with backticks (e.g. `` x$`a b` `` instead of `x$"a b"`).
* `length_levels_linter()` for using the specific function `nlevels()` instead of checking `length(levels(x))` (part of #884, @MichaelChirico).
* `scalar_in_linter()` for discouraging `%in%` when the right-hand side is a scalar, e.g. `x %in% 1` (part of #884, @MichaelChirico).
* `if_not_else_linter()` for encouraging `if` statements to be structured as `if (A) x else y` instead of `if (!A) y else x` (part of #884, @MichaelChirico).
* `repeat_linter()` for encouraging `repeat` for infinite loops instead of `while (TRUE)` (#2106, @MEO265).
* `length_test_linter()` detects the common mistake `length(x == 0)` which is meant to be `length(x) == 0` (#1991, @MichaelChirico).

### Extensions to existing linters

* `fixed_regex_linter()` gains an option `allow_unescaped` (default `FALSE`) to toggle linting regexes not requiring any escapes or character classes (#1689, @MichaelChirico). Thus `fixed_regex_linter(allow_unescaped = TRUE)` would lint on `grepl("[$]", x)` but not on `grepl("a", x)` since the latter does not use any regex special characters.
* `line_length_linter()` helpfully includes the line length in the lint message (#2057, @MichaelChirico).
* `conjunct_test_linter()` also lints usage like `dplyr::filter(x, A & B)` in favor of using `dplyr::filter(x, A, B)` (part of #884; #2110 and #2078, @salim-b and @MichaelChirico). Option `allow_filter` toggles when this applies. `allow_filter = "always"` drops such lints entirely, while `"not_dplyr"` only lints calls explicitly qualified as `dplyr::filter()`. The default, `"never"`, assumes all unqualified calls to `filter()` are `dplyr::filter()`.
* `sort_linter()` checks for code like `x == sort(x)` which is better served by using the function `is.unsorted()` (part of #884, @MichaelChirico).
* `paste_linter()` gains detection for file paths that are better constructed with `file.path()`, e.g. `paste0(dir, "/", file)` would be better as `file.path(dir, file)` (part of #884, #2082, @MichaelChirico). What exactly gets linted here can be fine-tuned with the `allow_file_path` option (`"double_slash"` by default, with alternatives `"never"` and `"always"`). When `"always"`, these rules are ignored. When `"double_slash"`, paths appearing to construct a URL that have consecutive forward slashes (`/`) are skipped. When `"never"`, even URLs should be constructed with `file.path()`.
* `seq_linter()` recommends `rev()` in the lint message for lints like `nrow(x):1` (#1542, @MichaelChirico).
* `function_argument_linter()` detects usage of `missing()` for the linted argument (#1546, @MichaelChirico). The simplest fix for `function_argument_linter()` lints is typically to set that argument to `NULL` by default, in which case it's usually preferable to update function logic checking `missing()` to check `is.null()` instead.
* `commas_linter()` gains an option `allow_trailing` (default `FALSE`) to allow trailing commas while indexing. (#2104, @MEO265)
* `unreachable_code_linter()`
  + checks for code inside `if (FALSE)` and other conditional loops with deterministically false conditions (#1428, @ME0265).
  + checks for unreachable code inside `if`, `else`, `for`, `while`, and `repeat` blocks, including combinations with `break` and `next` statements. (#2105, @ME0265).
* `implicit_assignment_linter()` gains an argument `allow_lazy` (default `FALSE`) that allows optionally skipping lazy assignments like `A && (B <- foo(A))` (#2016, @MichaelChirico).
* `unused_import_linter()` gains an argument `interpret_glue` (default `TRUE`) paralleling that in `object_usage_linter()` to toggle whether `glue::glue()` expressions should be inspected for exported object usage (#2042, @MichaelChirico).
* `default_undesirable_functions` is updated to also include `Sys.unsetenv()` and `structure()` (#2192 and #2228, @IndrajeetPatil and @MichaelChirico).
* Linters with logic around the magrittr pipe `%>%` consistently apply it to the other pipes `%!>%`, `%T>%`, `%<>%` (and possibly `%$%`) where appropriate (#2008, @MichaelChirico).
  + `brace_linter()`
  + `pipe_call_linter()`
  + `pipe_continuation_linter()`
  + `unnecessary_concatenation_linter()`
  + `unnecessary_placeholder_linter()`
* Linters with logic around function declarations consistently include the R 4.0.0 shorthand `\()` (#2190, @MichaelChirico).
  + `brace_linter()`
  + `function_left_parentheses_linter()`
  + `indentation_linter()`
  + `object_length_linter()`
  + `object_name_linter()`
  + `package_hooks_linter()`
  + `paren_body_linter()`
  + `unnecessary_lambda_linter()`
  + `unreachable_code_linter()`

### Lint accuracy fixes: removing false positives

* `fixed_regex_linter()`
  + Is pipe-aware, in particular removing false positives around piping into {stringr} functions like `x |> str_replace(fixed("a"), "b")` (#1811, @MichaelChirico).
  + Ignores non-string inputs to `pattern=` as a keyword argument (#2159, @MichaelChirico).
* Several linters avoiding false positives in `$` extractions get the same exceptions for `@` extractions, e.g. `S4@T` will no longer throw a `T_and_F_symbol_linter()` hit (#2039, @MichaelChirico).
  + `T_and_F_symbol_linter()`
  + `for_loop_index_linter()`
  + `literal_coercion_linter()`
  + `object_name_linter()`
  + `undesirable_function_linter()`
  + `unreachable_code_linter()`
  + `yoda_test_linter()`
* `sprintf_linter()` is pipe-aware, so that `x %>% sprintf(fmt = "%s")` no longer lints (#1943, @MichaelChirico).
* `condition_message_linter()` ignores usages of extracted calls like `env$stop(paste(a, b))` (#1455, @MichaelChirico).
* `inner_combine_linter()` no longer throws on length-1 calls to `c()` like `c(exp(2))` or `c(log(3))` (#2017, @MichaelChirico). Such usage is discouraged by `unnecessary_concatenation_linter()`, but `inner_combine_linter()` _per se_ does not apply.
* `sort_linter()` only lints on `order()` of a single vector, excluding e.g. `x[order(x, y)]` and `x[order(y, x)]` (#2156, @MichaelChirico).
* `redundant_ifelse_linter()` is aware of `dplyr::if_else()`'s `missing=` argument, so that `if_else(A, TRUE, FALSE, missing = FALSE)` doesn't lint, but `if_else(A, TRUE, FALSE, NA)` does (#1941, @MichaelChirico). Note that `dplyr::coalesce()` or `tidyr::replace_na()` may still be preferable.

### Lint accuracy fixes: removing false negatives

* `unreachable_code_linter()` finds unreachable code even in the presence of a comment or semicolon after `return()` or `stop()` (#2127, @MEO265).
* `implicit_assignment_linter()`
  + finds assignments in call arguments besides the first one (#2136, @MichaelChirico).
  + finds assignments in parenthetical expressions like `if (A && (B <- foo(A))) { }` (#2138, @MichaelChirico).
* `unnecessary_lambda_linter()` checks for cases using explicit returns, e.g. `lapply(x, \(xi) return(sum(xi)))` (#1567, @MichaelChirico).
  + thanks to @Bisaloo and @strengejacke for detecting a regression in the original fix (#2231, #2247).

# lintr 3.1.0

## Deprecations & Breaking Changes

* `.lintr` files can now be kept in the directory `.github/linters` for better compatibility with Super-Linter. Note that this may be a breaking change if you already have a config in `.github/linters` inside a subdirectory as well as in your R project's root, since the former will now be discovered first where it was ignored before. Please see `vignette("lintr")` for details on how configs are discovered (#1746, @tonyk7440 and @klmr).
* `single_quotes_linter()` is deprecated in favor of the more generalizable `quotes_linter()` (#1729, @MichaelChirico).
* `unneeded_concatentation_linter()` is deprecated in favor of `unnecessary_concatenation_linter()` for naming consistency (#1707, @IndrajeetPatil).
* `consecutive_stopifnot_linter()` is deprecated in favor of the more general (see below) `consecutive_assertion_linter()` (#1604, @MichaelChirico).
* `no_tab_linter()` is deprecated in favor of `whitespace_linter()` for naming consistency and future generalization (#1954, @MichaelChirico).
* `available_linters()` prioritizes `tags` over `exclude_tags` in the case of overlap, i.e., tags listed in both arguments are included, not excluded. We don't expect many people to be affected by this, and the old behavior was not made explicit in the documentation, but make note of it here since it required changing a test in lintr's own suite where `linters_with_tags()` implicitly assumed this behavior.
* `lint()`, `lint_dir()`, and `lint_package()` no longer accept certain arguments (`cache=` for `lint()`, `relative_path=` for the latter two) positionally. The `warning()` since 3.0.0 has been upgraded to an error.

## Bug fixes

* `linters_with_tags()` now includes the previously missing spaces around "and" when listing missing linters advertised by `available_linters()`. 
  This error message may appear e.g. when you update lintr to a version with new linters but don't restart your R session (#1946, @Bisaloo)

* `fixed_regex_linter()` is more robust to errors stemming from unrecognized escapes (#1545, #1845, @IndrajeetPatil).

* `get_source_expressions()` can handle Sweave/Rmarkdown documents with reference chunks like `<<ref_file>>` (#779, @MichaelChirico).
  Note that these are simply skipped, rather than attempting to retrieve the reference and also lint it.

* `assignment_linter()` no longer lints assignments in braces that include comments when `allow_trailing = FALSE` (#1701, @ashbaldry)

* `object_usage_linter()`
  + No longer silently ignores usage warnings that don't contain a quoted name (#1714, @AshesITR)
  + No longer fails on code with comments inside a multi-line call to `glue::glue()` (#1919, @MichaelChirico)

* `namespace_linter()` correctly recognizes backticked operators to be exported from respective namespaces (like `` rlang::`%||%` ``) (#1752, @IndrajeetPatil)

* `lint_package()` correctly finds a package from within a subdir if the `path` points to anywhere within the package (#1759, @AshesITR)

* Improved error behavior in `Lint()`, `lint()` and `xml_nodes_to_lints()` (#1427, #763, @AshesITR)
  + `Lint()` validates its inputs more thoroughly, preventing errors during `print.Lints` like "Error in rep.int(character, length) : invalid 'times' value:".
  + `lint()` no longer tries to create an expression tree with unexpected end of input errors, because they can be broken.
  + `xml_nodes_to_lints()` warns if it can't find lint locations and uses dummy locations as a fallback.

* `linters_with_defaults()` no longer erroneously marks linter factories as linters (#1725, @AshesITR).

* Row names for `available_linters()` data frame are now contiguous (#1781, @IndrajeetPatil).

* `object_name_linter()` allows all S3 group Generics (see `?base::groupGeneric`) and S3 generics defined in a different file in the same package (#1808, #1841, @AshesITR)

* `object_usage_linter()` improves identification of the exact source of a lint
  + for undefined variables in expressions with where the variable is used as a symbol in a usual way, for example in a formula or in an extraction with `$` (#1914, @MichaelChirico).
  + for general usage warnings without location info (#1986 and #1917, @AshesITR)

* `function_left_parentheses_linter()` produces a more specific lint (and no longer fails) when the opening parenthesis is on a different line than `function` or the call name (#1953, @MichaelChirico). Thanks also to @IndrajeetPatil and @lorenzwalthert for identifying a regression in the initial fix, #1963.

## Changes to defaults

* Set the default for the `except` argument in `duplicate_argument_linter()` to `c("mutate", "transmute")`.
  This allows sequential updates like `x |> mutate(a = b + 1, a = log(a))` (#1345, @IndrajeetPatil).

* `object_usage_linter()`
   + gains `skip_with` argument to skip code in `with()` expressions. To be consistent with
     `R CMD check`, it defaults to `TRUE` (#941, #1458, @IndrajeetPatil).
   + Handles backticked symbols inside {glue} expressions correctly, e.g. ``glue("{`x`}")`` correctly
     determines `x` was used (#1619, @MichaelChirico)
   + Detects problems inside R4.1.0+ lambda functions (`\(...)`) (#1933, @MichaelChirico)

* `spaces_inside_linter()` allows terminal missing keyword arguments (e.g. `alist(arg = )`; #540, @MichaelChirico)

* `brace_linter()` allows empty braced expression on the same line (e.g. `while (updating_condition()) { }`)
  regardless of `allow_single_line` to match the corresponding behavior in {styler}. This is an expedient while
  the style guide on handling this case awaits clarification: https://github.com/tidyverse/style/issues/191.
  (#1346, @MichaelChirico)

* `undesirable_function_linter()` and `undesirable_operator_linter()` now produce an error 
  if empty vector of undesirable functions or operators is provided (#1867, @IndrajeetPatil).

* New linters which are also included as defaults (see "New linters" for more details):
   + `indentation_linter()`
   + `quotes_linter()`
   + `unnecessary_concatenation_linter()`
   + `whitespace_linter()`

* `lint_package()` also looks for files in `exec/` (#1950, @jmaspons).

## New and improved features

* New `get_r_string()` helper to get the R-equivalent value of a string, especially useful for R-4-style raw strings.
  Previously an internal `lintr` helper, now exported to facilitate writing custom linters (#1493, @MichaelChirico).

* `object_usage_linter()` improves lint metadata when detecting undefined infix operators, e.g. `%>%` or `:=` (#1497, @MichaelChirico)

* `unused_import_linter()` can detect datasets from imported packages and no longer
  warns when a package is imported only for its datasets (#1545, @IndrajeetPatil).

* When a linter triggers an error, `lint()` will provide a more actionable summary of where the
  error occurred, particularly useful for cases like `lint_package()` where both the responsible file
  and the responsible linter would be unknown (@MichaelChirico).

  Typically, linters should not themselves cause R to stop -- syntax errors lead to error lints,
  for example. Please report such failures as they are likely bugs.

* `pipe_continuation_linter()` recognizes violations involving the native R pipe `|>` (#1609, @MichaelChirico)

* `paste_linter()` also catches usages like `paste(rep("*", 10L), collapse = "")` that can be written more
  concisely as `strrep("*", 10L)` (#1108, @MichaelChirico)

* `spaces_inside_linter()` produces lints for spaces inside `[[` (#1673, @IndrajeetPatil).

* `sprintf_linter()` also applies to `gettextf()` (#1677, @MichaelChirico)

* Documentation for all linters contains examples of code that does and does not produce lints (#1492, @IndrajeetPatil).

* `implicit_integer_linter()` gains parameter `allow_colon` to skip lints on expressions like `1:10` (#1155, @MichaelChirico)

* `infix_spaces_linter()` supports the native R pipe `|>` (#1793, @AshesITR)

* `unnecessary_concatenation_linter()` (f.k.a. `unneeded_concatenation_linter()`) no longer lints on `c(...)` (i.e., passing `...` in a function call) when `allow_single_expression = FALSE` (#1696, @MichaelChirico)

* `object_name_linter()` gains parameter `regexes` to allow custom naming conventions (#822, #1421, @AshesITR)

* `literal_coercion_linter()` reports a replacement in the lint message, e.g. code like `as.integer(1)` will
  suggest using `1L` instead, and code like `as.numeric(NA)` will suggest using `NA_real_` instead (#1439, @MichaelChirico)

* Added `format()` functions for `lint` and `lints` (#1784, @AshesITR)

* `all_linters()` function provides an easy way to access all available linters (#1843, @IndrajeetPatil)

* `missing_argument_linter()` allows missing arguments in `quote()` calls (#1889, @IndrajeetPatil). 

* `get_source_expressions()` correctly extracts indented code chunks from R Markdown documents, which helps avoid spurious lints related to whitespace (#1945, @MichaelChirico). The convention taken is that, within each chunk, all code is anchored relative to the leftmost non-whitespace column.

* `available_linters()` gives priority to `tags` over `exclude_tags` in the case of overlap. In particular, this means that `available_linters(tags = "deprecated")` will work to return deprecated linters without needing to specify `exclude_tags` (#1959, @MichaelChirico).

* The {lintr} configuration file is now searched in the system's user configuration path; the lintr config filename can
  also be configured explicitly by setting the environment variable `R_LINTR_LINTER_FILE` (#460, @klmr)

* Errors in the {lintr} configuration file now produce more informative error messages (#886, @AshesITR)

### New linters

* `matrix_apply_linter()` recommends use of dedicated `rowSums()`, `colSums()`, `colMeans()`, `rowMeans()` over `apply(., MARGIN, sum)` or `apply(., MARGIN, mean)`. The recommended alternative is much more efficient and more readable (#1869, @Bisaloo).

* `unnecessary_lambda_linter()`: detect unnecessary lambdas (anonymous functions), e.g.
  `lapply(x, function(xi) sum(xi))` can be `lapply(x, sum)` and `purrr::map(x, ~quantile(.x, 0.75, na.rm = TRUE))`
  can be `purrr::map(x, quantile, 0.75, na.rm = TRUE)`. Naming `probs = 0.75` can further improve readability (#1531, #1866, @MichaelChirico, @Bisaloo).

* `redundant_equals_linter()` for redundant comparisons to `TRUE` or `FALSE` like `is_treatment == TRUE` (#1500, @MichaelChirico)
* `lengths_linter()` for encouraging usage of `lengths(x)` instead of `sapply(x, length)` (and similar)

* `function_return_linter()` for handling issues in function `return()` statements. Currently handles assignments within the `return()`
  clause, e.g. `return(x <- foo())` (@MichaelChirico)

* `boolean_arithmetic_linter()` for identifying places where logical aggregations are more appropriate, e.g.
  `length(which(x == y)) == 0` is the same as `!any(x == y)` or even `all(x != y)` (@MichaelChirico)

* `for_loop_index_linter()` to prevent overwriting local variables in a `for` loop declared like `for (x in x) { ... }` (@MichaelChirico)

* `is_numeric_linter()` for redundant checks equivalent to `is.numeric(x)` such as `is.numeric(x) || is.integer(x)` or
  `class(x) %in% c("numeric", "integer")` (@MichaelChirico)

* `empty_assignment_linter()` for identifying empty assignments like `x = {}` that are more clearly written as `x = NULL` (@MichaelChirico)

* `unnecessary_placeholder_linter()` for identifying where usage of the {magrittr} placeholder `.` could be omitted (@MichaelChirico)

* `routine_registration_linter()` for identifying native routines that don't use registration (`useDynLib` in the `NAMESPACE`; @MichaelChirico)

* `indentation_linter()` for checking that the indentation conforms to 2-space Tidyverse-style (@AshesITR and @dgkf, #1411, #1792, #1898).

* `unnecessary_nested_if_linter()` for checking unnecessary nested `if` statements where a single 
  `if` statement with appropriate conditional expression would suffice (@IndrajeetPatil and @AshesITR, #1778).

* `implicit_assignment_linter()` for checking implicit assignments in function calls (@IndrajeetPatil and @AshesITR, #1777).

* `quotes_linter()` is a generalized version of (now deprecated) `single_quotes_linter()`. It accepts an argument `delimiter` to specify whether `"` or `'` should be the accepted method for delimiting character literals. The default, `"`, reflects the Tidyverse style guide recommendation and matches the behavior of `single_quotes_linter()`.

* `unnecessary_concatenation_linter()` is simply `unneeded_concatenation_linter()`, renamed.

* `consecutive_assertion_linter()` (f.k.a. `consecutive_stopifnot_linter()`) now lints for consecutive calls to `assertthat::assert_that()` (as long as the `msg=` argument is not used; #1604, @MichaelChirico).

* `whitespace_linter()` is simply `no_tab_linter()`, renamed. In the future, we plan to extend it to work for different whitespace preferences.

## Notes

* {lintr} now depends on R version 3.5.0, in line with the tidyverse policy for R version compatibility.

* `lint()` continues to support Rmarkdown documents. For users of custom .Rmd engines, e.g.
  `marginformat` from {tufte} or `theorem` from {bookdown}, note that those engines must be registered
  in {knitr} prior to running `lint()` in order for {lintr} to behave as expected, i.e., they should be
  shown as part of `knitr::knit_engines$get()`.
  
  For {tufte} and {bookdown} in particular, one only needs to load the package namespace to accomplish
  this (i.e., minimally `loadNamespace("tufte")` or `loadNamespace("bookdown")`, respectively, will
  register those packages' custom engines; since `library()` also runs `loadNamespace()`, running
  `library()` will also work). Note further that {tufte} only added this code to their `.onLoad()` recently
  after our request to do so (see https://github.com/rstudio/tufte/issues/117). Therefore, ensure you're using a
  more recent version to get the behavior described here for {tufte}.
  
  More generally, there is no requirement that `loadNamespace()` will register a package's custom {knitr}
  engines, so you may need to work with other package authors to figure out a solution for other engines.
  
  Thanks to Yihui and other developers for their helpful discussions around this issue (#797, @IndrajeetPatil).

* The output of `lint()` and `Lint()` gain S3 class `"list"` to assist with S3 dispatch (#1494, @MichaelChirico)
  + As a corollary, we now register an `as_tibble` method for class `lints`, conditional on {tibble} availability, to avoid dispatching to the `list` method which does not work with `lint()` output (#1997, @MichaelChirico)

* `object_usage_linter()` gives a more helpful warning when a `glue()` expression fails to evaluate (#1985, @MichaelChirico)

* The documentation of `object_name_linter()` now describes how `"symbols"`
works when passed to the `styles` parameter (#1924, @hedsnz).

# lintr 3.0.2

* Fix test to avoid leaving behind cache files in the global cache directory.

# lintr 3.0.1

* Skip multi-byte tests in non UTF-8 locales (#1504)

* `modify_defaults()` no longer uses the mistaken `"lintr_function"` S3 class, instead applying the
  `"linter"` class also common to `Linter()`. `Linter()` also includes `"function"` in the S3
  class of its output to facilitate S3 dispatch to `function` methods where appropriate (#1392, @MichaelChirico).

## Changes to defaults

* `brace_linter()` allows opening curly braces on a new line when there is 
  a comment ending the preceding line (#1433 and #1434, @IndrajeetPatil).

* `seq_linter()` produces lint for `seq(...)`, since it also cannot properly 
  handle empty edge cases (#1468, @IndrajeetPatil).

* `seq_linter()` additionally lints on `1:n()` (from {dplyr}) 
  and `1:.N` (from {data.table}) (#1396, @IndrajeetPatil).

* `literal_coercion_linter()` lints {rlang}'s atomic vector constructors 
  (i.e., `int()`, `chr()`, `lgl()`, and `dbl()`) if the argument is a scalar 
  (#1437, @IndrajeetPatil).

* `redundant_ifelse_linter()`'s lint message correctly suggests negation when 
  the `yes` condition is `0` (#1432, @IndrajeetPatil).

* `seq_linter()` provides more specific replacement code in lint message 
  (#1475, @IndrajeetPatil).

## New and improved features

* New `sort_linter()` to detect `x[order(x)]` and recommend the faster and clearer alternative: `sort(x)` (#1528, @Bisaloo)

* `unreachable_code_linter()` ignores trailing comments if they match a closing nolint block (#1347, @AshesITR).

* New `function_argument_linter()` to enforce that arguments with defaults appear last in function declarations,
  see the [Tidyverse design guide](https://design.tidyverse.org/required-no-defaults.html) (#450, @AshesITR).

* New `allow_trailing` argument added to `assignment_linter()` to check when assignment operators are at the 
  end of a line, and the value is on the following line (#1491, @ashbaldry) 

* New `sarif_output()` function to output lints to SARIF output (#1424, @shaopeng-gh)

* `commented_code_linter()` now lints commented argument code, containing a trailing comma, as well (#386, @AshesITR).
  For example a comment containing `#  na.rm = TRUE,` now triggers a lint.

## Bug fixes

* `object_length_linter()` does not fail in case there are dependencies with no exports (e.g. data-only packages) (#1424, #1509, @IndrajeetPatil).
* `get_source_expressions()` no longer fails on R files that match a knitr pattern (#743, #879, #1406, @AshesITR).
* Parse error lints now appear with the linter name `"error"` instead of `NA` (#1405, @AshesITR).  
  Also, linting no longer runs if the `source_expressions` contain invalid string data that would cause error messages
  in other linters. 
  in other linters.
* Prevent `lint()` from hanging on Rmd files with some syntax errors (#1443, @MichaelChirico).
* `get_source_expressions()` no longer omits trailing non-code lines from knitr files (#1400, #1415, @AshesITR).  
  This fixes the location information for `trailing_blank_lines_linter()` in RMarkdown documents without terminal
  newlines.
* The `vignette("lintr")` incorrectly cited `exclude` as the key for setting file exclusions in `.lintr` when it is 
  actually `exclusions`. (#1401, @AshesITR)
* Fixed file exclusion detection in `lint_dir()` so it no longer errors if there are multiple exclusions or no global
  exclusions configured for a single file (#1413, #1442, @AshesITR).

## Other changes

* The minimum needed version for soft dependency `{withr}` has been bumped to `2.5.0`
  (#1404, @IndrajeetPatil).
* Changed the deprecation warning for `with_defaults()` to also mention `modify_defaults()` (#1438, @AshesITR).
* Quarto files (`.qmd`) were supported out of the box. The documentation and the 
  testing infrastructure are updated to reflect this (#1486, @IndrajeetPatil).
  
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
  better match the [Tidyverse design principle](https://design.tidyverse.org/required-no-defaults.html) of
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
* **Encodings**: lintr now supports non-system character Encodings. The correct encoding
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
  with a lax mode for fewer false positive lints (#199, @fangly).

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
* New `semicolon_terminator_linter()` reports semicolons at the end of a line (#147,
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
* `lint_package()` now removes fully excluded files as soon as possible
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

# lintr 1.0.3

* Fix tests to work with changes in the parser in R 3.6

# lintr 1.0.2

* Fix tests to work with upcoming testthat release.

# lintr 1.0.1

* bugfix to work with knitr 1.16.7
* `expect_lint_free()` now is always skipped on CRAN. This is necessary because
  the non-binary R source may not be available when running tests on CRAN, and
  those tests may not be run in the package directory.

# lintr 1.0.0

* bugfix to work with testthat 1.0.0

# lintr 0.3.3

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
* Fixed bug that caused vim syntastic plugin not to work properly in windows (#46, @abossenbroek)
* allow lintr customization per project using `.lintr` config files.
* use `globalenv()` instead of `baseenv()` for default parent environment so
  that `methods` will be included.
* do not check object usage if eval fails.  Fixes (#24, reported by @fabian-s)
* `trailing_whitespace_linter` was reporting the incorrect line number
* Use RStudio source marker API to display lints (#37, @jjallaire)
* Permit single quotes if they quote literal double quotes (#28, @jackwasey)
* `# nolint` comments are respected with caching (#68, @krlmlr)
* Properly handle all knitr document formats
* Allow for (( when linting (#259, @nathaneastwood)
* Remove ^ from infix spaces to conform with tidyverse. (#302, @nathaneastwood)

# lintr 0.2.0

* Initial release
