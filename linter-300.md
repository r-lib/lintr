We are very excited to announce the release of [lintr](https://lintr.r-lib.org) 3.0.0! lintr provides
both a framework for [static analysis](https://www.perforce.com/blog/sca/what-static-analysis) of R packages
and scripts and a variety of linters designed to enforce the [tidyverse style guide](https://style.tidyverse.org/).

You can install it from CRAN with:

```r
install.packages("lintr")
```

Check our vignettes on integrating lintr with your preferred IDE
([`vignette("editors")`](https://lintr.r-lib.org/articles/editors.html))
and your preferred CI tools
([`vignette("continuous-integration")`](https://lintr.r-lib.org/articles/continuous-integration.html))
to get started using lintr. We've also added `lintr::use_lintr()` for a usethis-inspired
interactive tool to configure lintr for your package/repo.

This blog post will highlight the biggest changes coming in this update which drove us to declare it a major release.

## Linter factories

As of lintr 3.0.0, _all_ linters must be [function factories](https://adv-r.hadley.nz/function-factories.html).

Previously, only parameterizable linters (such as `line_length_linter`, which takes a parameter controlling how
wide lines are allowed to be without triggering a lint) were factories, but this led to (1) inconsistency -- some
linters were designated as calls like `line_length_linter(120)` while others were designated as names like
`no_tab_linter`; (2) brittleness -- some linters evolve to gain (or lose) parameters over time  (e.g. `assignment_linter`
gained two arguments, `allow_cascading_assign` and `allow_right_assign`, to fine-tune the handling of the
cascading assignment operators `<<-`/`->>` and right assignment operators `->`/`->>`, respectively); and
(3) performance -- factories can run some fixed computations at declaration and store them in the function environment,
whereas previously the calculation would need to be repeated on every expression of every file being linted.

This has two significant practical implications and are the main reason this is a major release.

First, lintr invocations should always use the call form, so old usages like:

```r
lint_package(linters = assignment_linter)
```

should be replaced with:

```r
lint_package(linters = assignment_linter())
```

We expect this to show up in most cases through users' .lintr configuration files.

Second, users implementing custom linters need to convert to function factories.

That means replacing:

```r
my_custom_linter <- function(source_expression) { ... }
```

With:

```r
my_custom_linter <- function() Linter(function(source_expression) { ... }))
```

## Linter metadatabase, linter documentation, and pkgdown

We have also overhauled how linters are documented. Previously, all linters
were documented on a single page and described in a quick blurb. This has
grown unwieldy as lintr now exports 72 linters! Now, each linter gets its own
page, which will make it easier to document any parameters, enumerate edge cases/
known false positives, add links to external resources, etc.

To make this even more navigable, we've also added some metadata to a database
in the form of tags for each linter. For example, `brace_linter` has tags
style, readability, default, and configurable. Each tag gets its own documentation
page (e.g. `?readability_linters`) which describes the tag and lists all of the
associated linters exported by lintr.

To further facilitate discoverability, we've added `available_linters()` and
`available_tags()` to list registered linters (including custom linters not
exported by lintr itself; see `?available_linters`).

Moreover, lintr's documentation is now available as a website thanks to
Hadley Wickham's contribution to create a pkgdown website for the package:
[lintr.r-lib.org](https://lintr.r-lib.org).

## Google linters

This release also features more than 30 new linters originally authored by Google developers.
Google adheres mostly to the tidyverse style guide and uses lintr to improve the consistency
of R code in its considerable internal R code base. These linters detect common issues with
readability, consistency, and performance. Here are some examples:

 - `any_is_na_linter` detects the usage of `any(is.na(x))`; `anyNA(x)` is nearly always a better choice,
   both for performance and for readability
 - `expect_named_linter` detects usage in [testthat](http://testthat.r-lib.org/) suites like
   `expect_equal(names(x), c("a", "b", "c"))`; `testthat` also exports `expect_named()` which is
   tailor made to make more readable tests like `expect_named(x, c("a", "b", "c"))`
 - `vector_logic_linter` detects usage of vector logic operators `|` and `&` in situations where
   scalar logic applies, e.g. `if (x | y) { ... }` should be `if (x || y) { ... }`. The latter
   is more efficient and less error-prone.
 - `strings_as_factors_linter` helps developers maintaining code that straddles the R 4.0.0 boundary,
   where the default value of `stringsAsFactors`
   [changed from `TRUE` to `FALSE`](https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/),
   by identifying usages of `data.frame()` that (1) have known string columns and (2) don't declare
   a value for `stringsAsFactors`, and thus rely on the default.

See the NEWS for the complete list.

## Other improvements

This is a big release -- almost 2 years in the making -- and there has been a plethora of smaller
but nonetheless important changes to lintr. Please check the NEWS for a complete enumeration of these;
here are a few more highlights:

# What's next in lintr

## Hosting linters for non-tidyverse style guides?

With the decision to accept a bevy of linters from Google that are not strictly related to the tidyverse
style guide, we also opened the door to hosting linters for enforcing other style guides, for example
the [Bioconductor R code guide](https://contributions.bioconductor.org/r-code.html). We look forward to
community contributions in this vein.

# Acknowledgements

Welcome [Kun Ren](@renkun-ken), Michael Chirico(@MichaelChirico),
and [Alexander Rosenstock](@AshesITR) to the lintr authors team!

And a great big thanks to the other 20 authors who have contributed to this release of lintr:

[@1beb](https://github.com/1beb), [@danielinteractive](https://github.com/danielinteractive),
[@dmurdoch](https://github.com/dmurdoch), [@dpprdan](https://github.com/dpprdan),
[@dragosmg](https://github.com/dragosmg), [@eitsupi](https://github.com/eitsupi),
[@fabian-s](https://github.com/fabian-s), [@fdlk](https://github.com/fdlk),
[@frederic-mahe](https://github.com/frederic-mahe), [@hadley](https://github.com/hadley),
[@huisman](https://github.com/huisman), [@jimhester](https://github.com/jimhester),
[@jonkeane](https://github.com/jonkeane), [@klmr](https://github.com/klmr),
[@kpagacz](https://github.com/kpagacz), [@leogama](https://github.com/leogama),
[@michaelquinn32](https://github.com/michaelquinn32), [@russHyde](https://github.com/russHyde),
[@thisisnic](https://github.com/thisisnic), [@yutannihilation](https://github.com/yutannihilation)
