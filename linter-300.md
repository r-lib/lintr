We are very excited to announce the release of [lintr](https://lintr.r-lib.org) 3.0.0! lintr provides
both a framework for [static analysis](https://www.perforce.com/blog/sca/what-static-analysis) of R packages
and scripts and a variety of linters designed to enforce the [tidyverse style guide](https://style.tidyverse.org/).

You can install it from CRAN with:

```r
install.packages("lintr")
```

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

## Google linters

# What's next in lintr
