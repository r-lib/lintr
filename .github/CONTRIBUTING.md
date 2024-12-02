# Contributing to `{lintr}`

This outlines how to propose a change to `{lintr}`. For a detailed discussion on contributing to this, r-lib, and other tidyverse packages, please see the [development contributing guide](https://rstd.io/tidy-contrib) and our [code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file.  You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. If you’ve found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed). See the tidyverse guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Adding a new linter

If you wish to contribute a new linter, the [Creating new linters](https://lintr.r-lib.org/articles/creating_linters.html) article serves as a comprehensive guide.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("r-lib/lintr", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 

*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`. At a minimum, please avoid submitting PRs from your fork's `main` branch` as this can make the review process more complicated.

*   Make your changes, commit them to Git, and create a PR using `usethis::pr_push()`. Follow the prompts in your browser to complete the process. Use a concise title for your PR that summarizes the change, and include `Fixes #issue-number` in the PR _description_. Doing so will automatically close the linked issue when the PR is merged. For complicated changes, add a textual overview of what your PR does in the description. Consider breaking up large PRs into a chain of more digestible+focused smaller PRs.

*  For user-facing changes, add a bullet appropriately in the top section of `NEWS.md` (i.e. below the first header). Follow the style described in <https://style.tidyverse.org/news.html>. Most importantly, your audience for NEWS items is a package user, i.e., _not_ a package developer.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles.

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. Contributions with test cases included are easier to accept.

## Code of Conduct

Please note that the lintr project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
