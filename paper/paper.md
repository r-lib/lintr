---
title: "Static Code Analysis for R"
date: "2024-06-21"
tags: ["R", "linter", "tidyverse"]
authors:
  - name: Jim Hester
    affiliation: 1
    orcid: 0000-0002-2739-7082
  - name: Florent Angly
    affiliation: ~
    orcid: ~
  - name: Michael Chirico
    affiliation: 2
    orcid: 0000-0003-0787-087X
  - name: Russ Hyde
    affiliation: 5
    orcid: ~
  - name: Ren Kun
    affiliation: ~
    orcid: ~
  - name: Indrajeet Patil
    orcid: 0000-0003-1995-6531
    affiliation: 4
  - name: Alexander Rosenstock
    affiliation: 3
    orcid: ~
affiliations:
  - index: 1
    name: Netflix
  - index: 2
    name: Google
  - index: 3
    name: Mathematisches Institut der Heinrich-Heine-Universität Düsseldorf
  - index: 4
    name: Preisenergie GmbH, Munich, Germany
  - index: 5
    name: Jumping Rivers
output: 
    md_document:
      variant: "markdown"
      preserve_yaml: true
      standalone: true
bibliography: paper.bib
csl: apa.csl
link-citations: yes
---

# Statement of Need

R is an interpreted, dynamically-typed programming language [@base2023].
It is a popular choice for statistical analysis and visualization, and
is used by a wide range of researchers and data scientists. The
`{lintr}` package is an open-source R package that provides static code
analysis [@enwiki:1218663830] to check for a variety of common problems
related to readability, efficiency, consistency, style, etc. In
particular, by default it enforces the tidyverse style guide
[@Wickham2023]. It is designed to be easy to use and integrate into
existing workflows, and can be used as part of an automated build or
continuous integration process. `{lintr}` also integrates with a number
of popular IDEs and text editors, such as RStudio and Visual Studio
Code, making it convenient for users to run `{lintr}` checks on their
code as they work.

# Features

As of this writing, `{lintr}` offers 113 linters.

``` r
library(lintr)

length(all_linters())
#> [1] 113
```

Naturally, we can't discuss all of them here. To see details about all
available linters, we encourage readers to see
<https://lintr.r-lib.org/dev/reference/index.html#individual-linters>.

We will showcase one linter for each kind of common problem found in R
code.

-   **Best practices**

`{lintr}` offers linters that can detect problematic antipatterns and
suggest alternatives that follow best practices.

For example, expressions like `ifelse(x, TRUE, FALSE)` and
`ifelse(x, FALSE, TRUE)` are redundant; just `x` or `!x` suffice in R
code where logical vectors are a core data structure. The
`redundant_ifelse_linter()` linter detects such discouraged usages.

``` r
lint(
  text = "ifelse(x >= 2.5, TRUE, FALSE)",
  linters = redundant_ifelse_linter()
)
#> <text>:1:1: warning: [redundant_ifelse_linter] Just use the
#>     logical condition (or its negation) directly instead of
#>     calling ifelse(x, TRUE, FALSE)
#> ifelse(x >= 2.5, TRUE, FALSE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```

``` r
lint(
  text = "x >= 2.5",
  linters = redundant_ifelse_linter()
)
```

-   **Efficiency**

Sometimes the users might not be aware of a more efficient way offered
by R for carrying out a computation. `{lintr}` offers linters to improve
code efficiency by avoiding common inefficient patterns.

For example, the `any_is_na_linter()` linter detects usages of
`any(is.na(x))` and suggests `anyNA(x)` as a more efficient alternative
to detect presence of *any* missing values.

``` r
lint(
  text = "any(is.na(x), na.rm = TRUE)",
  linters = any_is_na_linter()
)
#> <text>:1:1: warning: [any_is_na_linter] anyNA(x) is better
#>     than any(is.na(x)).
#> any(is.na(x), na.rm = TRUE)
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~
```

`anyNA()` in R is more efficient than `any(is.na())` because it stops
execution once a missing value is found, while `is.na()` evaluates the
entire vector.

``` r
lint(
  text = "anyNA(x)",
  linters = any_is_na_linter()
)
```

-   **Readability**

Coders spend significantly more time reading than writing code
[@mcconnell2004code]. Thus, writing readable code makes the code more
maintainable and reduces the possibility of introducing bugs stemming
from a poor understanding of the code.

`{lintr}` provides a number of linters that suggest more readable
alternatives. For example, `comparison_negation_linter()` blocks usages
like `!(x == y)` where a direct relational operator is appropriate.

``` r
lint(
  text = "!x == 2",
  linters = comparison_negation_linter()
)
#> <text>:1:1: warning: [comparison_negation_linter] Use x !=
#>     y, not !(x == y).
#> !x == 2
#> ^~~~~~~
```

Note also the complicated operator precedence. The more readable
alternative here uses `!=`:

``` r
lint(
  text = "x != 2",
  linters = comparison_negation_linter()
)
```

-   **Tidyverse style**

`{lintr}` also provides linters to enforce the style used throughout the
`{tidyverse}` [@Wickham2019] ecosystem of R packages. This style of
coding has been outlined in the tidyverse style guide [@Wickham2023].

For example, the style guide recommends using snake_case for
identifiers:

``` r
lint(
  text = "MyVar <- 1L",
  linters = object_name_linter()
)
#> <text>:1:1: style: [object_name_linter] Variable and
#>     function name style should match snake_case or symbols.
#> MyVar <- 1L
#> ^~~~~
```

``` r
lint(
  text = "my_var <- 1L",
  linters = object_name_linter()
)
```

-   **Common mistakes**

One category of linters helps you detect some common mistakes statically
and provide early feedback.

For example, duplicate arguments in function calls can sometimes cause
run-time errors:

``` r
mean(x = 1:5, x = 2:3)
#> Error in mean(x = 1:5, x = 2:3): formal argument "x" matched by multiple actual arguments
```

But `duplicate_argument_linter()` can check for this statically:

``` r
lint(
  text = "mean(x = 1:5, x = 2:3)",
  linters = duplicate_argument_linter()
)
#> <text>:1:15: warning: [duplicate_argument_linter] Avoid
#>     duplicate arguments in function calls.
#> mean(x = 1:5, x = 2:3)
#>               ^
```

Even for cases where duplicate arguments are not an error, this linter
explicitly discourages duplicate arguments.

``` r
lint(
  text = "list(x = TRUE, x = FALSE)",
  linters = duplicate_argument_linter()
)
#> <text>:1:16: warning: [duplicate_argument_linter] Avoid
#>     duplicate arguments in function calls.
#> list(x = TRUE, x = FALSE)
#>                ^
```

This is because objects with duplicated names objects can be hard to
work with programmatically and should typically be avoided.

``` r
l <- list(x = TRUE, x = FALSE)
l["x"]
#> $x
#> [1] TRUE
```

``` r
l[names(l) == "x"]
#> $x
#> [1] TRUE
#> 
#> $x
#> [1] FALSE
```

# Extensibility

`{lintr}` is designed for extensibility by allowing users to easily
create custom linting rules. There are two main ways to customize it:

-   Use additional arguments in existing linters. For example, although
    tidyverse style guide prefers snake_case for identifiers, if a
    project's conventions require it, the relevant linter can be
    customized to support it:

``` r
lint(
  text = "my.var <- 1L",
  linters = object_name_linter(styles = "dotted.case")
)
```

-   Create new linters (by leveraging functions like
    `lintr::make_linter_from_xpath()`) tailored to match project- or
    organization-specific coding standards.

# Benefits of using `{lintr}`

There are several benefits to using `{lintr}` to analyze and improve R
code. One of the most obvious is that it can help users identify and fix
problems in their code, which can save time and effort during the
development process. By catching issues early on, `{lintr}` can help
prevent bugs and other issues from creeping into code, which can save
time and effort when it comes to debugging and testing.

Another benefit of `{lintr}` is that it can help users write more
readable and maintainable code. By enforcing a consistent style and
highlighting potential issues, `{lintr}` can help users write code that
is easier to understand and work with. This is especially important for
larger projects or teams, where multiple contributors may be working on
the same codebase and it is important to ensure that code is easy to
follow and understand, particularly when frequently switching context
among code primarily authored by different people.

It can also be a useful tool for teaching and learning R. By providing
feedback on code style and potential issues, it can help users learn
good coding practices and improve their skills over time. This can be
especially useful for beginners, who may not yet be familiar with all of
the best practices for writing R code.

Finally, `{lintr}` has had a large and active user community since its
birth in 2014 which has contributed to its rapid development,
maintenance, and adoption. At the time of writing, `{lintr}` is in a
mature and stable state and therefore provides a reliable API that is
unlikely to feature fundamental breaking changes.

# Conclusion

`{lintr}` is a valuable tool for R users to help improve the quality and
reliability of their code. Its static code analysis capabilities,
combined with its flexibility and ease of use, make it relevant and
valuable for a wide range of applications.

# Licensing and Availability

`{lintr}` is licensed under the MIT License, with all source code openly
developed and stored on GitHub (<https://github.com/r-lib/lintr>), along
with a corresponding issue tracker for bug reporting and feature
enhancements.

# Conflicts of interest

The authors declare no conflict of interest.

# Funding

This work was not financially supported by any of the affiliated
institutions of the authors.

# Acknowledgments

`{lintr}` would not be possible without the immense work of the [R-core
team](https://www.r-project.org/contributors.html) who maintain the R
language and we are deeply indebted to them. We are also grateful to all
contributors to `{lintr}`.

# References {#references .unnumbered}
