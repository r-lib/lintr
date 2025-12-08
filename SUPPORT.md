# Getting help with lintr

Thanks for using lintr. Before filing an issue, there are a few places
to explore and pieces to put together to make the process as smooth as
possible.

## Making a reproducible example

Start by making a minimal **repr**oducible **ex**ample using the
[reprex](http://reprex.tidyverse.org/) package. For how to use reprex
effectivly, check out the [Get help!](https://www.tidyverse.org/help/)
section of the tidyverse site.

The most useful function to create reprexes for
[lintr](https://lintr.r-lib.org) issues is
[`lint()`](https://lintr.r-lib.org/reference/lint.md). You can include
code that doesn’t lint as expected with the linter in question. For
example,

``` r
library(lintr)

lint(
  text = "x = 1",
  linters = assignment_linter()
)
```

If code in question contains characters that need to be escaped,
consider using raw strings instead to save yourself some headache
figuring out the multiple levels of escapes.

## Asking for help

Armed with your reprex, the next step is to figure out [where to
ask](https://www.tidyverse.org/help/#where-to-ask).

If it’s a clarification question (e.g. you don’t know how to exclude
certain files from lint workflow), start with
[community.rstudio.com](https://community.rstudio.com/), and/or
StackOverflow. There are more people there to answer questions.

## Filing an issue

If it’s a bug, you can create an issue with a reprex.

If it’s a false positive or false negative lint, you can either create
an issue with a reprex in [lintr](https://lintr.r-lib.org) repository,
or discuss the underlying [style guide](https://style.tidyverse.org/)
itself in the respective
[repository](https://github.com/tidyverse/style/issues/).

Before opening a new issue, be sure to [search issues and pull
requests](https://github.com/r-lib/lintr/issues/) to make sure the bug
hasn’t been reported and/or already fixed in the development version. By
default, the search will be pre-populated with `is:issue is:open`. You
can [edit the
qualifiers](https://help.github.com/articles/searching-issues-and-pull-requests/)
(e.g. `is:pr`, `is:closed`) as needed. For example, you’d simply remove
`is:open` to search *all* issues in the repo, open or closed.

Thanks for your help!
