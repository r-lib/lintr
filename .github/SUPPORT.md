# Getting help with lintr

Thanks for using lintr. Before filing an issue, there are a few places
to explore and pieces to put together to make the process as smooth as possible.

Start by making a minimal **repr**oducible **ex**ample using the 
[reprex](http://reprex.tidyverse.org/) package. If you haven't heard of or used 
reprex before, you're in for a treat! Seriously, reprex will make all of your 
R-question-asking endeavors easier (which is a pretty insane ROI for the five to 
ten minutes it'll take you to learn what it's all about). For additional reprex
pointers, check out the [Get help!](https://www.tidyverse.org/help/) section of
the tidyverse site.

The most useful function to create reprexes for `{lintr}` issues is `lint()`.
You can include code that doesn't lint as expected with the linter in question.
For example,

```r
lint(
  text = "x = 1",
  linters = assignment_linter()
)
```

If code in question contains characters that need to be escaped, consider using
raw strings instead.

Armed with your reprex, the next step is to figure out [where to ask](https://www.tidyverse.org/help/#where-to-ask). 

If it's a clarification question (e.g. you don't know how to exclude certain 
files from lint workflow), start with [community.rstudio.com](https://community.rstudio.com/), 
and/or StackOverflow. There are more people there to answer questions.

If it's a bug, you can create an issue with a reprex.

If it's a false positive or false negative lint, you can either create an issue 
with a reprex in `{lintr}` repository, or discuss the underlying style guide
itself in the respective [repository](https://github.com/tidyverse/style/issues/).

Before opening a new issue, be sure to [search issues and pull requests](https://github.com/tidyverse/lintr/issues) to make sure the 
bug hasn't been reported and/or already fixed in the development version. By 
default, the search will be pre-populated with `is:issue is:open`. You can 
[edit the qualifiers](https://help.github.com/articles/searching-issues-and-pull-requests/) 
(e.g. `is:pr`, `is:closed`) as needed. For example, you'd simply
remove `is:open` to search _all_ issues in the repo, open or closed.


If you _are_ in the right place, and need to file an issue, please review the 
["File issues"](https://www.tidyverse.org/contribute/#issues) paragraph from 
the tidyverse contributing guidelines.

Thanks for your help!
