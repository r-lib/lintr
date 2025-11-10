# Check for common mistakes around sorting vectors

This linter checks for some common mistakes when using
[`order()`](https://rdrr.io/r/base/order.html) or
[`sort()`](https://rdrr.io/r/base/sort.html).

## Usage

``` r
sort_linter()
```

## Details

First, it requires usage of [`sort()`](https://rdrr.io/r/base/sort.html)
over `.[order(.)]`.

[`sort()`](https://rdrr.io/r/base/sort.html) is the dedicated option to
sort a list or vector. It is more legible and around twice as fast as
`.[order(.)]`, with the gap in performance growing with the vector size.

Second, it requires usage of
[`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html) over
equivalents using [`sort()`](https://rdrr.io/r/base/sort.html).

The base function
[`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html) exists to
test the sortedness of a vector. Prefer it to inefficient and
less-readable equivalents like `x != sort(x)`. The same goes for
checking `x == sort(x)` – use `!is.unsorted(x)` instead.

Moreover, use of `x == sort(x)` can be risky because
[`sort()`](https://rdrr.io/r/base/sort.html) drops missing elements by
default, meaning `==` might end up trying to compare vectors of
differing lengths.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "x[order(x)]",
  linters = sort_linter()
)
#> <text>:1:3: warning: [sort_linter] sort(x, na.last = TRUE) is better than x[order(x)]. Note that it's always preferable to save the output of order() for the same variable as a local variable than to re-compute it.
#> x[order(x)]
#>   ^~~~~~~~

lint(
  text = "x[order(x, decreasing = TRUE)]",
  linters = sort_linter()
)
#> <text>:1:3: warning: [sort_linter] sort(x, decreasing = TRUE, na.last = TRUE) is better than x[order(x, decreasing = TRUE)]. Note that it's always preferable to save the output of order() for the same variable as a local variable than to re-compute it.
#> x[order(x, decreasing = TRUE)]
#>   ^~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "sort(x) == x",
  linters = sort_linter()
)
#> <text>:1:1: warning: [sort_linter] Use !is.unsorted(x) to test the sortedness of a vector.
#> sort(x) == x
#> ^~~~~~~~~~~~

# okay
lint(
  text = "x[sample(order(x))]",
  linters = sort_linter()
)
#> ℹ No lints found.

lint(
  text = "y[order(x)]",
  linters = sort_linter()
)
#> ℹ No lints found.

lint(
  text = "sort(x, decreasing = TRUE) == x",
  linters = sort_linter()
)
#> ℹ No lints found.

# If you are sorting several objects based on the order of one of them, such
# as:
x <- sample(1:26)
y <- letters
newx <- x[order(x)]
newy <- y[order(x)]
# This will be flagged by the linter. However, in this very specific case,
# it would be clearer and more efficient to run order() once and assign it
# to an object, rather than mix and match order() and sort()
index <- order(x)
newx <- x[index]
newy <- y[index]
```
