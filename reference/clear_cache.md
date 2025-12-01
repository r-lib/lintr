# Clear the lintr cache

Clear the lintr cache

## Usage

``` r
clear_cache(file = NULL, path = NULL)
```

## Arguments

- file:

  filename whose cache to clear. If you pass `NULL`, it will delete all
  of the caches.

- path:

  directory to store caches. Reads option 'lintr.cache_directory' as the
  default.

## Value

0 for success, 1 for failure, invisibly.
