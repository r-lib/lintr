# Default undesirable functions and operators

Lists of function names and operators for
[`undesirable_function_linter()`](https://lintr.r-lib.org/reference/undesirable_function_linter.md)
and
[`undesirable_operator_linter()`](https://lintr.r-lib.org/reference/undesirable_operator_linter.md).
There is a list for the default elements and another that contains all
available elements. Use
[`modify_defaults()`](https://lintr.r-lib.org/reference/modify_defaults.md)
to produce a custom list.

## Usage

``` r
all_undesirable_functions

default_undesirable_functions

all_undesirable_operators

default_undesirable_operators
```

## Format

A named list of character strings.

## Details

The following functions are sometimes regarded as undesirable:

- [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) As an
  alternative, use
  [`withr::with_libpaths()`](https://withr.r-lib.org/reference/with_libpaths.html)
  for a temporary change instead of permanently modifying the library
  location.

- [`attach()`](https://rdrr.io/r/base/attach.html) As an alternative,
  use roxygen2's @importFrom statement in packages, or `::` in scripts.
  [`attach()`](https://rdrr.io/r/base/attach.html) modifies the global
  search path.

- [`browser()`](https://rdrr.io/r/base/browser.html) As an alternative,
  remove this likely leftover from debugging. It pauses execution when
  run.

- [`debug()`](https://rdrr.io/r/base/debug.html) As an alternative,
  remove this likely leftover from debugging. It traps a function and
  causes execution to pause when that function is run.

- [`debugcall()`](https://rdrr.io/r/utils/debugcall.html) As an
  alternative, remove this likely leftover from debugging. It traps a
  function and causes execution to pause when that function is run.

- [`debugonce()`](https://rdrr.io/r/base/debug.html) As an alternative,
  remove this likely leftover from debugging. It traps a function and
  causes execution to pause when that function is run.

- [`detach()`](https://rdrr.io/r/base/detach.html) As an alternative,
  avoid modifying the global search path. Detaching environments from
  the search path is rarely necessary in production code.

- [`library()`](https://rdrr.io/r/base/library.html) As an alternative,
  use roxygen2's @importFrom statement in packages and `::` in scripts,
  instead of modifying the global search path.

- [`mapply()`](https://rdrr.io/r/base/mapply.html) As an alternative,
  use [`Map()`](https://rdrr.io/r/base/funprog.html) to guarantee a list
  is returned and simplify accordingly.

- [`options()`](https://rdrr.io/r/base/options.html) As an alternative,
  use
  [`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
  for a temporary change instead of permanently modifying the session
  options.

- [`par()`](https://rdrr.io/r/graphics/par.html) As an alternative, use
  [`withr::with_par()`](https://withr.r-lib.org/reference/with_par.html)
  for a temporary change instead of permanently modifying the graphics
  device parameters.

- [`require()`](https://rdrr.io/r/base/library.html) As an alternative,
  use roxygen2's @importFrom statement in packages and
  [`library()`](https://rdrr.io/r/base/library.html) or `::` in scripts,
  instead of modifying the global search path.

- [`sapply()`](https://rdrr.io/r/base/lapply.html) As an alternative,
  use [`vapply()`](https://rdrr.io/r/base/lapply.html) with an
  appropriate `FUN.VALUE=` argument to obtain type-stable
  simplification.

- [`setwd()`](https://rdrr.io/r/base/getwd.html) As an alternative, use
  [`withr::with_dir()`](https://withr.r-lib.org/reference/with_dir.html)
  for a temporary change instead of modifying the global working
  directory.

- [`sink()`](https://rdrr.io/r/base/sink.html) As an alternative, use
  [`withr::with_sink()`](https://withr.r-lib.org/reference/with_sink.html)
  for a temporary redirection instead of permanently redirecting output.

- [`source()`](https://rdrr.io/r/base/source.html) As an alternative,
  manage dependencies through packages.
  [`source()`](https://rdrr.io/r/base/source.html) loads code into the
  global environment unless `local = TRUE` is used, which can cause
  hard-to-predict behavior.

- [`structure()`](https://rdrr.io/r/base/structure.html) As an
  alternative, Use `class<-`, `names<-`, and `attr<-` to set attributes.

- [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) As an
  alternative, use
  [`withr::with_envvar()`](https://withr.r-lib.org/reference/with_envvar.html)
  for a temporary change instead of permanently modifying global
  environment variables.

- [`Sys.setlocale()`](https://rdrr.io/r/base/locales.html) As an
  alternative, use
  [`withr::with_locale()`](https://withr.r-lib.org/reference/with_locale.html)
  for a temporary change instead of permanently modifying the session
  locale.

- [`trace()`](https://rdrr.io/r/base/trace.html) As an alternative,
  remove this likely leftover from debugging. It traps a function and
  causes execution of arbitrary code when that function is run.

- [`undebug()`](https://rdrr.io/r/base/debug.html) As an alternative,
  remove this likely leftover from debugging. It is only useful for
  interactive debugging with
  [`debug()`](https://rdrr.io/r/base/debug.html).

- [`untrace()`](https://rdrr.io/r/base/trace.html) As an alternative,
  remove this likely leftover from debugging. It is only useful for
  interactive debugging with
  [`trace()`](https://rdrr.io/r/base/trace.html).

The following operators are sometimes regarded as undesirable:

- [`->>`](https://rdrr.io/r/base/assignOps.html). It assigns outside the
  current environment in a way that can be hard to reason about. Prefer
  fully-encapsulated functions wherever possible, or, if necessary,
  assign to a specific environment with
  [`assign()`](https://rdrr.io/r/base/assign.html). Recall that you can
  create an environment at the desired scope with
  [`new.env()`](https://rdrr.io/r/base/environment.html).

- `:::`. It accesses non-exported functions inside packages. Code
  relying on these is likely to break in future versions of the package
  because the functions are not part of the public interface and may be
  changed or removed by the maintainers without notice. Use public
  functions via `::` instead.

- [`<<-`](https://rdrr.io/r/base/assignOps.html). It assigns outside the
  current environment in a way that can be hard to reason about. Prefer
  fully-encapsulated functions wherever possible, or, if necessary,
  assign to a specific environment with
  [`assign()`](https://rdrr.io/r/base/assign.html). Recall that you can
  create an environment at the desired scope with
  [`new.env()`](https://rdrr.io/r/base/environment.html).
