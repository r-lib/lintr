# lintr
[![R build status](https://github.com/jimhester/lintr/workflows/R-CMD-check/badge.svg)](https://github.com/jimhester/lintr/actions)
[![codecov.io](https://codecov.io/github/jimhester/lintr/coverage.svg?branch=master)](https://codecov.io/github/jimhester/lintr?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/lintr)](https://cran.r-project.org/package=lintr) [![Join the chat at https://gitter.im/jimhester-lintr/Lobby](https://badges.gitter.im/jimhester-lintr/Lobby.svg)](https://gitter.im/jimhester-lintr/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Static code analysis for R ##

`lintr` is an R package offering [static code analysis for R](https://en.wikipedia.org/wiki/Static_program_analysis). It checks adherence to a given style, syntax errors and possible semantic issues, see the animation below. In this README find out 

* [what linters i.e. checks are supported](#available-linters);

* [how to configure the project to e.g. tweak checks and ignore files](#project-configuration);

* [how to setup `lintr` for on-the-fly checking in different editors](#editors-setup);

* [how to use `lintr` in combination with continuous integration](#continuous-integration);

![lintr](http://i.imgur.com/acV27NV.gif "lintr")

### What to do with `lintr` output?

If you need a bit automatic help for re-styling your code, have a look at [the `styler` package](https://github.com/r-lib/styler)

## Available linters ##

* `Syntax errors`: reported by [parse](https://www.rdocumentation.org/packages/base/versions/3.4.0/topics/parse).
* `Misencoded files`: check that files are read using the correct encoding.
* `absolute_path_linter`: check that no absolute paths are used (e.g. "/var", "C:\\System", "~/docs").
* `assignment_linter`: check that `<-` is always used for assignment
* `assignment_spaces_linter`: checks that assignments only have one space before and after
* `backport_linter`: checks for usage of unavailable functions. Not reliable for testing r-devel dependencies.
* `camel_case_linter`: check that objects are not in camelCase.
* `closed_curly_linter`: check that closed curly braces should always be on their
  own line unless they are followed by an else.
* `commas_linter`: check that all commas are followed by spaces, but do not
  have spaces before them.
* `commented_code_linter`: check that there is no commented code outside of roxygen comments.
* `cyclocomp_linter`: check for overly complicated expressions.
* `equals_na_linter`: check for x == NA
* `extraction_operator_linter`: check that the `[[` operator is used when extracting a single
  element from an object, not `[` (subsetting) nor `$` (interactive use).
* `function_left_parentheses_linter`: check that all left parentheses in a
  function call do not have spaces before them.
* `implicit_integer_linter`: check that integers are explicitly typed using the form `1L` instead of `1`.
* `infix_spaces_linter`: check that all infix operators have spaces around them.
* `line_length_linter`: check the line length of both comments and code is less than
  length.
* `missing_argument_linter`: check that no missing argument is supplied to function calls.
* `missing_package_linter`: check that no packages loaded by
  `library()`, `require()`, `loadNamespace()`, and `requireNamespace()` are missing.
* `namespace_linter`: check if there are missing packages and symbols in namespace calls with `::` and `:::`.
* `no_tab_linter`: check that only spaces are used, never tabs.
* `nonportable_path_linter`: check that file.path() is used to construct safe and portable paths.
* `object_length_linter`: check that function and variable names are not more than `length` characters.
* `object_name_linter`: check that object names conform to a single naming
  style, e.g. CamelCase, camelCase, snake_case, SNAKE_CASE, dotted.case,
  lowercase, or UPPERCASE.
* `object_usage_linter`: check that closures have the proper usage using
  [codetools::checkUsage()](https://rdrr.io/cran/codetools/man/checkUsage.html).  Note this runs
  [base::eval()](https://rdrr.io/r/base/eval.html) on the code, so do not use with untrusted code.
* `open_curly_linter`: check that opening curly braces are never on their own
  line and are always followed by a newline.
* `paren_brace_linter`: check that there is a space between right parenthesis and an opening curly brace.
* `pipe_call_linter`: force explicit calls in magrittr pipes.
* `pipe_continuation_linter`: Check that each step in a pipeline is on a new
  line, or the entire pipe fits on one line.
* `semicolon_terminator_linter`: check that no semicolons terminate statements.
* `seq_linter`: check for `1:length(...)`, `1:nrow(...)`, `1:ncol(...)`,
  `1:NROW(...)`, and `1:NCOL(...)` expressions. These often cause bugs when the
  right hand side is zero. It is safer to use `seq_len()` or `seq_along()`
  instead.
* `single_quotes_linter`: check that only single quotes are used to delimit
  string constants.
* `spaces_inside_linter`: check that parentheses and square brackets do not have
  spaces directly inside them.
* `spaces_left_parentheses_linter`: check that all left parentheses have a space before them
  unless they are in a function call.
* `sprintf_linter`: check that the numbers of arguments are correct and types of arguments are compatible in
  `sprintf("string", ...)` calls.
* `todo_comment_linter`: check that the source contains no TODO comments (case-insensitive).
* `trailing_blank_lines_linter`: check there are no trailing blank lines.
* `trailing_whitespace_linter`: check there are no trailing whitespace characters.
* `T_and_F_symbol_linter`: avoid the symbols `T` and `F` (for `TRUE` and `FALSE`).
* `undesirable_function_linter`: report the use of undesirable functions, e.g. `options` or `sapply` and suggest an alternative.
* `undesirable_operator_linter`: report the use of undesirable operators, e.g. `:::` or `<<-` and
  suggest an alternative.
* `unneeded_concatenation_linter`: check that the `c` function is not used without arguments nor
  with a single constant.
  
### References ###
Most of the default linters are based on [Hadley Wickham's The tidyverse style guide](https://style.tidyverse.org/).

## Running `lintr` ##

There are several ways to use `lintr`.

When inside an R session, `lintr` can analyse all the R code in
a file (using `lintr::lint(file_path)`), package (`lintr::lint_package(pkg_path)`) or directory (`lintr::lint_dir(dir_path)`).

Similarly, `lintr` can be run from the command line using
`Rscript -e "lintr::lint_package(commandArgs(trailingOnly = TRUE))" pkg_path` (linux/mac; for windows use Rscript.exe).

Advanced users may run `lintr` during [continuous-integration](#continuous-integration), or [within their IDE or text editor](#editors-setup).

## Project Configuration ##

Lintr supports per-project configuration of the following fields.
The config file (default file name: `.lintr`) is in [Debian Control Field Format](https://www.debian.org/doc/debian-policy/#document-ch-controlfields).

- `linters` - see `?with_defaults` for example of specifying only a few non-default linters.
- `exclusions` - a list of filenames to exclude from linting.  You can use a
  named item to exclude only certain lines from a file.
- `exclude` - a regex pattern for lines to exclude from linting.  Default is "# nolint"
- `exclude_start` - a regex pattern to start exclusion range. Default is "# nolint start"
- `exclude_end` - a regex pattern to end exclusion range. Default is "# nolint end"
- `encoding` - the encoding used for source files. Default inferred from .Rproj or DESCRIPTION files, fallback to UTF-8


### .lintr File Example

Below is an example .lintr file that uses:

- 120 character line lengths
- Excludes a couple of files
- Disables a specific linter, and; 
- Sets different default exclude regexes
- Specifies the file encoding to be ISO-8859-1 (Latin 1)

```
linters: with_defaults(
  line_length_linter(120), 
  commented_code_linter = NULL
  )
exclusions: list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R", "tests/testthat/exclusions-test")
exclude: "# Exclude Linting"
exclude_start: "# Begin Exclude Linting"
exclude_end: "# End Exclude Linting"
encoding: "ISO-8859-1"
```

With the following command, you can create a configuration file for `lintr` that ignores all linters that show at least one error:

```r
# Create configuration file for lintr
# Source this file in package root directory

# List here files to exclude from lint checking, as a character vector
excluded_files <- c(
    list.files("data",      recursive = TRUE, full.names = TRUE),
    list.files("docs",      recursive = TRUE, full.names = TRUE),
    list.files("inst/doc",  recursive = TRUE, full.names = TRUE),
    list.files("man",       recursive = TRUE, full.names = TRUE),
    list.files("vignettes", recursive = TRUE, full.names = TRUE)
)

### Do not edit after this line ###

library(magrittr)
library(dplyr)

# Make sure we start fresh
if (file.exists(".lintr")) { file.remove(".lintr") }

# List current lints
lintr::lint_package() %>%
    as.data.frame %>%
    group_by(linter) %>%
    tally(sort = TRUE) %$%
    sprintf("linters: with_defaults(\n    %s\n    dummy_linter = NULL\n  )\n",
            paste0(linter, " = NULL, # ", n, collapse = "\n    ")) %>%
    cat(file = ".lintr")

sprintf("exclusions: list(\n    %s\n  )\n",
        paste0('"', excluded_files, '"', collapse = ",\n    ")) %>%
    cat(file = ".lintr", append = TRUE)

# Clean up workspace
remove(excluded_files)
```

The resulting configuration will contain each currently failing linter and the corresponding number of hits as a comment. Proceed by successively enabling linters, starting with those with the least number of hits. Note that this requires `lintr` 0.3.0.9001 or later.

If you are developing a package, you can add `^\.lintr$` to your `.Rbuildignore` file using `usethis::use_build_ignore(".lintr")`.

## Continuous integration ##
You can configure `lintr` to run as part of continuous integration (either for a package or a general project containing R files) in order to automatically check that commits and pull requests do not deteriorate code style. 

### For packages

#### GitHub Actions ###

If your package is on GitHub, the easiest way to do this is with GitHub Actions. 
The workflow configuration files use YAML syntax. The `usethis` package has some 
great functionality, that can help you with workflow files. The most straightforward 
way to add a `lint` workflow to your package is to use the [r-lib/actions](https://github.com/r-lib/actions/tree/master/examples)'s `lint` 
example. To do this with `usethis`, you need to call 

```r
usethis::use_github_action("lint")
```

This will create a workflow file called `lint.yaml` and place it in the correct 
location, namely in the `.github/workflows` directory of your repository. This file configures all the steps required to run `lintr::lint_package()` on your package.  

[lintr-bot](https://github.com/lintr-bot) will then add comments to the commit or 
pull request with the lints found and they will also be printed as [annotations](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/about-status-checks#types-of-status-checks-on-github) along side the status check on GitHub.  If you want to disable the commenting you can
set the environment variable `LINTR_COMMENT_BOT=false`.

#### Travis CI ###

If you want to run `lintr` on [Travis-CI](https://travis-ci.org), you will need
to have Travis install the package first.  This can be done by adding the
following line to your `.travis.yml`

```yaml
r_github_packages:
  - jimhester/lintr
```

We recommend running `lintr::lint_package()` as an [after_success step in your build process](#non-failing-lints)]

Just like with GitHub Actions, [lintr-bot](https://github.com/lintr-bot) will then 
add comments to the commit or pull request with the lints found and they will also be
printed on Travis-CI.  If you want to disable the commenting you can
set the environment variable `LINTR_COMMENT_BOT=false`.

##### Non-failing Lints ####
```yaml
after_success:
  - R CMD INSTALL $PKG_TARBALL
  - Rscript -e 'lintr::lint_package()'
```

Live example of a package using this setup: [`hibpwned`](https://github.com/lockedata/HIBPwned/blob/master/.travis.yml), [lintr-bot commenting on a PR](https://github.com/lockedata/HIBPwned/pull/30).

### For projects ###

You are not limited you using `lintr` for packages only, you can use it in combination with continuous integration for any other project. 

#### GitHub Actions ####

If your project is on GitHub, you could take advantage of GitHub Actions and the `usethis` functionality. [r-lib/actions](https://github.com/r-lib/actions/tree/master/examples) includes a `lint-project` example, which you can use by calling:

```r
usethis::use_github_action("lint-project")
```


## Installation of development version ##
To install the latest development version of lintr from GitHub

```r
devtools::install_github("jimhester/lintr")
```


## Editors setup ##

### RStudio ###
lintr lints are automatically displayed in the RStudio Markers pane, Rstudio versions (> v0.99.206).
![RStudio Example](http://i.imgur.com/PIKnpbn.png "Rstudio Example")

#### Installation ####
Install lintr, type `install.packages("lintr")` in the Console.

In order to show the "Markers" pane in RStudio:
Menu "Tools" -> "Global Options...", a window with title "Options" will pop up. In that window: Click "Code" on the left; Click "Diagnostics" tab; check "Show diagnostics for R".

To lint a source file `test.R` type in the Console `lintr::lint("test.R")` and look at the result in the "Markers" pane.

This package also includes two addins for linting the current source and package.
To bind the addin to a keyboard shortcut navigate to Tools > addins > 
Browse Addins > Keyboard Shortcuts. It's recommended to use Alt+Shift+L for
linting the current source code and Ctrl+Shift+Alt+L to code the package.
These are easy to remember as you are Alt+Shift+L(int) ;)

### Emacs ###
lintr has [built-in integration](http://www.flycheck.org/en/latest/languages.html#r) with [flycheck](https://github.com/flycheck/flycheck) versions greater than `0.23`.
![Emacs Example](http://i.imgur.com/vquPht3.gif "Emacs Example")

#### Installation ####
lintr is fully integrated into flycheck when using [ESS](http://ess.r-project.org/).  See the
installation documentation for those packages for more information.

#### Configuration ####
You can also configure what linters are used. e.g. using a different line length cutoff.
- `M-x customize-option` -> `flycheck-lintr-linters` -> `with_defaults(line_length_linter(120))`

### Vim - syntastic
lintr can be integrated with
[syntastic](https://github.com/vim-syntastic/syntastic) for on the fly linting.

![Vim Example](http://i.imgur.com/fR6Os5M.gif "Vim Example")

#### Installation ####
Put the file [syntastic/lintr.vim](inst/syntastic/lintr.vim)
in `syntastic/syntax_checkers/r`.  If you are using
[pathogen](https://github.com/tpope/vim-pathogen) this directory is
`~/.vim/bundles/syntastic/syntax_checkers/r`.

You will also need to add the following lines to your `.vimrc`.
```vim
let g:syntastic_enable_r_lintr_checker = 1
let g:syntastic_r_checkers = ['lintr']
```
#### Configuration ####
You can also configure what linters are used. e.g. using a different line length cutoff.
```vim
let g:syntastic_r_lintr_linters = "with_defaults(line_length_linter(120))"
```
### Vim - ALE
lintr can be integrated with
[ALE](https://github.com/dense-analysis/ale) for on the fly linting.

#### Installation ####
lintr is integrated with ALE and requires no additional installation.
#### Configuration ####
You can configure what linters are used. e.g. using a different line length cutoff.
```vim
let g:ale_r_lintr_options = "with_defaults(line_length_linter(120))"
```
You can also configure whether `lint` or `lint_package` is used. 
Set to 1 for `lint_package` and 0 (default) for `lint`.
```vim
let g:ale_r_lintr_lint_package = 1
```

See `:h ale_r_lintr` for more information.

Note that configuration through `.lintr` files are not supported.

### Sublime Text 3 ###
lintr can be integrated with
[Sublime Linter](https://github.com/SublimeLinter/SublimeLinter) for on the fly linting.

![Sublime Example](http://i.imgur.com/3pua2yz.gif "Sublime Example")

#### Installation ####
Simply install `sublimeLinter-contrib-lintr` using [Package Control](https://packagecontrol.io/).

For more information see [Sublime Linter Docs](http://sublimelinter.readthedocs.io/en/latest/installation.html#installing-via-pc)

#### Configuration ####
You can also configure what linters are used. e.g. disabling the assignment linter and using a different line length cutoff.
In the SublimeLinter User Settings
```
{
  "linters": {
    "lintr": {
      "linters": "with_defaults(assignment_linter = NULL, line_length_linter(120))"
    }
  }
}
```

### Atom ###
lintr can be integrated with
[Linter](https://github.com/steelbrain/linter) for on the fly linting.

![Atom Example](http://i.imgur.com/E1Isi4T.png "Atom Example")

#### Installation ####
Simply install `linter-lintr` from within Atom or on the command line with:
```bash
apm install linter-lintr
```

For more information and bug reports see [Atom linter-lintr](https://github.com/AtomLinter/linter-lintr).

### VSCode ###

In VSCode, [vscode-r-lsp](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r-lsp) presents the
lintr diagnostics from [languageserver](https://github.com/REditorSupport/languageserver).

![VSCode Example](https://user-images.githubusercontent.com/4662568/75946154-3e095500-5ed7-11ea-88e4-2afe09284362.png "VSCode Example")

#### Installation ####

Installing `languageserver` package in R and `vscode-r-lsp` extension in VSCode will enable lintr in VSCode by default or run the following command lines:

```bash
Rscript -e 'install.packages("languageserver")'
code --install-extension REditorSupport.r-lsp
```
