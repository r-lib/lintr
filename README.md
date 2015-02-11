# lintr
[![Build Status](https://travis-ci.org/jimhester/lintr.png?branch=master)](https://travis-ci.org/jimhester/lintr)
[![Coverage Status](https://img.shields.io/coveralls/jimhester/lintr.svg)](https://coveralls.io/r/jimhester/lintr?branch=master)

## Static code analysis for R ##

![lintr](http://i.imgur.com/acV27NV.gif "lintr")

### Emacs ###
lintr has [built-in integration](http://www.flycheck.org/en/latest/guide/languages.html#r) with [flycheck](https://github.com/flycheck/flycheck) versions greater than `0.23`.
![Emacs Example](http://i.imgur.com/vquPht3.gif "Emacs Example")

#### Installation ####
Put the following in your .emacs file to automatically use `lintr` to check R
files.  *Note you will need to change the path to where you put
[lintr.el](https://github.com/jimhester/lintr/raw/master/inst/flycheck/lintr.el)*

```emacs
(require 'flycheck)
(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode t)))
```
#### Configuration ####
You can also configure what linters are used. e.g. using a different line length cutoff.
- `M-X customize-opton` -> `flycheck-r-linters` -> `with_defaults(line_length_linter(120))`

### Vim
lintr can be integrated with
[syntastic](https://github.com/scrooloose/syntastic) for on the fly linting.

![Vim Example](http://i.imgur.com/fR6Os5M.gif "Vim Example")

#### Installation ####
Put the file [syntastic/lintr.vim](inst/syntastic/lintr.vim)
in `syntastic/syntax_checkers/r`.  If you are using
[pathogen](https://github.com/tpope/vim-pathogen) this directory is
`~/.vim/bundles/syntastic/syntax_checkers/r`.

You will also need to add the following lines to your `.vimrc`.
```vim
let g:syntastic_enable_r_lintr_checker = 1
let g:syntastic_r_checkers = 1
```
#### Configuration ####
You can also configure what linters are used. e.g. using a different line length cutoff.
```vim
let g:syntastic_r_lintr_linters = "with_defaults(line_length_linter(120))"
```

### Sublime Text 3 ###
lintr can be intergrated with
[Sublime Linter](https://github.com/SublimeLinter/SublimeLinter3) for on the fly linting.

![Sublime Example](http://i.imgur.com/3pua2yz.gif "Sublime Example")

#### Installation ####
Simply install `sublimeLinter-contrib-lintr` using [Package Control](https://packagecontrol.io/).

For more information see [Sublime Linter Docs](http://sublimelinter.readthedocs.org/en/latest/installation.html#installing-via-pc)

#### Configuration ####
You can also configure what linters are used. e.g. using a different line length cutoff.
In the SublimeLinter User Settings
```
{
  "user": {
    "linters": {
      "r": {
        "linters": "with_defaults(line_length_linter(120))"
      }
    }
  }
}
```

## Available linters ##

* `Syntax errors`: reported by [parse](http://www.inside-r.org/r-doc/base/parse).
* `object_usage_linter`: checks that closures have the proper usage using
  [codetools::checkUsage()](http://www.inside-r.org/r-doc/codetools/checkUsage).  Note this runs
  [base::eval()](http://www.inside-r.org/r-doc/base/eval) on the code, so do not use with untrusted code.
* `absolute_paths_linter`: checks that no absolute paths are used.
* `assignment_linter`: checks that `<-` is always used for assignment
* `closed_curly_linter`: check that closed curly braces should always be on their
  own line unless they follow an else.
* `commas_linter`: check that all commas are followed by spaces, but do not
  have spaces before them.
* `infix_spaces_linter`: check that all infix operators have spaces around them.
* `line_length_linter`: check the line length of both comments and code is less than
  length.
* `no_tab_linter`: check that only spaces are used, never tabs.
* `camel_case_linter`: check that function and variable names are not camelCase.
* `snake_case_linter`: check that function and variable names are not snake_case.
* `multiple_dots_linter`: check that function and variable names are separated by `_` rather than `.`.
* `object_length_linter`: check that function and variable names are not more than `length` characters.
* `open_curly_linter`: check that opening curly braces are never on their own
  line and are always followed by a newline.
* `single_quotes_linter`: checks that only single quotes are used to delimit
  string contestants.
* `spaces_inside_linter`: check that parentheses and square brackets do not have
  spaces directly inside them.
* `spaces_left_parentheses_linter`: check that all left parentheses have a space before them
  unless they are in a function call.
* `trailing_blank_lines_linter`: check there are no trailing blank lines.
* `trailing_whitespace_linter`: check there are no trailing whitespace characters.

## Project Configuration ##

Lintr supports per-project configuration of the following fields.  The config file is in [Debian Control Field Format](http://www.debian.org/doc/debian-policy/ch-controlfields.html).

- `linters` - see `?with_defaults` for example of specifying only a few non-default linters.
- `exclude` - a regex pattern for lines to exclude from linting.  Default is "# nolint"
- `exclude_start` - a regex pattern to start exclusion range. Default is "# nolint start"
- `exclude_end` - a regex pattern to end exclusion range. Default is "# nolint end"

An example file that uses 120 character line lengths and different exclude regexes would be

```
linters: with_defaults(line_length_linter(120))
exclude: "# Exclude Linting"
exclude_start: "# Begin Exclude Linting"
exclude_end: "# End Exclude Linting"
```

## References ##
Most of the default linters are based on [Hadley Wickham's R Style Guide](http://r-pkgs.had.co.nz/style.html).
