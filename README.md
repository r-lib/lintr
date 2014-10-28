# lintr
[![Build Status](https://travis-ci.org/jimhester/lintr.png?branch=master)](https://travis-ci.org/jimhester/lintr)

## A framework for linting R source code. ##

![lintr](https://github.com/jimhester/lintr/raw/master/lintr.png "lintr")

### Emacs ###
lintr can be integrated with
[flycheck](https://github.com/flycheck/flycheck) for on the fly linting.
![Emacs Example](https://github.com/jimhester/lintr/raw/master/flycheck.png "Emacs Example")

#### Installation ####
Put the following in your .emacs file to automatically use `lintr` to check R
files.  *Note you will need to change the path to where you put
[lintr.el](https://github.com/jimhester/lintr/raw/master/inst/flycheck/lintr.el)*

```emacs
(require 'flycheck)
(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode t)))
(load "path/to/flycheck-linter.el")
```

### Vim
lintr can be integrated with
[syntastic](https://github.com/scrooloose/syntastic) for on the fly linting.

![Vim Example](https://github.com/jimhester/lintr/raw/master/lintr_vim.gif "Vim Example")

#### Installation ####
Put the file [syntastic/lintr.vim](https://github.com/jimhester/lintr/raw/master/inst/syntastic/lintr.vim)
in `syntastic/syntax_checkers/r`.  If you are using
[pathogen](https://github.com/tpope/vim-pathogen) this directory is
`~/.vim/bundles/syntastic/syntax_checkers/r`.

## Available linters ##

* Syntax errors found by [parse](http://www.inside-r.org/r-doc/base/parse).
* object_usage_linter - checks that closures have the proper usage using
  [codetools::checkUsage()](http://www.inside-r.org/r-doc/codetools/checkUsage).  Note this runs
  [base::eval()](http://www.inside-r.org/r-doc/base/eval) on the code, so do not use with untrusted code.
* absolute_paths_linter - checks that no absolute paths are used.
* assignment_linter - checks that '<-' is always used for assignment
* closed_curly_linter - check that closed curly braces should always be on their
  own line unless they follow an else.
* commas_linter - check that all commas are followed by spaces, but do not
  have spaces before them.
* infix_spaces_linter - check that all infix operators have spaces around them.
* line_length_linter - check the line length of both comments and code is less than
  width.
* no_tab_linter - check that only spaces are used, never tabs.
* object_name_linter - check that objects
  1. Are never camelCase
  2. Are separated by '_' rather than '.'
  3. Are not more than `width` characters
* open_curly_linter - check that opening curly braces are never on their own
  line and are always followed by a newline.
* single_quotes_linter - checks that only single quotes are used to delimit
  string contestants.
* spaces_inside_linter - check that parentheses and square brackets do not have
  spaces directly inside them.
* spaces_left_parentheses_linter - check that all left parentheses have a space before them
  unless they are in a function call.
