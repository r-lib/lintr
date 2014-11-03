# lintr
[![Build Status](https://travis-ci.org/jimhester/lintr.png?branch=master)](https://travis-ci.org/jimhester/lintr)

## A framework for linting R source code. ##

![lintr](http://i.imgur.com/acV27NV.gif "lintr")

### Emacs ###
lintr can be integrated with
[flycheck](https://github.com/flycheck/flycheck) for on the fly linting.
![Emacs Example](http://i.imgur.com/vquPht3.gif "Emacs Example")

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
Clone the [repository](https://github.com/jimhester/SublimeLinter-contrib-R)

```bash
cd 'path/to/Sublime Text 3/Packages'
git clone https://github.com/jimhester/SublimeLinter-contrib-R.git
```

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
* `object_camel_case_linter`: check that function and variable names are not camelCase.
* `object_snake_case_linter`: check that function and variable names are not snake_case.
* `object_multiple_dots_linter`: check that function and variable names are separated by `_` rather than `.`.
* `object_length_linter`: check that function and variable names are not more than `length` characters.
* `open_curly_linter`: check that opening curly braces are never on their own
  line and are always followed by a newline.
* `single_quotes_linter`: checks that only single quotes are used to delimit
  string contestants.
* `spaces_inside_linter`: check that parentheses and square brackets do not have
  spaces directly inside them.
* `spaces_left_parentheses_linter`: check that all left parentheses have a space before them
  unless they are in a function call.
