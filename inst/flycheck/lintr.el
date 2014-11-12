(require 'dash)
(require 'flycheck)
(require 'ess)

(flycheck-def-option-var flycheck-r-linters "default_linters" r-lintr
  "What linters to use.

The value of this variable is a string with the linters to use."
  :type 'string
  :safe #'stringp)

(flycheck-def-option-var flycheck-r-cache "TRUE" r-lintr
  "Whether to cache or not.

The value of this variable is a string with either TRUE or FALSE."
  :type 'string
  :safe #'stringp)

(flycheck-define-checker r-lintr
  "A R syntax and type checker using lintr.

See URL `https://github.com/jimhester/lintr'."

 :command
 ("R" "--slave" "--restore" "--no-save" "-e"
  (eval (concat "library(lintr);lint(cache = " flycheck-r-cache ", commandArgs(TRUE), "
          flycheck-r-linters ")"))
  "--args" source)

 :error-patterns
 ((info line-start (file-name) ":" line ":" column ": " "style: "
           (message (one-or-more not-newline) line-end))
 (warning line-start (file-name) ":" line ":" column ": " "warning: "
         (message (one-or-more not-newline) line-end))
 (error line-start (file-name) ":" line ":" column ": " "error: "
         (message (one-or-more not-newline) line-end)))

 :modes ess-mode
)

(add-to-list 'flycheck-checkers 'r-lintr)

(provide 'flycheck-lintr)
