(require 'dash)
(require 'flycheck)
(require 'ess)

(flycheck-define-checker r-lintr
  "A R syntax and type checker using lintr.

See URL `https://github.com/jimhester/lintr'."

 :command
 ("R" "--slave" "--restore" "--no-save"
  "-e 'library(lintr);lint(commandArgs(TRUE))'" "--args" source)

 :error-patterns
 ((info line-start (file-name) ":" line ":" column ": " "style:"
           (message
             (one-or-more " ") (one-or-more not-newline)
             line-end)))
 ((warning line-start (file-name) ":" line ":" column ": " "warning:"
           (message
             (one-or-more " ") (one-or-more not-newline)
             line-end)))
 ((error line-start (file-name) ":" line ":" column ": " "error:"
         (message
           (one-or-more " ") (one-or-more not-newline)
           line-end)))

 :modes ess-mode
)

(add-to-list 'flycheck-checkers 'r-lintr)

(provide 'flycheck-lintr)
