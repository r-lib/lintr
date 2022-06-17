# This file contains all documentation for linter tags in lintr except for default_linters.

#' Style linters
#' @name style_linters
#' @description
#' Linters highlighting code style issues.
#' @evalRd rd_linters("style")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Robustness linters
#' @name robustness_linters
#' @description
#' Linters highlighting code robustness issues, such as possibly wrong edge case behaviour.
#' @evalRd rd_linters("robustness")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Best practices linters
#' @name best_practices_linters
#' @description
#' Linters checking the use of coding best practices, such as explicit typing of numeric constants.
#' @evalRd rd_linters("best_practices")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Consistency linters
#' @name consistency_linters
#' @description
#' Linters checking enforcing a consistent alternative if there are multiple syntactically valid ways to write
#' something.
#' @evalRd rd_linters("consistency")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Readability linters
#' @name readability_linters
#' @description
#' Linters highlighting readability issues, such as missing whitespace.
#' @evalRd rd_linters("readability")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Correctness linters
#' @name correctness_linters
#' @description
#' Linters highlighting possible programming mistakes, such as unused variables.
#' @evalRd rd_linters("correctness")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Common mistake linters
#' @name common_mistakes_linters
#' @description
#' Linters highlighting common mistakes, such as duplicate arguments.
#' @evalRd rd_linters("common_mistakes")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Efficiency linters
#' @name efficiency_linters
#' @description
#' Linters highlighting code efficiency problems, such as unnecessary function calls.
#' @evalRd rd_linters("efficiency")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Configurable linters
#' @name configurable_linters
#' @description
#' Generic linters which support custom configuration to your needs.
#' @evalRd rd_linters("configurable")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Package development linters
#' @name package_development_linters
#' @description
#' Linters useful to package developers, for example for writing consistent tests.
#' @evalRd rd_linters("package_development")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Deprecated linters
#' @name deprecated_linters
#' @description
#' Linters that are deprecated and provided for backwards compatiblility only.
#' These linters will be excluded from `linters_with_tags()` by default.
#' @evalRd rd_linters("deprecated")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL

#' Code executing linters
#' @name executing_linters
#' @description
#' Linters that evaluate parts of the linted code, such as loading referenced packages.
#' These linters should not be used with untrusted code, and may need dependencies of the linted package or project to
#' be available in order to function correctly.
#' @evalRd rd_linters("executing")
#' @seealso [linters] for a complete list of linters available in lintr.
NULL
