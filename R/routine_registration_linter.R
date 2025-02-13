#' Identify unregistered native routines
#'
#' It is preferable to register routines for efficiency and safety.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = '.Call("cpp_routine", PACKAGE = "mypkg")',
#'   linters = routine_registration_linter()
#' )
#'
#' lint(
#'   text = '.Fortran("f_routine", PACKAGE = "mypkg")',
#'   linters = routine_registration_linter()
#' )
#'
#' # okay
#' lint(
#'   text = ".Call(cpp_routine)",
#'   linters = routine_registration_linter()
#' )
#'
#' lint(
#'   text = ".Fortran(f_routine)",
#'   linters = routine_registration_linter()
#' )
#'
#' @evalRd rd_tags("routine_registration_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Registering-native-routines>
#'
#' @export
routine_registration_linter <- make_linter_from_function_xpath(
  function_names = c(".C", ".Call", ".Fortran", ".External"),
  xpath = "
  following-sibling::expr[1]/STR_CONST
    /parent::expr
  ",
  lint_message = "Register your native code routines with useDynLib and R_registerRoutines()."
)
