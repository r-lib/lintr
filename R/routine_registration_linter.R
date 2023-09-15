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
routine_registration_linter <- local({
  native_routine_callers <- c(".C", ".Call", ".Fortran", ".External")
  make_linter_from_xpath(
    xpath = glue("
    //SYMBOL_FUNCTION_CALL[ {xp_text_in_table(native_routine_callers)} ]
      /parent::expr
      /following-sibling::expr[1]/STR_CONST
      /parent::expr
    "),
    lint_message = "Register your native code routines with useDynLib and R_registerRoutines()."
  )
})
