#' @describeIn linters  Check that functions do not use global and other non-local variables.
#' Note that this uses \code{\link[base]{eval}}, so do not use with untrusted code.
#' @param variable A character vector defining which non-local variables to report:\describe{
#'   \item{global}{Global variables.}
#'   \item{other}{Other variables not in the local scope of the function in which they are used.}
#' }
#' @export
nonlocal_variable_linter <- function(variable = c("global", "other")) {
  match.arg(variable)
  function(source_file) {
    x <- global_xml_parsed_content(source_file)
    if (is.null(x)) {
      return()
    }
    funcs <- character()
    is_toplevel <- logical()
    if ("global" %in% variable) {
      funcs <- toplevel_functions_xml(x)
      is_toplevel <- rep.int(TRUE, length(funcs))
    }
    if ("other" %in% variable) {
      inner_funcs <- inner_functions_xml(x)
      funcs <- c(funcs, inner_funcs)
      is_toplevel <- c(is_toplevel, rep.int(FALSE, length(inner_funcs)))
    }
    Map(
      function(xml_func, is_toplevel) {
        locs <- nonlocal_var_locations(xml_func, source_file)
        print(locs)
        Map(
          function(var_name, var_loc) {
            Lint(
              filename = source_file$filename,
              line_number = var_loc[["line1"]],
              column_number = var_loc[["col1"]],
              type = "warning",
              message = paste0("Variable ", var_name, " is non-local"),
              line = source_file[["file_lines"]][[var_loc[["line1"]]]],
              ranges = list(c(var_loc[["col1"]], var_loc[["col2"]])),
              linter = "nonlocal_variable_linter"
            )
          },
          names(locs),
          locs
        )
      },
      funcs,
      is_toplevel
    )
  }
}

toplevel_functions_xml <- function(x) {
  xpath <- paste0(
    # top-level left-assigned function
    "/exprlist/expr[expr/SYMBOL][LEFT_ASSIGN|EQ_ASSIGN][expr[FUNCTION]]/expr[FUNCTION]",
    "|",
    # or top-level anonymous or right-assigned function
    "/exprlist/expr[FUNCTION]"
  )
  xml2::xml_find_all(x, xpath)
}

inner_functions_xml <- function(x) {
  xpath <- paste0(
    "/exprlist/*//expr[expr/SYMBOL][LEFT_ASSIGN|EQ_ASSIGN][expr[FUNCTION]]/expr[FUNCTION]",
    "|",
    "/exprlist/*//*[not(self::LEFT_ASSIGN) and not(self::EQ_ASSIGN)]/expr[FUNCTION]"
  )
  xml2::xml_find_all(x, xpath)
}

variable_locations <- function(x, var_names) {
  lapply(
    var_names,
    function(var_name) {
      xml_location(xml2::xml_find_first(x, paste0("//SYMBOL[text() = '", var_name,"']")))
    }
  )
}

function_source <- function(x, source_file) {
  pos <- xml_location(x)
  lines <- source_file[["file_lines"]][seq.int(pos[["line1"]], pos[["line2"]])]
  lines[[1L]] <- substr(lines[[1L]], pos[["col1"]], nchar(lines[[1L]]))
  last <- length(lines)
  lines[[last]] <- substr(lines[[last]], 1, pos[["col2"]])
  paste0(lines, collapse = "\n")
}

xml_location <- function(x) {
  attrs <- xml2::xml_attrs(x)
  pos <- as.integer(attrs)
  names(pos) <- names(attrs)
  pos
}

nonlocal_var_locations <- function(xml_func, source_file) {
  env <- new.env()
  func <- try_silently(eval(
    parse(text = function_source(xml_func, source_file), keep.source = TRUE),
    envir = env
  ))
  vars <- codetools::findGlobals(func)
  names(vars) <- vars
  is_nonlocal_var <- !vapply(vars, exists, logical(1), envir=env)
  vars <- vars[is_nonlocal_var]
  variable_locations(xml_func, vars)
}
