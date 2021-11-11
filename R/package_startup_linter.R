#' @describeIn linters Check various common "gotchas" in [.onLoad()] / [.onAttach()] namespace hooks that will
#'    cause `R CMD check` issues.
#' @export
package_startup_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # (1) improper messaging calls shouldn't be used inside .onLoad/.onAttach
    bad_msg_calls <- c("cat", "message", "print", "writeLines")
    bad_onload_calls <- c(bad_msg_calls, "packageStartupMessage")
    bad_onattach_calls <- c(bad_msg_calls, "library.dynam")

    bad_msg_call_xpath_fmt <- "
      //expr[SYMBOL[text() = '%s']]
      /following-sibling::expr[FUNCTION]
      //SYMBOL_FUNCTION_CALL[%s]
    "
    onload_bad_msg_call_xpath <- sprintf(bad_msg_call_xpath_fmt, '.onLoad', xp_text_in_table(bad_onload_calls))
    onattach_bad_msg_call_xpath <- sprintf(bad_msg_call_xpath_fmt, '.onAttach', xp_text_in_table(bad_onattach_calls))

    bad_msg_call_expr <- xml2::xml_find_all(xml, paste0(onload_bad_msg_call_xpath, "|", onattach_bad_msg_call_xpath))

    bad_msg_call_expr <- lapply(
      bad_msg_call_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "Don't use cat(), message(), print(), or writeLines() in .onLoad() or .onAttach().",
        "Put packageStartupMessage() calls in .onAttach(), not .onLoad().",
        "Put library.dynam() calls in .onLoad, not .onAttach()."
      ),
      type = "warning"
    )

    # (2) .onLoad() and .onAttach() should take two arguments, with names matching ^lib and ^pkg
    arg_name_xpath <- "
    //expr[SYMBOL[text() = '.onAttach' or text() = '.onLoad']]
    /following-sibling::expr[
      FUNCTION
      and (
        count(SYMBOL_FORMALS) != 2
        or SYMBOL_FORMALS[
          (position() = 1 and not(starts-with(text(), 'lib')))
          or (position() = 2 and not(starts-with(text(), 'pkg')))
        ]
      )
    ]
    "

    arg_name_expr <- xml2::xml_find_all(xml, arg_name_xpath)

    arg_name_lints <- lapply(
      arg_name_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        ".onAttach() and .onLoad() should take two arguments,",
        "with the first starting with 'lib' and the second starting with 'pkg'."
      ),
      type = "warning"
    )

    # (3) .onLoad and .onAttach() shouldn't call require(), library(), or installed.packages()
    library_require_xpath <- "
    //expr[SYMBOL[text() = '.onAttach' or text() = '.onLoad']]
    /following-sibling::expr//*[
      (self::SYMBOL or self::SYMBOL_FUNCTION_CALL)
      and (text() = 'require' or text() = 'library' or text() = 'installed.packages')
    ]
    "

    library_require_expr <- xml2::xml_find_all(xml, library_require_xpath)

    library_require_lints <- lapply(
      library_require_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "Don't alter the search() path in .onLoad() or .onAttach() by calling library() or require(),",
        "or slow down package load by running installed.packages()."
      ),
      type = "warning"
    )

    return(c(bad_msg_call_expr, arg_name_lints, library_require_lints))
  })
}

