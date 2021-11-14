#' @describeIn linters Check various common "gotchas" in [.onLoad()], [.onAttach()], [.Last.lib()], and [.onDetach()]
#'    namespace hooks that will cause `R CMD check` issues.
#' @export
package_hooks_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # (1) improper messaging calls shouldn't be used inside .onLoad()/.onAttach()
    bad_msg_calls <- c("cat", "message", "print", "writeLines")
    bad_onload_calls <- c(bad_msg_calls, "packageStartupMessage")
    bad_onattach_calls <- c(bad_msg_calls, "library.dynam")

    bad_msg_call_xpath_fmt <- "
      //expr[SYMBOL[text() = '%s']]
      /following-sibling::expr[FUNCTION]
      //SYMBOL_FUNCTION_CALL[%s]
    "

    bad_msg_fmt <- "Don't use %s() in %s()."

    onload_bad_msg_call_xpath <- sprintf(bad_msg_call_xpath_fmt, ".onLoad", xp_text_in_table(bad_onload_calls))
    onload_bad_msg_call_expr <- xml2::xml_find_all(xml, onload_bad_msg_call_xpath)
    onload_bad_msg_calls <- xml2::xml_text(onload_bad_msg_call_expr)
    onload_bad_msg_call_lints <- lapply(
      seq_along(onload_bad_msg_call_expr),
      function(ii) {
        message <- switch(
          onload_bad_msg_calls[ii],
          cat = ,
          message = ,
          print = ,
          writeLines = sprintf(bad_msg_fmt, onload_bad_msg_calls[ii], ".onLoad"),
          packageStartupMessage = "Put packageStartupMessage() calls in .onAttach(), not .onLoad()."
        )
        xml_nodes_to_lint(onload_bad_msg_call_expr[[ii]], source_file, message, type = "warning")
      }
    )

    onattach_bad_msg_call_xpath <- sprintf(bad_msg_call_xpath_fmt, '.onAttach', xp_text_in_table(bad_onattach_calls))
    onattach_bad_msg_call_expr <- xml2::xml_find_all(xml, onattach_bad_msg_call_xpath)
    onattach_bad_msg_calls <- xml2::xml_text(onattach_bad_msg_call_expr)
    onattach_bad_msg_call_lints <- lapply(
      seq_along(onattach_bad_msg_call_expr),
      function(ii) {
        message <- switch(
          onattach_bad_msg_calls[ii],
          cat = ,
          message = ,
          print = ,
          writeLines = sprintf(bad_msg_fmt, onattach_bad_msg_calls[ii], ".onAttach"),
          library.dynam = "Put library.dynam() calls in .onLoad, not .onAttach()."
        )
        xml_nodes_to_lint(onattach_bad_msg_call_expr[[ii]], source_file, message, type = "warning")
      }
    )

    # (2) .onLoad() and .onAttach() should take two arguments, with names matching ^lib and ^pkg
    load_arg_name_xpath <- "
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

    load_arg_name_expr <- xml2::xml_find_all(xml, load_arg_name_xpath)

    load_arg_name_lints <- lapply(
      load_arg_name_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        ".onAttach() and .onLoad() should take two arguments,",
        "with the first starting with 'lib' and the second starting with 'pkg'."
      ),
      type = "warning"
    )

    # (3) .onLoad() and .onAttach() shouldn't call require(), library(), or installed.packages()
    # NB: base only checks the SYMBOL_FUNCTION_CALL version, not SYMBOL.
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

    # (4) .Last.lib() and .onDetach() shouldn't call library.dynam.unload()
    bad_unload_call_xpath <- "
      //expr[SYMBOL[text() = '.Last.lib' or text() = '.onDetach']]
      /following-sibling::expr[FUNCTION]
      //SYMBOL_FUNCTION_CALL[text() = 'library.dynam.unload']
    "

    bad_unload_call_expr <- xml2::xml_find_all(xml, bad_unload_call_xpath)

    bad_unload_call_lints <- lapply(
      bad_unload_call_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = "Use library.dynam() calls in .onUnload, not .onDetach() or .Last.lib().",
      type = "warning"
    )

    # (5) .Last.lib() and .onDetach() should take one arguments with name matching ^lib
    unload_arg_name_xpath <- "
    //expr[SYMBOL[text() = '.onDetach' or text() = '.Last.lib']]
    /following-sibling::expr[
      FUNCTION
      and (
        count(SYMBOL_FORMALS) != 1
        or SYMBOL_FORMALS[not(starts-with(text(), 'lib'))]
      )
    ]
    "

    unload_arg_name_expr <- xml2::xml_find_all(xml, unload_arg_name_xpath)

    unload_arg_name_lints <- lapply(
      unload_arg_name_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = ".onDetach() and .Last.lib() should take one argument starting with 'lib'.",
      type = "warning"
    )

    return(c(
      onload_bad_msg_call_lints,
      onattach_bad_msg_call_lints,
      load_arg_name_lints,
      library_require_lints,
      bad_unload_call_lints,
      unload_arg_name_lints
    ))
  })
}

