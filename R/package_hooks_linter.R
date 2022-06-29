#' Package hooks linter
#'
#' Check various common "gotchas" in [.onLoad()], [.onAttach()], [.Last.lib()], and [.onDetach()]
#'   namespace hooks that will cause `R CMD check` issues. See Writing R Extensions for details.
#'
#' 1. `.onLoad()` shouldn't call [cat()], [message()], [print()], [writeLines()], [packageStartupMessage()],
#'    [require()], [library()], or [installed.packages()].
#' 2. `.onAttach()` shouldn't call `cat()`, `message()`, `print()`, `writeLines()`, [library.dynam()],
#'    `require()`, `library()`, or `installed.packages()`.
#' 3. `.Last.lib()` and `.onDetach()` shouldn't call [library.dynam.unload()].
#' 4. `.onLoad()` and `.onAttach()` should take two arguments, with names matching `^lib` and `^pkg`;
#'    `.Last.lib()` and `.onDetach()` should take one argument with name matching `^lib`.
#'
#' @evalRd rd_tags("package_hooks_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
package_hooks_linter <- function() {
  bad_msg_calls <- c("cat", "message", "print", "writeLines")
  bad_calls <- list(
    ".onLoad" = c(bad_msg_calls, "packageStartupMessage"),
    ".onAttach" = c(bad_msg_calls, "library.dynam")
  )
  bad_msg_call_xpath_fmt <- "
    //expr[SYMBOL[text() = '%s']]
    /following-sibling::expr[FUNCTION]
    //SYMBOL_FUNCTION_CALL[%s]
  "
  bad_call_xpaths <- vapply(
    seq_along(bad_calls),
    function(ii) sprintf(bad_msg_call_xpath_fmt, names(bad_calls)[ii], xp_text_in_table(bad_calls[[ii]])),
    character(1L)
  )
  names(bad_call_xpaths) <- names(bad_calls)

  make_bad_call_lint_message <- function(expr, hook) {
    call_name <- xml2::xml_text(expr)
    lint_message <- sprintf("Don't use %s() in %s().", call_name, hook)
    lint_message[call_name == "packageStartupMessage"] <-
      "Put packageStartupMessage() calls in .onAttach(), not .onLoad()."
    lint_message[call_name == "library.dynam"] <-
      "Put library.dynam() calls in .onLoad, not .onAttach()."
    lint_message
  }

  # lints here will hit the function <expr>,
  #   this path returns to the corresponding namespace hook's name
  ns_calls <- xp_text_in_table(c(".onLoad", ".onAttach", ".onDetach", ".Last.lib"))
  hook_xpath <- sprintf("string(./ancestor::expr/expr/SYMBOL[%s])", ns_calls)

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

  library_require_xpath <- "
  //expr[SYMBOL[text() = '.onAttach' or text() = '.onLoad']]
  /following-sibling::expr//*[1][
    (self::SYMBOL or self::SYMBOL_FUNCTION_CALL)
    and (text() = 'require' or text() = 'library' or text() = 'installed.packages')
  ]
  "

  bad_unload_call_xpath <- "
    //expr[SYMBOL[text() = '.Last.lib' or text() = '.onDetach']]
    /following-sibling::expr[FUNCTION]
    //SYMBOL_FUNCTION_CALL[text() = 'library.dynam.unload']
  "

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

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # inherits: source_expression, bad_call_xpaths
    bad_msg_call_lints <- function(xml, hook) {
      bad_expr <- xml2::xml_find_all(xml, bad_call_xpaths[[hook]])
      lint_message <- make_bad_call_lint_message(bad_expr, hook)
      xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
    }

    # (1) improper messaging calls shouldn't be used inside .onLoad()/.onAttach()
    onload_bad_msg_call_lints <- bad_msg_call_lints(xml, ".onLoad")
    onattach_bad_msg_call_lints <- bad_msg_call_lints(xml, ".onAttach")

    # (2) .onLoad() and .onAttach() should take two arguments, with names matching ^lib and ^pkg
    load_arg_name_expr <- xml2::xml_find_all(xml, load_arg_name_xpath)

    load_arg_name_message <- sprintf(
      "%s() should take two arguments, with the first starting with 'lib' and the second starting with 'pkg'.",
      xml2::xml_find_chr(load_arg_name_expr, hook_xpath)
    )
    load_arg_name_lints <-
      xml_nodes_to_lints(load_arg_name_expr, source_expression, load_arg_name_message, type = "warning")

    # (3) .onLoad() and .onAttach() shouldn't call require(), library(), or installed.packages()
    # NB: base only checks the SYMBOL_FUNCTION_CALL version, not SYMBOL.
    library_require_expr <- xml2::xml_find_all(xml, library_require_xpath)

    library_require_bad_call <- xml2::xml_text(library_require_expr)
    library_require_hook <- xml2::xml_find_chr(library_require_expr, hook_xpath)
    library_require_message <- character(length(library_require_bad_call))
    is_installed_packages <- library_require_bad_call == "installed.packages"
    library_require_message[is_installed_packages] <-
      sprintf("Don't slow down package load by running installed.packages() in %s().", library_require_hook)
    library_require_message[!is_installed_packages] <-
      sprintf("Don't alter the search() path in %s() by calling %s().", library_require_hook, library_require_bad_call)
    library_require_lints <-
      xml_nodes_to_lints(library_require_expr, source_expression, library_require_message, type = "warning")

    # (4) .Last.lib() and .onDetach() shouldn't call library.dynam.unload()
    bad_unload_call_expr <- xml2::xml_find_all(xml, bad_unload_call_xpath)

    bad_unload_call_message <- sprintf(
      "Use library.dynam.unload() calls in .onUnload(), not %s().",
      xml2::xml_find_chr(bad_unload_call_expr, hook_xpath)
    )
    bad_unload_call_lints <-
      xml_nodes_to_lints(bad_unload_call_expr, source_expression, bad_unload_call_message, type = "warning")

    # (5) .Last.lib() and .onDetach() should take one arguments with name matching ^lib
    unload_arg_name_expr <- xml2::xml_find_all(xml, unload_arg_name_xpath)

    unload_arg_name_message <- sprintf(
      "%s() should take one argument starting with 'lib'.",
      xml2::xml_find_chr(unload_arg_name_expr, hook_xpath)
    )
    unload_arg_name_lints <-
      xml_nodes_to_lints(unload_arg_name_expr, source_expression, unload_arg_name_message, type = "warning")

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
