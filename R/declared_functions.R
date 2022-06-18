declared_s3_generics <- function(x) {
  xpath <- paste0(
    # Top level expression which
    "/exprlist/expr",

    # Assigns to a symbol
    "[./LEFT_ASSIGN|EQ_ASSIGN]",
    "[./expr[FUNCTION]]",
    "[./expr/SYMBOL]",

    # Is a S3 Generic (contains call to UseMethod)
    "[.//SYMBOL_FUNCTION_CALL[text()='UseMethod']]",

    # Retrieve assigned name of the function
    "/expr/SYMBOL/text()")

  as.character(xml2::xml_find_all(x, xpath))
}
