declared_s3_generics <- function(x) {
  # Top level expression which assigns to a symbol or string
  #   and is an S3 Generic (contains call to UseMethod)
  # Retrieve assigned name of the function
  xpath <- "/exprlist
    /*[
      (LEFT_ASSIGN or EQ_ASSIGN)
      and expr[FUNCTION or OP-LAMBDA]
      and .//SYMBOL_FUNCTION_CALL[text() = 'UseMethod']
    ]
    /expr[1]
    /*[self::SYMBOL or self::STR_CONST]
  "

  get_r_string(xml_find_all_(x, xpath))
}
