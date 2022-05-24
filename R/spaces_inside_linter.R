#' Spaces inside linter
#'
#' Check that parentheses and square brackets do not have spaces directly
#'   inside them, i.e., directly following an opening delimiter or directly
#'   preceding a closing delimiter.
#'
#' @evalRd rd_tags("spaces_inside_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
spaces_inside_linter <- function() {
  left_xpath_condition <- "
    not(following-sibling::*[1][self::COMMENT])
    and @end != following-sibling::*[1]/@start - 1
    and @line1 = following-sibling::*[1]/@line2
  "
  left_xpath <- glue::glue("//OP-LEFT-BRACKET[{left_xpath_condition}] | //OP-LEFT-PAREN[{left_xpath_condition}]")

  right_xpath_condition <- "
    not(preceding-sibling::*[1][self::OP-COMMA])
    and @start != preceding-sibling::*[1]/@end + 1
    and @line1 = preceding-sibling::*[1]/@line2
  "
  right_xpath <- glue::glue("
    //OP-RIGHT-BRACKET[{right_xpath_condition}]/preceding-sibling::*[1] |
    //OP-RIGHT-PAREN[{right_xpath_condition}]/preceding-sibling::*[1]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    left_expr <- xml2::xml_find_all(xml, left_xpath)
    left_lints <- lapply(left_expr, function(expr) {
      line1 <- as.integer(xml2::xml_attr(expr, "line1"))
      col1 <- as.integer(xml2::xml_attr(expr, "col1")) + 1L
      col2 <- as.integer(xml2::xml_find_num(expr, "number(./following-sibling::*[1]/@col1)")) - 1L
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        message = switch(
          xml2::xml_text(expr),
          `[` = "Do not place spaces after square brackets.",
          `(` = "Do not place spaces after parentheses."
        ),
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2))
      )
    })

    right_expr <- xml2::xml_find_all(xml, right_xpath)
    right_lints <- lapply(right_expr, function(expr) {
      line1 <- as.integer(xml2::xml_attr(expr, "line1"))
      col1 <- as.integer(xml2::xml_attr(expr, "col2")) + 1L
      col2 <- as.integer(xml2::xml_find_num(expr, "number(./following-sibling::*[1]/@col1)")) - 1L
      Lint(
        filename = source_expression$filename,
        line_number = line1,
        column_number = col1,
        message = switch(
          xml2::xml_find_chr(expr, "string(./following-sibling::*[1])"),
          `]` = "Do not place spaces before square brackets.",
          `)` = "Do not place spaces before parentheses."
        ),
        line = source_expression$file_lines[[line1]],
        ranges = list(c(col1, col2))
      )
    })

    c(left_lints, right_lints)
  })
}
