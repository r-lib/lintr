# it doesn't produce invalid lints

    Code
      lints1 <- xml_nodes_to_lints(xml = node, source_expression = expr,
        lint_message = "lint_msg", column_number_xpath = xp_column_number,
        range_start_xpath = xp_invalid, range_end_xpath = xp_range_end)
    Condition
      Warning:
      x Could not find range start for lint.
      i Defaulting to start of line.

---

    Code
      lints2 <- xml_nodes_to_lints(xml = node, source_expression = expr,
        lint_message = "lint_msg", column_number_xpath = xp_column_number,
        range_start_xpath = xp_range_start, range_end_xpath = xp_invalid)
    Condition
      Warning:
      x Could not find range end for lint.
      i Defaulting to width 1.

---

    Code
      lints3 <- xml_nodes_to_lints(xml = node, source_expression = expr,
        lint_message = "lint_msg", column_number_xpath = xp_invalid,
        range_start_xpath = xp_range_start, range_end_xpath = xp_range_end)
    Condition
      Warning:
      x Could not find location for lint.
      i Defaulting to start of range.

