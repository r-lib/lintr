#ifndef LINTR_SRC_FIXED_REGEX_H_
#define LINTR_SRC_FIXED_REGEX_H_

#include <Rinternals.h>
#include <stdbool.h>

SEXP is_not_regex(SEXP patterns, SEXP skip_start, SEXP skip_end);
bool is_special(const char *s);
bool is_escape_char(const char *s);
int get_single_char_class_width(const char *s, int max_width);
bool is_valid_hex(const char *s);

#endif  // LINTR_SRC_FIXED_REGEX_H_
