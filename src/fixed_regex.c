#include "fixed_regex.h"
#include <Rinternals.h>
#include <stdbool.h>

#ifndef LOGICAL_RO
#define LOGICAL_RO LOGICAL
#endif

SEXP is_not_regex(SEXP patterns, SEXP skip_start, SEXP skip_end) {
  if (TYPEOF(patterns) != STRSXP) {
    Rf_error("Internal error: patterns should be a character vector, got %s",
             Rf_type2char(TYPEOF(patterns)));
  }
  if (TYPEOF(skip_start) != LGLSXP || TYPEOF(skip_end) != LGLSXP) {
    Rf_error("Internal error: skip_start and skip_end must be logical");
  }

  const int n_patterns = Rf_length(patterns);
  const int n_starts = Rf_length(skip_start);

  if (n_starts != 1 && n_starts != n_patterns) {
    Rf_error(
        "Internal error: skip_start should be a scalar or same length as "
        "patterns");
  }
  // NB: this precludes length(skip_end)=1, length(skip_start)=N,
  //   but that flexibility doesn't do anything for us as of now.
  if (n_starts != Rf_length(skip_end)) {
    Rf_error("Internal error: skip_start & skip_end must be same length");
  }

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n_patterns));
  int *out_pointer = LOGICAL(out);
  const int *skip_start_pointer = LOGICAL_RO(skip_start);
  const int *skip_end_pointer = LOGICAL_RO(skip_end);

  for (R_len_t i = 0; i < n_patterns; ++i) {
    const SEXP pattern_i = STRING_ELT(patterns, i);

    int pattern_i_size = Rf_length(pattern_i);
    const char *letters = CHAR(pattern_i);

    out_pointer[i] = 1;  // default value
    // string as received here is the string literal as read by the R parser,
    //   so it will start & end with either ' or ", which we can skip.
    // skip_start --> ignore the initial character, too.
    // `% n_starts` mask unifies the code for the length=1 and length=n_patterns
    int m = 1 + skip_start_pointer[i % n_starts];
    /* Two possible ways to be a regular expression:
     *   (1) Uses an unescaped special character
     *   (2) Uses a metacharacter like \s or \d
     */
    while (m < pattern_i_size - 1 - skip_end_pointer[i % n_starts]) {
      if (is_special(&letters[m])) {  // case (1)
        out_pointer[i] = 0;
        break;
      }
      /* [a-z], [._], [0a] etc are regex, but [.], [*], etc.
       *   can be replaced by a fixed regex
       */
      if (letters[m] == '[' && m + 2 < pattern_i_size) {
        // cursory avoidance of escaping like [][]
        if (letters[m + 1] != '[' && letters[m + 1] != ']' &&
            letters[m + 2] == ']') {
          m += 3;
          continue;
        } else {
          out_pointer[i] = 0;
          break;
        }
      }
      /* NB: we're passing through several levels of backslash escapes here,
       *   among C, R, XML, and regex. Confusing. But note that \ cannot be
       *   the last character of a STR_CONST (it will always be interpreted as
       *   an escape in that case).
       */
      if (letters[m] == '\\') {
        int n = m + 1;
        // consume backslashes
        while (n < pattern_i_size && letters[n] == '\\') {
          ++n;
        }
        if (n == pattern_i_size) {
          break;  // shouldn't really be possible, but if so, out[i]=TRUE
        }
        if ((n - m) % 4 == 2) {  // 4k+2 slashes: a regex escape like \\s
          if (is_escape_char(&letters[n])) {  // case (2)
            out_pointer[i] = 0;
            break;
          } else {  // something escaped that needn't be, e.g. \\: or \\/
            m = n + 1;
          }
        } else {  // 4k+0,1,3 slashes: escaped pairs + maybe an escape like \n
          m = n;
        }
      } else {
        ++m;
      }
    }
  }
  UNPROTECT(1);  // out
  return out;
}

// See ?regex and third_party/R/R/R_3_6_3/src/extra/tre/tre-parse.c
//   We can skip:
//     [       -- treated separately
//     ] ) }   -- only special when paired
//     - , =   -- only special within contexts matched by { or (
//     #       -- only applies to extended regex, we don't set
//     ~       -- I don't know what this is. Some search suggests it can
//                be used to delimit a regex, e.g.:
//                https://stackoverflow.com/q/938100/3576984
//                However that doesn't apply to R: grep("~HI~", "HI") is empty
bool is_special(const char *s) {
  return *s == '^' || *s == '$' || *s == '{' || *s == '(' || *s == '.' ||
         *s == '*' || *s == '+' || *s == '?' || *s == '|';
}

// See ?regex -- we're looking for anything that can be used like \<x> and
//   have special meaning in a regex. Most common cases are \s, \b, \d, \w,
//   but ?regex lists *many* obscure examples, so we conservatively match any
//   alphanumeric and < and >. Implicitly relies on ASCII-ish encoding in `s`
//   insofar as the letters and numbers appear consecutively.
bool is_escape_char(const char *s) {
  return (*s - 'a' >= 0 && *s - 'a' < 26) || (*s - 'A' >= 0 && *s - 'A' < 26) ||
         (*s - '0' >= 0 && *s - '0' < 10) || *s == '<' || *s == '>';
}

