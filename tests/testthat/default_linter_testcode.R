# Each of the default linters should throw at least one lint on this file

# assignment
# function_left_parentheses
# brace_linter
# commas
# paren_brace
f = function (x,y = 1){}

# commented_code
# some <- commented("out code")

# cyclocomp
# equals_na
# brace_linter
# infix_spaces
# line_length
# object_length
# object_name
# object_usage
# open_curly
# T_and_F_symbol
someComplicatedFunctionWithALongCamelCaseName <- function(x)
{
  y <- 1L
  if (1L > 2L && 2L > 3L && 3L > 4L && 4L > 5L && 5L*10L > 6L && 5L > 6L && 6L > 7L && x == NA) {T} else F
}

# vector_logic
if (1L & 2L) FALSE else TRUE

# function_brace
my_metric <- function(x)
  sum(x) + prod(x)

# no_tab
# pipe_continuation
# seq_linter
# spaces_inside
x <- 1L:10L
x[ 2L]
1L:length(x) %>% lapply(function(x) x*2L) %>%
	head()

# single_quotes
message('single_quotes')

# spaces_left_parentheses
# trailing_whitespace
# semicolon
x <- 42L; y <- 2L +(1L:10L)

# trailing_blank_lines

