# Look for unexported objects which are not used internally
# TODO(#1513): delete this after we have a dedicated linter for it

ns_contents <- parseNamespaceFile("lintr", "..")

export_names <- ns_contents$exports
registered_s3_methods <- apply(ns_contents$S3methods[, 1L:2L], 1L, paste, collapse = ".")

pkgload::load_all()

internal_names <- setdiff(
  ls(asNamespace("lintr"), all.names = TRUE),
  c(export_names, registered_s3_methods)
)

# .__global__, etc. 
internal_names <- grepv("^[.]__", internal_names, invert = TRUE)
# other generic R names
internal_names <- setdiff(internal_names, c(".onLoad", ".onAttach", ".packageName"))
# known false positives
#   - %||% is a mask for certain versions of R
#   - addin_lint* are used by RStudio even though they're not exported
#   - regexes_rd is used in ?object_name_linter by an \Sexpr execution
internal_names <- setdiff(internal_names, c("%||%", "addin_lint", "addin_lint_package", "regexes_rd"))

is_operator <- make.names(internal_names) != internal_names

operator_linter <- undesirable_operator_linter(internal_names[is_operator])
function_linter <- undesirable_function_linter(internal_names[!is_operator])

# suppressWarnings: #nolint for linters not present here
usages <- as.data.frame(suppressWarnings(lint_dir("R", linters = list(operator_linter, function_linter))))
usages$used_call <- gsub('.*["`]([^"`]*)["`].*', "\\1", usages$message)
# TODO(#2815): these should be excluded by the linter itself
usages <- subset(usages, !mapply(\(nm, l) grepl(sprintf("\\b%s <-", rex::rex(nm)), l), used_call, line))

# TODO(#2004): can this just come from get_source_expressions(), and/or will
#   the above linters "just work" once we're roxygen2-aware?
options(keep.source = TRUE)
roxy_usage <- character()
for (robj in roxygen2::parse_package()) {
  for (tag in robj$tags) {
    if (tag$tag == "evalRd") {
      roxy_usage <- c(roxy_usage,
        subset(getParseData(tag$val), token == "SYMBOL_FUNCTION_CALL", select = "text", drop = TRUE)
      )
    }
  }
}

unused_names <- setdiff(internal_names, c(usages$used_call, roxy_usage))

if (length(unused_names) > 0L) {
  stop("These objects are in the {lintr} namespace but don't have any usage: ", toString(unused_names))
}
