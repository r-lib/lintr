#' Backport linter
#'
#' Check for usage of unavailable functions. Not reliable for testing r-devel dependencies.
#'
#' @param r_version Minimum R version to test for compatibility. Defaults to
#'  the R version currently in use. The version can be specified as a version
#'  number, or as a version alias (such as `"devel"`, `"oldrel"`, `"oldrel-1"`).
#' @param except Character vector of functions to be excluded from linting.
#'  Use this to list explicitly defined backports, e.g. those imported from the `{backports}` package or manually
#'  defined in your package.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "trimws(x)",
#'   linters = backport_linter("3.0.0")
#' )
#'
#' lint(
#'   text = "str2lang(x)",
#'   linters = backport_linter("3.2.0")
#' )
#'
#' lint(
#'   text = "deparse1(expr)",
#'   linters = backport_linter("3.6.0")
#' )
#'
#' # okay
#' lint(
#'   text = "trimws(x)",
#'   linters = backport_linter("3.6.0")
#' )
#'
#' lint(
#'   text = "str2lang(x)",
#'   linters = backport_linter("3.2.0", except = "str2lang")
#' )
#'
#' # Version aliases instead of numbers can also be passed to `r_version`
#' lint(
#'   text = "deparse1(expr)",
#'   linters = backport_linter("release")
#' )
#'
#' @evalRd rd_tags("backport_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
backport_linter <- function(r_version = getRversion(), except = character()) {
  r_version <- normalize_r_version(r_version)

  if (all(r_version >= R_system_version(names(backports)))) {
    return(Linter(function(source_expression) list(), linter_level = "file"))
  }

  backport_blacklist <- backports[r_version < R_system_version(names(backports))]
  backport_blacklist <- lapply(backport_blacklist, setdiff, except)
  backport_index <- rep(names(backport_blacklist), times = lengths(backport_blacklist))
  names(backport_index) <- unlist(backport_blacklist)

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    used_symbols <- xml_find_all(xml, "//SYMBOL")
    used_symbols <- used_symbols[xml_text(used_symbols) %in% names(backport_index)]

    used_calls <- source_expression$xml_find_function_calls(names(backport_index))
    all_names_nodes <- combine_nodesets(
      xml_find_first(used_calls, "SYMBOL_FUNCTION_CALL"),
      used_symbols
    )
    all_names <- xml_text(all_names_nodes)

    bad_versions <- unname(backport_index[all_names])

    lint_message <- sprintf(
      "%s (R %s) is not always available for requested dependency (R >= %s).",
      all_names,
      bad_versions,
      r_version
    )
    xml_nodes_to_lints(
      all_names_nodes,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}

normalize_r_version <- function(r_version) {
  rx_release_spec <- rex(
    start,
    "release" %or%
      list("oldrel", maybe("-", digits)) %or%
      "devel",
    end
  )
  if (is.character(r_version) && re_matches(r_version, rx_release_spec)) {
    # Support devel, release, oldrel, oldrel-1, ...
    if (r_version == "oldrel") {
      r_version <- "oldrel-1"
    }

    all_versions <- names(backports)
    minor_versions <- unique(re_substitutes(all_versions, rex(".", digits, end), ""))
    version_names <- c("devel", "release", paste0("oldrel-", seq_len(length(minor_versions) - 2L)))
    if (!r_version %in% version_names) {
      # This can only trip if e.g. oldrel-99 is requested
      cli_abort(c(
        "{.arg r_version} is not valid:",
        i = "It must be a version number or one of {.str {version_names}}.",
        x = "You entered {.str {r_version}} instead."
      ))
    }
    requested_version <- minor_versions[match(r_version, table = version_names)]
    available_patches <- all_versions[startsWith(all_versions, requested_version)]
    selected_patch <- which.max(as.integer(
      substr(available_patches, start = nchar(requested_version) + 2L, stop = nchar(available_patches))
    ))

    r_version <- R_system_version(available_patches[selected_patch])
  } else if (is.character(r_version)) {
    r_version <- R_system_version(r_version, strict = TRUE)
  } else if (!inherits(r_version, "R_system_version")) {
    cli_abort("{.arg r_version} must be an R version number, returned by {.fun R_system_version}, or a string.")
  }
  if (r_version < "3.0.0") {
    cli_warn(c(
      x = "Depending on an R version older than {.val 3.0.0} is not recommended.",
      i = "Resetting {.arg r_version} to {.val 3.0.0}."
    ))
    r_version <- R_system_version("3.0.0")
  }
  r_version
}

# Sources:
# devel NEWS https://cran.rstudio.com/doc/manuals/r-devel/NEWS.html
# release NEWS https://cran.r-project.org/doc/manuals/r-release/NEWS.html
backports <- list(
  `4.6.0` = character(),
  `4.5.0` = c(
    # base
    "grepv", "use",
    # stats
    "qr.influence",
    # tools
    "check_packages_urls", "check_package_dois",
    "CRAN_current_db", "CRAN_aliases_db", "CRAN_rdxrefs_db", "CRAN_archive_db", "CRAN_authors_db",
    "R", "parse_URI_reference",
    "base_aliases_db", "base_rdxrefs_db",
    "sha256sum"
  ),
  `4.4.3` = character(), # need character() entries for oldrel specifications
  `4.4.0` = character(),
  `4.3.3` = character(),
  `4.3.0` = c("R_compiled_by", "array2DF"),
  `4.2.3` = character(),
  `4.2.1` = "findCRANmirror",
  `4.2.0` = c(".pretty", ".LC.categories", "Sys.setLanguage()"),
  `4.1.3` = character(),
  `4.1.0` = c("numToBits", "numToInts", "gregexec", "charClass", "checkRdContents", "...names"),
  `4.0.5` = character(),
  `4.0.0` = c(
    ".class2", ".S3method", "activeBindingFunction", "deparse1", "globalCallingHandlers",
    "infoRDS", "list2DF", "marginSums", "proportions", "R_user_dir", "socketTimeout", "tryInvokeRestart"
  ),
  `3.6.3` = character(),
  `3.6.0` = c(
    "asplit", "hcl.colors", "hcl.pals", "mem.maxNsize", "mem.maxVsize", "nullfile", "str2lang",
    "str2expression", "update_PACKAGES"
  ),
  `3.5.3` = character(),
  `3.5.0` = c("...elt", "...length", "askYesNo", "getDefaultCluster", "isFALSE", "packageDate", "warnErrList"),
  `3.4.4` = character(),
  `3.4.0` = c(
    "check_packages_in_dir_details", "CRAN_package_db", "debugcall", "hasName",
    "isS3stdgeneric", "strcapture", "Sys.setFileTime", "undebugcall"
  ),
  `3.3.3` = character(),
  `3.3.0` = c(
    ".traceback", "chkDots", "curlGetHeaders", "endsWith", "grouping", "isS3method",
    "makevars_site", "makevars_user", "Rcmd", "sigma", "startsWith", "strrep", "validEnc", "validUTF8"
  ),
  `3.2.5` = character(),
  `3.2.0` = c(
    ".getNamespaceInfo", "check_packages_in_dir_changes", "debuggingState",
    "dir.exists", "dynGet", "extSoftVersion", "get0", "grSoftVersion", "hsearch_db",
    "isNamespaceLoaded", "lengths", "libcurlVersion", "returnValue", "tclVersion", "toTitleCase", "trimws"
  ),
  `3.1.3` = "pcre_config",
  `3.1.2` = "icuGetCollate",
  `3.1.1` = c(".nknots.smspl", "promptImport"),
  `3.1.0` = c("agrepl", "anyNA", "changedFiles", "cospi", "fileSnapshot", "find_gs_cmd", "sinpi", "tanpi"),
  `3.0.3` = "La_version",
  `3.0.2` = c("assertCondition", "assertError", "assertWarning", "getVignetteInfo"),
  `3.0.0` = c(
    ".onDetach", "bitwAnd", "bitwNot", "bitwOr", "bitwShiftL", "bitwShiftR", "bitwXor",
    "check_packages_in_dir", "cite", "citeNatbib", "clearPushBack", "packageName",
    "process.events", "provideDimnames", "quartz.save", "rep_len"
  )
)
