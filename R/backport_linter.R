#' Backport linter
#'
#' Check for usage of unavailable functions. Not reliable for testing r-devel dependencies.
#'
#' @param r_version Minimum R version to test for compatibility
#' @param except Character vector of functions to be excluded from linting.
#'  Use this to list explicitly defined backports, e.g. those imported from the {backports} package or manually
#'  defined in your package.
#'
#' @evalRd rd_tags("backport_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
backport_linter <- function(r_version = getRversion(), except = character()) {
  r_version <- normalize_r_version(r_version)
  backport_blacklist <- backports[r_version < R_system_version(names(backports))]
  backport_blacklist <- lapply(backport_blacklist, function(nm) setdiff(nm, except))

  names_xpath <- "//SYMBOL | //SYMBOL_FUNCTION_CALL"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    if (all(r_version >= R_system_version(names(backports)))) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    all_names_nodes <- xml2::xml_find_all(xml, names_xpath)
    all_names <- xml2::xml_text(all_names_nodes)

    # not sapply/vapply, which may over-simplify to vector
    # rbind makes sure we have a matrix with dimensions [n_versions x n_names]
    # so that colSums() works to tell us which names are in an unavailable version
    # rbind not cbind because R is column-major --> which() below will be in column order
    needs_backport <- do.call(rbind, lapply(backport_blacklist, function(nm) all_names %in% nm))
    bad_idx <- colSums(needs_backport) > 0L

    # which(arr.ind) returns things in the same order as which()
    needs_backport_version_idx <- ((which(needs_backport) - 1L) %% length(backport_blacklist)) + 1L
    lint_message <- sprintf(
      paste("%s (R %s) is not available for dependency R >= %s.",
            "Use the `except` argument of `backport_linter()` to configure available backports."),
      all_names[bad_idx],
      names(backport_blacklist)[needs_backport_version_idx],
      r_version
    )
    xml_nodes_to_lints(
      all_names_nodes[bad_idx],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}

normalize_r_version <- function(r_version) {
  if (is.character(r_version) &&
    re_matches(r_version, rex(start, "release" %or%
      list("oldrel", maybe("-", digits)) %or%
      "devel", end))) {
    # Support devel, release, oldrel, oldrel-1, ...
    if (r_version == "oldrel") {
      r_version <- "oldrel-1"
    }

    all_versions <- names(backports)
    minor_versions <- unique(re_substitutes(all_versions, rex(".", digits, end), ""))
    version_names <- c("devel", "release", paste0("oldrel-", seq_len(length(minor_versions) - 2L)))
    if (!r_version %in% version_names) {
      # This can only trip if e.g. oldrel-99 is requested
      stop("`r_version` must be a version number or one of ", toString(sQuote(version_names)))
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
    stop("`r_version` must be a R version number, returned by R_system_version(), or a string.")
  }
  if (r_version < "3.0.0") {
    warning("It is not recommended to depend on an R version older than 3.0.0. Resetting 'r_version' to 3.0.0.")
    r_version <- R_system_version("3.0.0")
  }
  r_version
}

# Sources:
# devel NEWS https://cran.rstudio.com/doc/manuals/r-devel/NEWS.html
# release NEWS https://cran.r-project.org/doc/manuals/r-release/NEWS.html
backports <- list(
  `4.3.0` = character(), # R devel needs to be ahead of all other versions
  `4.2.0` = c(".pretty", ".LC.categories", "Sys.setLanguage()"),
  `4.1.3` = character(), # need these for oldrel specifications
  `4.1.0` = c("numToBits", "numToInts", "gregexec", "charClass", "checkRdContents", "...names"),
  `4.0.5` = character(), # need these for oldrel specifications
  `4.0.0` = c(
    ".class2", ".S3method", "activeBindingFunction", "deparse1", "globalCallingHandlers",
    "infoRDS", "list2DF", "marginSums", "proportions", "R_user_dir", "socketTimeout", "tryInvokeRestart"
  ),
  `3.6.3` = character(), # need these for oldrel specifications
  `3.6.0` = c(
    "asplit", "hcl.colors", "hcl.pals", "mem.maxNsize", "mem.maxVsize", "nullfile", "str2lang",
    "str2expression", "update_PACKAGES"
  ),
  `3.5.3` = character(), # need these for oldrel specifications
  `3.5.0` = c("...elt", "...length", "askYesNo", "getDefaultCluster", "isFALSE", "packageDate", "warnErrList"),
  `3.4.4` = character(), # need these for oldrel specifications
  `3.4.0` = c(
    "check_packages_in_dir_details", "CRAN_package_db", "debugcall", "hasName",
    "isS3stdgeneric", "strcapture", "Sys.setFileTime", "undebugcall"
  ),
  `3.3.3` = character(), # need these for oldrel specifications
  `3.3.0` = c(
    ".traceback", "chkDots", "curlGetHeaders", "endsWith", "grouping", "isS3method",
    "makevars_site", "makevars_user", "Rcmd", "sigma", "startsWith", "strrep", "validEnc", "validUTF8"
  ),
  `3.2.5` = character(), # need these for oldrel specifications
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
