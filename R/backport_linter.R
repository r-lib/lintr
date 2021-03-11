#' @describeIn linters that checks for usage of unavailable functions. Not reliable for testing r-devel dependencies.
#' @param r_version Minimum R version to test for compatibility
#' @export
backport_linter <- function(r_version = getRversion()) {
  Linter(function(source_file) {
    if (inherits(r_version, "numeric_version")) r_version <- format(r_version)
    if (r_version < "3.0.0") {
      warning("It is not recommended to depend on an R version older than 3.0.0. Resetting 'r_version' to 3.0.0.")
      r_version <- "3.0.0"
    }
    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    names_xpath <- "//*[self::SYMBOL or self::SYMBOL_FUNCTION_CALL]"
    all_names_nodes <- xml2::xml_find_all(xml, names_xpath)
    all_names <- xml2::xml_text(all_names_nodes)

    # guaranteed to include 1 by early return above; which.min fails if all TRUE (handled by nomatch)
    needs_backport_names <- backports[1:(match(FALSE, r_version < names(backports), nomatch = length(backports)) - 1L)]

    # not sapply/vapply, which may over-simplify to vector -- cbind makes sure we have a matrix so rowSums works
    needs_backport <- do.call(cbind, lapply(needs_backport_names, function(nm) all_names %in% nm))
    bad_idx <- rowSums(needs_backport) > 0L

    lapply(which(bad_idx), function(ii) {
      node <- all_names_nodes[[ii]]
      line1 <- xml2::xml_attr(node, "line1")
      col1 <- as.integer(xml2::xml_attr(node, "col1"))
      col2 <- as.integer(xml2::xml_attr(node, "col2"))
      # line1 != line2 can't happen because function symbols cannot span multiple lines
      stopifnot(xml2::xml_attr(node, "line2") == line1)
      Lint(
        filename = source_file$filename,
        line_number = as.integer(line1),
        column_number = col1,
        type = "warning",
        message = sprintf(
          "%s (R %s) is not available for dependency R >= %s.",
          all_names[ii], names(needs_backport_names)[which(needs_backport[ii, ])], r_version
        ),
        line = source_file$lines[[line1]],
        ranges = list(c(col1, col2))
      )
    })
  })
}

backports <- list(
  `devel` = c("...names", "checkRdContents", "numToBits", "numToInts", "packBits"),
  `4.0.0` = c(
    ".class2", ".S3method", "activeBindingFunction", "deparse1", "globalCallingHandlers",
    "infoRDS", "list2DF", "marginSums", "proportions", "R_user_dir", "socketTimeout", "tryInvokeRestart"
  ),
  `3.6.0` = c(
    "asplit", "hcl.colors", "hcl.pals", "mem.maxNsize", "mem.maxVsize", "nullfile", "str2lang",
    "str2expression", "update_PACKAGES"
  ),
  `3.5.0` = c("...elt", "...length", "askYesNo", "getDefaultCluster", "isFALSE", "packageDate", "warnErrList"),
  `3.4.0` = c(
    "check_packages_in_dir_details", "CRAN_package_db", "debugcall", "hasName",
    "isS3stdgeneric", "strcapture", "Sys.setFileTime", "undebugcall"
  ),
  `3.3.0` = c(
    ".traceback", "chkDots", "curlGetHeaders", "endsWith", "grouping", "isS3method",
    "makevars_site", "makevars_user", "Rcmd", "sigma", "startsWith", "strrep", "validEnc", "validUTF8"
  ),
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
