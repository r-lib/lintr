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
    return(Linter(\(source_expression) list(), linter_level = "file"))
  }

  backport_blacklist <- backports[r_version < R_system_version(names(backports))]
  backport_blacklist <- lapply(backport_blacklist, setdiff, except)
  backport_index <- rep(names(backport_blacklist), times = lengths(backport_blacklist))
  names(backport_index) <- unlist(backport_blacklist)

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    used_symbols <- xml_find_all_(xml, "//SYMBOL | //SPECIAL")
    used_symbols <- used_symbols[xml_text(used_symbols) %in% names(backport_index)]

    used_calls <- source_expression$xml_find_function_calls(names(backport_index))
    all_names_nodes <- combine_nodesets(
      xml_find_first_(used_calls, "SYMBOL_FUNCTION_CALL"),
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

backports <- list(
  `4.6.0` = c(
    # base
    "%notin%", "messageCondition", "mtfrm.Date",
    # grDevices
    "glyphFontVariation",
    # stats
    "free1way", "power.free1way.test", "ppplot", "rfree1way",
    # tcltk
    "tkgrid.child", "tkpack.child", "tkplace.child",
    # tools
    "checkSHA256sums", "verifySHA256signature",
    # utils
    "bitstring"
  ),
  `4.5.1` = "analyze_license", # tools
  `4.5.0` = c(
    # base
    ".set_ops_need_as_vector", "grepv", "summary.difftime", "zstdfile",
    # methods
    "matrixOps",
    # parallel
    "registerClusterType",
    # stats
    "qr.influence",
    # tools
    "CRAN_aliases_db", "CRAN_archive_db", "CRAN_authors_db",
    "CRAN_current_db", "CRAN_rdxrefs_db", "base_aliases_db",
    "base_rdxrefs_db", "check_package_dois", "check_package_urls",
    "parse_URI_reference", "sha256sum",
    # utils
    ".checkHT"
  ),
  `4.4.3` = character(), # need character() entries for oldrel specifications
  `4.4.1` = "R", # tools
  `4.4.0` = c(
    # base
    "%||%", ".formula2varlist", ".internalGenerics", ".rangeNum", "Exec",
    "Tailcall", "array2DF", "chooseOpsMethod", "chooseOpsMethod.default",
    "declare", "mtfrm.POSIXct", "nameOfClass", "nameOfClass.default",
    "range.POSIXct", "sort_by", "sort_by.data.frame", "sort_by.default",
    "use",
    # tools
    "pkg2HTML", "standard_package_names",
    # utils
    ".AtNames"
  ),
  `4.3.3` = character(),
  `4.3.1` = c(
    # parallel
    "closeNode", "recvData", "recvOneData", "sendData",
    # utils
    "findMatches"
  ),
  `4.3.0` = c(
    # base
    "$<-.POSIXlt", ".check_tzones", "R_compiled_by", "balancePOSIXlt",
    "is.finite.POSIXlt", "is.infinite.POSIXlt", "is.nan.POSIXlt",
    "unCfillPOSIXlt",
    # grDevices
    "embedGlyphs", "glyphAnchor", "glyphFont", "glyphFontList", "glyphHeight",
    "glyphHeightBottom", "glyphInfo", "glyphJust", "glyphWidth",
    "glyphWidthLeft",
    # grid
    "glyphGrob", "grid.glyph", "isClosed",
    # stats
    "toeplitz2",
    # tools
    "as.Rconcordance", "followConcordance", "matchConcordance"
  ),
  `4.2.3` = character(),
  `4.2.1` = c(
    # stats
    "psmirnov", "qsmirnov", "rsmirnov",
    # utils
    "findCRANmirror"
  ),
  `4.2.0` = c(
    # base
    ".LC.categories", ".pretty", "Sys.setLanguage", "as.vector.POSIXlt",
    "as.vector.data.frame", "mtfrm", "mtfrm.default",
    # grDevices
    ".clipPath", ".defineGroup", ".devUp", ".mask", ".opIndex", ".ruleIndex",
    ".useGroup",
    # grid
    "as.mask", "as.path", "defineGrob", "defnRotate", "defnScale",
    "defnTranslate", "emptyGTreeCoords", "emptyGrobCoords", "fillGrob",
    "fillStrokeGrob", "grid.define", "grid.fill", "grid.fillStroke",
    "grid.group", "grid.stroke", "grid.use", "gridCoords", "gridGTreeCoords",
    "gridGrobCoords", "groupFlip", "groupGrob", "groupRotate", "groupScale",
    "groupShear", "groupTranslate", "strokeGrob", "useGrob", "useRotate",
    "useScale", "useTranslate", "viewportRotate", "viewportScale",
    "viewportTransform", "viewportTranslate",
    # utils
    "clrhash", "gethash", "hashtab", "is.hashtab", "maphash", "numhash",
    "remhash", "sethash", "typhash"
  ),
  `4.1.3` = character(),
  `4.1.0` = c(
    # base
    "...names", "[<-.difftime", "all.equal.function", "c.factor", "gregexec",
    "isa", "numToBits", "numToInts", "rep.difftime", "xtfrm.data.frame",
    # grDevices
    ".linearGradientPattern", ".radialGradientPattern", ".setClipPath",
    ".setMask", ".setPattern", ".tilingPattern",
    # grid
    "editViewport", "linearGradient", "pattern", "radialGradient",
    # tools
    "checkRdContents",
    # utils
    "RtangleFinish", "RtangleRuncode", "charClass"
  ),
  `4.0.5` = character(),
  `4.0.1` = c(
    # base
    "activeBindingFunction",
    # grDevices
    "cairoSymbolFont"
  ),
  `4.0.0` = c(
    # base
    ".S3method", ".class2", "anyNA.data.frame", "as.list.difftime",
    "deparse1", "globalCallingHandlers", "infoRDS", "list2DF", "marginSums",
    "plot", "proportions", "sequence.default", "serverSocket", "socketAccept",
    "socketTimeout", "tryInvokeRestart",
    # grDevices
    "palette.colors", "palette.pals",
    # grid
    "unitType",
    # stats
    "Pair",
    # tools
    "R_user_dir"
  ),
  `3.6.3` = character(),
  `3.6.1` = c(
    # base
    "str2expression", "str2lang",
    # tools
    "update_PACKAGES"
  ),
  `3.6.0` = c(
    # base
    ".doSortWrap", "[[<-.POSIXlt", "allowInterrupts", "asplit",
    "conflictRules", "errorCondition", "length<-.difftime", "mem.maxNSize",
    "mem.maxVSize", "nullfile", "packageNotFoundError", "suspendInterrupts",
    "warningCondition",
    # grDevices
    "hcl.colors", "hcl.pals",
    # grid
    "delayGrob", "deviceDim", "deviceLoc", "emptyCoords", "grobCoords",
    "grobPoints", "isEmptyCoords", "unit.psum",
    # stats
    "DF2formula",
    # tools
    "vignetteInfo",
    # utils
    "osVersion"
  ),
  `3.5.3` = character(),
  `3.5.0` = c(
    # base
    "...elt", "...length", "..deparseOpts", ".Date", ".S3_methods_table",
    ".col", ".doWrap", ".fixupGFortranStderr", ".fixupGFortranStdout",
    ".makeSortEnum", ".row", ".rowNamesDF<-", "[.DLLInfoList", "[[.POSIXlt",
    "as.list.POSIXlt", "isFALSE", "isWrappable", "length<-.Date",
    "length<-.POSIXct", "length<-.POSIXlt", "print.summary.warnings",
    "summary.warnings",
    # grid
    "recordGrob",
    # parallel
    "getDefaultCluster",
    # stats
    ".vcov.aliased",
    # utils
    "asDateBuilt", "askYesNo", "packageDate", "warnErrList"
  ),
  `3.4.4` = character(),
  `3.4.1` = "CRAN_check_issues", # tools
  `3.4.0` = c(
    # base
    ".tryResumeInterrupt", ".valid.factor", "La_library", "diff.difftime",
    "duplicated.warnings", "print.eigen", "withAutoprint",
    # methods
    ".debugMethod", ".isMethodDebugged", ".undebugMethod", "isRematched",
    # tools
    "CRAN_check_details", "CRAN_check_results", "CRAN_memtest_notes",
    "CRAN_package_db", "check_packages_in_dir_details",
    "package_native_routine_registration_skeleton",
    # utils
    ".RtangleCodeLabel", ".romans", "debugcall", "hasName", "isS3stdGeneric",
    "strcapture", "undebugcall"
  ),
  `3.3.3` = character(),
  `3.3.1` = c(
    # tools
    "Rcmd", "makevars_site", "makevars_user", "summarize_CRAN_check_status"
  ),
  `3.3.0` = c(
    # base
    ".format.zeros", ".rmpkg", ".traceback", "[.table", "c.difftime",
    "chkDots", "endsWith", "grouping", "startsWith", "strrep", "validEnc",
    "validUTF8",
    # methods
    "externalRefMethod",
    # stats
    "sigma",
    # tcltk
    "tkimage.delete", "tkimage.height", "tkimage.inuse", "tkimage.type",
    "tkimage.types", "tkimage.width", "ttkscale", "ttkspinbox",
    # tools
    "langElts", "nonS3methods",
    # utils
    "isS3method"
  ),
  `3.2.5` = character(),
  `3.2.0` = c(
    # base
    ".getNamespaceInfo", ".maskedMsg", "[.Dlist", "[<-.numeric_version",
    "all.equal.envRefClass", "all.equal.environment", "as.data.frame.noquote",
    "c.warnings", "curlGetHeaders", "debuggingState", "dir.exists", "dynGet",
    "extSoftVersion", "file.mode", "file.mtime", "file.size", "forceAndCall",
    "get0", "is.na<-.numeric_version", "isNamespaceLoaded", "lengths",
    "libcurlVersion", "print.Dlist", "returnValue", "trimws",
    "unique.warnings",
    # grDevices
    "grSoftVersion",
    # methods
    ".S4methods",
    # stats
    ".nknots.smspl",
    # tcltk
    "tclVersion",
    # tools
    "check_packages_in_dir_changes", "loadPkgRdMacros", "loadRdMacros",
    "toTitleCase",
    # utils
    ".S3methods", "hsearch_db", "hsearch_db_concepts", "hsearch_db_keywords"
  ),
  `3.1.3` = "pcre_config", # base
  `3.1.2` = "icuGetCollate", # base
  `3.1.1` = "promptImport", # utils
  `3.1.0` = c(
    # base
    "$.data.frame", "..getNamespace", "OlsonNames", "[.warnings", "agrepl",
    "all.equal.POSIXt", "anyNA", "anyNA.POSIXlt", "anyNA.numeric_version",
    "cospi", "dontCheck", "sinpi", "tanpi",
    # grid
    "current.parent", "current.rotation", "explode", "forceGrob", "grid.grep",
    "legendGrob", "resolveHJust", "resolveRasterSize", "resolveVJust",
    # stats
    ".lm.fit", ".preformat.ts", "confint.lm", "dummy.coef.lm",
    # tools
    "buildVignette", "find_gs_cmd",
    # utils
    "changedFiles", "fileSnapshot", "suppressForeignCheck"
  ),
  `3.0.3` = "La_version", # base
  `3.0.2` = c(
    # parallel
    "mcMap",
    # tools
    "assertCondition", "assertError", "assertWarning", "getVignetteInfo"
  ),
  `3.0.1` = "depth", # grid
  `3.0.0` = c(
    # base
    ".External2", ".bincode", ".detach", ".getNamespace", ".kappa_tri",
    ".mapply", "@<-", "bitwAnd", "bitwNot", "bitwOr", "bitwShiftL",
    "bitwShiftR", "bitwXor", "clearPushBack", "numeric", "provideDimnames",
    "rep_len", "summary.proc_time",
    # grDevices
    "cairo_pdf", "cairo_ps", "svg",
    # grid
    "grid.delay", "grid.force", "grid.reorder", "grid.revert", "makeContent",
    "makeContext", "reorderGrob",
    # methods
    "checkAtAssignment", "evalOnLoad", "evalqOnLoad", "getLoadActions",
    "hasLoadAction", "insertClassMethods", "setLoadAction", "setLoadActions",
    # tools
    ".print.via.format", "checkPoFile", "checkPoFiles",
    "check_packages_in_dir", "getBibstyle", "make_translations_pkg",
    "summarize_check_packages_in_dir_depends",
    "summarize_check_packages_in_dir_results",
    "summarize_check_packages_in_dir_timings", "update_pkg_po",
    "vignetteEngine",
    # utils
    "aspell_package_C_files", "aspell_package_R_files", "cite", "citeNatbib",
    "getParseData", "getParseText", "globalVariables", "packageName",
    "process.events"
  )
)
