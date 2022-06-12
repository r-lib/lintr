#!/usr/local/bin/Rscript

# compare the lints obtained before/after a given PR/branch vs a base branch (default main).
#
# How to use:
#   See below (param_list <-) for documentation of the script's arguments.
#   Most importantly, you'll need to provide a `--pkg_dir` pointing to a
#     local directory containing R packages, e.g., a CRAN mirror or GitHub
#     directory containing some number of packages. For a CRAN mirror at $CRAN_MIRROR,
#     the correct subdirectory to use is $CRAN_MIRROR/src/contrib, which contains the R
#     source code (the other binary directories only contain R code in inst/).
#   The script is executable, e.g. you can run the following from the lintr TLD:
#     ./dev/compare_branches --pkg_dir=/path/to/cran --sample_size=50 ...
#   The script outputs a CSV with the lint results for the script options to --outfile.
#   To compare the results of a PR to that at current HEAD, you could e.g. run
#     ./dev/compare_branches --branch=my-feature-branch ...
#   And then compare the results found in the new CSV file in .dev

# TODO
#  - handle the case when working directory is not the lintr directory
#  - support an interface for ad hoc download of packages to support running
#    the script without needing a CRAN mirror more easily/friendly

suppressPackageStartupMessages({
  library(optparse)
  library(data.table, include.only = c("fwrite", "rbindlist", "data.table", "dcast"))
  library(dplyr)
  library(purrr)
  library(tibble)
  library(gert)
  library(pkgload)
})

if (!file.exists("lintr.Rproj")) {
  stop("compare_branches.R should be run inside the lintr-package directory")
}

# move to temp repo. this allows multiple executions of this
#   script simultaneously (otherwise the branch state across
#   executions will collide), as well as continuing dev
#   on the "main" package clone while the script runs (otherwise
#   all current edits must be checked in before running)
# named lintr_repo in case this script happens to be run against
#   a tar of lintr itself...
# TODO(michaelchirico): use git clone instead to just clone a single commit to tempdir()
temp_repo <- file.path(tempdir(), "lintr_repo")
dir.create(temp_repo)
invisible(file.copy(".", temp_repo, recursive = TRUE))
message("Executing from copy of repo at ", temp_repo)
old_wd <- setwd(temp_repo)
# ensure no accidental pushes to any remote from this temp repo, so we can abuse it safely
remotes <- system2("git", "remote", stdout = TRUE)
for (remote in remotes) system2("git", c("remote", "remove", remote))
.Last <- function() {
  setwd(old_wd)
  unlink(temp_repo, recursive = TRUE)
}

param_list <- list(
  optparse::make_option(
    "--linters",
    default = if (interactive()) {
      readline("Provide a comma-separated list of linters to compare (skip to include all linters on each branch): ")
    },
    help = "Run the comparison for these linter(s) (comma-separated)"
  ),
  optparse::make_option(
    "--base_branch",
    default = if (interactive()) {
      readline("Name a branch/tag to use as base (skip to use main): ")
    } else {
      "main"
    },
    help = "Compare to this branch/tag"
  ),
  optparse::make_option(
    "--branch",
    default = if (interactive()) {
      readline("Name a branch/tag to compare to base_branch (or skip to enter a PR# or to run only on base_branch): ")
    },
    help = "Run the comparison for base vs. this branch/tag"
  ),
  optparse::make_option(
    "--pr",
    default = if (interactive()) {
      # NB: optparse handles integer conversion
      readline("Name a PR # to compare to the base branch (skip if you've entered a branch or to run only on base_branch): ")
    },
    type = "integer",
    help = "Run the comparison for base_branch vs. this PR"
  ),
  optparse::make_option(
    "--pkg_dir",
    default = if (nzchar(cran_mirror <- Sys.getenv("CRAN_MIRROR"))) {
      dir <- file.path(cran_mirror, "src", "contrib")
      message("Using the CRAN miror found at Sys.getenv('CRAN_MIRROR'): ", dir)
      dir
    } else if (interactive()) {
      readline("Provide a directory where to select packages (skip to select the current directory): ")
    },
    help = "Run the comparison using all packages in this directory"
  ),
  optparse::make_option(
    "--packages",
    default = if (interactive()) {
      readline("Provide a comma-separated list of packages (skip to include all directories for sampling): ")
    },
    help = "Run the comparison using these packages (comma-separated)"
  ),
  optparse::make_option(
    "--sample_size",
    type = "integer",
    default = if (interactive()) {
      readline("Enter the number of packages to include (skip to include all): ")
    },
    help = "Select a sample of this number of packages from 'packages' or 'pkg_dir'"
  ),
  optparse::make_option(
    "--outfile",
    default = file.path(".dev", sprintf("lintr_compare_branches_%d.csv", as.integer(Sys.time()))),
    help = "Destination file to which to write the output"
  ),
  optparse::make_option(
    "--benchmark",
    default = if (interactive()) {
      askYesNo("Benchmark the timing of linter execution?")
    },
    type = "logical",
    help = "Whether to run performance diagnostics of the branch(es)"
  )
)

params <- optparse::parse_args(optparse::OptionParser(option_list = param_list))
params$outdir <- dirname(params$outfile)

# treat any skipped arguments from the prompt as missing
if (interactive()) {
  for (opt in c("linters", "branch", "pr", "packages", "pkg_dir", "sample_size")) {
    # typed arguments get cast even when missing, probably to NA
    if (isTRUE(is.na(params[[opt]]) || params[[opt]] == "")) params[[opt]] <- NULL
  }
  if (isTRUE(is.na(params$base_branch) || params$base_branch == "")) params$base_branch <- "main"
}

if (params$benchmark) {
  library(microbenchmark)
  recorded_timings <- new.env()
}

if (is.null(params$linters)) {
  linter_names <- "_all_"
} else {
  linter_names <- strsplit(params$linters, ",", fixed = TRUE)[[1L]]
}

base_branch <- params$base_branch
if (is.null(base_branch) || is.na(base_branch) || !nzchar(base_branch)) {
  stop("Please supply a base branch (--base-branch)")
}

# prioritize "branch"
is_branch <- FALSE
has_target <- TRUE
if (!is.null(params$branch)) {
  branch <- params$branch
  is_branch <- TRUE
} else if (!is.null(params$pr)) {
  pr <- params$pr
} else {
  has_target <- FALSE
}

if (is.null(params$pkg_dir)) {
  # TODO: I think we need to enable running the script outside
  #   the lintr directory in order for this to work. the intention is
  #   to be able to run compare_branches --packages=p1,p2 --linters=l1,l2
  #   and it looks in the executing directory for p1,p2.
  stop("pkg_dir is required")
  params$pkg_dir <- "."
}
packages <- list.files(normalizePath(params$pkg_dir), full.names = TRUE)
if (!is.null(params$packages)) {
  # strip version numbers
  package_names <- gsub("_.*", "", basename(packages))
  packages <- packages[package_names %in% strsplit(params$packages, ",", fixed = TRUE)[[1L]]]
}
# filter to (1) package directories or (2) package tar.gz files
packages <- packages[
  file.exists(packages) &
    (
      file.exists(file.path(packages, "DESCRIPTION")) |
        grepl("^[a-zA-Z0-9.]+_[0-9.-]+\\.tar\\.gz", basename(packages))
    )
]

if (length(packages) == 0L) {
  stop("No packages found!")
}

if (is.null(params$sample_size)) {
  n_packages <- length(packages)
} else {
  if (params$sample_size <= 0) {
    stop("Please request >0 packages")
  }
  if (params$sample_size > length(packages)) {
    message(sprintf(
      "Requested a sample of %d packages but only %d are available; running on all packages",
      params$sample_size,
      length(packages)
    ))
    n_packages <- length(packages)
  } else {
    n_packages <- params$sample_size
  }
  # draw sample & randomize order
  packages <- sample(packages, size = n_packages)
}

# test if nchar(., "chars") works as intended
#   for all files in dir (see #541)
test_encoding <- function(dir) {
  !inherits(what = "error", tryCatch(
    for (r_file in list.files(dir, pattern = "(?i)\\.r(?:md)?$", recursive = TRUE, full.names = TRUE)) {
      # lintr has better encoding support since 8cd6ad~linter>2.0.1~Jul 2021; use
      #   the accompanying helper if possible. clunkier default otherwise.
      encoding <- tryCatch(lintr:::find_default_encoding(r_file), error = function(...) NULL)
      local({
        con <- file(r_file, encoding = encoding %||% "UTF-8")
        on.exit(close(con))
        lines <- readLines(con, warn = FALSE)
        nchar(lines)
        nchar(lines, "chars")
      })
    },
    error = identity
  ))
}

# read Depends from DESCRIPTION
get_deps <- function(pkg) {
  deps <- read.dcf(file.path(pkg, "DESCRIPTION"), c("Imports", "Depends"))
  deps <- toString(deps[!is.na(deps)])
  if (deps == "") {
    return(character())
  }
  deps <- strsplit(deps, ",", fixed = TRUE)[[1L]]
  deps <- trimws(gsub("\\([^)]*\\)", "", deps))
  deps <- deps[deps != "R"]
  deps
}

lint_one_package <- function(package, linters, out_dir, check_deps) {
  package_is_dir <- file.info(package)$isdir
  package_name <- gsub("_.*", "", basename(package))
  if (params$benchmark) {
    recorded_timings$current_package <- package_name
    on.exit(rm("current_package", envir = recorded_timings))
  }

  if (!package_is_dir) {
    tmp <- file.path(tempdir(), package_name)
    # TODO: only extract files that lintr::lint_package() cares about
    # package_files <- utils::untar(package, list = TRUE)
    # lint_files <- grep(file.path(package_name, "(R|tests|inst|vignettes|data-raw|demo)"), package_files, value = TRUE)
    # exclude directories because untar() gets confused when extracting path/to and then path/to/file
    # lint_files <- lint_files[!endsWith(lint_files, "/")]
    # --strip-components makes sure the output structure is
    # /path/to/tmp/pkg/ instead of /path/to/tmp/pkg/pkg
    utils::untar(package, exdir = tmp, extras = "--strip-components=1")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    package <- tmp
  }
  if (!test_encoding(package)) {
    warning(sprintf(
      "Package %s has some files with unknown encoding; skipping",
      package_name
    ))
    return(FALSE)
  }
  # object_usage_linter requires running package code, which may
  #   not work if the package has unavailable Depends;
  # object_name_linter also tries to run loadNamespace on Imports
  #   found in the target package's NAMESPACE file
  if (check_deps) {
    package_deps <- get_deps(package)
    if ("tcltk" %in% package_deps && !capabilities("tcltk")) {
      warning(sprintf(
        "Package %s depends on tcltk, which is not available (via capabilities()); skipping",
        package_names[ii]
      ))
      return(FALSE)
    }
    try_deps <- tryCatch(
      find.package(package_deps),
      error = identity,
      warning = identity
    )
    if (inherits(try_deps, c("warning", "error"))) {
      warning(sprintf(
        "Some package Dependencies for %s were unavailable: %s; skipping",
        package_name,
        gsub("there (?:are no packages|is no package) called ", "", try_deps$message)
      ))
      return(FALSE)
    }
  }

  # terminal_blank_lines_linter() started suppressing terminal newline warning
  #   in d20768a~lintr>2.0.1~Feb 2021; prior to that, we get a ton of those warnings.
  #   ignore them because they're innocuous.
  # also ignore lintr's deprecations to make including all linters easier
  suppressMessages(withCallingHandlers(
    lints <- as.data.frame(lint_dir(package, linters = linters, parse_settings = FALSE)),
    warning = function(cond) {
      if (!grepl("ncomplete final line found|was deprecated in lintr", cond$message)) {
        warning(cond$message, call. = FALSE)
      }
      invokeRestart("muffleWarning")
    }
  ))
  if (nrow(lints) > 0L) data.table::fwrite(lints, file.path(out_dir, paste0(package_name, ".csv")))
  TRUE
}

# available_linters is the preferred way to do this, but only
#   available since aafba4~lintr>2.0.1~Mar 2022. relying on
#   'exports ending in _linter' seems to work for v2.0.1,
#   but not clear how robust it is in general.
get_all_linters <- function() {
  lintr_exports <- getNamespaceExports("lintr")
  if ("available_linters" %in% lintr_exports) {
    linter_names <- available_linters(tags = NULL)$linter
  } else {
    linter_names <- grep("_linter$", lintr_exports, value = TRUE)
  }
}

# since 2d76469~lintr>2.0.1~Feb 2021, all linters are function factories.
#   but we also want to support the earlier 'simple' linters for robustness
get_linter_from_name <- function(linter_name) {
  tryCatch(
    # apparently v2.0.1 does not have a default because somehow a0fff5~linter<v2.0.1~May 2017
    #   did not make it into the release?
    if (linter_name == "line_length_linter" && !is.integer(formals(linter_name)$length)) {
      eval(call(linter_name, 80L))
    } else {
      eval(call(linter_name))
    },
    error = function(cond) eval(as.name(linter_name))
  )
}

max_digits <- function(n) as.integer(ceiling(log10(n)))

# might be better to pre-initialize everything to seed at 0,
#   but it requires knowing all the filenames in advance, which
#   aren't revealed until inside of lint_dir().
set_duration <- function(t0, branch, linter, package, file) {
  duration <- microbenchmark::get_nanotime() - t0
  current_total <- recorded_timings[[branch]][[linter]][[package]][[file]]
  if (is.null(current_total)) current_total <- 0
  recorded_timings[[branch]][[linter]][[package]][[file]] <- current_total + duration
}

get_tracer <- function(branch, linter_name) {
  # leave recorded_timings$current_package unevaluated -- needs to be read at execution time
  bquote({
    t0 <- microbenchmark::get_nanotime()
    # get first positional argument value (argument name may differ over time)
    filename <- eval(as.name(names(match.call())[2L]))$filename
    on.exit(set_duration(t0, .(branch), .(linter_name), recorded_timings$current_package, filename))
  })
}

get_benchmarked_linter <- function(linter_name, branch) {
  linter <- get_linter_from_name(linter_name)

  # workaround for apparent R bugs. See r-devel#18354 and r-devel#18355.
  old_class <- class(linter)
  old_name <- attr(linter, "name")
  this_environment <- environment()
  class(linter) <- NULL
  suppressMessages(trace(
    what = linter,
    where = this_environment,
    tracer = get_tracer(branch, linter_name),
    print = FALSE
  ))
  class(linter) <- old_class
  attr(linter, "name") <- old_name
  linter
}

# gert::git_branch_checkout() doesn't support checking out tags directly
#   (like CLI 'git checkout' flexibly does); use this workaround instead
switch_to_ref <- function(ref) {
  tryCatch(
    gert::git_branch_checkout(ref, force = TRUE),
    error = function(cond) {
      tag_metadata <- gert::git_tag_list(ref)
      tag_hash <- tag_metadata$commit
      if (length(tag_hash) == 0L) {
        stop("Unable to find branch or tag '", ref, "'")
      }
      # if an actual tag is provided, we'll get 0 or 1 results. but
      #   git_tag_list() supports wildcards too, e.g. git_tag_list("v2*"), so safeguard:
      if (length(tag_hash) > 1L) {
        warning("Matched more than one tag! Selecting the first of: ", toString(tag_metadata$name))
        tag_hash <- tag_hash[1L]
      }
      # no way to checkout a commit directly, so create a branch based to it instead -- gert#147
      # also don't have checkout --force, so just reset to prevent that from blocking -- gert#177
      gert::git_reset_hard()
      gert::git_branch_create(paste(sample(letters), collapse = ""), ref = tag_hash)
    }
  )
}

run_workflow <- function(what, packages, linter_names, branch, number) {
  t0 <- Sys.time()
  old_branch <- gert::git_branch()
  on.exit({
    gert::git_branch_checkout(old_branch)
    t1 <- Sys.time()
    message("  Completed on ", what, " in ", format(difftime(t1, t0, units = "mins"), digits = 1L))
  })

  # safe to use force=TRUE because we're in temp_repo
  if (what == "pr") {
    gert::git_branch_checkout("main", force = TRUE)
    gert::git_checkout_pull_request(number)
  } else {
    switch_to_ref(branch)
  }
  pkgload::load_all(export_all = FALSE)

  if (identical(linter_names, "_all_")) {
    linter_names <- get_all_linters()
  }
  check_deps <- any(c("object_usage_linter", "object_name_linter") %in% linter_names)

  if (params$benchmark) {
    recorded_timings[[branch]] <- list()
    linters <- lapply(linter_names, get_benchmarked_linter, branch)
  } else {
    linters <- lapply(linter_names, get_linter_from_name)
  }

  # accumulate results sequentially to allow for interruptions of long-running executions without losing progress
  out_temp_dir <- file.path(old_wd, params$outdir, ".partial", if (what == "pr") paste0("pr", number) else branch)
  dir.create(out_temp_dir, recursive = TRUE, showWarnings = FALSE)

  linted_packages <- 0L
  package_i <- 0L
  pkgs_width <- max_digits(length(packages))
  done_width <- max_digits(n_packages)
  stdout_width <- getOption("width")
  # given how common it can be to skip packages (e.g. due to uninstalled
  #   dependencies), use a while loop to try and reach n_packages instead
  #   of just iterating over n_packages (which may in actuality lint
  #   far fewer than that number)
  while (linted_packages < n_packages) {
    package_i <- package_i + 1L
    if (package_i > length(packages)) break
    package <- packages[[package_i]]
    package_str <- gsub("_.*", "", basename(package))
    success <- lint_one_package(package, linters, out_temp_dir, check_deps)
    linted_packages <- linted_packages + success
    # TODO(michaelchirico): this stopped working interactively (only dumps at the end of the loop) -- why?
    cat(sprintf(
      "\r[%0*s : %0*s / %d] %s%s",
      pkgs_width, package_i, done_width, linted_packages, n_packages, package_str,
      # {[, ,:, , ,/, ,], }: 9 characters, plus 5 characters extra buffer
      strrep(" ", stdout_width - 14L - pkgs_width - 2 * done_width - nchar(package_str))
    ))
  }
  cat("\n")
  if (linted_packages == 0L) {
    stop("Couldn't successfully lint any packages")
  }
  if (linted_packages < n_packages) {
    message(sprintf("Requested %d packages, but could only lint %d", n_packages, linted_packages))
  }
}

if (has_target) {
  message("Comparing the output of the following linters: ", toString(linter_names))
  if (is_branch) {
    message("Comparing branch ", branch, " to ", base_branch)
    target <- branch
  } else {
    message("Comparing PR#", pr, " to ", base_branch)
    target <- pr
  }
} else {
  message("Running the following linters: ", toString(linter_names))
}
if (length(packages) > 50L) {
  message(
    "Comparing output of lint_dir run on many packages; here are 50: ",
    toString(basename(sample(packages, 50L)))
  )
} else {
  message(
    "Comparing output ", if (params$benchmark) "and performance ",
    "of lint_dir run for the following packages: ",
    toString(basename(packages))
  )
}

if (dir.exists(file.path(params$outdir, ".partial"))) {
  message(
    "** A .partial directory already exists in ", params$outdir, "; ",
    "these will be included here. Please interrupt & delete these files first if this is not intended"
  )
}

# 3 nested loops, organized for efficiency
#  (1) (outermost) branch (only build & install the package once per branch)
#  (2) (central) packages (only unzip the package once per branch)
#  (3) (innermost) linters (once the package is installed, easy to cycle through linters)
run_workflow("branch", packages, linter_names, branch = base_branch)
if (has_target) {
  if (is_branch) {
    run_workflow("branch", packages, linter_names, branch = target)
  } else {
    run_workflow("pr", packages, linter_names, number = target)
  }
}

setwd(old_wd)
message("Writing lint comparison output to ", params$outfile)

load_partial_results <- function(target, is_branch) {
  directory <- file.path(params$outdir, ".partial", if (is_branch) target else paste0("pr", target))
  files <- list.files(directory, full.names = TRUE)
  names(files) <- gsub("\\.csv$", "", basename(files))
  purrr::map_df(files, readr::read_csv, show_col_types = FALSE, .id = "package")
}

if (has_target) {
  lints <- dplyr::bind_rows(
    base = load_partial_results(base_branch, TRUE),
    branch = load_partial_results(target, is_branch),
    .id = "source"
  )
} else {
  lints <- load_partial_results(base_branch, TRUE)
}
unlink(file.path(params$outdir, ".partial"), recursive = TRUE)
data.table::fwrite(lints, params$outfile, row.names = FALSE)

if (params$benchmark) {
  benchmark_file <- gsub("\\.csv$", "_benchmark_timings.csv", params$outfile)
  message("Writing benchmark timing output to ", benchmark_file)
  # lots of nesting, fun
  timings_data <- rbindlist(
    idcol = "branch",
    lapply(
      recorded_timings,
      function(branch) rbindlist(
        idcol = "linter",
        lapply(
          branch,
          function(linter) rbindlist(
            idcol = "package",
            lapply(
              linter,
              function(package) data.table::data.table(filename = names(package), duration = unlist(package))
            )
          )
        )
      )
    )
  )
  # delete noisy/redundant information from filename
  timings_data[, filename := sub(file.path(tempdir(), .BY$package, ""), "", filename), by = package]
  # save data in wide format to save some space (data gets saved as column names)
  timings_data[,
    data.table::dcast(.SD, linter + package + filename ~ branch, value.var = "duration")
  ][,
    data.table::fwrite(timings_data, benchmark_file)
  ]
}

if (interactive()) {
  unlink(temp_repo, recursive = TRUE)
} else {
  warnings()
}
