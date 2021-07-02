#!/usr/local/bin/Rscript

# compare the lints obtained before/after a given PR/branch vs current master

library(optparse)
library(dplyr)
library(purrr)
library(tibble)
library(usethis)
library(gert)
library(devtools)

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
temp_repo <- file.path(tempdir(), "lintr_repo")
dir.create(temp_repo)
invisible(file.copy(".", temp_repo, recursive = TRUE))
message("Executing from copy of repo at ", temp_repo)
old_wd <- setwd(temp_repo)
if (!interactive()) {
  .Last <- function() {
    setwd(old_wd)
    unlink(temp_repo, recursive = TRUE)
  }
}

param_list <- list(
  optparse::make_option(
    "--linters",
    default = "object_usage_linter",
    help = "Run the comparison for these linter(s) (comma-separated) [default %default]"
  ),
  optparse::make_option(
    "--branch",
    default = if (interactive()) {
      readline("Name a branch to compare to master (or skip to enter a PR#): ")
    },
    help = "Run the comparison for master vs. this branch"
  ),
  optparse::make_option(
    "--pr",
    default = if (interactive()) {
      # NB: optparse handles integer conversion
      readline("Name a PR # to compare to master (skip if you've entered a branch): ")
    },
    type = "integer",
    help = "Run the comparison for master vs. this PR"
  ),
  optparse::make_option(
    "--packages",
    default = if (interactive()) {
      readline("Provide a comma-separated list of packages (skip to provide a directory): ")
    },
    help = "Run the comparison using these packages (comma-separated)"
  ),
  optparse::make_option(
    "--pkg_dir",
    default = if (interactive()) {
      readline("Provide a directory where to select packages (skip if already provided as a list): ")
    },
    help = "Run the comparison using all packages in this directory"
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
    default = file.path("~", sprintf("lintr_compare_branches_%d.csv", as.integer(Sys.time()))),
    help = "Destination file to which to write the output"
  )
)

params <- optparse::parse_args(optparse::OptionParser(option_list = param_list))
# treat any skipped arguments from the prompt as missing
if (interactive()) {
  for (opt in c("branch", "pr", "packages", "pkg_dir", "sample_size")) {
    # typed arguments get cast even when missing, probably to NA
    if (is.na(params[[opt]]) || params[[opt]] == "") params[[opt]] <- NULL
  }
}

linter_names <- strsplit(params$linters, ",", fixed = TRUE)[[1L]]

# prioritize "branch"
is_branch <- FALSE
if (!is.null(params$branch)) {
  branch <- params$branch
  is_branch <- TRUE
} else if (!is.null(params$pr)) {
  pr <- params$pr
} else {
  stop("Please supply a branch (--branch) or a PR number (--pr)")
}

# prioritize packages
if (!is.null(params$packages)) {
  packages <- strsplit(params$packages, ",", fixed = TRUE)[[1L]]
} else if (!is.null(params$pkg_dir)) {
  packages <- list.files(normalizePath(params$pkg_dir), full.names = TRUE)
} else {
  stop("Please supply a comma-separated list of packages (--packages) or a directory of packages (--pkg_dir)")
}
# filter to (1) package directories or (2) package tar.gz files
packages <- packages[
  file.exists(packages) &
    (
      file.exists(file.path(packages, "DESCRIPTION")) |
        grepl("^[a-zA-Z0-9.]+_[0-9.-]+\\.tar\\.gz", basename(packages))
    )
]

if (is.null(params$sample_size)) {
  n_packages <- length(packages)
} else {
  if (params$sample_size > length(packages)) {
    message(sprintf(
      "Requested a sample of %d pacakges but only %d are available; running on all packages",
      params$sample_size,
      length(packages)
    ))
    n_packages <- length(packages)
  } else {
    n_packages <- params$sample_size
  }
  # randomize the order
  packages <- sample(packages)
}

# test if nchar(., "chars") works as intended
#   for all files in dir (see #541)
test_encoding <- function(dir) {
  tryCatch({
    lapply(
      list.files(dir, pattern = "(?i)\\.r(?:md)?$", recursive = TRUE, full.names = TRUE),
      function(x) {
        con <- file(x, encoding = lintr:::find_default_encoding(x) %||% "UTF-8")
        on.exit(close(con))
        nchar(readLines(con, warn = FALSE))
      }
    )
    FALSE
  }, error = function(x) TRUE)
}

# read Depends from DESCRIPTION
get_deps <- function(pkg) {
  deps <- read.dcf(file.path(pkg, "DESCRIPTION"), c("Imports", "Depends"))
  deps <- toString(deps[!is.na(deps)])
  if (deps == "") return(character())
  deps <- strsplit(deps, ",", fixed = TRUE)[[1L]]
  deps <- trimws(gsub("\\([^)]*\\)", "", deps))
  deps <- deps[deps != "R"]
  deps
}

lint_all_packages <- function(pkgs, linter, check_depends, warn = TRUE) {
  pkg_is_dir <- file.info(pkgs)$isdir
  pkg_names <- dplyr::if_else(
    pkg_is_dir,
    basename(pkgs),
    gsub("_.*", "", basename(pkgs))
  )

  # given how common it is to skip packages (e.g. due to uninstalled
  #   dependencies), use a while loop to try and reach n_packages instead
  #   of just iterating over n_packages (which may in actuality lint
  #   far fewer than that number)
  lints <- vector("list", n_packages)
  lint_names <- character(n_packages)
  ii <- 1L
  jj <- 0L
  while (ii <= length(pkgs) && jj <= n_packages) {
    if (pkg_is_dir[ii]) {
      pkg <- pkgs[ii]
    } else {
      tmp <- file.path(tempdir(), pkg_names[ii])
      on.exit(unlink(tmp, recursive = TRUE))
      # --strip-components makes sure the output structure is
      # /path/to/tmp/pkg/ instead of /path/to/tmp/pkg/pkg
      utils::untar(pkgs[ii], exdir = tmp, extras="--strip-components=1")
      pkg <- tmp
    }
    if (test_encoding(pkg)) {
      if (warn) warning(sprintf(
        "Package %s has some files with unknown encoding; skipping",
        pkg_names[ii]
      ))
      ii <- ii + 1L
      next
    }
    # object_usage_linter requires running package code, which may
    #   not work if the package has unavailable Depends;
    # object_name_linter also tries to run loadNamespace on Imports
    #   found in the target package's NAMESPACE file
    if (check_depends) {
      pkg_deps <- get_deps(pkg)
      if ("tcltk" %in% pkg_deps && !capabilities("tcltk")) {
        if (warn) warning(sprintf(
          "Package %s depends on tcltk, which is not available (via capabilities()); skipping",
          pkg_names[ii]
        ))
        ii <- ii + 1L
        next
      }
      try_deps <- tryCatch(
        find.package(pkg_deps),
        error = identity,
        warning = identity
      )
      if (inherits(try_deps, c("warning", "error"))) {
        if (warn) warning(sprintf(
          "Some package Dependencies for %s were unavailable: %s; skipping",
          pkg_names[ii],
          gsub("there (?:are no packages|is no package) called ", "", try_deps$message)
        ))
        ii <- ii + 1L
        next
      }
    }
    jj <- jj + 1L
    lints[[jj]] <- lint_dir(pkg, linters = linter, parse_settings = FALSE)
    lint_names[jj] <- pkg_names[ii]
    ii <- ii + 1L
  }
  if (jj == 0L) {
    stop("Couldn't successfully lint any packages")
  }
  if (jj < n_packages) {
    message(sprintf("Requested %d packages, but could only lint %d", n_packages, jj))
    lints = lints[1:jj]
    lint_names = lint_names[1:jj]
  }
  return(rlang::set_names(lints, lint_names))
}

format_lints <- function(x) {
  x %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows(.id = "package")
}

run_lints <- function(pkgs, linter, check_depends, warn = TRUE) {
  format_lints(lint_all_packages(pkgs, linter, check_depends, warn))
}

run_on <- function(what, pkgs, linter_name, ...) {
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    message("Completed on ", what, " in ", format(difftime(t1, t0, units = "mins")))
  })

  # safe to use force=TRUE because we're in temp_repo
  switch(
    what,
    master = {
      gert::git_branch_checkout("master", force = TRUE)
    },
    pr = {
      # pr_fetch doesn't expose this so use this to reset
      gert::git_branch_checkout("master", force = TRUE)
      usethis::pr_fetch(...)
    },
    branch = {
      gert::git_branch_checkout(..., force = TRUE)
    }
  )
  devtools::load_all()

  linter <- get(linter_name)()

  check_depends <- linter_name %in% c("object_usage_linter", "object_name_linter")

  # only show the warnings on "master" so as not to be repetitive
  run_lints(pkgs, linter, check_depends = check_depends, warn = what == "master")
}

run_pr_workflow <- function(linter_name, pkgs, pr) {
  old_branch <- gert::git_branch()
  on.exit(gert::git_branch_checkout(old_branch))

  dplyr::bind_rows(
    main = run_on("master", pkgs, linter_name),
    pr = run_on("pr", pkgs, linter_name, number = pr),
    .id = "source"
  )
}

run_branch_workflow <- function(linter_name, pkgs, branch) {
  old_branch <- gert::git_branch()
  on.exit(gert::git_branch_checkout(old_branch))

  dplyr::bind_rows(
    main = run_on("master", pkgs, linter_name),
    branch = run_on("branch", pkgs, linter_name, branch = branch),
    .id = "source"
  )
}

###############################################################################
# TODO: handle the case when working directory is not the lintr directory
###############################################################################

message("Comparing the output of the following linters: ", toString(linter_names))
if (is_branch) {
  message("Comparing branch ", branch, " to master")
} else {
  message("Comparing PR#", pr, " to master")
}
if (length(packages) > 50L) {
  message(
    "Comparing output of lint_dir run on many packages; here are 50: ",
    toString(basename(sample(packages, 50L)))
  )
} else {
  message(
    "Comparing output of lint_dir run for the following packages: ",
    toString(basename(packages))
  )
}

if (is_branch) {
  lints <- purrr::map_df(linter_names, run_branch_workflow, packages, branch)
} else {
  lints <- purrr::map_df(linter_names, run_pr_workflow, packages, pr)
}

message("Writing output to ", params$outfile)
write.csv(lints, params$outfile, row.names = FALSE)

if (interactive()) {
  setwd(old_wd)
  unlink(temp_repo, recursive = TRUE)
}
