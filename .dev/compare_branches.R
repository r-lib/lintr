#!/usr/local/bin/Rscript

# compare the lints obtained before/after a given PR/branch vs current master

library(optparse)
library(dplyr)
library(purrr)
library(tibble)
library(usethis)
library(gert)
library(devtools)

if(!file.exists("lintr.Rproj")) {
  "compare_branches.R should be run inside the lintr-package directory"
}

param_list <- list(
  optparse::make_option(
    "--linters",
    default = "object_usage_linter",
    help = "Run the comparison for these linter(s) (comma-separated) [default %default]"
  ),
  optparse::make_option(
    "--branch",
    help = "Run the comparison for master vs. this branch"
  ),
  optparse::make_option(
    "--pr",
    type = "integer",
    help = "Run the comparison for master vs. this PR"
  ),
  optparse::make_option(
    "--packages",
     help = "Run the comparison using these packages (comma-separated)"
  ),
  optparse::make_option(
    "--pkg_dir",
    help = "Run the comparison using all packages in this directory"
  ),
  optparse::make_option(
    "--sample_size",
    type = "integer",
    help = "Select a sample of this number of packages from 'packages' or 'pkg_dir'"
  ),
  optparse::make_option(
    "--outfile",
    default = file.path("~", sprintf("lintr_compare_branches_%d.csv", as.integer(Sys.time()))),
    help = "Destination file to which to write the output"
  )
)

params <- optparse::parse_args(optparse::OptionParse(option_list = param_list))

linter_names <- strsplit(params$linters, ",", fixed = TRUE)[[1L]]

# prioritize "branch"
is_branch <- FALSE
if (!is.null(params$branch)) {
  branch <- params$branch
  is_branch <- TRUE
} else if (!is.null(params$pr)) {
  pr <- params$pr
} else {
  message("Please supply a branch (--branch) or a PR number (--pr)")
  q("no")
}

# prioritize packages
if (!is.null(params$packages)) {
  packages <- strsplit(params$packages, ",", fixed = TRUE)[[1L]]
} else if (!is.null(params$pkg_dir)) {
  packages <- list.files(normalizePath(params$pkg_dir), full.names = TRUE)
} else {
  message("Please supply a comma-separated list of packages (--packages) or a directory of packages (--pkg_dir)")
  q("no")
}
# filter to (1) package directories or (2) package tar.gz files
packages <- packages[
  file.exists(packages) &
    (
      file.exists(file.path(packages, "DESCRIPTION")) |
        grepl("^[a-zA-Z0-9.]+_[0-9.-]+\\.tar\\.gz", basename(packages))
    )
]

if (!is.null(params$sample_size)) {
  packages <- sample(packages, min(length(packages), params$sample_size))
}

# read Depends from DESCRIPTION
get_deps <- function(pkg) {
  deps <- read.dcf(file.path(pkg, "DESCRIPTION"), "Depends")
  if (is.na(deps)) return(character())
  deps <- strsplit(deps, ",", fixed = TRUE)[[1L]]
  deps <- trimws(gsub("\\([^)]*\\)", "", deps))
  deps <- deps[deps != "R"]
  deps
}

lint_all_packages <- function(pkgs, linter) {
  pkg_is_dir <- file.info(pkgs)$isdir
  pkg_names <- dplyr::if_else(
    pkg_is_dir,
    basename(pkgs),
    gsub("_.*", "", basename(pkgs))
  )

  map(
    seq_along(pkgs),
    function(ii) {
      if (!pkg_is_dir[ii]) {
        tmp <- file.path(tempdir(), pkg_names[ii])
        on.exit(unlink(tmp, recursive = TRUE))
        # --strip-components makes sure the output structure is
        # /path/to/tmp/pkg/ instead of /path/to/tmp/pkg/pkg
        utils::untar(pkgs[ii], exdir = tmp, extras="--strip-components=1")
        pkg <- tmp
      }
      # devtools::load_all() may not work for packages with Depends
      tryCatch(
        find.package(get_deps(pkg)),
        warning = function(w) stop("Package dependencies missing:\n", w$message)
      )
      lint_dir(pkg, linters = linter, parse_settings = FALSE)
    }
  ) %>%
    set_names(pkg_names)
}

format_lints <- function(x) {
  x %>%
    purrr::map(as_tibble) %>%
    dplyr::bind_rows(.id = "package")
}

run_lints <- function(pkgs, linter) {
  format_lints(lint_all_packages(pkgs, linter))
}

run_on <- function(what, pkgs, linter_name, ...) {
  switch(
    what,
    master = {
      gert::git_branch_checkout("master")
    },
    pr = {
      usethis::pr_fetch(...)
    },
    branch = {
      gert::git_branch_checkout(...)
    }
  )
  devtools::load_all()

  linter <- get(linter_name)()

  run_lints(pkgs, linter)
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
# TODO: handle both command line args and interactive runs
# TODO: handle the case when working directory is not the lintr directory
###############################################################################

message(pr)
message(toString(linter_names))
message("Any package repo found in these directories will be analysed:", toString(basename(packages)))

if (is_branch) {
  lints <- purrr::map_df(linter_names, run_branch_workflow, packages, branch)
} else {
  lints <- purrr::map_df(linter_names, run_pr_workflow, packages, pr)
}

write.csv(lints, outfile, row.names = FALSE)
