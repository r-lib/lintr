#!/usr/local/bin/Rscript

# compare the lints obtained before/after a given PR
# the PR is always compared against master

# arguments
# --linters=linter1,linter2,...
#   run the comparison for these linters
# --branch=branch
#   run the comparison for master vs this branch
# --pr=pr
#   run the comparison for master vs this pr
# --packages=/path/to/package1,/path/to/package2,...
#   run the comparison for these packages
# --pkg_dir=pkg_dir
#   run the comparison for all packages in dir
# --sample_size
#   select a sample of the packages found in
#   --packages/--pkg_dir of the given size
# --outfile
#   a file to which to write the output

library(magrittr)
library(dplyr)
library(purrr)
library(tibble)
library(usethis)
library(gert)
library(devtools)

if(!file.exists("lintr.Rproj")) {
  "compare_branches.R should be run inside the lintr-package directory"
}

args <- strsplit(gsub("^--", "", commandArgs(TRUE)), "=", fixed = TRUE)
args <- setNames(
  vapply(args, `[`, character(1L), 2L),
  vapply(args, `[`, character(1L), 1L)
)
if ("linters" %in% names(args)) {
  linter_names <- strsplit(args["linters"], ",", fixed = TRUE)[[1L]]
} else {
  linter_names <- "object_usage_linter"
}
# prioritize "branch"
is_branch <- FALSE
if ("branch" %in% names(args)) {
  branch <- args["branch"]
  is_branch <- TRUE
} else if ("pr" %in% names(args)) {
  pr <- args["pr"]
} else {
  pr <- 709L
}

# prioritize packages
if ("packages" %in% names(args)) {
  packages <- strsplit(args["packages"], ",", fixed = TRUE)[[1L]]
} else if ("pkg_dir" %in% names(args)) {
  packages <- list.files(normalizePath(args["pkg_dir"]), full.names = TRUE)
} else {
  packages <- file.path("~", "proj", "code_as_data", "data", "packages")
}
# filter to (1) package directories or (2) package tar.gz files
packages <- packages[
  file.exists(packages) &
    (
      file.exists(file.path(packages, "DESCRIPTION")) |
        grepl("^[a-zA-Z0-9.]+_[0-9.-]+\\.tar\\.gz", basename(packages))
    )
]

if ("sample_size" %in% names(args)) {
  packages <- sample(packages, min(length(packages), as.integer(args["sample_size"])))
}

if ("outfile" %in% names(args)) {
  outfile <- args["outfile"]
} else {
  outfile <- normalizePath(
    file.path("~", sprintf("lintr_compare_branches_%d.csv", as.integer(Sys.time())))
  )
}

# In lintr directory

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
        utils::untar(pkgs[ii], exdir = tmp)
        pkg <- tmp
      }
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
    pr = run_on("pr", pkgs, linter_name, pr),
    .id = "source"
  )
}

run_branch_workflow <- function(linter_name, pkgs, branch) {
  old_branch <- gert::git_branch()
  on.exit(gert::git_branch_checkout(old_branch))

  dplyr::bind_rows(
    main = run_on("master", pkgs, linter_name),
    branch = run_on("branch", pkgs, linter_name, branch),
    .id = "source"
  )
}

###############################################################################

# TODO: handle both command line args and interactive runs
# TODO: handle the case when working directory is not the lintr directory
# TODO: convert to the original branch (if this was not master)
#   - at the end of the workflow (currently this always converts back to
#   master)
#   - and if there is any error when running the workflow
# TODO: save data.frame of lints to file

###############################################################################

message(pr)
message(toString(linter_names))
message("Any package repo found in these directories will be analysed:", toString(packages))

if (is_branch) {
  lints <- purrr::map_df(linter_names, run_pr_workflow, packages, pr)
} else {
  lints <- purrr::map_df(linter_names, run_branch_workflow, packages, branch)
}

write.csv(lints, outfile, row.names = FALSE)
