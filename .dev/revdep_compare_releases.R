#!/usr/bin/env Rscript
library(withr)
library(pkgload)

dev_dir <- getwd()
old_branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
old_release <- "v2.0.1"
main <- "main"

all_repos <- c(readLines("revdep-repos"), readLines("revdep-extra-repos"))
all_repos <- grep("^#", all_repos, invert = TRUE, value = TRUE)

setup <- function(version) {
  system2("git", c("checkout", "--quiet", version))
  pkgload::load_all()
  out_dir <- file.path(dev_dir, "revdep_comparison", version)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

find_r_package_below <- function(top_dir) {
  withr::local_dir(top_dir)
  # most common case: we land in the package directory
  if (file.exists("DESCRIPTION")) {
    return(top_dir)
  }
  # could also do well with breadth-first search, but this code is simpler
  desc_file <- list.files(top_dir, full.names = TRUE, recursive = TRUE, pattern = "^DESCRIPTION$")
  if (length(desc_file) == 0L) {
    stop("Could not find any DESCRIPTION below ", top_dir)
  } else if (length(desc_file) > 1L) {
    stop("Could not uniquely identify an R package in ", top_dir, ". Found these: ", toString(desc_file))
  }
  dirname(desc_file)
}

clone_and_lint <- function(repo) {
  message("Cloning repo ", repo, " and running lint_package()...")
  repo_dir <- withr::local_tempdir(basename(repo))
  # gert::git_clone() doesn't support shallow clones -- gert#101
  system2(
    "git",
    c("clone", "--depth=1", "--quiet", repo, repo_dir)
  )
  withr::local_dir(find_r_package_below(repo_dir))
  package <- read.dcf("DESCRIPTION", "Package")

  warnings <- character()
  withCallingHandlers(
    tryCatch(
      {
        lints <- lintr::lint_package()
        utils::write.csv(as.data.frame(lints), file.path(out_dir, paste0(package, ".csv")))
      },
      error = function(cond) {
        writeLines(conditionMessage(cond), file.path(out_dir, paste0(package, ".failure")))
      }
    ),
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  if (length(warnings) > 0L) {
    writeLines(warnings, file.path(out_dir, paste0(package, ".warnings")))
  }
}

out_dir <- setup(main)
for (repo in all_repos) clone_and_lint(repo)

out_dir <- setup(old_release)
for (repo in all_repos) clone_and_lint(repo)

system2("git", c("checkout", "--quiet", old_branch))
