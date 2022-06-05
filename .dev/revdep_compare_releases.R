#!/usr/bin/env Rscript
library(withr)
library(pkgload)
library(data.table)

new_packages <- setdiff(readLines("revdep-packages"), rownames(installed.packages()))
install.packages(new_packages, repos = "https://cran.rstudio.com")
failed_install <- setdiff(new_packages, rownames(installed.packages()))
if (length(failed_install) > 0L) {
  warning("Failed to install some dependencies; please install these packages manually: ", toString(failed_install))
}

dev_dir <- getwd()
old_branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
old_release <- "v2.0.1"
main <- "main"

all_repos <- c(readLines("revdep-repos"), readLines("revdep-extra-repos"))[1:10]
all_repos <- grep("^#", all_repos, invert = TRUE, value = TRUE)

lint_timings <- new.env()
lint_timings$repo_timing <- vector("list", length(all_repos))
names(lint_timings$repo_timing) <- all_repos

setup <- function(version) {
  message("Checking out & loading ", version)
  system2("git", c("checkout", "--quiet", version))
  pkgload::load_all(quiet = TRUE)
  out_dir <- file.path(dev_dir, "revdep_comparison", version)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  message("Cloning repos and running lint_package()...")
  out_dir
}

cleanup <- function(version) {
  lint_timings[[version]] <- lint_timings$repo_timing
  lint_timings$repo_timing <- NULL
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
  message("  * ", repo)
  repo_dir <- withr::local_tempdir(basename(repo))
  # gert::git_clone() doesn't support shallow clones -- gert#101
  system2(
    "git",
    c("clone", "--depth=1", "--quiet", repo, repo_dir)
  )
  withr::local_dir(find_r_package_below(repo_dir))
  package <- read.dcf("DESCRIPTION", "Package")

  # if (package %in% failed_install) return()

  start_time <- proc.time()
  on.exit(lint_timings$repo_timing[[repo]] <- proc.time() - start_time)
  warnings <- character()
  withCallingHandlers(
    tryCatch(
      {
        lints <- lintr::lint_package()
        utils::write.csv(
          as.data.frame(lints),
          file.path(out_dir, paste0(package, ".csv")),
          row.names = FALSE
        )
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

load_lints <- function(version, filter_errors = TRUE) {
  csv_files <- list.files(file.path(dev_dir, "revdep_comparison", version), pattern = "\\.csv$", full.names = TRUE)
  names(csv_files) <- gsub("\\.csv$", "", basename(csv_files))
  lints <- data.table::rbindlist(lapply(csv_files, utils::read.csv), idcol = "repo")
  if (filter_errors) lints <- lints[, if (!any(type == "error")) .SD, by = .(repo, filename)]
  lints
}

out_dir <- setup(main)
for (repo in all_repos) clone_and_lint(repo)
cleanup(main)

out_dir <- setup(old_release)
for (repo in all_repos) clone_and_lint(repo)
cleanup(old_release)

system2("git", c("checkout", "--quiet", old_branch))

main_lints <- load_lints(main)
old_lints <- load_lints(old_release)

main_lints[!old_lints, on = c("repo", "filename", "line_number")]
old_lints[!main_lints, on = c("repo", "filename", "line_number")]
