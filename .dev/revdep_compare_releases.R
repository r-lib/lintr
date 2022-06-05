#!/usr/bin/env Rscript
library(withr)
library(pkgload)
library(data.table)

repo_data <- rbind(
  data.table::fread("revdep-repos"),
  data.table::fread("revdep-extra-repos", skip = 2L),
  data.table::fread("revdep-no-repos")
)
setkey(repo_data, repo)

new_packages <- setdiff(repo_data$package, rownames(installed.packages()))
install.packages(new_packages, repos = "https://cran.rstudio.com")
failed_install <- setdiff(new_packages, rownames(installed.packages()))
if (length(failed_install) > 0L) {
  stop("Failed to install some dependencies; please install these packages manually: ", toString(failed_install))
}

dev_dir <- getwd()
dev_branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
old_release <- "v2.0.1"
main <- "main"

all_repos <- repo_data$repo

lint_timings <- new.env()
lint_timings$repo_timing <- vector("list", length(all_repos))
names(lint_timings$repo_timing) <- all_repos

# ---- Helpers ----
result_path <- function(...) file.path(dev_dir, "revdep_comparison", ...)

setup <- function(version) {
  message("Checking out & loading ", version)
  system2("git", c("checkout", "--quiet", version))
  pkgload::load_all(quiet = TRUE)
  out_dir <- result_path(version)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  message("Cloning repos and running lint_package()...")
  out_dir
}

cleanup <- function(version) {
  repo_data[, paste0("elapsed_", version) := elapsed]
  repo_data[, elapsed := NULL]

  setwd(dev_dir)
  system2("git", c("checkout", "--quiet", dev_branch))
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

clone_and_lint <- function(repo_url) {
  message("  * ", repo_url)
  repo_dir <- withr::local_tempdir(basename(repo_url))
  # gert::git_clone() doesn't support shallow clones -- gert#101
  system2(
    "git",
    c("clone", "--depth=1", "--quiet", repo_url, repo_dir)
  )
  withr::local_dir(find_r_package_below(repo_dir))
  package <- read.dcf("DESCRIPTION", "Package")

  start_time <- proc.time()
  on.exit(repo_data[.(repo_url), elapsed := (proc.time() - start_time)["elapsed"]])
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
  csv_files <- list.files(result_path(version), pattern = "\\.csv$", full.names = TRUE)
  names(csv_files) <- gsub("\\.csv$", "", basename(csv_files))
  lints <- data.table::rbindlist(lapply(csv_files, utils::read.csv), idcol = "repo")
  if (filter_errors) lints <- lints[, if (!any(type == "error")) .SD, by = .(repo, filename)]
  lints
}

as.data.table.proc_time <- function(x) {
  data.table(user = x[[1L]], system = x[[2L]], elapsed = x[[3L]])
}

match_and_strip <- function(x, pattern) gsub(pattern, "", grep(pattern, x, value = TRUE))

summarize_failures <- function(version, failures) {
  files <- result_path(version, failures)
  packages <- gsub("\\.failures$", "", failures)

  package_failures <- sapply(files, function(x) paste(unique(readLines(x)), collapse = " ||| "))

  paste(sprintf("  %s: %s", packages, package_failures), collapse = "\n")
}

summarize_lint_delta <- function(new, old) {
  new_version <- new$version[1L]
  old_version <- old$version[1L]

  new_error_files <- new[type == "error", .(. = TRUE), by = .(package, filename)]
  old_error_files <- new[type == "error", .(. = TRUE), by = .(package, filename)]

  all_error_files <- merge(
    new_error_files, old_error_files,
    by = c("package", "filename"), suffixes = c("new", "old"),
    all = TRUE
  )
  if (anyNA(all_error_files)) {
    if (anyNA(all_error_files$.new)) {
      all_error_files[
        is.na(.new),
        message("Linting failed ", old_version, " but not ", new_version, ": ", toString(filename))
      ]
    }
    if (anyNA(all_error_files$.old)) {
      all_error_files[
        is.na(.old),
        message("Linting failed ", new_version, " but not ", old_version, ": ", toString(filename))
      ]
    }
  }
  new <- new[!all_error_files, on = c("package", "filename")]
  old <- old[!all_error_files, on = c("package", "filename")]

  new_only <- new[!old, on = c("package", "filename", "line_number")]
  new_only[, version := NULL]

  old_only <- old[!new, on = c("package", "filename", "line_number")]
  old_only[, version := NULL]

  withr::local_options(datatable.print.nrows = Inf)

  message("Count of lints found on ", new_version, " but not on ", old_version, ": ", nrow(new_only))
  message("Count of these by linter:")
  print(new_only[, .N, by = linter][order(-N)])
  message("Sample of <=10 hits from each linter:")
  print(new_only[sample(.N), head(.SD, 10L), by = linter][, .(package, filename, linter, line)])

  message("Count of lints found on ", old_version, " but not on ", new_version, ": ", nrow(old_only))
  message("Count of these by linter:")
  print(old_only[, .N, by = linter][order(-N)])
  message("Sample of <=10 hits from each linter:")
  print(old_only[sample(.N), head(.SD, 10L), by = linter][, .(package, filename, linter, line)])
}

# ---- Main linting execution ----
out_dir <- setup(main)
for (repo in all_repos) clone_and_lint(repo)
cleanup(main)

out_dir <- setup(old_release)
for (repo in all_repos) clone_and_lint(repo)
cleanup(old_release)

# ---- Success comparison ----
main_results <- list.files(result_path(main), full.names = TRUE)
old_results <- list.files(result_path(old_release), full.names = TRUE)

main_only <- setdiff(basename(main_results), basename(old_results))
old_only <- setdiff(basename(old_results), basename(main_results))
shared <- intersect(basename(main_results), basename(old_results))

message("The following packages warned on ", main, " only: ")
message("  ", toString(match_and_strip(main_only, "\\.warnings$")), "\n")
message("The following packages warned on ", old_release, " only: ")
message("  ", toString(match_and_strip(old_only, "\\.warnings$")), "\n")
message("The following packages warned on both branches: ")
message("  ", toString(match_and_strip(shared, "\\.warnings$")), "\n")

main_only <- grep("warnings$", main_only, invert = TRUE, value = TRUE)
old_only <- grep("warnings$", old_only, invert = TRUE, value = TRUE)
shared <- grep("warnings$", shared, invert = TRUE, value = TRUE)

message("The following packages failed on ", main, " only: ")
message(summarize_failures(main, grep("\\.failure", main_only, value = TRUE)))
message("The following packages failed on ", old_release, " only: ")
message(summarize_failures(old_release, grep("\\.failure", old_only, value = TRUE)))
message("The following packages failed on both branches:\n  ", toString(match_and_strip(shared, "\\.failure")), "\n")

shared <- grep("csv$", shared, value = TRUE)

# ---- Lint output comparison ----
main_lints <- rbindlist(lapply(setNames(result_path(main, shared), shared), utils::read.csv), idcol = "package")
main_lints[, version := main]
main_lints[, package := gsub("\\.csv$", "", package)]
old_lints <- rbindlist(lapply(setNames(result_path(old_release, shared), shared), utils::read.csv), idcol = "package")
old_lints[, version := old_release]
old_lints[, package := gsub("\\.csv$", "", package)]

summarize_lint_delta(main_lints, old_lints)

# ---- Timing comparison ----
elapsed_main <- repo_data[[paste0("elapsed_", main)]]
elapsed_old <- repo_data[[paste0("elapsed_", old_release)]]

message("Comparison of total time to run lint_packages() across all repos:")
message(sprintf(
  "  %.0fm to run on %s, %.0fm to run on %s",
  sum(elapsed_main), main, sum(elapsed_old), old_release
))

repo_data[, delta := elapsed_main - elapsed_old]
repo_data[, delta_pct := 100 * delta / elapsed_old]
message("Comparison of time to run lint_package() on each repo (new - old; negative -> faster)")
repo_data[
  order(delta),
  { print(.SD); quantile(delta, 0:10/10) }
]

# ---- Prepare e-mails for maintainers ----
