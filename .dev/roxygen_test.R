# Test to ensure roxygenize() has been run on the current PR
library(tools)
library(roxygen2)

old_dir <- file.path(tempdir(), "man")
if (dir.exists(old_dir)) unlink(old_dir, recursive = TRUE)
stopifnot(file.copy("man", tempdir(), recursive = TRUE))
old_files <- list.files(old_dir, pattern = "\\.Rd$")
new_dir <- "man"

.Last <- function() {
  if (!dir.exists(old_dir)) {
    return(invisible())
  }
  unlink(new_dir, recursive = TRUE)
  file.copy(old_dir, ".", recursive = TRUE)
  unlink(old_dir, recursive = TRUE)
}

# Rd2txt() prints to its out= argument, so we'd have to compare file contents;
#   plain parse_Rd() keeps srcref info that encodes the file path, which as.character() strips.
normalize_rd <- function(rd_file) as.character(parse_Rd(rd_file))

rd_equal <- function(f1, f2) isTRUE(all.equal(normalize_rd(f1), normalize_rd(f2)))

check_roxygenize_idempotent <- function(LOCALE) {
  set_locale_res <- tryCatch(Sys.setlocale("LC_COLLATE", LOCALE), warning = identity, error = identity)
  if (inherits(set_locale_res, "condition")) {
    message(sprintf(
      "Skipping LOCALE=%s because it is not supported: %s",
      LOCALE, conditionMessage(set_locale_res)
    ))
    return(TRUE)
  }

  # Ensure man/ is in its original state before running roxygenize()
  unlink(new_dir, recursive = TRUE)
  stopifnot(file.copy(old_dir, ".", recursive = TRUE))

  suppressMessages(roxygenize()) # 'loading lintr'
  
  new_files <- list.files(new_dir, pattern = "\\.Rd$")
  
  any_failed <- FALSE

  old_not_new <- setdiff(old_files, new_files)
  if (length(old_not_new) > 0L) {
    cat(sprintf(
      "Found saved .Rd files gone from a fresh run of roxygenize() in LOCALE=%s: %s\n",
      LOCALE, toString(old_not_new)
    ))
    any_failed <- TRUE
  }
  
  new_not_old <- setdiff(new_files, old_files)
  if (length(new_not_old) > 0L) {
    cat(sprintf(
      "Found new .Rd files from a fresh run of roxygenize() in LOCALE=%s: %s\n",
      LOCALE, toString(new_not_old)
    ))
    any_failed <- TRUE
  }
    
  for (file in new_files) {
    old_file <- file.path(old_dir, file)
    new_file <- file.path(new_dir, file)
    if (rd_equal(old_file, new_file)) {
      next
    }
    cat(sprintf("roxygenize() output differs from saved output for %s in LOCALE=%s.\n", file, LOCALE))
    cat("Here's the 'diff' comparison of the two files:\n")
    cat("  [---]: saved output in man/ directory\n")
    cat("  [+++]: roxygenize() output of R/ sources\n")
    system2("diff", c("--unified", old_file, new_file))
    any_failed <- TRUE
  }

  !any_failed
}

# Run the check in a few locales to ensure there's no idempotency issues w.r.t. sorting, too
all_locales_passed <- TRUE
for (LOCALE in c("C", "en_US.utf8", "hu_HU.utf8", "ja_JP.utf8")) {
  all_locales_passed <- check_roxygenize_idempotent(LOCALE) && all_locales_passed
}

if (!all_locales_passed) {
  message("roxygenize() check failed.")
  q(status = 1)
}
