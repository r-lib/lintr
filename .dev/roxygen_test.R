# Test to ensure roxygenize() has been run on the current PR
library(tools)
library(roxygen2)

old_dir <- file.path(tempdir(), "man")
if (dir.exists(old_dir)) unlink(old_dir, recursive = TRUE)
file.copy("man", tempdir(), recursive = TRUE)
new_dir <- "man"
.Last <- function() unlink(old_dir, recursive = TRUE)

roxygenize()

old_files <- list.files(old_dir, pattern = "\\.Rd$")
new_files <- list.files(new_dir, pattern = "\\.Rd$")

old_not_new <- setdiff(old_files, new_files)
if (length(old_not_new) > 0L) {
  stop("Found saved .Rd files gone from a fresh run of roxygenize(): ", toString(old_not_new))
}

new_not_old <- setdiff(new_files, old_files)
if (length(new_not_old) > 0L) {
  stop("Found new .Rd files from a fresh run of roxygenize(): ", toString(new_not_old))
}

# Rd2txt() prints to its out= argument, so we'd have to compare file contents;
#   plain parse_Rd() keeps srcref info that encodes the file path, which as.character() strips.
comparable_rd <- function(rd_file) as.character(parse_Rd(rd_file))

rd_equal <- function(f1, f2) isTRUE(all.equal(comparable_rd(f1), comparable_rd(f2)))

for (file in new_files) {
  if (rd_equal(file.path(old_dir, file), file.path(new_dir, file))) {
    next
  }
  stop("roxygenize() output differs from saved output for ", file, ".")
}

unlink(old_dir, recursive = TRUE)
