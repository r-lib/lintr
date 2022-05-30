#!/usr/bin/Rscript
# Script to get URL keys from CRAN
#   of packags that Suggest or Import lintr

cran_db <- as.data.frame(available.packages(), stringsAsFactors = FALSE)
lintr_re <- "\\blintr\\b"
lintr_pkg <- cran_db[
  with(cran_db, grepl(lintr_re, Suggests) | grepl(lintr_re, Imports)),
  "Package"
]

# simple retry mechanism
desc_url_fmt <- "https://cran.r-project.org/web/packages/%s/DESCRIPTION"
extract_url <- function(pkg, initial_sleep = 1, retry_sleep = 60) {
  desc_url <- sprintf(desc_url_fmt, pkg)
  retry <- function(cond) {
    message("Sleepy... 😴")
    Sys.sleep(retry_sleep)
    url(desc_url, open = "r")
  }
  Sys.sleep(initial_sleep)
  url_conn <- tryCatch(url(desc_url, open = "r"), error = retry, warning = retry)
  pkg_data <- read.dcf(url_conn, "URL")
  drop(pkg_data)
}

extract_github_repo <- function(urls) {
  matches <- regexpr("https?://git(hub|lab).com/[a-zA-Z0-9_-]+/[a-zA-Z0-9._-]+", urls)
  matched <- !is.na(matches) & matches > 0L

  out <- character(length(urls))
  out[matched] <- substr(
    urls[matched],
    matches[matched],
    matches[matched] + attr(matches, "match.length")[matched] - 1L
  )
  out
}

urls <- character(length(lintr_pkg))
# for loop makes debugging easier
for (ii in seq_along(urls)) {
  urls[ii] <- extract_url(lintr_pkg[ii])
}

git_urls <- extract_github_repo(urls)
matched <- nzchar(git_urls)

writeLines(sort(lintr_pkg[!matched]), "reverse-imports-no-repos")
writeLines(sort(git_urls[matched]), "reverse-imports-repos")
