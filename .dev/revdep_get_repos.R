#!/usr/bin/env Rscript
# Script to get URL keys from CRAN
#   of packags that Suggest or Import lintr

cran_db <- as.data.frame(available.packages(repos = "https://cran.r-project.org"), stringsAsFactors = FALSE)
lintr_re <- "\\blintr\\b"
lintr_pkg <- cran_db[
  with(cran_db, grepl(lintr_re, Suggests) | grepl(lintr_re, Imports)),
  "Package"
]

# simple retry mechanism
desc_url_fmt <- "https://cran.r-project.org/web/packages/%s/DESCRIPTION"
extract_desc_fields <- function(pkg, keys, initial_sleep = 1, retry_sleep = 60) {
  desc_url <- sprintf(desc_url_fmt, pkg)
  retry <- function(cond) {
    message("Sleepy... 😴")
    Sys.sleep(retry_sleep)
    url(desc_url, open = "r")
  }
  Sys.sleep(initial_sleep)
  url_conn <- tryCatch(url(desc_url, open = "r"), error = retry, warning = retry)
  on.exit(close(url_conn))

  pkg_data <- read.dcf(url_conn, keys)
  # NB: NA --> "NA", which is fine -- no matches below
  toString(pkg_data)
}

extract_github_repo <- function(urls) {
  matches <- regexpr("https?://git(hub|lab).com/[a-zA-Z0-9_-]+/[a-zA-Z0-9._-]+", urls)
  matched <- matches > 0L

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
  urls[ii] <- extract_desc_fields(lintr_pkg[ii], c("URL", "BugReports"))
}

git_urls <- extract_github_repo(urls)
matched <- nzchar(git_urls)

utils::write.csv(
  data.frame(package = lintr_pkg[!matched], repo = file.path("https://github.com/cran", lintr_pkg[!matched])),
  "revdep-no-repos",
  row.names = FALSE, quote = FALSE
)
utils::write.csv(
  data.frame(package = lintr_pkg[matched], repo = git_urls[matched]),
  "revdep-repos",
  row.names = FALSE, quote = FALSE
)
