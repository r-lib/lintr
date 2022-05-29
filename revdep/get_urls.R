#!/usr/bin/Rscript
# Script to get URL keys from CRAN
#   of packags that Suggest or Import lintr

cran_db = as.data.frame(available.packages())
lintr_re = "\\blintr\\b"
lintr_pkg = cran_db[
  with(cran_db, grepl(lintr_re, Suggests) | grepl(lintr_re, Imports)),
  "Package"
]

# simple retry mechanism
desc_url_fmt = "https://cran.r-project.org/web/packages/%s/DESCRIPTION"
extract_url = function(pkg, initial_sleep = 1, retry_sleep = 60) {
  desc_url = sprintf(desc_url_fmt, pkg)
  retry = function(cond) {
    message("Sleepy... 😴")
    Sys.sleep(retry_sleep)
    url(desc_url, open = "r")
  }
  Sys.sleep(initial_sleep)
  url_conn = tryCatch(url(desc_url, open = "r"), error = retry, warning = retry)
  pkg_data = read.dcf(url_conn, "URL")
  drop(pkg_data)
}

extract_github_repo = function(urls) {
  matches = gregexpr("https?://git(hub|lab).com/[a-zA-Z0-9_-]+/[a-zA-Z0-9._-]+", urls)
  starts = sapply(matches, `[[`, 1L)
  matched = starts > 0L

  urls = urls[matched]
  starts = starts[matched]
  matches = matches[matched]

  unname(substr(urls, starts, starts + sapply(matches, attr, "match.length") - 1L))
}

urls = character(length(lintr_pkg))
# for loop makes debugging easier
for (ii in seq_along(urls)) {
  urls[ii] <- extract_url(lintr_pkg[ii])
}

urls = sort(extract_github_repo(urls[!is.na(urls)]))

writeLines(urls, "reverse-imports-urls")
