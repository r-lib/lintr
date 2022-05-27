#!/usr/bin/Rscript
# Script to get URL keys from CRAN
#   of packags that Suggest or Import lintr
library(xml2)

cran_db = as.data.frame(available.packages())
lintr_re = "(^|\\s)lintr(,|\\s+\\(|$)"
lintr_pkg = cran_db[
  with(cran_db, grepl(lintr_re, Suggests) | grepl(lintr_re, Imports)),
  "Package"
]

# simple retry mechanism
url_fmt = "https://cran.r-project.org/web/packages/%s/index.html"
retry_read = function(pkg, initial_sleep = 1, retry_sleep = 60) {
  url = sprintf(url_fmt, pkg)
  retry = function(cond) {
    message("Sleepy... 😴")
    Sys.sleep(retry_sleep)
    xml2::read_html(url)
  }
  Sys.sleep(initial_sleep)
  tryCatch(xml2::read_html(url), error = retry)
}

urls = vector("list", length(lintr_pkg))
url_xpath = "string(//tr[td[starts-with(text(), 'URL')]]/td/a)"
# for loop makes debugging easier
for (ii in seq_along(urls)) {
  urls[[ii]] <- lintr_pkg[[ii]] %>%
    retry_read() %>%
    xml2::xml_find_chr(cran_pg, url_xpath)
}

urls = unlist(urls)
urls = grep("^https?://github", urls, value = TRUE)

writeLines(urls, "reverse-imports-urls")
