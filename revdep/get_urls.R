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

urls = vector("list", length(lintr_pkg))
url_fmt = "https://cran.r-project.org/web/packages/%s/index.html"
for (ii in seq_along(urls)) {
  # simple retry mechanism
  read_pkg = function() xml2::read_html(sprintf(url_fmt, lintr_pkg[ii]))
  retry_read = function(cond) {
    message("Sleepy... 😴")
    Sys.sleep(60)
    read_pkg()
  }
  cran_pg = tryCatch(read_pkg(), error = retry_read)
  urls[[ii]] <- xml2::xml_find_chr(cran_pg, "string(//tr[td[starts-with(text(), 'URL')]]/td/a)")
  Sys.sleep(1)
}

urls = unlist(urls)
urls = grep("^https?://github", urls, value = TRUE)

writeLines(urls, "reverse-imports-urls")
