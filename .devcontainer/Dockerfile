FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
  read.dcf('DESCRIPTION', 'Imports') |>
  strsplit(',[\\n ]*') |>
  unlist() |>
  gsub(pattern = '[\\n ]*[(].*', replacement = '') |>
  setdiff(tools:::.get_standard_package_names()$base) |>
  c('pkgload', 'testthat', 'patrick') |>
  install.packages()
"
