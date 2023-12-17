FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
  read.dcf('DESCRIPTION', 'Imports') |>
  strsplit(',[\\n ]*') |>
  unlist() |>
  gsub(pattern = '[\\n ]*[(].*', replacement = '') |>
  c('pkgload', 'testthat') |>
  install.packages()
"
