FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
read.dcf("DESCRIPTION", "Imports") |>
  tools:::.split_dependencies() |>
  names() |>
  # patrick also pulls in testthat and pkgload
  c("patrick") |>
  install.packages()
"
