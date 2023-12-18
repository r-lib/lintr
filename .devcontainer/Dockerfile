FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

# NB: patrick also pulls in testthat and pkgload
RUN Rscript -e '\
read.dcf("DESCRIPTION", "Imports") |>\
  tools:::.split_dependencies() |>\
  names() |>\
  c("patrick") |>\
  install.packages()
'
