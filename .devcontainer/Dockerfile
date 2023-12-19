FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

COPY DESCRIPTION .

# NB: patrick also pulls in testthat and pkgload
# hard-copied from DESCRIPTION
RUN Rscript -e '                           \
  install.packages("remotes");             \
  remotes::install_deps(dependencies = c(  \
    "Depends",                             \
    "Imports",                             \
    "Config/needs/development"             \
  ))                                       \
'
