FROM rocker/r-base

RUN apt-get -qq update && \
  apt-get install -y --no-install-recommends git libxml2-dev

COPY DESCRIPTION .

RUN Rscript -e '                           \
  install.packages("remotes");             \
  remotes::install_deps(dependencies = c(  \
    "Imports",                             \
    "Config/needs/development"             \
  ))                                       \
'
