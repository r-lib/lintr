FROM rocker/r-devel

RUN apt-get -qq update && \
  apt-get install -y --no-install-recommends git libxml2-dev

COPY DESCRIPTION .

RUN Rscript -e '                                \
  options(repos="https://cloud.r-project.org"); \
  remotes::install_deps(dependencies = c(       \
    "Imports",                                  \
    "Config/needs/development"                  \
  ))                                            \
'
