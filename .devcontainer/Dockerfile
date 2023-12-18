FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
  install.packages('remotes')
  remotes::install_deps()
  # also pulls in testthat and pkgload
  install.packages('patrick')
"
