FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
  # patrick also pulls in testthat and pkgload
  install.packages(c('remotes', 'patrick'))
  remotes::install_deps()
"
