FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

RUN Rscript -e "
  install.packages('remotes')
  lintr_deps <- remotes::dev_package_deps()
  # patrick also pulls in testthat and pkgload
  install.packages(c(lintr_deps, 'patrick'))
"
