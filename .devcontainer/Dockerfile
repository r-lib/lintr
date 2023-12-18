FROM rocker/r-base

RUN apt-get -qq update \
  && apt-get install -y --no-install-recommends \
  git libxml2-dev

COPY DESCRIPTION .

# NB: patrick also pulls in testthat and pkgload
# hard-copied from DESCRIPTION
RUN Rscript -e '     \
install.packages(c(  \
  "backports",       \
  "codetools",       \
  "cyclocomp",       \
  "digest",          \
  "glue",            \
  "knitr",           \
  "rex",             \
  "xml2",            \
  "xmlparsedata",    \
  "patrick",         \
  NULL               \
))                   \
'
