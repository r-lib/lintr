FROM docker.io/rhub/r-minimal:devel

RUN apk update && \
  apk add --no-cache git gcc g++ libxml2 linux-headers musl-dev

COPY DESCRIPTION .

RUN Rscript -e '                                \
  options(repos="https://cloud.r-project.org"); \
  install.packages("remotes");                  \
  remotes::install_deps(dependencies = c(       \
    "Imports",                                  \
    "Config/needs/development"                  \
  ))                                            \
'
