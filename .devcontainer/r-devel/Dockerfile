FROM docker.io/rhub/r-minimal:devel

# This minimal image deletes the default repo list, so set one here
#   for interactive R sessions to WAI.
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' \
  >> /usr/local/lib/R/etc/Rprofile.site

RUN apk update && \
  apk add --no-cache git gcc g++ libxml2-dev linux-headers musl-dev

COPY DESCRIPTION .

RUN Rscript -e '                                \
  install.packages("remotes");                  \
  remotes::install_deps(dependencies = c(       \
    "Imports",                                  \
    "Config/needs/development"                  \
  ))                                            \
'
