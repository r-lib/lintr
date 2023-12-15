in_ci <- function() {
  in_travis() || in_wercker() || in_jenkins()
}

ci_type <- function() {
  if (in_travis()) {
    return("travis")
  }
  if (in_wercker()) {
    return("wercker")
  }
  if (in_jenkins()) {
    return("jenkins")
  }
  ""
}

in_jenkins <- function() {
  nzchar(Sys.getenv("JENKINS_URL")) && !is.null(jenkins_build_info())
}

jenkins_build_info <- function() {
  git_url <- Sys.getenv("GIT_URL", Sys.getenv("GIT_URL_1", NA))
  if (is.na(git_url)) {
    return(NULL)
  }

  pattern <- "(https?:\\/\\/|git@)github\\.com[:\\/](.+\\/.+)\\.git"
  if (!length(grep(pattern, git_url))) {
    return(NULL)
  }
  slug <- gsub(pattern, "\\2", git_url)

  slug_info <- strsplit(slug, "/", fixed = TRUE)[[1L]]

  list(
    user = slug_info[1L],
    repo = slug_info[2L],
    pull = Sys.getenv("CHANGE_ID", NA) %||% NULL,
    commit = Sys.getenv("GIT_COMMIT", NA) %||% NULL
  )
}

in_travis <- function() {
  nzchar(Sys.getenv("TRAVIS_REPO_SLUG"))
}

travis_build_info <- function() {
  slug <- Sys.getenv("TRAVIS_REPO_SLUG")
  slug_info <- strsplit(slug, "/", fixed = TRUE)[[1L]]

  list(
    user = slug_info[1L] %||% "",
    repo = slug_info[2L] %||% "",
    pull = Sys.getenv("TRAVIS_PULL_REQUEST"),
    branch = Sys.getenv("TRAVIS_BRANCH"),
    commit = Sys.getenv("TRAVIS_COMMIT")
  )
}

in_wercker <- function() {
  nzchar(Sys.getenv("WERCKER_GIT_BRANCH"))
}

ci_build_info <- function() {
  type <- ci_type()
  switch(
    type,
    travis = travis_build_info(),
    wercker = wercker_build_info(),
    jenkins = jenkins_build_info()
  )
}

wercker_build_info <- function() {
  list(
    user = Sys.getenv("WERCKER_GIT_OWNER"),
    repo = Sys.getenv("WERCKER_GIT_REPOSITORY"),
    branch = Sys.getenv("WERCKER_GIT_BRANCH"),
    commit = Sys.getenv("WERCKER_GIT_COMMIT")
  )
}

# nocov start
github_comment <- function(text, info = NULL, token = settings$comment_token) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required to post comments with github_comment().", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to post comments with github_comment().", call. = FALSE)
  }

  if (is.null(info)) {
    info <- ci_build_info()
  }

  if (!is.null(info$pull) && info$pull != "false") {
    api_subdir <- file.path("issues", info$pull)
  } else if (!is.null(info$commit)) {
    api_subdir <- file.path("commits", info$commit)
  } else {
    stop("Expected a pull or a commit, but received ci_build_info() = ", format(info), call. = FALSE)
  }
  req <- httr2::request("https://api.github.com")
  req <- httr2::req_url_path_append(req, file.path("repos", info$user, info$repo, api_subdir, "comments"))
  req <- httr2::req_body_json(jsonlite::unbox(text))
  req <- httr2::req_url_query(access_token = token)
  httr2::req_perform(req)
}
# nocov end
