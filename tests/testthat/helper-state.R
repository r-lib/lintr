testthat::set_state_inspector(function() {
  list(
    attached = search(),
    connections = nrow(showConnections()),
    cwd = getwd(),
    envvars = Sys.getenv(),
    libpaths = .libPaths(),
    locale = Sys.getlocale(),
    # sometimes a dependency might add  a custom option, so we need to
    # make sure we don't fail because of them
    options = options()[names(.Options)],
    packages = .packages(all.available = TRUE),
    NULL
  )
})
