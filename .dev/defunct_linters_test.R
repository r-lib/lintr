defunct_linters <- subset(
  read.csv("inst/lintr/linters.csv"),
  grepl("\\bdefunct\\b", tags),
  "linter"
)

pkgload::load_all()
found_idx <- defunct_linters %in% getNamespaceExports("lintr")
if (!all(found_idx)) {
  stop(
    "Missing 'defunct'-tagged linters: ", toString(defunct_linters[!found_idx]), "."
  )
}
