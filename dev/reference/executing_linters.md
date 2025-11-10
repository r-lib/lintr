# Code executing linters

Linters that evaluate parts of the linted code, such as loading
referenced packages. These linters should not be used with untrusted
code, and may need dependencies of the linted package or project to be
available in order to function correctly. For package authors, note that
this includes loading the package itself, e.g. with
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
or installing and attaching the package.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Linters

The following linters are tagged with 'executing':

- [`namespace_linter`](https://lintr.r-lib.org/dev/reference/namespace_linter.md)

- [`object_length_linter`](https://lintr.r-lib.org/dev/reference/object_length_linter.md)

- [`object_name_linter`](https://lintr.r-lib.org/dev/reference/object_name_linter.md)

- [`object_overwrite_linter`](https://lintr.r-lib.org/dev/reference/object_overwrite_linter.md)

- [`object_usage_linter`](https://lintr.r-lib.org/dev/reference/object_usage_linter.md)

- [`unused_import_linter`](https://lintr.r-lib.org/dev/reference/unused_import_linter.md)
