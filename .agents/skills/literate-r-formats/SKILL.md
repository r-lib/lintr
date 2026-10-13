---
name: literate-r-formats
description: >-
  Guidelines and architecture for handling literate R documents (`.Rmd`, `.qmd`,
  `.Rnw`, `.Rhtml`, `.Rtex`, `.Rrst`, `.Rtxt`) and source extraction in the
  r-lib/lintr package. Use when modifying `R/extract.R`,
  `R/get_source_expressions.R`, or `tests/testthat/test-knitr_formats.R`. Don't
  use for standard `.R` file linters that do not interact with chunk extraction.
---

# Literate R Formats & Source Extraction in `r-lib/lintr`

When modifying how `lintr` extracts R source code from literate programming and
multi-language documents (RMarkdown `.Rmd`, Quarto `.qmd`, Sweave `.Rnw`, HTML
`.Rhtml`, LaTeX `.Rtex`, reST `.Rrst`, or AsciiDoc `.Rtxt`), adhere to the
following architectural conventions in `R/extract.R`:

## 1. Line & Column Preservation via `NA_character_` & Prefix Masking

- **Never drop or collapse lines during extraction**: `extract_r_source()`
  extracts R code from literate documents by masking non-R lines (markdown text,
  YAML frontmatter, HTML tags, and skipped code chunks) with `NA_character_`.
- **Preserve 1-to-1 line index mapping**: Keeping non-source lines as
  `NA_character_` ensures that line indices in the extracted character vector
  strictly match the 1-indexed line numbers of the original file
  (`source_expression$lines`). This guarantees that diagnostic `line_number`
  values reported by linters map 1-to-1 to the user's source file.
- **Prefix and indentation masking (`replace_prefix()`)**: For formats with
  in-chunk line prefixes (`pattern$chunk.code`, such as `%` comment prefixes in
  `.Rtex`) or indented code chunks (`chunks[["indents"]] > 0L`), replace prefix
  characters or strip uniform chunk indentation carefully so column offsets and
  `indentation_linter()` diagnostics remain accurate.

## 2. Chunk Bounds & `eval=FALSE` Filtering

- **Two-phase chunk boundary detection**: `get_chunk_positions(pattern, lines)`
  determines chunk bounds by pairing opening pattern matches
  (`filter_chunk_start_positions()`) with closing pattern matches
  (`filter_chunk_end_positions()`), handling back-to-back Sweave chunks (`<<>>=`
  without an intervening `@`) and retaining blocks containing inner code
  (`ends - starts > 1L`).
- **Filter non-evaluated chunks**: Filter out unevaluated chunks by inspecting
  both chunk header options and inside-chunk YAML/pipe options (`#| eval: false`,
  `#| engine: python`):
  ```r
  is_eval_chunk <- function(start, end, lines, pattern) {
    header <- lines[start]
    params_src <- trimws(gsub(pattern$chunk.begin, "\\1", header))
    header_params <- safe_csv_options(params_src)

    code <- lines[(start + 1L):(end - 1L)]
    body_params <- tryCatch(
      suppressMessages(suppressWarnings(xfun::divide_chunk("r", code)))$options,
      error = \(e) NULL
    )

    engine <- body_params$engine %||% header_params$engine
    if (!is.null(engine) && tolower(as.character(engine[1L])) != "r") {
      return(FALSE)
    }

    eval_value <- body_params$eval %||% header_params$eval
    # nolint next: T_and_F_symbol_linter.
    if (identical(eval_value, quote(F))) {
      return(FALSE)
    }
    !isFALSE(eval_value)
  }
  ```

## 3. Engine Detection

- **Detect R engines on opening lines**: `is_r_chunk_header()` inspects chunk
  start lines to retain chunks targeting R engines (`{r}`, `{R}`, `engine = "R"`)
  while skipping non-R engines (`{python}`, `{extendr}`, `{ojs}`, `{mermaid}`,
  `{dot}`, `engine = "bash"`).

## 4. Multi-Format Testing Conventions (`tests/testthat/test-knitr_formats.R`)

- **Test across document families**: When modifying `R/extract.R`, verify
  extraction behavior across major literate families (`.Rmd`, `.qmd`, `.Rnw`,
  `.Rtex`) inside `tests/testthat/test-knitr_formats.R`.
- **Test zero-chunk documents**: Always verify that documents containing zero
  chunks (such as plain markdown text or YAML header-only files) extract cleanly
  without throwing index out-of-bounds errors.
- **Coarse parse-error assertions**: When asserting document/chunk-level parse
  errors on malformed literate files, assert `message` and `line_number` without
  over-specifying a trivial `column_number = 1L`.
