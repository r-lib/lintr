---
name: literate-r-formats
description: Guidelines and architecture for handling literate R documents (.Rmd, .qmd, .Rnw, .Rhtml) and source extraction in the r-lib/lintr package. Use this skill when modifying R/extract.R or working with non-R source files.
---

# Literate R Formats & Source Extraction in `r-lib/lintr`

When modifying how `lintr` extracts R source code from literate programming and multi-language documents (RMarkdown `.Rmd`, Quarto `.qmd`, Sweave `.Rnw`, or HTML `.Rhtml`), adhere to the following architectural conventions and extraction principles located primarily in `R/extract.R`.

## 1. Line Number Preservation via `NA_character_` Masking
- **Never drop or collapse lines during extraction:** `extract_r_source()` extracts R code from literate documents by masking non-R lines (markdown text, YAML frontmatter, HTML tags, and skipped code chunks) with `NA_character_`.
- **Preserve 1-to-1 line index mapping:** Keeping non-source lines as `NA_character_` ensures that line indices in the extracted character vector strictly match the 1-indexed line numbers of the original file (`source_expression$lines`). This guarantees that diagnostic line numbers (`line_number`) reported by linters map exactly to the user's source file.

## 2. Chunk Bounds & `eval=FALSE` Filtering
- **Two-phase chunk boundary detection:** `get_chunk_positions(pattern, lines)` determines chunk bounds by pairing opening pattern matches (`filter_chunk_start_positions()`) with closing pattern matches (`filter_chunk_end_positions()`), then retaining only blocks containing at least one line of inner code (`ends - starts > 1L`).
- **Filter non-evaluated chunks cleanly:** Filter out unevaluated chunks by inspecting both chunk header options and inside-chunk YAML/pipe options (`#| eval: false`).
- **Use exported `xfun` parsers:** Extract and parse chunk parameters using exported utilities from `xfun` (`xfun::csv_options()` for header strings and `xfun::divide_chunk("r", code)$options` for body lines) rather than invoking unexported `knitr` functions (`"knitr" %:::% "parse_params"`):
  ```r
  non_eval_chunk <- function(start, end, lines, pattern) {
    header <- lines[start]
    params_src <- trimws(gsub(pattern$chunk.begin, "\\1", header))
    header_params <- xfun::csv_options(params_src)

    code <- lines[(start + 1L):(end - 1L)]
    body_params <- xfun::divide_chunk("r", code)$options

    eval_value <- body_params$eval %||% header_params$eval
    # nolint next: T_and_F_symbol_linter.
    if (identical(eval_value, quote(F))) {
      return(TRUE)
    }
    isFALSE(eval_value)
  }
  ```

## 3. Engine Detection and Extension Handling
- **Detect non-R engines on opening lines:** `defines_knitr_engine()` inspects chunk start lines to drop chunks explicitly targeting non-R engines (`{python}`, `engine = "bash"`).
- **Use `xfun::file_ext()`:** When detecting file extensions to look up `knitr::all_patterns` (e.g. inside `get_knitr_pattern()`), use `tolower(xfun::file_ext(filename))` cleanly rather than `("knitr" %:::% "file_ext")(filename)`.

## 4. Multi-Format Testing Conventions
- **Test across document families:** When modifying `R/extract.R`, verify extraction behavior across all major literate families (`.Rmd`, `.qmd`, and `.Rnw`) inside `tests/testthat/test-knitr_formats.R`.
- **Test zero-chunk documents:** Always verify that documents containing zero chunks (such as plain markdown text or YAML header-only files) extract cleanly without throwing index out-of-bounds errors.
