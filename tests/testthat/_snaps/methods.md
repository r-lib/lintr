# print.lint works for inline data, even in RStudio

    Code
      options("lintr.rstudio_source_markers")
    Output
      $lintr.rstudio_source_markers
      [1] FALSE
      
    Code
      print(l)
    Output
      <text>:1:3: style: [assignment_linter] Use <-, not =, for assignment.
      x = 1
        ^

---

    Code
      options("lintr.rstudio_source_markers")
    Output
      $lintr.rstudio_source_markers
      [1] TRUE
      
    Code
      print(l)
    Output
      <text>:1:3: style: [assignment_linter] Use <-, not =, for assignment.
      x = 1
        ^

