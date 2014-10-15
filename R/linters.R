add_linter("absolute.paths", linter(

    apply = function(content, ...) {
      content <- strip_comments(content)
      which(has_absolute_paths(content))
    },

    takes = is_r_code_file,

    message = function(content, lines) {
      make_linter_message("The following lines contain absolute paths",
                        content,
                        lines)
    },

    suggestion = "Paths should be to files within the project directory."

))

add_linter("invalid.relative.paths", linter(

  apply = function(content, ...) {
    content <- strip_comments(content)
    bad_relative_paths(content, ...)
  },

  takes = is_r_code_file,

  message = function(content, lines) {
    make_linter_message("The following lines contain invalid relative paths (resolved outside of project directory)",
                      content,
                      lines)
  },

  suggestion = "Paths should be to files within the project directory."

))
