# read_config_file() bubbles up warnings helpfully, without erroring (#2253)

    Code
      lint_dir()
    Condition
      Warning:
      Warning encountered while loading config:
        Warning from config setting 'linters' in 'NULL':
          Depending on an R version older than 3.0.0 is not recommended. Resetting `r_version` to 3.0.0.

