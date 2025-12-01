# GitLab Report for lint results

Generate a report of the linting results using the
[GitLab](https://docs.gitlab.com/ci/testing/code_quality/#code-quality-report-format)
format.

## Usage

``` r
gitlab_output(lints, filename = "lintr_results.json")
```

## Arguments

- lints:

  The linting results

- filename:

  The file name of the output report

## Details

lintr only supports three severity types ("style", "warning", and
"error") while the GitLab format supports five ("info", "minor",
"major", "critical", and "blocker"). The types "style", "warning", and
"error" are mapped to the GitLab types "info", "major", and "blocker",
respectively. The GitLab types "minor" and "critical" are ignored.
