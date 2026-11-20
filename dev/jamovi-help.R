# Prepare and document
jmvtools::prepare()
devtools::document()

# Install in jamovi or R
jmvtools::install()
devtools::install()

# Code style
system2("air", c("format", "R", "tests"))
lintr::lint_package()

# Tests, coverage, and checks
devtools::test()
devtools::test_coverage()
devtools::test_coverage(report = "zero")
devtools::check()

# Review, accept, or reject snapshot changes
testthat::snapshot_review()
# testthat::snapshot_accept()
# testthat::snapshot_reject()

# Tidy DESCRIPTION
usethis::use_tidy_description()

# Release to GitHub
# usethis::use_github_release()
