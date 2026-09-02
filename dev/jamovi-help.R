# Install in jamovi
jmvtools::install()


# This function reformats and cleans up your DESCRIPTION file
usethis::use_tidy_description()


# load and document
devtools::load_all()
devtools::document()


# Release to github
usethis::use_github_release()
