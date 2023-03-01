Add cran requirements

* All exported functions contain \value within documentation
* `on.exit(setwd(origdir), add = TRUE)` is called directly after `origdir <- getwd()` R/builder.R

## Test environments

* local macOS install, R 4.2.2
* R-hub (devel and release)
* win-builder

## R CMD check results

0 errors | 0 warnings | 2 note

* This is a new release.

Possibly misspelled words in DESCRIPTION:
  CodeBuild (3:21)

**Author Note:** CodeBuild is an Amazon Web Service, however it has been wrapped in single quotes as part of 'Amazon Web Service CodeBuild' within the DESCRIPTION.
