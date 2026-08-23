* System requirements of 'ghostscript' or 'pdftk' for editing pdf bookmarks and/or documentation info entries
  and 'exiftool' for editing pdf documentation info entries and/or xmp metadata.
  The examples and tests shouldn't throw an ERROR if one (or all)
  of these are not installed.

## Test environments

* local (linux, R 4.6.1)
* win-builder (windows, R devel)
* github actions (windows, R release)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

OK

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

