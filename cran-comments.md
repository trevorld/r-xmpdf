* Unit tests have been adjusted to prevent CRAN R CMD check ERRORs on certain CRAN platforms
  that don't have `ghostscript` installed or are ran in a non-Unicode locale.
* System requirements of 'ghostscript' or 'pdftk' for editing pdf bookmarks and/or documentation info entries 
  and 'exiftool' for editing pdf documentation info entries and/or xmp metadata.
  The examples and tests shouldn't not throw an ERROR if one (or all)
  of these are not installed.

## Test environments

* local (linux, R 4.2.2)
* win-builder (windows, R devel)
* github actions (windows, R release)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

* OK
