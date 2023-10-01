* Unit tests have been adjusted to prevent an CRAN R CMD check ERROR on
  an M1 Mac machine running macOS 14.
* System requirements of 'ghostscript' or 'pdftk' for editing pdf bookmarks and/or documentation info entries 
  and 'exiftool' for editing pdf documentation info entries and/or xmp metadata.
  The examples and tests shouldn't not throw an ERROR if one (or all)
  of these are not installed.

## Test environments

* local (linux, R 4.3.1)
* win-builder (windows, R devel)
* github actions (windows, R release)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

* OK
