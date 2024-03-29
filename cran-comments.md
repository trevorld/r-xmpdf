* System requirements of 'ghostscript' or 'pdftk' for editing pdf bookmarks and/or documentation info entries 
  and 'exiftool' for editing pdf documentation info entries and/or xmp metadata.
  The examples and tests shouldn't throw an ERROR if one (or all)
  of these are not installed.

## Test environments

* local (linux, R 4.3.3)
* win-builder (windows, R devel)
* github actions (windows, R release)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

1 NOTE generated on a subset of platforms:

```
found 3 marked UTF-8 strings 
```

The `spdx_license_list` data set contains details about 478 open source license
from the "SPDX License List" <https://spdx.org/licenses/>.
The "name" column of this data set includes the official name of the licenses.
Three of these official license names use non-ASCII characters.
