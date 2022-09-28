# piecepackr.metadata

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/piecepackr.metadata)](https://cran.r-project.org/package=piecepackr.metadata)

[![R-CMD-check](https://github.com/piecepackr/piecepackr.metadata/workflows/R-CMD-check/badge.svg)](https://github.com/piecepackr/piecepackr.metadata/actions)

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Related Software](#similar)

## <a name="overview">Overview</a>

`{piecepackr.metadata}` provides functions for getting and setting pdf [documentation info](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) entries and [bookmarks](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#bookmarks-out).

In the future we plan to add support for getting/setting Extensible Metadata Platform (XMP) for a variety of media formats with a particular interest in setting Creative Commons license metadata.

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/bittermelon")
```

Depending on what you'd like to do you'll need to install some additional R packages and/or command-line tools:

* To get pdf documentation info entries you'll either need [{pdftools}](https://docs.ropensci.org/pdftools/) or the command-line tool [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* To set pdf documentation info entries you'll either need the command-line tool [ghostscript](https://www.ghostscript.com/) or the command-line tool [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* To get pdf bookmarks you'll need the command-line tool [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* To set pdf bookmarks you'll either need the command-line tool [ghostscript](https://www.ghostscript.com/) or the command-line tool [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* To get the number of pages in a pdf you'll either need [{qpdf}](https://cran.r-project.org/web/packages/qpdf/index.html), the command-line tool [ghostscript](https://www.ghostscript.com/), or the command-line tool [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)

## <a name="examples">Examples</a>

## <a name="similar">Related Software</a>

Note most of the R packages below are focused on **getting** metadata rather than **setting** metadata
and/or only provide low-level wrappers around the relevant command-line tools.
Please feel free to [open a pull request to add any missing relevant R packages](https://github.com/piecepackr/piecepackr.metadata/edit/main/README.Rmd).

### exiftool

* [{exifr}](https://github.com/paleolimbot/exifr)
* [{exiftoolr}](https://github.com/JoshOBrien/exiftoolr)
* [exiftool](https://exiftool.org/)

### other exif tools

* [{exif}](https://github.com/Ironholds/exif) reads EXIF from jpeg images
* [{exiv}](https://github.com/hrbrmstr/exiv) read and write ‘Exif’, ‘ID3v1’ and ‘ID3v2’ image/media tags
* [{magick}](https://github.com/ropensci/magick) has `image_attributes()` which reads EXIF image tags.

### ghostscript

* [{tools}] has `find_gs_cmd()` to find a GhostScript executable in a cross-platform way.
* [ghostscript](https://www.ghostscript.com/)

### poppler

* [{pdftools}](https://docs.ropensci.org/pdftools/)
* [{Rpoppler}](https://cran.r-project.org/web/packages/Rpoppler/index.html)
* [poppler](https://poppler.freedesktop.org/)

### qpdf

* [{qpdf}](https://cran.r-project.org/web/packages/qpdf/index.html)
* [qpdf](https://qpdf.sourceforge.io/)

### pdftk

* [{animation}](https://yihui.org/animation/) has `pdftk()`, a low-level wrapper around the `pdftk` command-line tool.
* [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* [pdftk-java](https://gitlab.com/pdftk-java/pdftk)

### tabula

* [{tabulizer}](https://github.com/ropensci/tabulizer)
* [tabula-java](https://github.com/tabulapdf/tabula-java/)