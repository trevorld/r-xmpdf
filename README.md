# xmpdf <img src="man/figures/logo.png" align="right" width="200px" alt="xmpdf hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/xmpdf)](https://cran.r-project.org/package=xmpdf)

[![R-CMD-check](https://github.com/trevorld/r-xmpdf/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-xmpdf/actions)

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Limitations by backend](#comparison)
* [Related Software](#similar)

## <a name="overview">Overview</a>

`{xmpdf}` provides functions for getting and setting [Extensibe Metadata Platform (XMP)](https://en.wikipedia.org/wiki/Extensible_Metadata_Platform) metadata in a variety of media file formats as well as getting and setting PDF [documentation info](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) entries and [bookmarks](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#bookmarks-out) (aka outline aka table of contents).

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/r-xmpdf")
```

Depending on what you'd like to do you'll need to install some additional R packages and/or command-line tools:

* **[{qpdf}](https://cran.r-project.org/web/packages/qpdf/index.html)** can be used to get the number of pages in a pdf.  Note currently a dependency of [{pdftools}](https://docs.ropensci.org/pdftools/).

  + `install.packages("qpdf")`

* **[{pdftools}](https://docs.ropensci.org/pdftools/)** can be used to get documentation info entries.
  Note currently depends on [{qpdf}](https://cran.r-project.org/web/packages/qpdf/index.html).

  + `install.packages("pdftools")` will probably install `{qpdf}` as well

* **[exiftool](https://exiftool.org/)** can be used to get/set xmp metadata in a variety of media files.  Can also be used to get the number of pages in a pdf.  Note can be installed by [{exiftoolr}](https://github.com/JoshOBrien/exiftoolr).

  + `install.packages("exiftoolr"); exiftoolr::install_exiftool()` (Cross-Platform) 
  + `sudo apt-get install libimage-exiftool-perl` (Debian/Ubuntu)

* **[ghostscript](https://www.ghostscript.com/)** can be used to set pdf bookmarks and documentation info entries. Can also be used to get the number of pages in a pdf.

  + `sudo apt-get install ghostscript` (Debian/Ubuntu)

* **[pdftk-java](https://gitlab.com/pdftk-java/pdftk)** or perhaps **[pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)** can be used to get/set pdf bookmarks and documentation info entries.  Can also be used to get the number of pages in a pdf.

  + `sudo apt-get install pdftk-java` (Debian/Ubuntu)

## <a name="examples">Examples</a>

A simple example where we create a two page pdf using `pdf()` and then add XMP metadata, PDF documentation info metadata, and PDF bookmarks to it:


```r
library("grid")
library("xmpdf")

# Create a two page pdf
f <- tempfile(fileext = ".pdf")
pdf(f, onefile = TRUE)
grid.text("Page 1")
grid.newpage()
grid.text("Page 2")
invisible(dev.off())

# Edit XMP metadata
print(get_xmp(f)[[1]])
```

```
## no XMP metadata found
```

```r
x <- xmp(title = "Two Boring Pages", creator = "John Doe")
set_xmp(x, f)
print(get_xmp(f)[[1]])
```

```
## x:XMPToolkit : Image::ExifTool 12.40 
## dc:Creator : John Doe 
## dc:Title : Two Boring Pages
```

```r
# Edit PDF documentation info
print(get_docinfo(f)[[1]])
```

```
## Author: 
## CreationDate: 2022-09-30 16:06:05
## Creator: R
## Producer: R 4.2.1
## Title: R Graphics Output
## Subject: 
## Keywords: 
## ModDate: 2022-09-30 16:06:05
```

```r
d <- docinfo(author = "John Doe",
             title = "Two Boring Pages",
             keywords = c("R", "xmpdf"),
             filename = f)
set_docinfo(d, f)
print(get_docinfo(f)[[1]])
```

```
## Author: John Doe
## CreationDate: 2022-09-30 16:06:05
## Creator: R
## Producer: GPL Ghostscript 9.55.0
## Title: Two Boring Pages
## Subject: 
## Keywords: R, xmpdf
## ModDate: 2022-09-30 16:06:05
```

```r
# Edit PDF bookmarks
print(get_bookmarks(f)[[1]])
```

```
## [1] title page  level count color style
## <0 rows> (or 0-length row.names)
```

```r
bookmarks <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
set_bookmarks(bookmarks, f)
print(get_bookmarks(f)[[1]])
```

```
##    title page level count color style
## 1 Page 1    1     1    NA  <NA>    NA
## 2 Page 2    2     1    NA  <NA>    NA
```

```r
unlink(f)
```

## <a name="comparison">Limitations by backend</a>

`{xmpdf}` Feature | `exiftool` | `pdftk` | `ghostscript`
---|---|---|---|
Get XMP metadata | Yes | No | No
Set XMP metadata | Yes | No | Poor: when documentation info metadata is set then as a side effect it seems will also be set as XMP metadata
Get PDF bookmarks | No | Okay: can only get Title, Page number, and Level | No
Set PDF bookmarks | No | Okay: can only set Title, Page number, and Level | Good: supports most bookmarks features including color and style but only action supported is to view a particular page.
Get PDF documentation info | Yes | Yes | No
Set PDF documentation info | Yes | Yes | Yes: as a side effect when documentation info metadata is set then it seems will also be set as XMP metadata
Unicode support | Yes | Yes | No

Known limitations:

* `get_bookmarks_pdftk()` doesn't report information about bookmarks color, style, and whether the bookmarks
  should start open or closed.
* `set_bookmarks_gs()` supports most bookmarks features including color and style but only action supported is to view a particular page and doesn't work with Unicode titles.
* `set_bookmarks_pdftk()` only supports setting the title, page number, and level of bookmarks.
* All of the `set_docinfo()` methods currently do not support arbitrary info dictionary entries.
* `set_docinfo_gs()` doesn't work with Unicode info dictionary values.
* As a side effect `set_docinfo_gs()` seems to also update in previously set matching XPN metadata
  while `set_docinfo_exiftool()` and `set_docinfo_pdftk()` don't update
  any previously set matching XPN metadata.
  Some pdf viewers will preferentially use the previously set document title from XPN metadata
  if it exists instead of using the title set in documentation info dictionary entry.
  Consider also manually setting this XPN metadata using `set_xmp()`.
* Old metadata information is usually not deleted from the files by these 
  operations (i.e. these operations are often theoretically reversible).
  If deleting the old metadata is important one may want to consider calling
  `qpdf::pdf_compress(input, linearize = TRUE)` at the end.

## <a name="similar">Related Software</a>

Note most of the R packages below are focused on **getting** metadata rather than **setting** metadata
and/or only provide low-level wrappers around the relevant command-line tools.
Please feel free to [open a pull request to add any missing relevant R packages](https://github.com/trevorld/r-xmpdf/edit/main/README.Rmd).

### exiftool

* [{exifr}](https://github.com/paleolimbot/exifr) 
  provides a high-level wrapper to read metadata as well as a low-level wrapper around the `exiftool` command-line tool.
  Can download `exiftool`.
* [{exiftoolr}](https://github.com/JoshOBrien/exiftoolr) 
  provides high-level wrapper to read metadata as well as a low-level wrapper around the `exiftool` command-line tool.
  Can download `exiftool`.
* [exiftool](https://exiftool.org/)

### other exif tools

* [{exif}](https://github.com/Ironholds/exif) reads EXIF from jpeg images
* [{exiv}](https://github.com/hrbrmstr/exiv) read and write ‘Exif’, ‘ID3v1’ and ‘ID3v2’ image/media tags
* [{magick}](https://github.com/ropensci/magick) has `image_attributes()` which reads EXIF image tags.

### ghostscript

* `{tools}` has `find_gs_cmd()` to find a GhostScript executable in a cross-platform way.
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
