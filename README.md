# xmpdf <img src="man/figures/logo.png" align="right" width="200px" alt="xmpdf hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/xmpdf)](https://cran.r-project.org/package=xmpdf)
[![R-CMD-check](https://github.com/trevorld/r-xmpdf/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-xmpdf/actions)
[![codecov](https://codecov.io/github/trevorld/r-xmpdf/branch/main/graph/badge.svg?token=K12GJIBIL7)](https://codecov.io/github/trevorld/r-xmpdf)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Add XMP/docinfo metadata and bookmarks to a pdf](#pdfcreate)
  + [Add Google Images and Creative Commons license XMP metadata to an image](#pnglicense)

* [Limitations by backend](#comparison)
* [External links](#links)

  + [Metadata links](#standards)
  + [Related software](#similar)

## <a name="overview">Overview</a>

`{xmpdf}` provides functions for getting and setting [Extensibe Metadata Platform (XMP)](https://en.wikipedia.org/wiki/Extensible_Metadata_Platform) metadata in a variety of media file formats as well as getting and setting PDF [documentation info](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo) entries and [bookmarks](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#bookmarks-out) (aka outline aka table of contents).

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/r-xmpdf")
```

Depending on what you'd like to do you'll need to install some additional R packages and/or command-line tools:

* **[{qpdf}](https://cran.r-project.org/package=qpdf)** can be used to concatenate pdf files together as well as get the number of pages in a pdf.
  Note currently a dependency of [{pdftools}](https://docs.ropensci.org/pdftools/).

  + `install.packages("qpdf")`

* **[{pdftools}](https://docs.ropensci.org/pdftools/)** can be used to get documentation info entries in pdf files.
  Note currently depends on [{qpdf}](https://cran.r-project.org/web/packages/qpdf/index.html).

  + `install.packages("pdftools")` will probably install `{qpdf}` as well

* **[exiftool](https://exiftool.org/)** can be used to get/set xmp metadata in a variety of media files as well as documentation info entries in pdf files.  Can also be used to get the number of pages in a pdf.  Note can be installed by [{exiftoolr}](https://github.com/JoshOBrien/exiftoolr).

  + `install.packages("exiftoolr"); exiftoolr::install_exiftool()` (Cross-Platform) 
  + `sudo apt-get install libimage-exiftool-perl` (Debian/Ubuntu)
  + `brew install exiftool` (Homebrew)
  + `choco install exiftool` (Chocolately)

* **[ghostscript](https://www.ghostscript.com/)** can be used to set bookmarks and documentation info entries in pdf files. 
  Can also be used to concatenate pdf files together as well as get the number of pages in a pdf.

  + `sudo apt-get install ghostscript` (Debian/Ubuntu)
  + `brew install ghostscript` (Homebrew)
  + `choco install ghostscript` (Chocolately)

* **[pdftk-java](https://gitlab.com/pdftk-java/pdftk)** or perhaps **[pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)** can be used to get/set bookmarks and documentation info entries in pdf files.  
  Can also be used to concatenate pdf files together as well as get the number of pages in a pdf.

  + `sudo apt-get install pdftk-java` (Debian/Ubuntu)
  + `brew install pdftk-java` (Homebrew)
  + `choco install pdftk-java` (Chocolately)

## <a name="examples">Examples</a>

### <a name="pdfcreate">Add XMP/docinfo metadata and bookmarks to a pdf</a>

A simple example where we create a two page pdf using `pdf()` and then add XMP metadata, PDF documentation info metadata, and PDF bookmarks to it:


```r
library("xmpdf")

# Create a two page pdf using `pdf()`
f <- tempfile(fileext = ".pdf")
pdf(f, onefile = TRUE)
grid::grid.text("Page 1")
grid::grid.newpage()
grid::grid.text("Page 2")
invisible(dev.off())

# See what default metadata `pdf()` created
get_docinfo(f)[[1]] |> print()
```

```
## Author: NULL
## CreationDate: 2023-02-03T14:26:40
## Creator: R
## Producer: R 4.2.2
## Title: R Graphics Output
## Subject: NULL
## Keywords: NULL
## ModDate: 2023-02-03T14:26:40
```

```r
get_xmp(f)[[1]] |> print()
```

```
## No XMP metadata found
```

```r
get_bookmarks(f)[[1]] |> print()
```

```
## [1] title    page     level    count    open     color    fontface
## <0 rows> (or 0-length row.names)
```

```r
# Edit PDF documentation info
d <- get_docinfo(f)[[1]] |>
       update(author = "John Doe",
              title = "Two Boring Pages",
              keywords = c("R", "xmpdf"))
set_docinfo(d, f)
get_docinfo(f)[[1]] |> print()
```

```
## Author: John Doe
## CreationDate: 2023-02-03T14:26:40
## Creator: R
## Producer: GPL Ghostscript 9.55.0
## Title: Two Boring Pages
## Subject: NULL
## Keywords: R, xmpdf
## ModDate: 2023-02-03T14:26:40
```

```r
# Edit XMP metadata
x <- as_xmp(d) |>
       update(attribution_url = "https://example.com/attribution",
              date_created = Sys.Date(),
              spdx_id = "CC-BY-4.0")
set_xmp(x, f)
get_xmp(f)[[1]] |> print()
```

```
##    cc:attributionName := John Doe
##    cc:attributionURL := https://example.com/attribution
##    cc:license := https://creativecommons.org/licenses/by/4.0/
##    dc:creator := John Doe
##    dc:rights := © 2023 John Doe. Some rights reserved.
##    dc:title := Two Boring Pages
##    pdf:Keywords := R, xmpdf
##    pdf:Producer := R 4.2.2
##    photoshop:Credit := John Doe
##    photoshop:DateCreated := 2023-02-03
##    xmp:CreateDate := 2023-02-03T14:26:40
##    xmp:CreatorTool := R
##    xmp:ModifyDate := 2023-02-03T14:26:40
##    xmpRights:Marked := TRUE
##    xmpRights:UsageTerms := This work is licensed to the public under the Creative Commons
##         Attribution 4.0 International license
##         https://creativecommons.org/licenses/by/4.0/
##    xmpRights:WebStatement := https://creativecommons.org/licenses/by/4.0/
```

```r
# Edit PDF bookmarks
bm <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
set_bookmarks(bm, f)
get_bookmarks(f)[[1]] |> print()
```

```
##    title page level count open color fontface
## 1 Page 1    1     1    NA   NA  <NA>     <NA>
## 2 Page 2    2     1    NA   NA  <NA>     <NA>
```

```r
unlink(f)
```

### <a name="pnglicense">Add Google Images and Creative Commons license XMP metadata to an image</a>

Besides pdf files with `exiftool` we can also edit the XMP metadata for [a large number of image formats](https://exiftool.org/#supported)
including "gif", "png", "jpeg", "tiff", and "webp".  In particular we may be interested in setting the subset of [IPTC Photo XMP metadata displayed by Google Images](https://iptc.org/standards/photo-metadata/quick-guide-to-iptc-photo-metadata-and-google-images/) as well as embedding [Creative Commons license XMP metadata](https://wiki.creativecommons.org/wiki/XMP).


```r
library("xmpdf")
f <- tempfile(fileext = ".png")
png(f)
grid::grid.text("This is an image!")
dev.off() |> invisible()

get_xmp(f)[[1]] |> print()
```

```
## No XMP metadata found
```

```r
x <- xmp(attribution_url = "https://example.com/attribution",
         creator = "John Doe",
         description = "An image caption",
         date_created = Sys.Date(),
         spdx_id = "CC-BY-4.0")
print(x, mode = "google_images", xmp_only = TRUE)
```

```
##    dc:creator := John Doe
## => dc:rights = © 2023 John Doe. Some rights reserved.
## => photoshop:Credit = John Doe
## X  plus:Licensor (not currently supported by {xmpdf})
## => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/
```

```r
print(x, mode = "creative_commons", xmp_only = TRUE)
```

```
## => cc:attributionName = John Doe
##    cc:attributionURL := https://example.com/attribution
## => cc:license = https://creativecommons.org/licenses/by/4.0/
##    cc:morePermissions := NULL
## => dc:rights = © 2023 John Doe. Some rights reserved.
## => xmpRights:Marked = TRUE
## => xmpRights:UsageTerms = This work is licensed to the public under the Creative Commons
##         Attribution 4.0 International license
##         https://creativecommons.org/licenses/by/4.0/
## => xmpRights:WebStatement = https://creativecommons.org/licenses/by/4.0/
```

```r
set_xmp(x, f)
get_xmp(f)[[1]] |> print()
```

```
##    cc:attributionName := John Doe
##    cc:attributionURL := https://example.com/attribution
##    cc:license := https://creativecommons.org/licenses/by/4.0/
##    dc:creator := John Doe
##    dc:description := An image caption
##    dc:rights := © 2023 John Doe. Some rights reserved.
##    photoshop:Credit := John Doe
##    photoshop:DateCreated := 2023-02-03
##    xmpRights:Marked := TRUE
##    xmpRights:UsageTerms := This work is licensed to the public under the Creative Commons
##         Attribution 4.0 International license
##         https://creativecommons.org/licenses/by/4.0/
##    xmpRights:WebStatement := https://creativecommons.org/licenses/by/4.0/
```

```r
unlink(f)
```


## <a name="comparison">Limitations by backend</a>

`{xmpdf}` feature | `exiftool` | `pdftk` | `ghostscript`
---|---|---|---|
Get XMP metadata | **Yes** | **No** | **No**
Set XMP metadata | **Yes** | **No** | **Poor**: when documentation info metadata is set then as a side effect it seems the documentation info metadata will also be set as XMP metadata
Get PDF bookmarks | **No** | **Okay**: can only get Title, Page number, and Level | **No**
Set PDF bookmarks | **No** | **Okay**: can only set Title, Page number, and Level | **Good**: supports most bookmarks features including color and font face but only action supported is to view a particular page
Get PDF documentation info | **Good**: may "widen" datetimes which are less than "second" precision | **Yes** | **No**
Set PDF documentation info | **Good**: may "widen" datetimes which are less than "second" precision | **Yes** | **Yes**: as a side effect when documentation info metadata is set then it seems will also be set as XMP metadata
Concatenate PDF files | **No** | **Yes** | **Yes**

Known limitations:

* `get_bookmarks_pdftk()` doesn't report information about bookmarks color, font face, and whether the bookmarks
  should start open or closed.
* `get_docinfo_exiftool()` "widens" datetimes to second precision.  An hour-only UTC offset will be "widened" to minute precision.
* `set_bookmarks_gs()` supports most bookmarks features including color and font face but only action supported is to view a particular page.
* `set_bookmarks_pdftk()` only supports setting the title, page number, and level of bookmarks.
* `set_docinfo_exiftool()` "widens" datetimes to second precision.  An hour-only UTC offset will be "widened" to minute precision.
* All of the `set_docinfo()` methods currently do not support arbitrary info dictionary entries.
* As a side effect `set_docinfo_gs()` seems to also update any matching XPN metadata
  while `set_docinfo_exiftool()` and `set_docinfo_pdftk()` don't update
  any previously set matching XPN metadata.
  Some pdf viewers will preferentially use the previously set document title from XPN metadata
  if it exists instead of using the title set in documentation info dictionary entry.
  Consider also manually setting this XPN metadata using `set_xmp()` 
* Old metadata information is usually not deleted from the files by these 
  operations (i.e. these operations are often theoretically reversible).
  If deleting the old metadata is important one may want to consider calling
  `qpdf::pdf_compress(input, linearize = TRUE)` at the end.

## <a name="links">External links</a>

### <a name="standards">Metadata links</a>

* [basic pdfmark features](https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#) (Adobe) documents "Documentation info dictionary" metadata and "Bookmarks"
* [IPTC Photo Metadata Standard](https://iptc.org/standards/photo-metadata/iptc-standard/) (IPTC) is a popular XMP metadata standard for photos
* [Quick guide to IPTC Photo Metadata on Google Images](https://iptc.org/standards/photo-metadata/quick-guide-to-iptc-photo-metadata-and-google-images/) (IPTC) describes the subset of the IPTC Photo Metadata Standard used by Google Photos to list photo credits and license information
* [xmp-docs](https://github.com/adobe/xmp-docs/tree/master/XMPNamespaces) (Adobe) describes some common XMP tags
* [XMP](https://wiki.creativecommons.org/wiki/XMP) (Creative Commons) describes a standard for using XMP to embed Creative Commons license information
* [XMP tags](https://exiftool.org/TagNames/XMP.html) (exiftool) is a fairly comprehensive list of XMP tags

### <a name="similar">Related software</a>

Note most of the R packages listed below are focused on **getting** metadata rather than **setting** metadata
and/or only provide low-level wrappers around the relevant command-line tools.
Please feel free to [open a pull request to add any missing relevant R packages](https://github.com/trevorld/r-xmpdf/edit/main/README.Rmd).

#### exempi

* [exempi](https://libopenraw.freedesktop.org/exempi/)

#### exiftool

* [{exifr}](https://github.com/paleolimbot/exifr) 
  provides a high-level wrapper to read metadata as well as a low-level wrapper around the `exiftool` command-line tool.
  Can download `exiftool`.
* [{exiftoolr}](https://github.com/JoshOBrien/exiftoolr) 
  provides high-level wrapper to read metadata as well as a low-level wrapper around the `exiftool` command-line tool.
  Can download `exiftool`.
* [exiftool](https://exiftool.org/)

#### exiv2

* [{exiv}](https://github.com/hrbrmstr/exiv) read and write ‘Exif’, ‘ID3v1’ and ‘ID3v2’ image/media tags
* [exiv2](https://exiv2.org/)

#### other exif tools

* [{exif}](https://github.com/Ironholds/exif) reads EXIF from jpeg images
* [{magick}](https://github.com/ropensci/magick) has `image_attributes()` which reads EXIF image tags.

#### ghostscript

* `{tools}` has `find_gs_cmd()` to find a GhostScript executable in a cross-platform way.
* [ghostscript](https://www.ghostscript.com/)

#### poppler

* [{pdftools}](https://docs.ropensci.org/pdftools/)
* [{Rpoppler}](https://cran.r-project.org/package=Rpoppler)
* [poppler](https://poppler.freedesktop.org/)

#### qpdf

* [{qpdf}](https://cran.r-project.org/package=qpdf)
* [qpdf](https://qpdf.sourceforge.io/)

#### pdftk

* [{animation}](https://yihui.org/animation/) has `pdftk()`, a low-level wrapper around the `pdftk` command-line tool.
* [pdftk](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/)
* [pdftk-java](https://gitlab.com/pdftk-java/pdftk)

#### tabula

* [{tabulizer}](https://github.com/ropensci/tabulizer)
* [tabula-java](https://github.com/tabulapdf/tabula-java/)

#### xpdf

* [xpdf](http://www.xpdfreader.com/about.html)'s `pdfinfo` tool
