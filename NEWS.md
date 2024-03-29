xmpdf 0.2.0
===========

New features
------------

* `get_bookmarks_pdftools()` gets pdf bookmark information using `pdftools::pdf_toc()` (#30).
  Note though due to poppler library limitations it can't return page numbers that correspond to each bookmark.
  Hence `get_bookmarks()` will try to use the following helper functions in the following order:

  1. `get_bookmarks_pdftk()`
  2. `get_bookmarks_pdftools()`

Bug fixes and minor improvements
--------------------------------

* We can again disable old bookmarks when setting new bookmarks
  in `set_bookmarks_gs()` for the newest versions of `ghostscript` (#59).
  The new approach means we could remove some GPLv3 code and have relicensed the package under the MIT license.
* Improved support for reading/writing Unicode values in non-Unicode locales (#57).
* `get_bookmarks()` and `get_bookmarks_pdftk()` no longer returns its data frame value invisibly.

xmpdf 0.1.4
===========

* Unit tests have been adjusted to prevent a CRAN R CMD check ERROR on
  an M1 Mac machine running macOS 14 (#58).

xmpdf 0.1.3
===========

Bug fixes and minor improvements
--------------------------------

* `set_docinfo_exiftool()` should now losslessly write all legal datetimes (#55).
  `set_docinfo()`'s priority order has been updated to the following:

  1. `set_docinfo_exiftool()`
  2. `set_docinfo_gs()`
  3. `set_docinfo_pdftk()`

* Unit tests have been adjusted to prevent CRAN R CMD check ERRORs on certain CRAN platforms
  that don't have `ghostscript` installed or are ran in a non-Unicode locale (#56).

xmpdf 0.1.1
===========

Initial features
----------------

* `cat_bookmarks()` concatenates a list of bookmarks
  into a single bookmarks data frame while updating the page numbers.
  Useful if wanting to concatenate multiple pdf files together and
  would like to preserve the bookmarks information.
* `get_bookmarks()` returns a data frame with pdf bookmarks information.
  `get_bookmarks()` will try to use the following helper functions in the following order:

  1. `get_bookmarks_pdftk()` which wraps `pdftk` command-line tool

* `set_bookmarks()` sets (replaces) pdf bookmarks information.
  `set_bookmarks()` will try to use the following helper functions in the following order:

  1. `set_bookmarks_gs()` which wraps `ghostscript` command-line tool
  2. `set_bookmarks_pdftk()` which wraps `pdftk` command-line tool

* `docinfo()` provides an `{R6}` object to represent pdf documentation info entries.
* `as_docinfo()` can coerce objects into `docinfo()` objects.
  In particular there is an `as_docinfo.xmp()` method to coerce `xmp()` objects.
* `get_docinfo()` reads in pdf documentation info entries from pdf file(s).
  `get_docinfo()` will try to use the following helper functions in the following order:

  1. `get_docinfo_pdftk()` which wraps `pdftk` command-line tool
  2. `get_docinfo_exiftool()` which wraps `exiftool` command-line tool
  3. `get_docinfo_pdftools()` which wraps `pdftools::pdf_info()`

* `set_docinfo()` sets pdf documentation info entries to a pdf file.
  `set_docinfo()` will try to use the following helper functions in the following order:

  1. `set_docinfo_gs()` which wraps `ghostscript` command-line tool
  2. `set_docinfo_pdftk()` which wraps `pdftk` command-line tool
  3. `set_docinfo_exiftool()` which wraps `exiftool` command-line tool

* `xmp()` provides an `{R6}` object to represent xmp metadata.

  + `as_lang_alt()` supports "language alternative" XMP tags.

* `as_xmp()` can coerce objects into `xmp()` objects.
  In particular there is an `as_xmp.docinfo()` method to coerce `docinfo()` objects.
* `get_xmp()` reads in xmp metadata from media file(s).
  `get_xmp()` will try to use the following helper functions in the following order:

  1. `get_xmp_exiftool()` which wraps `exiftool` command-line tool

* `set_xmp()` sets xmp metadata in media file(s).
  `set_xmp()` will try to use the following helper functions in the following order:

  1. `set_xmp_exiftool()` which wraps `exiftool` command-line tool

* `cat_pages()` concatenates (pdf) file(s) into a single output pdf.
  `cat_pages()` will try to use the following helper functions in the following order:
 
  1. `cat_pages_qpdf()` which wraps `qpdf::pdf_combine()`
  2. `cat_pages_pdftk()` which wraps `pdftk` command-line tool
  3. `cat_pages_gs()` which wraps `ghostscript` command-line tool

* `n_pages()` returns the number of pages in the (pdf) file(s).
  `n_pages()` will try to use the following helper functions in the following order:
 
  1. `n_pages_qpdf()` which wraps `qpdf::pdf_length()`
  2. `n_pages_exiftool()` which wraps `exiftool` command-line tool
  3. `n_pages_pdftk()` which wraps `pdftk` command-line tool
  4. `n_pages_gs()` which wraps `ghostscript` command-line tool

* The following functions detect support for various higher-level features:

  * `supports_get_bookmarks()`
  * `supports_set_bookmarks()`
  * `supports_get_docinfo()`
  * `supports_set_docinfo()`
  * `supports_get_xmp()`
  * `supports_set_xmp()`
  * `supports_n_pages()`

* The following functions detect support for various command-line tools needed for some
  lower-level helper functions:

  * `supports_exiftool()` detects support for `exiftool`
  * `supports_gs()` detects support for `ghostscript`
  * `supports_pdftk()` detects support for `pdftk`

* `enable_feature_message()` returns a character vector with the
  information needed to install the requested feature.
  Formatted for use with `rlang::abort()`, `rlang::warn()`, or `rlang::inform()`.

* The `spdx_licenses` data set contains the [SPDX License List](https://spdx.org/licenses/).
