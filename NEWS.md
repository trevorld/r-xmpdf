xmpdf 0.1.0 (development)
=========================

Initial features
----------------

* `get_bookmarks()` returns a data frame with pdf bookmarks information.
  `get_bookmarks()` will try to use the following helper functions in the following order:

  1. `get_bookmarks_pdftk()` which wraps `pdftk` command-line tool

* `set_bookmarks()` sets (replaces) pdf bookmarks information (#2).
  `set_bookmarks()` will try to use the following helper functions in the following order:

  1. `set_bookmarks_gs()` which wraps `ghostscript` command-line tool
  2. `set_bookmarks_pdftk()` which wraps `pdftk` command-line tool

* `docinfo()` provides an `{R6}` object to represent pdf documentation info entries.
* `get_docinfo()` reads in pdf documentation info entries from pdf file(s).
  `get_docinfo()` will try to use the following helper functions in the following order:

  1. `get_docinfo_pdftools()` which wraps `pdftools::pdf_info()`
  2. `get_docinfo_exiftool()` which wraps `exiftool` command-line tool
  3. `get_docinfo_pdftk()` which wraps `pdftk` command-line tool

* `set_docinfo()` sets pdf documentation info entries to a pdf file (#3).
  `set_docinfo()` will try to use the following helper functions in the following order:

  1. `set_docinfo_gs()` which wraps `ghostscript` command-line tool
  2. `set_docinfo_exiftool()` which wraps `exiftool` command-line tool
  3. `set_docinfo_pdftk()` which wraps `pdftk` command-line tool

* `get_xmp()` reads in xmp metadata from media file(s).
  `get_xmp()` will try to use the following helper functions in the following order:

  1. `get_xmp_exiftool()` which wraps `exiftool` command-line tool

* `set_xmp()` sets xmp metadata in media file(s).
  `set_xmp()` will try to use the following helper functions in the following order:

  1. `set_xmp_exiftool()` which wraps `exiftool` command-line tool

* `n_pages()` returns the number of pages in the (pdf) file(s).
  `n_pages()` will try to use the following helper functions in the following order:
 
  1. `n_pages_qpdf()` which wraps `qpdf::pdf_length()`
  2. `n_pages_pdftk()` which wraps `pdftk` command-line tool
  3. `n_pages_gs()` which wraps `ghostscript` command-line tool
  4. `n_pages_exiftool()` which wraps `exiftool` command-line tool

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
