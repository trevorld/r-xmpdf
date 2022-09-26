piecepackr.metadata (development)
=================================

Initial features
----------------

* `n_pages()` returns the number of pages in the (pdf) file(s).
  `n_pages()` will try to use the following helper functions in the following order:
 
  1. `n_pages_qpdf()` which wraps `qpdf::pdf_length()`.
  2. `n_pages_pdftk()` which wraps `pdftk` command-line tool
  3. `n_pages_gs()` which wraps `ghostscript` command-line tool
