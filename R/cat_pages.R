#' Concatenate pdf documents together
#'
#' `cat_pages()` concatenates pdf documents together.
#'
#' `cat_pages()` will try to use the following helper functions in the following order:
#'
#' 1. `cat_pages_qpdf()` which wraps [qpdf::pdf_combine()]
#' 2. `cat_pages_pdftk()` which wraps `pdftk` command-line tool
#' 3. `cat_pages_gs()` which wraps `ghostscript` command-line tool
#'
#' @param input Filename(s) (pdf) to concatenate together
#' @param output Filename (pdf) to save concatenated output to
#' @return The (output) filename invisibly.
#' @seealso [supports_cat_pages()], [supports_gs()], and [supports_pdftk()] to detect support for these features.
#'          [cat_bookmarks()] for generating bookmarks for concatenated files.
#' @examples
#' if (supports_cat_pages() && require("grid", quietly = TRUE)) {
#'   # Create two different two-page pdf files
#'   make_pdf <- function(f, title) {
#'     pdf(f, onefile = TRUE, title = title)
#'     grid.text(paste(title, "Page 1"))
#'     grid.newpage()
#'     grid.text(paste(title, "Page 2"))
#'     invisible(dev.off())
#'   }
#'   f1 <- tempfile(fileext = "_doc1.pdf")
#'   on.exit(unlink(f1))
#'   make_pdf(f1, "Document 1")
#'
#'   f2 <- tempfile(fileext = "_doc2.pdf")
#'   on.exit(unlink(f2))
#'   make_pdf(f2, "Document 2")
#'
#'   fc <- tempfile(fileext = "_cat.pdf")
#'   on.exit(unlink(fc))
#'   cat_pages(c(f1, f2), fc)
#'
#'   # Use `cat_bookmarks()` to create pdf bookmarks for concatenated output files
#'   if (supports_get_bookmarks() && supports_set_bookmarks()) {
#'      l <- get_bookmarks(c(f1, f2))
#'      bm <- cat_bookmarks(l, "title")
#'      set_bookmarks(bm, fc)
#'      print(get_bookmarks(fc)[[1]])
#'   }
#'   unlink(f1)
#'   unlink(f2)
#'   unlink(fc)
#' }
#' @export
cat_pages <- function(input, output) {
    if (supports_qpdf()) {
        cat_pages_qpdf(input, output)
    } else if (supports_pdftk()) {
        cat_pages_pdftk(input, output)
    } else if (supports_gs()) {
        cat_pages_gs(input, output)
    } else {
        msg <- c(need_to_install_str("cat_pages()"),
                 install_package_str("qpdf"),
                 install_pdftk_str(),
                 install_gs_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname cat_pages
#' @export
cat_pages_gs <- function(input, output) {
    cmd <- gs()
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    args <- c("-q", "-o", shQuote(output),
              "-sDEVICE=pdfwrite", "-sAutoRotatePages=None",
              shQuote(input))
    xmpdf_system2(cmd, args)
    invisible(output)
}

#' @rdname cat_pages
#' @export
cat_pages_pdftk <- function(input, output) {
    cmd <- pdftk()
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    args <- c(shQuote(input), "cat", "output", shQuote(output))
    xmpdf_system2(cmd, args)
    invisible(output)
}

#' @rdname cat_pages
#' @export
cat_pages_qpdf <- function(input, output) {
    assert_suggested("qpdf")
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    qpdf::pdf_combine(input, output)
    invisible(output)
}
