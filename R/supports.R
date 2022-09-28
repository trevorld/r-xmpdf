#' Detect support for features
#'
#' `supports_get_bookmarks()`, `supports_set_bookmarks()`,
#' `supports_get_docinfo()`, `supports_set_docinfo()`,
#' and `supports_n_pages()`
#' detects support for the functions
#' [get_bookmarks()], [set_bookmarks()], [get_docinfo()], [set_docinfo()], and [n_pages()] respectively.
#' `supports_gs()` and `supports_pdftk()` detects support for the command-line tools
#' `ghostscript` and `pdftk` respectively as used by various lower-level functions.
#'
#' * `supports_gs()` detects support for the command-line tool `ghostscript` which is
#'   required for [set_docinfo_gs()], [set_bookmarks_gs()], and [n_pages_gs()].
#' * `supports_pdftk()` detects support for the command-line tool `pdftk` which is
#'   required for [get_docinfo_pdftk()] and [set_docinfo_pdftk()],
#' * `requireNamespace("qpdf", quietly = TRUE)` detects support for the R packages `qpdf`
#'   which is required for [n_pages_qpdf()].
#' * `requireNamespace("pdftools", quietly = TRUE)` detects support for the R package `pdftools`
#'   which is required for [get_docinfo_pdftools()].
#' @examples
#'   # Detect for higher-level features
#'   supports_get_docinfo()
#'   supports_set_docinfo()
#'   supports_get_bookmarks()
#'   supports_set_bookmarks()
#'   supports_n_pages()
#'
#'   # Detect support for lower-level helper features
#'   supports_gs()
#'   supports_pdftk()
#'   requireNamespace("qpdf", quietly = TRUE)
#'   requireNamespace("pdftools", quietly = TRUE)
#' @name supports
NULL

#' @rdname supports
#' @export
supports_get_bookmarks <- function() {
    supports_pdftk()
}

#' @rdname supports
#' @export
supports_set_bookmarks <- function() {
    supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_get_docinfo <- function() {
    supports_pdftools() || supports_pdftk()
}

#' @rdname supports
#' @export
supports_set_docinfo <- function() {
    supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_n_pages <- function() {
    supports_qpdf() || supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_gs <- function() {
    tools::find_gs_cmd() != ""
}

#' @rdname supports
#' @export
supports_pdftk <- function() {
    Sys.which("pdftk") != ""
}

supports_qpdf <- function() {
    requireNamespace("qpdf", quietly = TRUE)
}
supports_pdftools <- function() {
    requireNamespace("pdftools", quietly = TRUE)
}

gs <- function() {
    get_cmd("ghostscript", tools::find_gs_cmd)
}

pdftk <- function() {
    get_cmd("pdftk", function() Sys.which("pdftk"))
}

get_cmd <- function(name, cmd_fn = function() Sys.which(name)) {
    cmd <- cmd_fn()
    if (cmd == "")
        abort(sprintf("Can't find system dependency `%s` on PATH", name))
    cmd
}
