# SPDX-License-Identifier: MIT

#' Detect support for features
#'
#' `supports_get_bookmarks()`, `supports_set_bookmarks()`,
#' `supports_get_docinfo()`, `supports_set_docinfo()`,
#' `supports_get_xmp()`, `supports_set_xmp()`,
#' `supports_cat_pages()`, and `supports_n_pages()`
#' detects support for the functions
#' [get_bookmarks()], [set_bookmarks()],
#' [get_docinfo()], [set_docinfo()],
#' [get_xmp()], [set_xmp()],
#' [cat_pages()], and [n_pages()] respectively.
#' `supports_exiftool()`, `supports_gs()` and `supports_pdftk()`
#' detects support for the command-line tools
#' `exiftool`, `ghostscript` and `pdftk` respectively as used by various lower-level functions.
#'
#' * `supports_exiftool()` detects support for the command-line tool `exiftool` which is
#'   required for [get_docinfo_exiftool()], [get_xmp_exiftool()], [set_xmp_exiftool()], and [n_pages_exiftool()].
#' * `supports_gs()` detects support for the command-line tool `ghostscript` which is
#'   required for [set_docinfo_gs()], [set_bookmarks_gs()], [cat_pages_gs()], and [n_pages_gs()].
#' * `supports_pdftk()` detects support for the command-line tool `pdftk` which is
#'   required for [get_bookmarks_pdftk()], [set_bookmarks_pdftk()],
#'   [get_docinfo_pdftk()], [set_docinfo_pdftk()], [cat_pages_pdftk()], and [n_pages_pdftk()].
#' * `requireNamespace("qpdf", quietly = TRUE)` detects support for the R packages `qpdf`
#'   which is required for [cat_pages_qpdf()] and [n_pages_qpdf()].
#' @examples
#'   # Detect for higher-level features
#'   supports_get_docinfo()
#'   supports_set_docinfo()
#'   supports_get_bookmarks()
#'   supports_set_bookmarks()
#'   supports_get_xmp()
#'   supports_set_xmp()
#'   supports_cat_pages()
#'   supports_n_pages()
#'
#'   # Detect support for lower-level helper features
#'   supports_exiftool()
#'   supports_gs()
#'   supports_pdftk()
#'   print(requireNamespace("qpdf", quietly = TRUE))
#'   print(requireNamespace("pdftools", quietly = TRUE))
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
    supports_exiftool() || supports_pdftk() || supports_pdftools()
}

#' @rdname supports
#' @export
supports_set_docinfo <- function() {
    supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_get_xmp <- function() {
    supports_exiftool()
}

#' @rdname supports
#' @export
supports_set_xmp <- function() {
    supports_exiftool()
}

#' @rdname supports
#' @export
supports_cat_pages <- function() {
    supports_qpdf() || supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_n_pages <- function() {
    supports_exiftool() || supports_qpdf() || supports_pdftk() || supports_gs()
}

#' @rdname supports
#' @export
supports_exiftool <- function() {
    as.logical(find_exiftool_cmd() != "")
}

#' @rdname supports
#' @export
supports_gs <- function() {
    as.logical(find_gs_cmd() != "")
}

#' @rdname supports
#' @export
supports_pdftk <- function() {
    as.logical(find_pdftk_cmd() != "")
}

find_exiftool_cmd <- function() {
    if (getOption("xmpdf_disable_exiftool", FALSE)) {
        ""
    } else if (requireNamespace("exiftoolr", quietly = TRUE)) {
        cmd <- try(exiftoolr::configure_exiftoolr(quiet = TRUE),
                   silent = TRUE)
        if (inherits(cmd, "try-error"))
            ""
        else
            cmd
    } else {
        Sys.which(Sys.getenv("ET_EXIFTOOL_PATH", "exiftool"))
    }
}

find_gs_cmd <- function() {
    if (getOption("xmpdf_disable_gs", FALSE))
        ""
    else
        tools::find_gs_cmd()
}

find_pdftk_cmd <- function() {
    if (getOption("xmpdf_disable_pdftk", FALSE))
        ""
    else
        Sys.which(Sys.getenv("PDFTK_PATH", "pdftk"))
}

# supports_jsonlite <- function() {
#     requireNamespace("jsonlite", quietly = TRUE)
# }
supports_qpdf <- function() {
    requireNamespace("qpdf", quietly = TRUE) &&
        !getOption("xmpdf_disable_qpdf", FALSE)
}
supports_pdftools <- function() {
    requireNamespace("pdftools", quietly = TRUE) &&
        !getOption("xmpdf_disable_pdftools", FALSE)
}

gs <- function() {
    get_cmd("ghostscript", find_gs_cmd, install_gs_str)
}

pdftk <- function() {
    get_cmd("pdftk", find_pdftk_cmd, install_pdftk_str)
}

exiftool <- function() {
    get_cmd("exiftool", find_exiftool_cmd, install_exiftool_str)
}

#### Use more enhance error message with
# 'install_exiftool_str()', 'install_gs_str()', 'install_pdftk_str()'
get_cmd <- function(name,
                    cmd_fn = function() Sys.which(name),
                    msg_fn = function() install_cmd_str(name)) {
    cmd <- cmd_fn()
    if (cmd == "")
        abort(msg_fn(), class = "xmpdf_suggested_package")
    cmd
}
