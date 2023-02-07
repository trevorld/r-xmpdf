# SPDX-License-Identifier: MIT

#' Get number of pages in a document
#'
#' `n_pages()` returns the number of pages in the (pdf) file(s).
#'
#' `n_pages()` will try to use the following helper functions in the following order:
#'
#' 1. `n_pages_qpdf()` which wraps [qpdf::pdf_length()]
#' 2. `n_pages_exiftool()` which wraps `exiftool` command-line tool
#' 3. `n_pages_pdftk()` which wraps `pdftk` command-line tool
#' 4. `n_pages_gs()` which wraps `ghostscript` command-line tool
#'
#' @param filename Character vector of filenames.
#' @param use_names If `TRUE` (default) use `filename` as the names of the result.
#' @examples
#' if (supports_n_pages() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'   print(n_pages(f))
#'   unlink(f)
#' }
#' @return An integer vector of number of pages within each file.
#' @seealso [supports_n_pages()] detects support for this feature.
#' @export
n_pages <- function(filename, use_names = TRUE) {
    if (supports_qpdf()) {
        n_pages_qpdf(filename, use_names = use_names)
    } else if (supports_exiftool()) {
        n_pages_exiftool(filename, use_names = use_names)
    } else if (supports_pdftk()) {
        n_pages_pdftk(filename, use_names = use_names)
    # } else if (has_cmd("pdfinfo")) {
    #     n_pages_pdfinfo(filename, use_names = use_names)
    } else if (supports_gs()) {
        n_pages_gs(filename, use_names = use_names)
    } else {
        abort(msg_n_pages(), class = "xmpdf_suggested_package")
    }
}

#' @rdname n_pages
#' @export
n_pages_exiftool <- function(filename, use_names = TRUE) {
    filename <- normalizePath(filename, mustWork = TRUE)
    sapply(filename, USE.NAMES = use_names, FUN = function(f) {
        md <- get_exiftool_metadata(f, "-pdf:pagecount")
        as.integer(md[["PDF:PageCount"]])
    })
}

#' @rdname n_pages
#' @export
n_pages_qpdf <- function(filename, use_names = TRUE) {
    assert_suggested("qpdf")
    filename <- normalizePath(filename, mustWork = TRUE)
    sapply(filename, USE.NAMES = use_names, FUN = function(f) qpdf::pdf_length(f))
}

# #' @rdname n_pages
# #' @export
# n_pages_pdfinfo <- function(filename, use_names = TRUE) {
#     cmd <- Sys.which("pdfinfo")
#     filename <- shQuote(normalizePath(filename, mustWork = TRUE))
#     sapply(filename, USE.NAMES = use_names, FUN = function(f) {
#         pdfinfo <- xmpdf_system2(cmd, f)
#         pdfinfo <- grep("^Pages:", pdfinfo, value=TRUE)
#         as.integer(strsplit(pdfinfo, " +")[[1]][2])
#     })
# }

#' @rdname n_pages
#' @export
n_pages_pdftk <- function(filename, use_names = TRUE) {
    cmd <- pdftk()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    sapply(filename, USE.NAMES = use_names, FUN = function(f) {
        args <- c(f, "dump_data_utf8")
        pdfinfo <- xmpdf_system2(cmd, args)
        pdfinfo <- grep("^NumberOfPages:", pdfinfo, value=TRUE)
        as.integer(strsplit(pdfinfo, ":")[[1]][2])
    })
}

#' @rdname n_pages
#' @export
n_pages_gs <- function(filename, use_names = TRUE) {
    cmd <- gs()
    filename <- normalizePath(filename, winslash="/", mustWork = TRUE)
    sapply(filename, USE.NAMES = use_names, FUN = function(f) {
        args <- c("-q", "-dNODISPLAY", "-dNOSAFER", "-c",
                  paste(paste0('"(', f, ")"),
                        "(r)", "file", "runpdfbegin", "pdfpagecount", "=", 'quit"'))
        as.integer(xmpdf_system2(cmd, args))
    })
}
