# SPDX-License-Identifier: MIT

#' Set/get xmp metadata
#'
#' `get_xmp()` gets xmp metadata from a file.
#' `set_xmp()` sets xmp metadata for a file.
#'
#' `get_xmp()` will try to use the following helper functions in the following order:
#'
#' 1. `get_xmp_exiftool()` which wraps `exiftool` command-line tool
#'
#' `set_xmp()` will try to use the following helper functions in the following order:
#'
#' 1. `set_xmp_exiftool()` which wraps `exiftool` command-line tool
#'
#' @param filename Filename(s) to extract xmp metadata from.
#' @param use_names If `TRUE` (default) use `filename` as the names of the result.
#' @param xmp An [xmp()] object.
#' @param input Input filename.
#' @param output Output filename.
#' @return `get_xmp()` returns a list of [xmp()] objects.
#'         `set_xmp()` returns the (output) filename invisibly.
#' @seealso [xmp()] for more information about xmp metadata objects.
#'   [supports_get_xmp()], [supports_set_xmp()], and [supports_exiftool()] to detect support for these features.  For more info about xmp metadata see <https://www.exiftool.org/TagNames/XMP.html>.
#' @examples
#' if (supports_set_xmp() && supports_get_xmp() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   x <- xmp(Title = "An XMP title", creator = "John Doe")
#'   set_xmp(x, f)
#'   print(get_xmp(f)[[1]])
#' }
#' @name edit_xmp
NULL

#' @rdname edit_xmp
#' @export
get_xmp <- function(filename, use_names = TRUE) {
    if (supports_exiftool()) {
        get_xmp_exiftool(filename, use_names = use_names)
    } else {
        msg <- c(need_to_install_str("get_xmp()"),
                 install_exiftool_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname edit_xmp
#' @export
get_xmp_exiftool <- function(filename, use_names = TRUE) {
    l <- lapply(filename, get_xmp_exiftool_helper)
    use_filenames(l, use_names, filename)
}
get_xmp_exiftool_helper <- function(filename) {
    md <- get_exiftool_metadata(filename, tags="-XMP:all")
    md <- md[grep("^XMP-", names(md))]
    names(md) <- gsub("^XMP-", "", names(md))
    as_xmp(md)
}

#' @rdname edit_xmp
#' @export
set_xmp <- function(xmp, input, output = input) {
    if (supports_exiftool()) {
        set_xmp_exiftool(xmp, input, output)
    } else {
        msg <- c(need_to_install_str("set_xmp()"),
                 install_exiftool_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname edit_xmp
#' @export
set_xmp_exiftool <- function(xmp, input, output = input) {
    xmp <- as_xmp(xmp)
    set_exiftool_metadata(xmp$exiftool_tags(), input, output)
}
