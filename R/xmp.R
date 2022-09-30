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
#' @param xmp A named list of xmp metadata.  The names are either the xmp tag names or alternatively the xmp namespace and tag names separated by ":".  The values are the xmp values.
#' @param input Input filename.
#' @param output Output filename.
#' @return `get_xmp()` returns a list of named lists: the names are the xmp namespace and tag names separated by ":" and the values are the xmp values.  Note any datetimes will be a string in `"%Y-%m-%dT%H:%M:%S%z"` format.
#'         `set_xmp()` returns the (output) filename invisibly.
#' @seealso [supports_get_xmp()], [supports_set_xmp()], and [supports_exiftool()] to detect support for these features.  For more info about xmp metadata see <https://www.exiftool.org/TagNames/XMP.html>.
#' @examples
#' if (supports_set_xmp() && supports_get_xmp() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   print(get_xmp(f)[[1]])
#'   \dontshow{cat("\n")}
#'
#'   xmp <- list(Title = "An XMP title")
#'   set_xmp(xmp, f)
#'   print(get_xmp(f)[[1]])
#' }
#' @name xmp
NULL

#' @rdname xmp
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

#' @rdname xmp
#' @export
get_xmp_exiftool <- function(filename, use_names = TRUE) {
    l <- lapply(filename, get_xmp_exiftool_helper)
    if (use_names)
        names(l) <- filename
    else
        names(l) <- NULL
    l
}
get_xmp_exiftool_helper <- function(filename) {
    md <- get_exiftool_metadata(filename, tags="-XMP:all")
    md <- md[grep("^XMP-", names(md))]
    names(md) <- gsub("^XMP-", "", names(md))
    md
}

#' @rdname xmp
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

#' @rdname xmp
#' @export
set_xmp_exiftool <- function(xmp, input, output = input) {
    nms <- names(xmp)
    idx_ns <- grep(":", nms)
    if (length(idx_ns))
        nms[idx_ns] <- paste0("XMP-", nms[idx_ns])
    idx_no_ns <- grep(":", nms, invert = TRUE)
    if (length(idx_no_ns))
        nms[idx_no_ns] <- paste0("XMP:", nms[idx_no_ns])
    names(xmp) <- nms
    set_exiftool_metadata(xmp, input, output)
}
