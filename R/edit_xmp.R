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
#'   x <- xmp(attribution_url = "https://example.com/attribution",
#'            creator = "John Doe",
#'            description = "An image caption",
#'            date_created = Sys.Date(),
#'            spdx_id = "CC-BY-4.0")
#'   print(x)
#'   print(x, mode = "google_images", xmp_only = TRUE)
#'   print(x, mode = "creative_commons", xmp_only = TRUE)
#'
#'   if (supports_set_xmp() &&
#'       supports_get_xmp() &&
#'       capabilities("png") &&
#'       requireNamespace("grid", quietly = TRUE)) {
#'
#'     f <- tempfile(fileext = ".png")
#'     png(f)
#'     grid::grid.text("This is an image!")
#'     invisible(dev.off())
#'     set_xmp(x, f)
#'     print(get_xmp(f)[[1]])
#'   }
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
    if (any(grepl("-", names(md)))) {
        md <- extract_lang_alt(md)
    }
    x <- as_xmp(md)
    x$auto_xmp <- NULL
    x
}

#### lang_alt
extract_lang_alt <- function(x) {
    tags <- unique(gsub("^([[:alnum:]:]+)-(.*)$", "\\1",
                        grep("-", names(x), value = TRUE)))
    for (tag in tags) {
        names(x) <- ifelse(names(x) == tag, paste0(tag, "-x-default"), names(x))
        i <- grep(paste0("^", tag), names(x))
        x_tag <- x[i]
        names(x_tag) <- substr(names(x_tag), nchar(tag) + 2L, nchar(names(x_tag)))
        x_la <- list(as_lang_alt(x_tag))
        names(x_la) <- tag
        x <- c(x[-i], x_la)
    }
    x
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
    set_exiftool_metadata(xmp$exiftool_tags(), input, output, mode = "xmp")
}
