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

#' xmp metadata object
#'
#' `xmp()` creates
#' Such objects can be used with [set_xmp()] to edit XMP medata for a variety of media formats
#' and such objects are returned by [get_xmp()].
#'
#' @param ... Entries of xmp metadata.  The names are either the xmp tag names or alternatively the xmp namespace and tag names separated by ":".  The values are the xmp values.
#' @return An xmp object as can be used with [set_xmp()].  Basically a named list whose names are the (optional) xmp namespace and tag names separated by ":" and the values are the xmp values.  Datetimes should be a datetime object such as [POSIXlt()] or else a character vector in the `"%Y-%m-%dT%H:%M:%S%z"` format.
#' @seealso [get_xmp()] and [set_xmp()] for getting/setting such information from/to a variety of media file formats.
#'          [as_xmp()] for coercing to this object.
#'    [as_docinfo()] can be used to coerce `xmp()` objects into [docinfo()] objects.
#' @examples
#' if (supports_set_xmp() && supports_get_xmp() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   x <- xmp(title = "An XMP title", creator = "John Doe")
#'   set_xmp(x, f)
#'   print(get_xmp(f)[[1]])
#' }
#' @name xmp
#' @export
xmp <- function(...) {
    l <- list(...)
    as_xmp(l)
}

#' Coerce to xmp objects
#'
#' `as_xmp()` coerces objects into an [xmp()] object.
#'
#' @param x An object that can reasonably be coerced to a [xmp()] object.
#' @param ... Further arguments passed to or from other methods.
#' @return An [xmp()] object.
#' @examples
#'  di <- docinfo(author = "John Doe", title = "A Title")
#'  as_xmp(di)
#'
#'  l <- list(`dc:Creator` = "John Doe", `dc:Title` = "A Title")
#'  as_xmp(l)
#' @export
as_xmp <- function(x, ...) {
    UseMethod("as_xmp")
}

#' @export
as_xmp.default <- function(x, ...) {
    as_xmp(as.list(x))
}

#' @rdname as_xmp
#' @export
as_xmp.docinfo <- function(x, ...) {
    x$xmp()
}

#' @rdname as_xmp
#' @export
as_xmp.list <- function(x, ...) {
    class(x) <- c("xmp", class(x))
    x
}

#' @export
as_xmp.xmp <- function(x, ...) {
    x
}

#' @export
print.xmp <- function(x, ...) {
    if (length(x) == 0L) {
        cat("no XMP metadata found")
    } else {
        for (name in names(x))
            cat(paste0(name, " : ", d_format(x[[name]]), "\n"))
    }
    invisible(NULL)
}


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
