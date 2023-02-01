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
#'  l <- list(`dc:creator` = "John Doe", `dc:title` = "A Title")
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
    xmp <- xmp()
    for (key in names(x))
        xmp$set_item(key, x[[key]])
    xmp
}

#' @export
as_xmp.xmp <- function(x, ...) {
    x
}
