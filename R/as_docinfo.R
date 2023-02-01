#' Coerce to docinfo objects
#'
#' `as_docinfo()` coerces objects into a [docinfo()] object.
#'
#' @param x An object that can reasonably be coerced to a [docinfo()] object.
#' @param ... Further arguments passed to or from other methods.
#' @return A [docinfo()] object.
#' @examples
#'  x <- xmp(`dc:Creator` = "John Doe", `dc:Title` = "A Title")
#'  as_docinfo(x)
#'
#' @export
as_docinfo <- function(x, ...) {
    UseMethod("as_docinfo")
}

#' @export
as_docinfo.docinfo <- function(x, ...) {
    x
}

#' @export
as_docinfo.default <- function(x, ...) {
    l <- as.list(x)
    d <- docinfo()
    for (key in names(l))
        d$set_item(key, l[[key]])
    d
}

#' @rdname as_docinfo
#' @export
as_docinfo.xmp <- function(x, ...) {
    d <- docinfo()
    d$title <- x$title
    d$author <- paste(x$creator, collapse = " and ")
    d$subject <- x$description
    d$producer <- x$producer
    d$keywords <- x$keywords
    d$creation_date <- x$create_date
    d$creator <- x$creator_tool
    d$mod_date <- x$modify_date
    d
}
