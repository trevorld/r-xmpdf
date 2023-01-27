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
    l <- as.list(x)
    names(l) <- tolower(names(l))
    di <- docinfo()
    if (hasName(l, "dc:title"))
        di$title <- l[["dc:title"]]
    else if (hasName(l, "title"))
        di$title <- l[["title"]]

    if (hasName(l, "dc:creator"))
        di$author <- l[["dc:creator"]]
    else if (hasName(l, "creator"))
        di$creator <- l[["creator"]]

    if (hasName(l, "dc:description"))
        di$subject <- l[["dc:description"]]
    else if (hasName(l, "description"))
        di$subject <- l[["description"]]

    if (hasName(l, "pdf:producer"))
       di$producer <- l[["pdf:producer"]]
    else if (hasName(l, "producer"))
       di$producer <- l[["producer"]]

    if (hasName(l, "pdf:keywords"))
       di$keywords <- l[["pdf:keywords"]]
    else if (hasName(l, "keywords"))
       di$keywords <- l[["keywords"]]

    if (hasName(l, "xmp:createdate"))
        di$creation_date <- l[["xmp:createdate"]]
    else if (hasName(l, "createdate"))
        di$creation_date <- l[["createdate"]]

    if (hasName(l, "xmp:creatortool"))
        di$creator <- l[["xmp:creatortool"]]
    else if (hasName(l, "creatortool"))
        di$creator <- l[["creatortool"]]

    if (hasName(l, "xmp:modifydate"))
        di$creation_date <- l[["xmp:modifydate"]]
    else if (hasName(l, "modifydate"))
        di$creation_date <- l[["modifydate"]]

    di
}
