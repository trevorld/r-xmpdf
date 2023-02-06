# SPDX-License-Identifier: MIT

#' Coerce to XMP "language alternative" structure
#'
#' `as_lang_alt()` coerces to an XMP "language alternative" structure
#' suitable for use with [xmp()] objects.
#'
#' @param x Object suitable for coercing
#' @param ... Ignored
#' @param default_lang Language tag value to copy as the "x-default"
#' @seealso [xmp()], [as_xmp()], [get_xmp()], and [set_xmp()].
#'        For more information about the XMP "language alternative" structure see
#'        <https://github.com/adobe/xmp-docs/blob/master/XMPNamespaces/XMPDataTypes/CoreProperties.md#language-alternative>.
#' @return A named list of class "lang_alt".
#' @name as_lang_alt
#' @examples
#'   as_lang_alt("A single title")
#'   as_lang_alt(c(en = "An English Title", fr = "A French Title"))
#'   as_lang_alt(c(en = "An English Title", fr = "A French Title"), default_lang = "en")
#'   as_lang_alt(list(en = "An English Title", fr = "A French Title"))
NULL

#' @rdname as_lang_alt
#' @export
as_lang_alt <- function(x, ...) {
    UseMethod("as_lang_alt")
}

#' @rdname as_lang_alt
#' @export
as_lang_alt.character <- function(x, ..., default_lang = getOption("xmpdf_default_lang")) {
    if (is.null(names(x))) {
        stopifnot(length(x) == 1L)
        as_lang_alt.list(list(`x-default` = x))
    } else {
        as_lang_alt.list(as.list(x), default_lang = default_lang)
    }
}

#' @rdname as_lang_alt
#' @export
as_lang_alt.lang_alt <- function(x, ...) {
    x
}

#' @rdname as_lang_alt
#' @export
as_lang_alt.list <- function(x, ..., default_lang = getOption("xmpdf_default_lang")) {
    if (is.null(names(x))) {
        stopifnot(length(x) == 1L)
        as_lang_alt.list(list(`x-default` = x[[1]]))
    } else {
        stopifnot(all(sapply(x, is.character)),
                  sum(names(x) %in% c("", "x-default")) <= 1)
        names(x) <- ifelse(names(x) == "", "x-default", names(x))
        if (!is.null(default_lang) &&
            default_lang %in% names(x) &&
            !("x-default" %in% names(x))) {
            x <- c(list(`x-default` = x[[default_lang]]), x)
        }
        if ("x-default" %in% names(x) && names(x)[1] != "x-default") {
            i <- which(names(x) == "x-default")
            x <- c(x[i], x[-i])
        }
        class(x) <- "lang_alt"
        x
    }
}
