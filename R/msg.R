# SPDX-License-Identifier: MIT

#' Messages for how to enable feature
#'
#' `enable_feature_message()` returns a character vector with the information
#' needed to install the requested feature.
#' Formatted for use with [rlang::abort()], [rlang::warn()], or [rlang::inform()].
#'
#' @param feature Which `xmpdf` feature to give information for.
#' @return A character vector formatted for use with [rlang::abort()], [rlang::warn()], or [rlang::inform()].
#' @examples
#'   rlang::inform(enable_feature_message("get_bookmarks"))
#' @export
enable_feature_message <- function(feature = c("cat_pages", "get_bookmarks", "get_docinfo", "get_xmp",
                                               "n_pages", "set_bookmarks", "set_docinfo", "set_xmp")) {
    feature <- match.arg(feature)
    switch(feature,
           cat_pages = msg_cat_pages(),
           get_bookmarks = msg_get_bookmarks(),
           get_docinfo = msg_get_docinfo(),
           get_xmp = msg_get_xmp(),
           n_pages = msg_n_pages(),
           set_bookmarks = msg_set_bookmarks(),
           set_docinfo = msg_set_docinfo(),
           set_xmp = msg_set_xmp()
           )
}

msg_cat_pages <- function() {
    c(need_to_install_str("cat_pages()"),
      install_package_str("qpdf"),
      install_pdftk_str(),
      install_gs_str()
    )
}

msg_get_bookmarks <- function() {
    c(need_to_install_str("get_bookmarks()"),
      install_pdftk_str()
    )
}

msg_get_docinfo <- function() {
    c(need_to_install_str("get_docinfo()"),
      install_exiftool_str(),
      install_pdftk_str(),
      install_package_str("pdftools")
    )
}

msg_get_xmp <- function() {
    c(need_to_install_str("get_xmp()"),
      install_exiftool_str()
    )
}

msg_n_pages <- function() {
    c(need_to_install_str("n_pages()"),
      install_package_str("qpdf"),
      install_exiftool_str(),
      install_pdftk_str(),
      install_gs_str()
    )
}

msg_set_bookmarks <- function() {
    c(need_to_install_str("set_bookmarks()"),
      install_gs_str(),
      install_pdftk_str()
    )
}

msg_set_docinfo <- function() {
    c(need_to_install_str("set_docinfo()"),
      install_gs_str(),
      install_exiftool_str(),
      install_pdftk_str()
    )
}

msg_set_xmp <- function() {
    c(need_to_install_str("set_xmp()"),
      install_exiftool_str()
    )
}
