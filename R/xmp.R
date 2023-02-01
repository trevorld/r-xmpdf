# SPDX-License-Identifier: MIT

#' XMP metadata object
#'
#' `xmp()` creates an XMP metadata object.
#' Such objects can be used with [set_xmp()] to edit XMP medata for a variety of media formats
#' and such objects are returned by [get_xmp()].
#'
#' @param create_date The date the document was created (XMP tag `xmp:CreateDate`).
#'                Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                Related pdf documentation info key is `CreationDate`.
#' @param creator The document's author(s) (XMP tag `dc:creator`).
#'                Related pdf documentation info key is `Author`.
#'                Core IPTC photo metadata used by Google Photos.
#'                If `credit` is missing and `"photoshop:Credit"` in `auto_xmp` then
#'                we'll also use this for the `photoshop:Credit` XMP tag.
#' @param creator_tool The name of the application that originally created the document.
#'                Related pdf documentation info key is `Creator`.
#' @param credit Credit line field (XMP tag `photoshop:Credit`).
#'               Core IPTC photo metadata used by Google Photos.
#'               If missing and `"photoshop:Credit"` in `auto_xmp` and `dc:creator` non-missing
#'               then will automatically use `paste(creator, collapse = " and ")`.
#' @param description The document's subject (XMP tag `dc:description`).
#'                Related pdf documentation info key is `Subject`.
#' @param modify_date The date the document was last modified (XMP tag `xmp:ModifyDate`).
#'                 Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                 Related pdf documentation info key is `ModDate`.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#'                 Related pdf documentation info key is `pdf:Keywords`.
#'                 Will be coerced into a string by `paste(keywords, collapse = ", ")`.
#' @param producer The name of the application that converted the document to pdf (XMP tag `pdf:Producer`).
#'                Related pdf documentation info key is `Producer`.
#' @param title The document's title (XMP tag `dc:title`).
#'                Related pdf documentation info key is `Title`.
#' @param web_statement Web Statement of Rights (XMP tag `xmpRights:WebStatement`):
#'                      a string of a full URL with license information about the page.
#'                      If `xmpRights:WebStatement` in `auto_xmp` and `spdx_id` is not `NULL` then
#'                      we'll automatically use an URL from [spdx_licenses] corresponding to that license.
#' @param ... Entries of xmp metadata.  The names are either the xmp tag names or alternatively the xmp namespace and tag names separated by ":".  The values are the xmp values.
#' @param spdx_id The id of a license in the SPDX license list.  See [spdx_licenses].
#' @param auto_xmp Character vector of XMP metadata we should try to automatically determine
#'                 if missing from other XMP metadata and `spdx_id`.
#' @return An xmp object as can be used with [set_xmp()].  Basically a named list whose names are the (optional) xmp namespace and tag names separated by ":" and the values are the xmp values.
#'         Datetimes should be a datetime object such as [POSIXlt()].
#' @seealso [get_xmp()] and [set_xmp()] for getting/setting such information from/to a variety of media file formats.
#'          [as_xmp()] for coercing to this object.
#'    [as_docinfo()] can be used to coerce `xmp()` objects into [docinfo()] objects.
#' @section `xmp` R6 Class Methods:\describe{
#'     \item{`get_item(key)`}{Get XMP metadata value for key `key`.
#'           Can also use the relevant active bindings to get more common values.}
#'     \item{`set_item(key, value)`}{Set XMP metadata key `key` with value `value`.
#'           Can also use the relevant active bindings to set XMP metadata values.}
#'     \item{`update(x)`}{Update XMP metadata entries
#'                        using non-`NULL` entries in `x` coerced by [as_xmp()].}
#' }
#' @section `xmp` R6 Active Bindings:\describe{
#'    \item{`creator`}{The document's author.}
#'    \item{`create_date`}{The date the document was created.}
#'    \item{`creator_tool`}{The name of the application that originally created the document.}
#'    \item{`credit`}{Credit line.}
#'    \item{`description`}{The document's description (subject).}
#'    \item{`keywords`}{Character vector of keywords for this document (for cross-document searching).}
#'    \item{`modify_date`}{The date the document was last modified.}
#'    \item{`producer`}{The name of the application that converted the document (to pdf).}
#'    \item{`title`}{The document's title.}
#'    \item{`web_statement`}{Web statement of rights for the document}
#'    \item{`spdx_id`}{The id of a license in the SPDX license list.  See [spdx_licenses].}
#'    \item{`auto_xmp`}{Character vector of XMP metadata we should try to automatically determine
#'                      if missing from other XMP metadata and `spdx_id`.}
#' }
#'
#' @section XMP tag recommendations:
#'
#' * <https://exiftool.org/TagNames/XMP.html> recommends "dc", "xmp", "iptcCore", and "iptcExt" schemas if possible
#' * <https://github.com/adobe/xmp-docs/tree/master/XMPNamespaces> are descriptions of some common XMP tags
#' * <https://www.iptc.org/std/photometadata/specification/IPTC-PhotoMetadata#xmp-namespaces-and-identifiers> is popular for photos
#' * <https://developers.google.com/search/docs/appearance/structured-data/image-license-metadata#iptc-photo-metadata> are the subset of IPTC photo metadata which Google Photos uses (if no structured data on web page)
#' * <https://wiki.creativecommons.org/wiki/XMP> are Creative Commons license recommendations
#'
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
xmp <- function(...,
                creator = NULL,  description = NULL, title = NULL, # dc
                create_date = NULL, creator_tool = NULL, modify_date = NULL, # xmp
                keywords = NULL, producer = NULL, # pdf
                credit = NULL, # photoshop
                spdx_id = NULL,
                auto_xmp = c("photoshop:Credit", "xmpRights:WebStatement")) {
    Xmp$new(...,
            creator = creator,  description = description, title = title,
            create_date = create_date, creator_tool = creator_tool, modify_date = modify_date,
            keywords = keywords, producer = producer,
            credit = credit,
            spdx_id = spdx_id, auto_xmp = auto_xmp)
}

Xmp <- R6Class("xmp",
    public = list(
        initialize = function(...) {
            l <- list(...)
            for (key in names(l)) {
                value <- l[[key]]
                if (!is.null(value))
                    self$set_item(key, l[[key]])
            }
            invisible(NULL)
        },
        print = function() {
            text <- character(0)
            for (key in KNOWN_XMP_TAGS) {
                value <- self$get_item(key)
                if (is.null(value)) next
                if (private$is_auto(key))
                    text <- append(text, paste("=>", key, "=", x_format(value)))
                else
                    text <- append(text, paste(key, ":=", x_format(value)))
            }
            if (length(text)) {
                if (length(self$auto_xmp))
                    text <- c(paste("auto_xmp (not XMP tag): ", x_format(self$auto_xmp)), text)
                if (!is.null(self$spdx_id))
                    text <- c(paste("spdx_id (not XMP tag) :=", x_format(self$spdx_id)), text)
                invisible(cat(text, sep="\n"))
            } else {
                invisible(cat("No XMP metadata found\n"))
            }
        },
        get_item = function(key) {
            lkey <- tolower(key)
            if (lkey %in% c("creator", "dc:creator")) {
                self$creator
            } else if (lkey %in% c("description", "dc:description")) {
                self$description
            } else if (lkey %in% c("title", "dc:title")) {
                self$title
            } else if (lkey %in% c("producer", "pdf:producer")) {
                self$producer
            } else if (lkey %in% c("keywords", "pdf:keywords")) {
                self$keywords
            } else if (lkey %in% c("credit", "credit", "photoshop:credit")) {
                self$credit
            } else if (lkey %in% c("create_date", "createdate", "xmp:createdate")) {
                self$create_date
            } else if (lkey %in% c("creator_tool", "creatortool", "xmp:creatortool")) {
                self$creator_tool
            } else if (lkey %in% c("modify_date", "modifydate", "xmp:modifydate")) {
                self$modify_date
            } else if (lkey %in% c("web_statement", "webstatement", "xmprights:webstatement")) {
                self$web_statement
            } else if (lkey %in% c("spdx_id")) {
                self$spdx_id
            } else if (lkey %in% c("auto_xmp")) {
                self$auto_xmp
            } else {
                private$tags$other[[key]]
            }
        },
        set_item = function(key, value) {
            lkey <- tolower(key)
            if (lkey %in% c("creator", "dc:creator")) {
                self$creator <- value
            } else if (lkey %in% c("description", "dc:description")) {
                self$description <- value
            } else if (lkey %in% c("title", "dc:title")) {
                self$title <- value
            } else if (lkey %in% c("producer", "pdf:producer")) {
                self$producer <- value
            } else if (lkey %in% c("keywords", "pdf:keywords")) {
                self$keywords <- value
            } else if (lkey %in% c("credit", "photoshop:credit")) {
                self$credit <- value
            } else if (lkey %in% c("create_date", "createdate", "xmp:createdate")) {
                self$create_date <- value
            } else if (lkey %in% c("creator_tool", "creatortool", "xmp:creatortool")) {
                self$creator_tool <- value
            } else if (lkey %in% c("modify_date", "modifydate", "xmp:modifydate")) {
                self$modify_date <- value
            } else if (lkey %in% c("web_statement", "webstatement", "xmprights:webstatement")) {
                self$web_statement <- value
            } else if (lkey %in% c("spdx_id")) {
                self$spdx_id <- value
            } else if (lkey %in% c("auto_xmp")) {
                self$auto_xmp <- value
            } else {
                private$tags$other[[key]] <- value
            }
        },
        update = function(x) {
            x <- as_xmp(x)
            for (key in x$get_nonnull_keys())
                self$set_item(key, x$get_item(key))
            invisible(NULL)
        },
        exiftool_tags = function() {
            tags <- list()
            if (!is.null(self$creator))
                tags[["XMP-dc:creator"]] <- self$creator
            if (!is.null(self$create_date))
                tags[["XMP-xmp:CreateDate"]] <- self$create_date
            if (!is.null(self$creator))
                tags[["XMP-xmp:CreatorTool"]] <- self$creator_tool
            if (!is.null(self$credit))
                tags[["XMP-photoshop:Credit"]] <- self$credit
            if (!is.null(self$producer))
                tags[["XMP-pdf:Producer"]] <- self$producer
            if (!is.null(self$title))
                tags[["XMP-dc:title"]] <- self$title
            if (!is.null(self$description))
                tags[["XMP-dc:description"]] <- self$description
            if (!is.null(self$keywords))
                tags[["XMP-pdf:Keywords"]] <- self$keywords
            if (!is.null(self$modify_date))
                tags[["XMP-xmp:ModifyDate"]] <-  self$modify_date
            if (!is.null(self$web_statement))
                tags[["XMP-xmpRights:WebStatement"]] <- self$web_statement
            for (key in names(private$tags$other)) {
                ekey <- paste0("XMP-", key)
                tags[[ekey]] <- private$tags$other[[key]] #### more formatting needed?
            }
            tags
        },
        get_nonnull_keys = function() {
            keys <- character(0)
            if (!is.null(private$tags$creator))
                keys <- append(keys, "dc:creator")
            if (!is.null(private$tags$create_date))
                keys <- append(keys, "xmp:CreateDate")
            if (!is.null(private$tags$creator_tool))
                keys <- append(keys, "xmp:CreatorTool")
            if (!is.null(private$tags$producer))
                keys <- append(keys, "pdf:Producer")
            if (!is.null(private$tags$title))
                keys <- append(keys, "dc:Title")
            if (!is.null(private$tags$description))
                keys <- append(keys, "dc:Description")
            if (!is.null(private$tags$keywords))
                keys <- append(keys, "pdf:Keywords")
            if (!is.null(private$tags$modify_date))
                keys <- append(keys, "xmp:ModifyDate")
            if (!is.null(private$tags$web_statement))
                keys <- append(keys, "xmpRights:WebStatement")
            keys <- append(keys, names(private$tags$other))
            if (!is.null(self$spdx_id))
                keys <- append(keys, "spdx_id")
            keys
        }
    ),
    active = list(
        creator = function(value) {
            if (missing(value)) {
                private$tags$creator
            } else {
                private$tags$creator <- as_character_value(value) #### string+?
            }
        },
        create_date = function(value) {
            if (missing(value))
                private$tags$create_date
            else
                private$tags$create_date <- as_datetime_value(value)
        },
        creator_tool = function(value) {
            if (missing(value))
                private$tags$creator_tool
            else
                private$tags$creator_tool <- as_character_value(value)
        },
        credit = function(value) {
            if (missing(value)) {
                value <- private$tags$credit
                if (is.null(value) && !is.null(private$tags$creator))
                    value <- paste(private$tags$creator, collapse = " and ")
                value
            } else {
                private$tags$credit <- as_character_value(value)
            }
        },
        description = function(value) {
            if (missing(value))
                private$tags$description
            else
                private$tags$description <- as_character_value(value)
        },
        keywords = function(value) {
            if (missing(value))
                private$tags$keywords
            else
                private$tags$keywords <- paste(value, collapse = ", ")
        },
        modify_date = function(value) {
            if (missing(value))
                private$tags$modify_date
            else
                private$tags$modify_date <- as_datetime_value(value)
        },
        producer = function(value) {
            if (missing(value))
                private$tags$producer
            else
                private$tags$producer <- as_character_value(value)
        },
        title = function(value) {
            if (missing(value))
                private$tags$title
            else
                private$tags$title <- as_character_value(value)
        },
        web_statement = function(value) {
            if (missing(value)) {
                value <- private$tags$web_statement
                if (is.null(value) && !is.null(private$tags$spdx_id)) {
                    value <- xmpdf::spdx_licenses[self$spdx_id, "url_alt"]
                    if (is.na(value))
                        value <- xmpdf::spdx_licenses[self$spdx_id, "url"]
                }
                value
            } else {
                private$tags$web_statement <- as_url_value(value)
            }
        },
        spdx_id = function(value) {
            if (missing(value)) {
                private$tags$spdx_id
            } else {
                stopifnot(value %in% xmpdf::spdx_licenses$id)
                private$tags$spdx_id <- as_character_value(value)
            }
        },
        auto_xmp = function(value) {
            if (missing(value)) {
                private$tags$auto_xmp
            } else {
                private$tags$auto_xmp <- value
            }
        }
    ),
    private = list(
        is_auto = function(key) {
            lkey <- tolower(key)
            if (lkey %in% c("credit", "photoshop:credit")) {
                is.null(private$tags$credit)
            } else if (lkey %in% c("web_statement", "webstatement", "xmprights:webstatement")) {
                is.null(private$tags$web_statement)
            } else {
                FALSE
            }
        },
        tags = list(other = list())
    )
)

x_format <- d_format

KNOWN_XMP_TAGS <- c("dc:creator", "dc:description", "dc:title",
                    "pdf:Keywords", "pdf:Producer",
                    "photoshop:Credit",
                    "xmp:CreateDate", "xmp:CreatorTool", "xmp:ModifyDate",
                    "xmpRights:WebStatement")

#' @export
update.xmp <- function(object, ...) {
    x <- object$clone()
    x$update(as_xmp(list(...)))
    x
}

#### Throw error if not a proper URL?
as_url_value <- function(value) {
    if (is.null(value))
        NULL
    else
        value
}
