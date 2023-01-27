# SPDX-License-Identifier: MIT

#' XMP metadata object
#'
#' `xmp()` creates an XMP metadata object.
#' Such objects can be used with [set_xmp()] to edit XMP medata for a variety of media formats
#' and such objects are returned by [get_xmp()].
#'
#' @section XMP tag recommendations:
#'
#' * <https://exiftool.org/TagNames/XMP.html> recommends "dc", "xmp", "iptcCore", and "iptcExt" schemas if possible
#' * <https://www.iptc.org/std/photometadata/specification/IPTC-PhotoMetadata#xmp-namespaces-and-identifiers> is popular for photos
#' * <https://exiftool.org/TagNames/XMP.html#cc> are Creative Commons license recommendations
#'
#' @param creator The document's author(s) (XMP tag `dc:Creator`).
#'                Related pdf documentation info key is `Author`.
#' @param create_date The date the document was created (XMP tag `xmp:CreateDate`).
#'                Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                Related pdf documentation info key is `CreationDate`.
#' @param creator_tool The name of the application that originally created the document (if converted to pdf).
#'                Related pdf documentation info key is `xmp:CreatorTool`.
#' @param producer The name of the application that converted the document to pdf (XMP tag `pdf:Producer`).
#'                Related pdf documentation info key is `Producer`.
#' @param title The document's title (XMP tag `dc:Title`).
#'                Related pdf documentation info key is `Title`.
#' @param description The document's subject (XMP tag `dc:Description`).
#'                Related pdf documentation info key is `Subject`.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#'                 Related pdf documentation info key is `pdf:Keywords`.
#'                 Will be coerced into a string by `paste(keywords, collapse = ", ")`.
#' @param modify_date The date the document was last modified (XMP tag `xmp:ModifyDate`).
#'                 Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                 Related pdf documentation info key is `ModDate`.
#' @param spdx_id The id of a license in the SPDX license list.  See [spdx_licenses].
#' @param ... Entries of xmp metadata.  The names are either the xmp tag names or alternatively the xmp namespace and tag names separated by ":".  The values are the xmp values.
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
#'    \item{`description`}{The document's description (subject).}
#'    \item{`keywords`}{Character vector of keywords for this document (for cross-document searching).}
#'    \item{`modify_date`}{The date the document was last modified.}
#'    \item{`producer`}{The name of the application that converted the document (to pdf).}
#'    \item{`spdx_id`}{The id of a license in the SPDX license list.  See [spdx_licenses].}
#'    \item{`title`}{The document's title.}
#' }
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
                creator = NULL,  description = NULL, title = NULL,
                create_date = NULL, creator_tool = NULL, modify_date = NULL,
                keywords = NULL, producer = NULL,
                spdx_id = NULL) {
    Xmp$new(...,
            creator = creator,  description = description, title = title,
            create_date = create_date, creator_tool = creator_tool, modify_date = modify_date,
            keywords = keywords, producer = producer,
            spdx_id = spdx_id)
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
            if (!is.null(self$spdx_id))
                text <- append(text, paste("spdx_id (not XMP tag) :=", d_format(self$spdx_id)))
            if (!is.null(self$creator))
                text <- append(text, paste("dc:Creator :=", d_format(self$creator)))
            if (!is.null(self$description))
                text <- append(text, paste("dc:Description :=", d_format(self$description)))
            if (!is.null(self$title))
                text <- append(text, paste("dc:Title :=", d_format(self$title)))
            if (!is.null(self$keywords))
                text <- append(text, paste("pdf:Keywords :=",  d_format(self$keywords)))
            if (!is.null(self$producer))
                text <- append(text, paste("pdf:Producer :=", d_format(self$producer)))
            if (!is.null(self$create_date))
                text <- append(text, paste("xmp:CreateDate :=", d_format(self$create_date)))
            if (!is.null(self$creator_tool))
                text <- append(text, paste("xmp:CreatorTool :=", d_format(self$creator_tool)))
            if (!is.null(self$modify_date))
                text <- append(text, paste("xmp:ModifyDate :=", d_format(self$modify_date)))
            for (key in names(private$tags$other))
                text <- append(text, paste(key, ":=", d_format(private$tags$other[[key]])))
            if (length(text))
                invisible(cat(text, sep="\n"))
            else
                invisible(cat("No XMP metadata found\n"))
        },
        get_item = function(key) {
            if (key %in% c("creator", "Creator", "dc:Creator")) {
                self$creator
            } else if (key %in% c("description", "Description", "dc:Description")) {
                self$description
            } else if (key %in% c("title", "Title", "dc:Title")) {
                self$title
            } else if (key %in% c("producer", "Producer", "pdf:Producer")) {
                self$producer
            } else if (key %in% c("keywords", "Keywords", "pdf:Keywords")) {
                self$keywords
            } else if (key %in% c("create_date", "CreateDate", "xmp:CreateDate")) {
                self$create_date
            } else if (key %in% c("creator_tool", "CreatorTool", "xmp:CreatorTool")) {
                self$creator_tool
            } else if (key %in% c("modify_date", "ModifyDate", "xmp:ModifyDate")) {
                self$modify_date
            } else if (key %in% c("spdx_id")) {
                self$spdx_id
            } else {
                private$tags$other[[key]]
            }
        },
        set_item = function(key, value) {
            if (key %in% c("creator", "Creator", "dc:Creator")) {
                self$creator <- value
            } else if (key %in% c("description", "Description", "dc:Description")) {
                self$description <- value
            } else if (key %in% c("title", "Title", "dc:Title")) {
                self$title <- value
            } else if (key %in% c("producer", "Producer", "pdf:Producer")) {
                self$producer <- value
            } else if (key %in% c("keywords", "Keywords", "pdf:Keywords")) {
                self$keywords <- value
            } else if (key %in% c("create_date", "CreateDate", "xmp:CreateDate")) {
                self$create_date <- value
            } else if (key %in% c("creator_tool", "CreatorTool", "xmp:CreatorTool")) {
                self$creator_tool <- value
            } else if (key %in% c("modify_date", "ModifyDate", "xmp:ModifyDate")) {
                self$modify_date <- value
            } else if (key %in% c("spdx_id")) {
                self$spdx_id <- value
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
                tags[["XMP-dc:Creator"]] <- self$creator
            if (!is.null(self$create_date))
                tags[["XMP-xmp:CreateDate"]] <- self$create_date
            if (!is.null(self$creator))
                tags[["XMP-xmp:CreatorTool"]] <- self$creator_tool
            if (!is.null(self$producer))
                tags[["XMP-pdf:Producer"]] <- self$producer
            if (!is.null(self$title))
                tags[["XMP-dc:Title"]] <- self$title
            if (!is.null(self$description))
                tags[["XMP-dc:Description"]] <- self$description
            if (!is.null(self$keywords))
                tags[["XMP-pdf:Keywords"]] <- self$keywords
            if (!is.null(self$modify_date))
                tags[["XMP-xmp:ModifyDate"]] <-  self$modify_date
            for (key in names(private$tags$other)) {
                ekey <- paste0("XMP-", key)
                tags[[ekey]] <- private$tags$other[[key]] #### more formatting needed?
            }
            tags
        },
        get_nonnull_keys = function() {
            keys <- character(0)
            if (!is.null(private$tags$creator))
                keys <- append(keys, "dc:Creator")
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
            keys <- append(keys, names(private$tags$other))
            if (!is.null(self$spdx_id))
                keys <- append(keys, "spdx_id")
            keys
        }
    ),
    active = list(
        creator = function(value) {
            if (missing(value))
                private$tags$creator
            else
                private$tags$creator <- as_character_value(value) #### string+?
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
        spdx_id = function(value) {
            if (missing(value)) {
                private$tags$spdx_id
            } else {
                stopifnot(value %in% xmpdf::spdx_licenses$id)
                private$tags$spdx_id <- as_character_value(value)
            }
        },
        title = function(value) {
            if (missing(value))
                private$tags$title
            else
                private$tags$title <- as_character_value(value)
        }
    ),
    private = list(
        tags = list(other = list())
    )
)

#' @export
update.xmp <- function(object, ...) {
    x <- object$clone()
    x$update(as_xmp(list(...)))
    x
}
