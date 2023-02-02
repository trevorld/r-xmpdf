# SPDX-License-Identifier: MIT

#' XMP metadata object
#'
#' `xmp()` creates an XMP metadata object.
#' Such objects can be used with [set_xmp()] to edit XMP medata for a variety of media formats
#' and such objects are returned by [get_xmp()].
#'
#' @param attribution_name The name to be used when attributing the work (XMP tag `cc:attributionName`).
#'                Recommended by Creative Commons.
#'               If missing and `"cc:attributionName"` in `auto_xmp` and
#'               and `photoshop:Credit` non-missing will use that else if `dc:creator` non-missing
#'               then will automatically use `paste(creator, collapse = " and ")`.
#' @param attribution_url The URL to be used when attributing the work (XMP tag `cc:attributionURL`).
#'                Recommended by Creative Commons.
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
#' @param license The URL of (open source) license terms (XMP tag `cc:license`).
#'                Recommended by Creative Commons.
#'                Note `xmpRights:WebStatement` set in `web_statement` is a more popular XMP tag (e.g. used by Google Images)
#'                that can also hold the URL of license terms or a verifying web statement.
#'                If `cc:license` in `auto_xmp` and `spdx_id` is not `NULL` then
#'                we'll automatically use an URL from [spdx_licenses] corresponding to that license.
#' @param marked Whether the document is a rights-managed resource (XMP tag `xmpRights:Marked`).
#'               Use `TRUE` if rights-managed, `FALSE` if public domain, and `NULL` if unknown.
#'               Creative Commons recommends setting this.
#'               If `xmpRights:Marked` in `auto_xmp` and `spdx_id` is not `NULL` then
#'               we can automatically set this for a subset of SPDX licenses (including all Creative Commons licenses).
#' @param modify_date The date the document was last modified (XMP tag `xmp:ModifyDate`).
#'                 Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                 Related pdf documentation info key is `ModDate`.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#'                 Related pdf documentation info key is `pdf:Keywords`.
#'                 Will be coerced into a string by `paste(keywords, collapse = ", ")`.
#' @param producer The name of the application that converted the document to pdf (XMP tag `pdf:Producer`).
#'                Related pdf documentation info key is `Producer`.
#' @param rights (copy)right information about the document.
#'                Core IPTC photo metadata used by Google Photos that Creative Commons also recommends setting.
#' @param title The document's title (XMP tag `dc:title`).
#'                Related pdf documentation info key is `Title`.
#' @param web_statement Web Statement of Rights (XMP tag `xmpRights:WebStatement`):
#'                      a string of a full URL with license information about the page.
#'                      If `xmpRights:WebStatement` in `auto_xmp` and `spdx_id` is not `NULL` then
#'                      we'll automatically use an URL from [spdx_licenses] corresponding to that license.
#'                      Core IPTC photo metadata used by Google Photos.
#'                      Also recommended by Creative Commons (who also recommends using a "verifying" web statement).
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
#'    \item{`attribution_name`}{The name to attribute the document.}
#'    \item{`attribution_url`}{The URL to attribute the document.}
#'    \item{`creator`}{The document's author.}
#'    \item{`create_date`}{The date the document was created.}
#'    \item{`creator_tool`}{The name of the application that originally created the document.}
#'    \item{`credit`}{Credit line.}
#'    \item{`description`}{The document's description (subject).}
#'    \item{`keywords`}{Character vector of keywords for this document (for cross-document searching).}
#'    \item{`license`}{URL of (open-source) license terms the document is licensed under.}
#'    \item{`marked`}{Boolean of whether this is a rights-managed document.}
#'    \item{`modify_date`}{The date the document was last modified.}
#'    \item{`producer`}{The name of the application that converted the document (to pdf).}
#'    \item{`rights`}{The document's copy(right) information.}
#'    \item{`title`}{The document's title.}
#'    \item{`web_statement`}{A URL string for the web statement of rights for the document.}
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
                creator = NULL,  description = NULL, rights = NULL, title = NULL, # dc
                create_date = NULL, creator_tool = NULL, modify_date = NULL, # xmp
                marked = NULL, web_statement = NULL, # xmpRights
                attribution_name = NULL, attribution_url = NULL, license = NULL, # cc
                keywords = NULL, producer = NULL, # pdf
                credit = NULL, # photoshop
                spdx_id = NULL,
                auto_xmp = c("cc:attributionName", "cc:license", "photoshop:Credit",
                             "xmpRights:Marked", "xmpRights:WebStatement")) {
    Xmp$new(...,
            creator = creator,  description = description, rights = rights, title = title,
            keywords = keywords, producer = producer,
            attribution_name = attribution_name, attribution_url = attribution_url, license = license,
            credit = credit,
            create_date = create_date, creator_tool = creator_tool, modify_date = modify_date,
            marked = marked, web_statement = web_statement,
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
            if (lkey %in% c("attribution_name", "attributionname", "cc:attributionname")) {
                self$attribution_name
            } else if (lkey %in% c("attribution_url", "attributionurl", "cc:attributionurl")) {
                self$attribution_url
            } else if (lkey %in% c("license", "cc:license")) {
                self$license
            } else if (lkey %in% c("creator", "dc:creator")) {
                self$creator
            } else if (lkey %in% c("description", "dc:description")) {
                self$description
            } else if (lkey %in% c("rights", "dc:rights")) {
                self$rights
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
            } else if (lkey %in% c("marked", "xmprights:marked")) {
                self$marked
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
            if (lkey %in% c("attribution_name", "attributionname", "cc:attributionname")) {
                self$attribution_name <- value
            } else if (lkey %in% c("attribution_url", "attributionurl", "cc:attributionurl")) {
                self$attribution_url <- value
            } else if (lkey %in% c("license", "cc:license")) {
                self$license <- value
            } else if (lkey %in% c("creator", "dc:creator")) {
                self$creator <- value
            } else if (lkey %in% c("description", "dc:description")) {
                self$description <- value
            } else if (lkey %in% c("rights", "dc:rights")) {
                self$rights <- value
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
            } else if (lkey %in% c("marked", "xmprights:marked")) {
                self$marked <- value
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
            if (!is.null(self$attribution_name))
                tags[["XMP-cc:attributionName"]] <- self$attribution_name
            if (!is.null(self$attribution_url))
                tags[["XMP-cc:attributionURL"]] <- self$attribution_url
            if (!is.null(self$creator))
                tags[["XMP-dc:creator"]] <- self$creator
            if (!is.null(self$create_date))
                tags[["XMP-xmp:CreateDate"]] <- self$create_date
            if (!is.null(self$creator_tool))
                tags[["XMP-xmp:CreatorTool"]] <- self$creator_tool
            if (!is.null(self$credit))
                tags[["XMP-photoshop:Credit"]] <- self$credit
            if (!is.null(self$description))
                tags[["XMP-dc:description"]] <- self$description
            if (!is.null(self$keywords))
                tags[["XMP-pdf:Keywords"]] <- self$keywords
            if (!is.null(self$license))
                tags[["XMP-cc:license"]] <- self$license
            if (!is.null(self$modify_date))
                tags[["XMP-xmp:ModifyDate"]] <-  self$modify_date
            if (!is.null(self$marked))
                tags[["XMP-xmpRights:Marked"]] <- self$marked
            if (!is.null(self$producer))
                tags[["XMP-pdf:Producer"]] <- self$producer
            if (!is.null(self$rights))
                tags[["XMP-dc:rights"]] <- self$rights
            if (!is.null(self$title))
                tags[["XMP-dc:title"]] <- self$title
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
            if (!is.null(private$tags$attribution_name))
                keys <- append(keys, "cc:attributionName")
            if (!is.null(private$tags$attribution_url))
                keys <- append(keys, "cc:attributionURL")
            if (!is.null(private$tags$creator))
                keys <- append(keys, "dc:creator")
            if (!is.null(private$tags$description))
                keys <- append(keys, "dc:description")
            if (!is.null(private$tags$create_date))
                keys <- append(keys, "xmp:CreateDate")
            if (!is.null(private$tags$creator_tool))
                keys <- append(keys, "xmp:CreatorTool")
            if (!is.null(private$tags$producer))
                keys <- append(keys, "pdf:Producer")
            if (!is.null(private$tags$keywords))
                keys <- append(keys, "pdf:Keywords")
            if (!is.null(private$tags$license))
                keys <- append(keys, "cc:license")
            if (!is.null(private$tags$modify_date))
                keys <- append(keys, "xmp:ModifyDate")
            if (!is.null(private$tags$marked))
                keys <- append(keys, "xmpRights:Marked")
            if (!is.null(private$tags$rights))
                keys <- append(keys, "dc:rights")
            if (!is.null(private$tags$title))
                keys <- append(keys, "dc:title")
            if (!is.null(private$tags$web_statement))
                keys <- append(keys, "xmpRights:WebStatement")
            keys <- append(keys, names(private$tags$other))
            if (!is.null(self$spdx_id))
                keys <- append(keys, "spdx_id")
            keys
        }
    ),
    active = list(
        attribution_name = function(value) {
            if (missing(value)) {
                private$fill_credit(private$tags$attribution_name)
            } else {
                private$tags$attribution_name <- as_character_value(value)
            }
        },
        attribution_url = function(value) private$active_helper("attribution_url", value, as_url_value),
        creator = function(value) private$active_helper("creator", value),
        create_date = function(value) private$active_helper("create_date", value, as_datetime_value),
        creator_tool = function(value) private$active_helper("creator_tool", value),
        credit = function(value) {
            if (missing(value)) {
                private$fill_credit(NULL)
            } else {
                private$tags$credit <- as_character_value(value)
            }
        },
        description = function(value) private$active_helper("description", value),
        keywords = function(value) {
            if (missing(value))
                private$tags$keywords
            else
                private$tags$keywords <- paste(value, collapse = ", ")
        },
        license = function(value) {
            if (missing(value)) {
                value <- private$tags$license
                private$fill_spdx_url(value)
            } else {
                private$tags$license <- as_url_value(value)
            }
        },
        marked = function(value) {
            if (missing(value)) {
                value <- private$tags$marked
                if (is.null(value) && !is.null(private$tags$spdx_id)) {
                    value <- !xmpdf::spdx_licenses[self$spdx_id, "pd"]
                    if (is.na(value))
                        value <- NULL
                }
                value
            } else {
                private$tags$marked <- as_logical_value(value)
            }
        },
        modify_date = function(value) private$active_helper("modify_date", value, as_datetime_value),
        producer = function(value) private$active_helper("producer", value),
        rights = function(value) private$active_helper("rights", value),
        title = function(value) private$active_helper("title", value),
        web_statement = function(value) {
            if (missing(value)) {
                value <- private$tags$web_statement
                private$fill_spdx_url(value)
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
        auto_xmp = function(value) private$active_helper("auto_xmp", value)
    ),
    private = list(
        is_auto = function(key) {
            lkey <- tolower(key)
            if (lkey %in% c("attribution_name", "attributionname", "cc:attributionname")) {
                is.null(private$tags$attribution_name)
            } else if (lkey %in% c("credit", "photoshop:credit")) {
                is.null(private$tags$credit)
            } else if (lkey %in% c("license", "cc:license")) {
                is.null(private$tags$license)
            } else if (lkey %in% c("marked", "xmprights:marked")) {
                is.null(private$tags$marked)
            } else if (lkey %in% c("web_statement", "webstatement", "xmprights:webstatement")) {
                is.null(private$tags$web_statement)
            } else {
                FALSE
            }
        },
        active_helper = function(key, value, as_fn = as_character_value) {
            if (missing(value))
                private$tags[[key]]
            else
                private$tags[[key]] <- as_fn(value)
        },
        fill_credit = function(value) {
            if (is.null(value))
                value <- private$tags$credit
            if (is.null(value) && !is.null(private$tags$creator))
                value <- paste(private$tags$creator, collapse = " and ")
            value
        },
        fill_spdx_url = function(value) {
            if (is.null(value) && !is.null(private$tags$spdx_id)) {
                value <- xmpdf::spdx_licenses[self$spdx_id, "url_alt"]
                if (is.na(value))
                    value <- xmpdf::spdx_licenses[self$spdx_id, "url"]
            }
            value
        },
        tags = list(other = list())
    )
)

x_format <- d_format

KNOWN_XMP_TAGS <- c("cc:attributionName", "cc:attributionURL", "cc:license",
                    "dc:creator", "dc:description", "dc:rights", "dc:title",
                    "pdf:Keywords", "pdf:Producer",
                    "photoshop:Credit",
                    "xmp:CreateDate", "xmp:CreatorTool", "xmp:ModifyDate",
                    "xmpRights:Marked", "xmpRights:WebStatement")

#' @export
update.xmp <- function(object, ...) {
    x <- object$clone()
    x$update(as_xmp(list(...)))
    x
}

as_logical_value <- function(value) {
    if (is.null(value))
        NULL
    else
        as.logical(value)
}

#### Throw error if not a proper URL?
as_url_value <- function(value) {
    if (is.null(value))
        NULL
    else
        value
}
