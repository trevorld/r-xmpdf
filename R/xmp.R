# SPDX-License-Identifier: MIT

#' XMP metadata object
#'
#' `xmp()` creates an XMP metadata object.
#' Such objects can be used with [set_xmp()] to edit XMP medata for a variety of media formats
#' and such objects are returned by [get_xmp()].
#'
#' @param alt_text Brief textual description that can be used as its "alt text" (XMP tag `Iptc4xmpCore:AltTextAccessibility`).
#'                Will be coerced by [as_lang_alt()].
#'                Core IPTC photo metadata.
#' @param attribution_name The name to be used when attributing the work (XMP tag `cc:attributionName`).
#'                Recommended by Creative Commons.
#'               If missing and `"cc:attributionName"` in `auto_xmp` and
#'               and `photoshop:Credit` non-missing will use that else if `dc:creator` non-missing
#'               then will automatically use `paste(creator, collapse = " and ")`.
#' @param attribution_url The URL to be used when attributing the work (XMP tag `cc:attributionURL`).
#'                Recommended by Creative Commons.
#' @param create_date The date the digital document was created (XMP tag `xmp:CreateDate`).
#'                Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                Related pdf documentation info key is `CreationDate`.
#'                Not to be confused with `photoshop:DateCreated` which is the
#'                date the intellectual content was created.
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
#' @param date_created The date the intellectual content was created (XMP tag `photoshop:DateCreated`).
#'               Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'               Core IPTC photo metadata.
#'               Not to be confused with `xmp:CreateDate` for when the digital document was created.
#' @param description The document's subject (XMP tag `dc:description`).
#'                Will be coerced by [as_lang_alt()].
#'                Core IPTC photo metadata.
#'                Related pdf documentation info key is `Subject`.
#' @param ext_description An extended description (for accessibility)
#'                if the "alt text" is insufficient (XMP tag `Iptc4xmpCore:ExtDescrAccessibility`).
#'                Will be coerced by [as_lang_alt()].
#'                Core IPTC photo metadata.
#' @param headline A short synopsis of the document (XMP tag `photoshop:Headline`).
#'                Core IPTC photo metadata.
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
#' @param more_permissions A URL for additional permissions beyond the `license` (XMP tag `cc:license`).
#'                 Recommended by Creative Commons.
#'                 Contrast with the `LicensorURL` property of `plus:Licensor` XMP tag.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#'                 Related pdf documentation info key is `pdf:Keywords`.
#'                 Will be coerced into a string by `paste(keywords, collapse = ", ")`.
#' @param producer The name of the application that converted the document to pdf (XMP tag `pdf:Producer`).
#'                Related pdf documentation info key is `Producer`.
#' @param rights (copy)right information about the document (XMP tag `dc:rights`).
#'                Will be coerced by [as_lang_alt()].
#'                Core IPTC photo metadata used by Google Photos that Creative Commons also recommends setting.
#'                If `dc:rights` in `auto_xmp` and `creator` and `date_created` are not `NULL` then
#'                we can automatically generate a basic copyright statement with the help of `spdx_id`.
#' @param subject List of description phrases, keywords, classification codes (XMP tag `dc:subject`).
#'                Core IPTC photo metadata.
#'                A character vector.
#'                Similar but less popular to the XMP tag `pdf:Keywords` which is a string.
#'                If `dc:subject` in `auto_xmp` and `keywords` is not NULL then we can
#'                automatically extract the keywords from it using `strsplit(keywords, ", ")[[1]]`.
#' @param title The document's title (XMP tag `dc:title`).
#'              Will be coerced by [as_lang_alt()].
#'              Related pdf documentation info key is `Title`.
#' @param usage_terms A string describing legal terms of use for the document (XMP tag `xmpRights:UsageTerms`).
#'                    Will be coerced by [as_lang_alt()].
#'                    Core IPTC photo metadata and recommended by Creative Commons.
#'                    If `xmpRights:UsageTerms` in `auto_xmp` and `spdx_id` is not `NULL` then
#'                    we can automatically set this with that license's name and URL.
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
#'     \item{`fig_process(..., auto = c("fig.alt", "fig.cap", "fig.scap"))`}{
#'           Returns a function to embed XMP metadata suitable for use with
#'           `{knitr}`'s `fig.process` chunk option.
#'           `...` are local XMP metadata changes for this function.
#'           `auto` are which chunk options should be used to further update metadata values.}
#'     \item{`get_item(key)`}{Get XMP metadata value for key `key`.
#'           Can also use the relevant active bindings to get more common values.}
#'     \item{`print(mode = c("null_omit", "google_images", "creative_commons", "all"), xmp_only = FALSE)`}{
#'           Print out XMP metadata values.  If `mode` is "null_omit" print out
#'           which metadata would be embedded.  If `mode` is "google images" print out
#'           values for the five fields Google Images uses.  If `mode` is `creative_commons`
#'           print out the values for the fields Creative Commons recommends be set when
#'           using their licenses.  If mode is `all` print out values for all
#'           XMP metadata that we provide active bindings for (even if `NULL`).
#'           If `xmp_only` is `TRUE` then don't print out `spdx_id` and `auto_xmp` values.}
#'     \item{`set_item(key, value)`}{Set XMP metadata key `key` with value `value`.
#'           Can also use the relevant active bindings to set XMP metadata values.}
#'     \item{`update(x)`}{Update XMP metadata entries
#'                        using non-`NULL` entries in `x` coerced by [as_xmp()].}
#' }
#' @section `xmp` R6 Active Bindings:\describe{
#'    \item{`alt_text`}{The image's alt text (accessibility).}
#'    \item{`attribution_name`}{The name to attribute the document.}
#'    \item{`attribution_url`}{The URL to attribute the document.}
#'    \item{`creator`}{The document's author.}
#'    \item{`create_date`}{The date the document was created.}
#'    \item{`creator_tool`}{The name of the application that originally created the document.}
#'    \item{`credit`}{Credit line.}
#'    \item{`date_created`}{The date the document's intellectual content was created}
#'    \item{`description`}{The document's description.}
#'    \item{`ext_description`}{An extended description for accessibility.}
#'    \item{`headline`}{A short synopsis of document.}
#'    \item{`keywords`}{String of keywords for this document (less popular than `subject`)).}
#'    \item{`license`}{URL of (open-source) license terms the document is licensed under.}
#'    \item{`marked`}{Boolean of whether this is a rights-managed document.}
#'    \item{`modify_date`}{The date the document was last modified.}
#'    \item{`more_permissions`}{URL for acquiring additional permissions beyond `license`.}
#'    \item{`producer`}{The name of the application that converted the document (to pdf).}
#'    \item{`rights`}{The document's copy(right) information.}
#'    \item{`subject`}{Vector of key phrases/words/codes for this document (more popular than `keywords`)).}
#'    \item{`title`}{The document's title.}
#'    \item{`usage_terms`}{The document's rights usage terms.}
#'    \item{`web_statement`}{A URL string for the web statement of rights for the document.}
#'    \item{`spdx_id`}{The id of a license in the SPDX license list.  See [spdx_licenses].}
#'    \item{`auto_xmp`}{Character vector of XMP metadata we should try to automatically determine
#'                      if missing from other XMP metadata and `spdx_id`.}
#' }
#'
#' @section XMP tag recommendations:
#'
#' * <https://exiftool.org/TagNames/XMP.html> recommends "dc", "xmp", "Iptc4xmpCore", and "Iptc4xmpExt" schemas if possible
#' * <https://github.com/adobe/xmp-docs/tree/master/XMPNamespaces> are descriptions of some common XMP tags
#' * <https://www.iptc.org/std/photometadata/specification/IPTC-PhotoMetadata#xmp-namespaces-and-identifiers> is popular for photos
#' * <https://developers.google.com/search/docs/appearance/structured-data/image-license-metadata#iptc-photo-metadata> are the subset of IPTC photo metadata which Google Photos uses (if no structured data on web page)
#' * <https://wiki.creativecommons.org/wiki/XMP> are Creative Commons license recommendations
#'
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
#' @name xmp
#' @export
xmp <- function(...,
                alt_text = NULL,
                attribution_name = NULL,
                attribution_url = NULL,
                create_date = NULL,
                creator = NULL,
                creator_tool = NULL,
                credit = NULL,
                date_created = NULL,
                description = NULL,
                ext_description = NULL,
                headline = NULL,
                keywords = NULL,
                license = NULL,
                marked = NULL,
                modify_date = NULL,
                more_permissions = NULL,
                producer = NULL,
                rights = NULL,
                subject = NULL,
                title = NULL,
                usage_terms = NULL,
                web_statement = NULL,
                auto_xmp = c("cc:attributionName", "cc:license",
                             "dc:rights", "dc:subject",
                             "photoshop:Credit",
                             "xmpRights:Marked", "xmpRights:UsageTerms", "xmpRights:WebStatement"),
                spdx_id = NULL) {
    Xmp$new(...,
            creator = creator,  description = description, rights = rights,
            subject = subject, title = title, # dc
            keywords = keywords, producer = producer, # pdf
            attribution_name = attribution_name, attribution_url = attribution_url, #cc
            license = license, more_permissions = more_permissions,
            alt_text = alt_text, ext_description = ext_description, # Iptc4xmpCore
            credit = credit, date_created = date_created, headline = headline, # photoshop
            create_date = create_date, creator_tool = creator_tool, modify_date = modify_date, # xmp
            marked = marked, usage_terms = usage_terms, web_statement = web_statement, # xmpRights
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
        print = function(mode = c("null_omit", "google_images", "creative_commons", "all"), xmp_only = FALSE) {
            mode <- match.arg(mode)
            text <- character(0)
            tags <- switch(mode,
                           google_images = c("dc:creator",
                                             "dc:rights",
                                             "photoshop:Credit",
                                             "xmpRights:WebStatement"),
                           creative_commons = c("cc:attributionName",
                                                "cc:attributionURL",
                                                "cc:license",
                                                "cc:morePermissions",
                                                "dc:rights",
                                                "xmpRights:Marked",
                                                "xmpRights:UsageTerms",
                                                "xmpRights:WebStatement"),
                           sort(c(KNOWN_XMP_TAGS, names(private$tags$other))))
            for (key in tags) {
                value <- self$get_item(key)
                if (is.null(value) && mode == "null_omit") next
                if (private$is_auto(key))
                    text <- append(text, paste("=>", key, "=", x_format(value)))
                else
                    text <- append(text, paste("  ", key, ":=", x_format(value)))
            }
            if (length(text)) {
                if (mode == "google_images") {
                    text <- c(text[1:3], "X  plus:Licensor (not currently supported by {xmpdf})", text[4])
                }
                if (!xmp_only) {
                    if (length(self$auto_xmp))
                        text <- c(paste("i  auto_xmp (not XMP tag) := ", x_format(self$auto_xmp)), text)
                    if (!is.null(self$spdx_id))
                        text <- c(paste("i  spdx_id (not XMP tag) :=", x_format(self$spdx_id)), text)
                }
                invisible(cat(text, sep="\n"))
            } else {
                invisible(cat("No XMP metadata found\n"))
            }
        },
        fig_process = function(..., auto = c("fig.alt", "fig.cap", "fig.scap")) {
            x <- update(self, ...)
            function(path, options) {
                if ("fig.alt" %in% auto && !is.null(options[["fig.alt"]]))
                    x$alt_text <- options[["fig.alt"]]
                if ("fig.cap" %in% auto && !is.null(options[["fig.cap"]]))
                    x$description <- options[["fig.cap"]]
                if ("fig.scap" %in% auto && !is.null(options[["fig.scap"]]))
                    x$headline <- options[["fig.scap"]]
                xmpdf::set_xmp(x, path)
                invisible(path)
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
            } else if (lkey %in% c("subject", "dc:subject")) {
                self$subject
            } else if (lkey %in% c("rights", "dc:rights")) {
                self$rights
            } else if (lkey %in% c("title", "dc:title")) {
                self$title
            } else if (lkey %in% c("alt_text", "alttextaccessibility",
                            "iptccore:alttextaccessibility", "iptc4xmpcore:alttextaccessibility")) {
                self$alt_text
            } else if (lkey %in% c("ext_description", "extdescraccessibility",
                                   "iptccore:extdescraccessibility",
                                   "iptc4xmpcore:extdescraccessibility")) {
                self$ext_description
            } else if (lkey %in% c("producer", "pdf:producer")) {
                self$producer
            } else if (lkey %in% c("keywords", "pdf:keywords")) {
                self$keywords
            } else if (lkey %in% c("credit", "photoshop:credit")) {
                self$credit
            } else if (lkey %in% c("date_created", "datecreated", "photoshop:datecreated")) {
                self$date_created
            } else if (lkey %in% c("headline", "photoshop:headline")) {
                self$headline
            } else if (lkey %in% c("create_date", "createdate", "xmp:createdate")) {
                self$create_date
            } else if (lkey %in% c("creator_tool", "creatortool", "xmp:creatortool")) {
                self$creator_tool
            } else if (lkey %in% c("modify_date", "modifydate", "xmp:modifydate")) {
                self$modify_date
            } else if (lkey %in% c("marked", "xmprights:marked")) {
                self$marked
            } else if (lkey %in% c("more_permissions", "morepermissions", "cc:morepermissions")) {
                self$more_permissions
            } else if (lkey %in% c("usage_terms", "usageterms", "xmprights:usageterms")) {
                self$usage_terms
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
            } else if (lkey %in% c("more_permissions", "morepermissions", "cc:morepermissions")) {
                self$more_permissions <- value
            } else if (lkey %in% c("creator", "dc:creator")) {
                self$creator <- value
            } else if (lkey %in% c("description", "dc:description")) {
                self$description <- value
            } else if (lkey %in% c("rights", "dc:rights")) {
                self$rights <- value
            } else if (lkey %in% c("subject", "dc:subject")) {
                self$subject <- value
            } else if (lkey %in% c("title", "dc:title")) {
                self$title <- value
            } else if (lkey %in% c("alt_text", "alttextaccessibility", "iptc4xmpcore:alttextaccessibility")) {
                self$alt_text <- value
            } else if (lkey %in% c("ext_description", "extdescraccessibility",
                                   "iptccore:extdescraccessibility",
                                   "iptc4xmpcore:extdescraccessibility")) {
                self$ext_description <- value
            } else if (lkey %in% c("producer", "pdf:producer")) {
                self$producer <- value
            } else if (lkey %in% c("keywords", "pdf:keywords")) {
                self$keywords <- value
            } else if (lkey %in% c("credit", "photoshop:credit")) {
                self$credit <- value
            } else if (lkey %in% c("date_created", "datecreated", "photoshop:datecreated")) {
                self$date_created <- value
            } else if (lkey %in% c("headline", "photoshop:headline")) {
                self$headline <- value
            } else if (lkey %in% c("create_date", "createdate", "xmp:createdate")) {
                self$create_date <- value
            } else if (lkey %in% c("creator_tool", "creatortool", "xmp:creatortool")) {
                self$creator_tool <- value
            } else if (lkey %in% c("modify_date", "modifydate", "xmp:modifydate")) {
                self$modify_date <- value
            } else if (lkey %in% c("marked", "xmprights:marked")) {
                self$marked <- value
            } else if (lkey %in% c("usage_terms", "usageterms", "xmprights:usageterms")) {
                self$usage_terms <- value
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
            if (!is.null(self$alt_text))
                tags[["XMP-iptcCore:AltTextAccessibility"]] <- self$alt_text
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
            if (!is.null(self$date_created))
                tags[["XMP-photoshop:DateCreated"]] <- self$date_created
            if (!is.null(self$headline))
                tags[["XMP-photoshop:Headline"]] <- self$headline
            if (!is.null(self$description))
                tags[["XMP-dc:description"]] <- self$description
            if (!is.null(self$ext_description))
                tags[["XMP-iptcCore:ExtDescrAccessibility"]] <- self$ext_description
            if (!is.null(self$keywords))
                tags[["XMP-pdf:Keywords"]] <- self$keywords
            if (!is.null(self$license))
                tags[["XMP-cc:license"]] <- self$license
            if (!is.null(self$marked))
                tags[["XMP-xmpRights:Marked"]] <- self$marked
            if (!is.null(self$modify_date))
                tags[["XMP-xmp:ModifyDate"]] <-  self$modify_date
            if (!is.null(self$more_permissions))
                tags[["XMP-cc:morePermissions"]] <- self$more_permissions
            if (!is.null(self$producer))
                tags[["XMP-pdf:Producer"]] <- self$producer
            if (!is.null(self$rights))
                tags[["XMP-dc:rights"]] <- self$rights
            if (!is.null(self$subject))
                tags[["XMP-dc:subject"]] <- self$subject
            if (!is.null(self$title))
                tags[["XMP-dc:title"]] <- self$title
            if (!is.null(self$usage_terms))
                tags[["XMP-xmpRights:UsageTerms"]] <- self$usage_terms
            if (!is.null(self$web_statement))
                tags[["XMP-xmpRights:WebStatement"]] <- self$web_statement
            for (key in names(private$tags$other)) {
                ekey <- gsub("Iptc4xmp", "iptc", key)
                ekey <- paste0("XMP-", ekey)
                tags[[ekey]] <- private$tags$other[[key]] #### more formatting needed?
            }
            tags
        },
        get_nonnull_keys = function() {
            keys <- character(0)
            if (!is.null(private$tags$alt_text))
                keys <- append(keys, "Iptc4xmpCore:AltTextAccessibility")
            if (!is.null(private$tags$attribution_name))
                keys <- append(keys, "cc:attributionName")
            if (!is.null(private$tags$attribution_url))
                keys <- append(keys, "cc:attributionURL")
            if (!is.null(private$tags$creator))
                keys <- append(keys, "dc:creator")
            if (!is.null(private$tags$create_date))
                keys <- append(keys, "xmp:CreateDate")
            if (!is.null(private$tags$creator_tool))
                keys <- append(keys, "xmp:CreatorTool")
            if (!is.null(private$tags$date_created))
                keys <- append(keys, "photoshop:DateCreated")
            if (!is.null(private$tags$description))
                keys <- append(keys, "dc:description")
            if (!is.null(private$tags$ext_description))
                keys <- append(keys, "Iptc4xmpCore:ExtDescrAccessibility")
            if (!is.null(private$tags$headline))
                keys <- append(keys, "photoshop:Headline")
            if (!is.null(private$tags$keywords))
                keys <- append(keys, "pdf:Keywords")
            if (!is.null(private$tags$license))
                keys <- append(keys, "cc:license")
            if (!is.null(private$tags$marked))
                keys <- append(keys, "xmpRights:Marked")
            if (!is.null(private$tags$modify_date))
                keys <- append(keys, "xmp:ModifyDate")
            if (!is.null(private$tags$more_permissions))
                keys <- append(keys, "cc:morePermissions")
            if (!is.null(private$tags$producer))
                keys <- append(keys, "pdf:Producer")
            if (!is.null(private$tags$rights))
                keys <- append(keys, "dc:rights")
            if (!is.null(private$tags$subject))
                keys <- append(keys, "dc:subject")
            if (!is.null(private$tags$title))
                keys <- append(keys, "dc:title")
            if (!is.null(private$tags$usage_terms))
                keys <- append(keys, "xmpRights:UsageTerms")
            if (!is.null(private$tags$web_statement))
                keys <- append(keys, "xmpRights:WebStatement")
            keys <- append(keys, names(private$tags$other))
            if (!is.null(self$spdx_id))
                keys <- append(keys, "spdx_id")
            keys
        }
    ),
    active = list(
        alt_text = function(value) private$active_helper("alt_text", value, as_la_value),
        attribution_name = function(value) {
            if (missing(value)) {
                value <- private$tags$attribution_name
                if (is.null(value) && "cc:attributionName" %in% self$auto_xmp)
                    value <- private$auto_credit()
                value
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
                value <- private$tags$credit
                if (is.null(value) && "photoshop:Credit" %in% self$auto_xmp)
                    value <- private$auto_credit()
                value
            } else {
                private$tags$credit <- as_character_value(value)
            }
        },
        date_created = function(value) private$active_helper("date_created", value, as_datetime_value),
        description = function(value) private$active_helper("description", value, as_la_value),
        ext_description = function(value) private$active_helper("ext_description", value, as_la_value),
        headline = function(value) private$active_helper("headline", value),
        keywords = function(value) {
            if (missing(value))
                private$tags$keywords
            else
                private$tags$keywords <- paste(value, collapse = ", ")
        },
        license = function(value) {
            if (missing(value)) {
                value <- private$tags$license
                if (is.null(value) && "cc:license" %in% self$auto_xmp)
                    value <- private$auto_spdx_url()
                value
            } else {
                private$tags$license <- as_url_value(value)
            }
        },
        marked = function(value) {
            if (missing(value)) {
                value <- private$tags$marked
                if (is.null(value) &&
                    "xmpRights:Marked" %in% self$auto_xmp &&
                    !is.null(private$tags$spdx_id)) {
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
        more_permissions = function(value) private$active_helper("more_permissions",
                                                                 value, as_url_value),
        producer = function(value) private$active_helper("producer", value),
        rights = function(value) {
            if (missing(value)) {
                value <- private$tags$rights
                if (is.null(value) && "dc:rights" %in% self$auto_xmp)
                    value <- private$auto_rights()
                value
            } else {
                private$tags$rights <- as_la_value(value)
            }

        },
        subject = function(value) {
            if (missing(value)) {
                value <- private$tags$subject
                if (is.null(value) && "dc:subject" %in% self$auto_xmp) {
                    if (!is.null(private$tags$keywords))
                        value <- strsplit(private$tags$keywords, ", ")[[1]]
                }
                value
            } else {
                private$tags$subject <- as_character_value(value)
            }

        },
        title = function(value) private$active_helper("title", value, as_la_value),
        usage_terms = function(value) {
            if (missing(value)) {
                value <- private$tags$usage_terms
                if (is.null(value) && "xmpRights:UsageTerms" %in% self$auto_xmp)
                    value <- private$auto_usage_terms()
                value
            } else {
                private$tags$usage_terms <- as_la_value(value)
            }
        },
        web_statement = function(value) {
            if (missing(value)) {
                value <- private$tags$web_statement
                if (is.null(value) && "xmpRights:WebStatement" %in% self$auto_xmp)
                    value <- private$auto_spdx_url()
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
            } else if (lkey %in% c("rights", "dc:rights")) {
                is.null(private$tags$rights)
            } else if (lkey %in% c("subject", "dc:subject")) {
                is.null(private$tags$subject)
            } else if (lkey %in% c("usage_terms", "usageterms", "xmprights:usageterms")) {
                is.null(private$tags$usage_terms)
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
        auto_credit = function() {
            value <- private$tags$credit
            if (is.null(value) && !is.null(private$tags[["creator"]]))
                value <- paste(private$tags[["creator"]], collapse = " and ")
            value
        },
        auto_rights = function() {
            #### Update if/when we support plus:CopyrightOwner
            value <- NULL
            if (!is.null(private$tags$creator) && !is.null(private$tags$date_created)) {
                owner <- paste(private$tags$creator, collapse = " and ")
                year <- datetimeoffset::format_iso8601(private$tags$date_created, precision = "year")
                if (is.null(self$spdx_id)) {
                    value <- paste0("\u00a9 ", year, " ", owner, ". All rights reserved.")
                } else if (isTRUE(xmpdf::spdx_licenses[self$spdx_id, "pd"])) {
                    value <- paste0("In the public domain. No rights reserved.")
                } else {
                    value <- paste0("\u00a9 ", year, " ", owner, ". Some rights reserved.")
                }
            }
            value
        },
        auto_spdx_url = function() {
            if (!is.null(private$tags$spdx_id)) {
                url <- xmpdf::spdx_licenses[self$spdx_id, "url_alt"]
                if (is.na(url))
                    url <- xmpdf::spdx_licenses[self$spdx_id, "url"]
                url
            } else {
                NULL
            }
        },
        auto_usage_terms = function() {
            value <- NULL
            if (!is.null(private$tags$spdx_id)) {
                name <- xmpdf::spdx_licenses[self$spdx_id, "name"]
                url <- xmpdf::spdx_licenses[self$spdx_id, "url_alt"]
                if (is.na(url))
                    url <- xmpdf::spdx_licenses[self$spdx_id, "url"]
                value <- paste("This work is licensed to the public under the",
                               name, "license", url)
            }
            value
        },
        tags = list(other = list())
    )
)

x_format <- function(value) {
    value <- d_format(value)
    paste(strwrap(value, exdent = 8), collapse = "\n")
}

KNOWN_XMP_TAGS <- c("cc:attributionName", "cc:attributionURL",
                    "cc:license", "cc:morePermissions",
                    "dc:creator", "dc:description", "dc:rights", "dc:subject", "dc:title",
                    "Iptc4xmpCore:AltTextAccessibility", "Iptc4xmpCore:ExtDescrAccessibility",
                    "pdf:Keywords", "pdf:Producer",
                    "photoshop:Credit", "photoshop:DateCreated", "photoshop:Headline",
                    "xmp:CreateDate", "xmp:CreatorTool", "xmp:ModifyDate",
                    "xmpRights:Marked", "xmpRights:UsageTerms", "xmpRights:WebStatement")

#' @export
update.xmp <- function(object, ...) {
    x <- object$clone()
    x$update(as_xmp(list(...)))
    x
}

as_la_value <- function(value) {
    if (is.null(value))
        NULL
    else
        as_lang_alt(value)
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
