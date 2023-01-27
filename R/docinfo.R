# SPDX-License-Identifier: MIT

#' PDF documentation info dictionary object
#'
#' `docinfo()` creates a PDF documentation info dictionary object.
#' Such objects can be used with [set_docinfo()] to edit PDF documentation info dictionary entries
#' and such objects are returned by [get_docinfo()].
#' @param author The document's author.  Matching xmp metadata tag is `dc:Creator`.
#' @param creation_date The date the document was created.
#'                Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                Matching xmp metadata tag is `xmp:CreateDate`.
#' @param creator The name of the application that originally created the document (if converted to pdf).
#'                Matching xmp metadata tag is `xmp:CreatorTool`.
#' @param producer The name of the application that converted the document to pdf.
#'                Matching xmp metadata tag is `pdf:Producer`.
#' @param title The document's title.  Matching xmp metadata tag is `dc:Title`.
#' @param subject The document's subject.  Matching xmp metadata tag is `dc:Description`.
#' @param keywords Keywords for this document (for cross-document searching).
#'                 Matching xmp metadata tag is `pdf:Keywords`.
#'                 Will be coerced into a string by `paste(keywords, collapse = ", ")`.
#' @param mod_date The date the document was last modified.
#'                 Will be coerced by [datetimeoffset::as_datetimeoffset()].
#'                 Matching xmp metadata tag is `xmp:ModifyDate`.
#' @seealso [get_docinfo()] and [set_docinfo()] for getting/setting such information from/to PDF files.
#'          [as_docinfo()] for coercing to this object.
#'    [as_xmp()] can be used to coerce `docinfo()` objects into [xmp()] objects.
#' @section Known limitations:
#'
#'   * Currently does not support arbitrary info dictionary entries.
#'
#' @section `docinfo` R6 Class Methods:\describe{
#'     \item{`get_item(key)`}{Get documentation info value for key `key`.
#'           Can also use the relevant active bindings to get documentation info values.}
#'     \item{`set_item(key, value)`}{Set documentation info key `key` with value `value`.
#'           Can also use the relevant active bindings to set documentation info values.}
#'     \item{`update(x)`}{Update documentation info key entries
#'                        using non-`NULL` entries in object `x` coerced by [as_docinfo()].}
#' }
#' @section `docinfo` R6 Active Bindings:\describe{
#'    \item{`author`}{The document's author.}
#'    \item{`creation_date`}{The date the document was created.}
#'    \item{`creator`}{The name of the application that originally created the document (if converted to pdf).}
#'    \item{`producer`}{The name of the application that converted the document to pdf.}
#'    \item{`title`}{The document's title.}
#'    \item{`subject`}{The document's subject.}
#'    \item{`keywords`}{Keywords for this document (for cross-document searching).}
#'    \item{`mod_date`}{The date the document was last modified.}
#' }
#' @examples
#' if (supports_set_docinfo() && supports_get_docinfo() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   cat("\nInitial documentation info\n")
#'   d <- get_docinfo(f)[[1]]
#'   print(d)
#'
#'   d <- update(d,
#'               author = "John Doe",
#'               title = "Two Boring Pages",
#'               keywords = "R, xmpdf")
#'   set_docinfo(d, f)
#'
#'   cat("\nDocumentation info after setting it\n")
#'   print(get_docinfo(f)[[1]])
#'
#'   unlink(f)
#' }
#' @export
docinfo <- function(author = NULL, creation_date = NULL, creator = NULL, producer = NULL,
                    title = NULL, subject = NULL, keywords = NULL, mod_date = NULL) {

    DocInfo$new(author = author, creation_date = creation_date,
                creator = creator, producer = producer,
                title = title, subject = subject, keywords = keywords, mod_date = mod_date)
}

DocInfo <- R6Class("docinfo",
    public = list(
        initialize = function(author = NULL, creation_date = NULL,
                              creator = NULL, producer = NULL,
                              title = NULL, subject = NULL,
                              keywords = NULL, mod_date = NULL) {
            if (!is.null(author))
                self$author <- author
            if (!is.null(creation_date))
                self$creation_date <- creation_date
            if (!is.null(creator))
                self$creator <- creator
            if (!is.null(producer))
                self$producer <- producer
            if (!is.null(title))
                self$title <- title
            if (!is.null(subject))
                self$subject <- subject
            if (!is.null(keywords))
                self$keywords <- keywords
            if (!is.null(mod_date))
                self$mod_date <- mod_date
            invisible(NULL)
        },
        print = function() {
            text <- c(paste("Author:", d_format(self$author)),
                      paste("CreationDate:", d_format(self$creation_date)),
                      paste("Creator:", d_format(self$creator)),
                      paste("Producer:", d_format(self$producer)),
                      paste("Title:", d_format(self$title)),
                      paste("Subject:", d_format(self$subject)),
                      paste("Keywords:",  d_format(self$keywords)),
                      paste("ModDate:", d_format(self$mod_date)))
            invisible(cat(text, sep="\n"))
        },
        get_item = function(key) {
            if (key == "Author") {
                self$author
            } else if (key == "CreationDate") {
                self$creation_date
            } else if (key == "Creator") {
                self$creator
            } else if (key == "Producer") {
                self$producer
            } else if (key == "Title") {
                self$title
            } else if (key == "Subject") {
                self$subject
            } else if (key == "Keywords") {
                self$keywords
            } else if (key == "ModDate") {
                self$mod_date
            } else {
                msg <- sprintf("We don't support key '%s' yet.", key)
                abort(msg)
            }
        },
        set_item = function(key, value) {
            if (key %in% c("author", "Author")) {
                self$author <- value
            } else if (key %in% c("creation_date", "CreationDate")) {
                self$creation_date <- value
            } else if (key %in% c("creator", "Creator")) {
                self$creator <- value
            } else if (key %in% c("producer", "Producer")) {
                self$producer <- value
            } else if (key %in% c("title", "Title")) {
                self$title <- value
            } else if (key %in% c("subject", "Subject")) {
                self$subject <- value
            } else if (key %in% c("keywords", "Keywords")) {
                self$keywords <- value
            } else if (key %in% c("mod_date", "ModDate")) {
                self$mod_date <- value
            } else {
                msg <- sprintf("We don't support key '%s' yet.", key)
                abort(msg)
            }
        },
        update = function(x) {
            di <- as_docinfo(x)
            if (!is.null(di$author))
                self$author <- di$author
            if (!is.null(di$creation_date))
                self$creation_date <- di$creation_date
            if (!is.null(di$creator))
                self$creator <- di$creator
            if (!is.null(di$producer))
                self$producer <- di$producer
            if (!is.null(di$title))
                self$title <- di$title
            if (!is.null(di$subject))
                self$subject <- di$subject
            if (!is.null(di$keywords))
                self$keywords <- di$keywords
            if (!is.null(di$mod_date))
                self$mod_date <-  di$mod_date
            invisible(NULL)
        },
        exiftool_tags = function() {
            # We're using a date format equivalent to R's "%Y-%m-%dT%H:%M:%S%z"
            tags <- list()
            if (!is.null(self$author))
                tags[["PDF:Author"]] <- self$author
            if (!is.null(self$creation_date))
                tags[["PDF:CreateDate"]] <- self$creation_date
            if (!is.null(self$creator))
                tags[["PDF:Creator"]] <- self$creator
            if (!is.null(self$producer))
                tags[["PDF:Producer"]] <- self$producer
            if (!is.null(self$title))
                tags[["PDF:Title"]] <- self$title
            if (!is.null(self$subject))
                tags[["PDF:Subject"]] <- self$subject
            if (!is.null(self$keywords))
                tags[["PDF:Keywords"]] <- self$keywords
            if (!is.null(self$mod_date))
                tags[["PDF:ModifyDate"]] <-  self$mod_date
            tags
        },
        pdfmark = function(raw = FALSE) {
            if (raw)
                private$pdfmark_raw()
            else
                private$pdfmark_character()
        },
        pdftk = function() {
            tags <- character()
            if (!is.null(self$author))
                tags <- append(tags, entry_pdftk("Author", self$author))
            if (!is.null(self$creation_date))
                tags <- append(tags, entry_pdftk("CreationDate",
                                                 to_date_pdfmark(self$creation_date)))
            if (!is.null(self$creator))
                tags <- append(tags, entry_pdftk("Creator", self$creator))
            if (!is.null(self$producer))
                tags <- append(tags, entry_pdftk("Producer", self$producer))
            if (!is.null(self$title))
                tags <- append(tags, entry_pdftk("Title", self$title))
            if (!is.null(self$subject))
                tags <- append(tags, entry_pdftk("Subject", self$subject))
            if (!is.null(self$keywords))
                tags <- append(tags, entry_pdftk("Keywords",
                                                 self$keywords))
            if (!is.null(self$mod_date))
                tags <- append(tags, entry_pdftk("ModDate",
                                                 to_date_pdfmark(self$mod_date)))
            tags
        },
        xmp = function() {
            # these are the XMP tags that `ghostscript` chooses as equivalent
            # to the eight documentation info dictionary entries
            tags <- list()
            if (!is.null(self$title))
                tags[["dc:Title"]] <- self$title
            if (!is.null(self$author))
                tags[["dc:Creator"]] <- self$author
            if (!is.null(self$subject))
                tags[["dc:Description"]] <- self$subject

            if (!is.null(self$producer))
                tags[["pdf:Producer"]] <- self$producer
            if (!is.null(self$keywords))
                tags[["pdf:Keywords"]] <- self$keywords

            if (!is.null(self$creation_date))
                tags[["xmp:CreateDate"]] <- self$creation_date
            if (!is.null(self$creator))
                tags[["xmp:CreatorTool"]] <- self$creator
            if (!is.null(self$mod_date))
                tags[["xmp:ModifyDate"]] <-  self$mod_date

            as_xmp(tags)
        }
    ),
    active = list(
        author = function(value) {
            if (missing(value)) {
                private$val$author
            } else {
                private$val$author <- as_character_value(value)
            }
        },
        creation_date = function(value) {
            if (missing(value)) {
                private$val$creation_date
            } else {
                private$val$creation_date <- as_datetime_value(value)
            }
        },
        creator = function(value) {
            if (missing(value)) {
                private$val$creator
            } else {
                private$val$creator <- as_character_value(value)
            }
        },
        producer = function(value) {
            if (missing(value)) {
                private$val$producer
            } else {
                private$val$producer <- as_character_value(value)
            }
        },
        title = function(value) {
            if (missing(value)) {
                private$val$title
            } else {
                private$val$title <- as_character_value(value)
            }
        },
        subject = function(value) {
            if (missing(value)) {
                private$val$subject
            } else {
                private$val$subject <- as_character_value(value)
            }
        },
        keywords = function(value) {
            if (missing(value)) {
                private$val$keywords
            } else {
                private$val$keywords <- paste(value, collapse = ", ")
            }
        },
        mod_date = function(value) {
            if (missing(value)) {
                private$val$mod_date
            } else {
                private$val$mod_date <- as_datetime_value(value)
            }
        }
    ),
    private = list(
        val = list(),
        pdfmark_character = function() {
            tags <- "["
            if (!is.null(self$author))
                tags <- append(tags, sprintf(" /Author (%s)\n", self$author))
            if (!is.null(self$creation_date))
                tags <- append(tags, sprintf(" /CreationDate (%s)\n",
                                             to_date_pdfmark(self$creation_date)))
            if (!is.null(self$creator))
                tags <- append(tags, sprintf(" /Creator (%s)\n", self$creator))
            if (!is.null(self$producer))
                tags <- append(tags, sprintf(" /Producer (%s)\n", self$producer))
            if (!is.null(self$title))
                tags <- append(tags, sprintf(" /Title (%s)\n", self$title))
            if (!is.null(self$subject))
                tags <- append(tags, sprintf(" /Subject (%s)\n", self$subject))
            if (!is.null(self$keywords))
                tags <- append(tags, sprintf(" /Keywords (%s)\n",
                                             paste(self$keywords, collapse = ", ")))
            if (!is.null(self$mod_date))
                tags <- append(tags, sprintf(" /ModDate (%s)\n",
                                             to_date_pdfmark(self$mod_date)))
            tags <- append(tags, " /DOCINFO pdfmark\n")
            paste(tags, collapse="")
        },
        pdfmark_raw = function() {
            tags <- iconv("[", to = "latin1", toRaw = TRUE)[[1]]
            if (!is.null(self$author))
                tags <- append(tags, raw_pdfmark_entry(" /Author (", self$author, ")\n"))
            if (!is.null(self$creation_date)) {
                creation_date <- sprintf(" /CreationDate (%s)\n", to_date_pdfmark(self$creation_date))
                tags <- append(tags, iconv(creation_date, to = "latin1", toRaw = TRUE)[[1]])
            }
            if (!is.null(self$creator))
                tags <- append(tags, raw_pdfmark_entry(" /Creator (", self$creator, ")\n"))
            if (!is.null(self$producer))
                tags <- append(tags, raw_pdfmark_entry(" /Producer (", self$producer, ")\n"))
            if (!is.null(self$title))
                tags <- append(tags, raw_pdfmark_entry(" /Title (", self$title, ")\n"))
            if (!is.null(self$subject))
                tags <- append(tags, raw_pdfmark_entry(" /Subject (", self$subject, ")\n"))
            if (!is.null(self$keywords)) {
                keywords <- paste(self$keywords, collapse = ", ")
                tags <- append(tags, raw_pdfmark_entry(" /Keywords (", keywords, ")\n"))
            }
            if (!is.null(self$mod_date)) {
                mod_date <- sprintf(" /ModDate (%s)\n", to_date_pdfmark(self$mod_date))
                tags <- append(tags, iconv(mod_date, to = "latin1", toRaw = TRUE)[[1]])
            }
            tags <- append(tags,
                           iconv(" /DOCINFO pdfmark\n", to = "latin1", toRaw = TRUE)[[1]])
            tags
        }
    )
)

#' @export
as.list.docinfo <- function(x, ...) {
    l <- list()
    if (!is.null(x$author))
        l$author <- x$author
    if (!is.null(x$creation_date))
        l$creation_date <- x$creation_date
    if (!is.null(x$creator))
        l$creator <- x$creator
    if (!is.null(x$producer))
        l$producer <- x$producer
    if (!is.null(x$title))
        l$title <- x$title
    if (!is.null(x$subject))
        l$subject <- x$subject
    if (!is.null(x$keywords))
        l$keywords <- x$keywords
    if (!is.null(x$mod_date))
        l$mod_date <-  x$mod_date
    l
}

#' @export
update.docinfo <- function(object, ...) {
    d <- object$clone()
    d$update(as_docinfo(list(...)))
    d
}

to_date_pdfmark <- function(date) {
    if (is.null(date)) {
        NULL
    } else if (is.character(date)) {
        ""
    } else {
        datetimeoffset::format_pdfmark(date)
    }
}

pdfmark_string <- function(value) {
    paste0("(", value, ")")
}

raw_pdfmark_entry <- function(open, value, close) {
    r <- iconv(open, to = "latin1", toRaw = TRUE)[[1]]
    l1 <- iconv(value, to = "latin1")
    if (is.na(l1)) { # Unicode needs to be "UTF-16BE" while rest needs to be "latin1"
        r <- append(r, iconv(paste0("\ufeff", value), to = "UTF-16BE", toRaw = TRUE)[[1]])
    } else {
        r <- append(r, iconv(value, to = "latin1", toRaw = TRUE)[[1]])
    }
    append(r, iconv(close, to = "latin1", toRaw = TRUE)[[1]])
}

d_format <- function(value) {
    if (is.null(value)) {
        "NULL"
    } else if (length(value) > 1) {
        paste(value, collapse = ", ")
    } else if (is.character(value)) {
        value
    } else {
        format(value)
    }
}

entry_pdftk <- function(key, value) {
    c("InfoBegin",
      paste("InfoKey:", key),
      paste("InfoValue:", value))
}

as_character_value <- function(value) {
    if (is.null(value))
        NULL
    else
        value
}

as_datetime_value <- function(value) {
    if (is.null(value)) {
        NULL
    } else {
        datetimeoffset::as_datetimeoffset(value)
    }
}
