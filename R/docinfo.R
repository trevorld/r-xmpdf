#' Set/get pdf document info dictionary
#'
#' `docinfo()` creates an object of pdf document info dictionary information from scratch.
#' `get_docinfo()` gets pdf document info from a file.
#' `set_docinfo()` sets pdf document info for a file.
#'
#' `get_docinfo()` will try to use the following helper functions in the following order:
#'
#' 1. `get_docinfo_pdftools()` which wraps [pdftools::pdf_info()]
#' 2. `get_docinfo_pdftk()` which wraps `pdftk` command-line tool
#'
#' `set_docinfo()` will try to use the following helper functions in the following order:
#'
#' 1. `set_docinfo_gs()` which wraps `ghostscript` command-line tool
#' 2. `set_docinfo_pdftk()` which wraps `pdftk` command-line tool
#'
#' @param author The document's author.
#' @param creation_date The date the document was created. Will be coerced by `as.Date()`.
#' @param creator The name of the application that originally created the document (if converted to pdf).
#' @param producer The name of the application that converted the document to pdf.
#' @param title The document's title.
#' @param subject The document's subject.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#' @param mod_date The date the document was last modified. Will be coerced by `as.Date()`.
#'                 If left `NULL` will default to `Sys.Date()` when used to set documentation info entry.
#' @param filename Filename (pdf) to extract info dictionary entries from.
#'                 Any such entries will be overridden by any manually set entries in [docinfo()].
#' @param docinfo A "docinfo" object (as returned by [docinfo()] or [get_docinfo()]).
#' @param input Input pdf filename.
#' @param output Output pdf filename.
#' @return `docinfo()` and `get_docinfo()` returns a "docinfo" R6 class.
#'         `set_docinfo()` returns the (output) filename invisibly.
#' @section `docinfo` R6 Class Methods:\describe{
#'     \item{`pdfmark()`}{Return a string of pdfmark info for use with `ghostscript`.}
#'     \item{`pdftk()`}{Return a string of pdfmark metadata for use with `pdftk`.}
#'     \item{`set_item(key, value)`}{Set documentation info key `key` with value `value`.}
#' }
#' @section Known limitations:
#'
#'   * Currently does not support arbitrary info dictionary entries.
#'   * Currently only supports date year, month, and day for `CreationDate` and `ModDate` entries
#'     (does not support hours, minutes, seconds and relation to GMT).
#'   * `set_docinfo_pdftk()` won't update any previously set XPN metadata.
#'     Some pdf viewers will preferentially use the previously set document title from XPN metadata
#'     if it exists instead of using the documentation info dictionary entry.
#'
#' @examples
#' if (piecepackr.metadata:::supports_set_docinfo() &&
#'     piecepackr.metadata:::supports_get_docinfo() &&
#'     require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   di_get1 <- get_docinfo(f)
#'   print(di_get1$title)
#'   print(di_get1$author)
#'
#'   di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
#'   set_docinfo(di_set, f)
#'
#'   di_get2 <- get_docinfo(f)
#'   print(di_get2$title)
#'   print(di_get2$author)
#'   unlink(f)
#' }
#' @seealso <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo>
#' @export
docinfo <- function(author = NULL, creation_date = NULL, creator = NULL, producer = NULL,
                    title = NULL, subject = NULL, keywords = NULL, mod_date = NULL,
                    filename = NULL) {

    DocInfo$new(author = author, creation_date = creation_date,
                creator = creator, producer = producer,
                title = title, subject = subject, keywords = keywords, mod_date = mod_date,
                filename = filename)
}

supports_set_docinfo <- function() {
    has_cmd("pdftk") || has_gs()
}
supports_get_docinfo <- function() {
    requireNamespace("pdftools", quietly = TRUE) || has_cmd("pdftk")
}

to_date_pdfmark <- function(date) {
    if (is.null(date))
        NULL
    else
        as.character(date, "D:%Y%m%d")
}

from_date_pdfmark <- function(string) {
    string <- gsub("^(D:)*([[:digit:]]{8}).*", "\\2", string)
    as.Date(string, format = "%Y%m%d")
}

entry_pdftk <- function(key, value) {
    c("InfoBegin",
      paste("InfoKey:", key),
      paste("InfoValue:", value))
}

get_pdftk_metadata <- function(filename) {
    cmd <- get_cmd("pdftk")
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    args <- c(filename, "dump_data_utf8")
    system2(cmd, args, stdout=TRUE)
}

#' @rdname docinfo
#' @export
get_docinfo <- function(filename) {
    if (requireNamespace("pdftools", quietly = TRUE)) {
        get_docinfo_pdftools(filename)
    } else if (has_cmd("pdftk")) {
        get_docinfo_pdftk(filename)
    } else {
        msg <- c("You'll need to install a suggested package or command to use 'get_docinfo'.",
                 i = "Use 'install.packages(\"pdftools\")' to install {pdftools}",
                 i = "Or install `pdftk` command"
        )
        abort(msg, class = "piecepackr_suggested_package")
    }
}

#' @rdname docinfo
#' @export
get_docinfo_pdftools <- function(filename) {
    info <- pdftools::pdf_info(filename)
    dinfo <- docinfo()
    for (i in seq_along(info$keys)) {
        key <- names(info$keys)[i]
        if (key %in% c("Author", "Creator", "Producer", "Title", "Subject", "Keywords")) {
            dinfo$set_item(names(info$keys)[i], info$keys[[i]])
        } else {
            msg <- sprintf("We don't support key '%s' yet.", key)
            warn(msg)
        }
    }
    if (!is.null(info$created))
        dinfo$creation_date <- info$created
    if (!is.null(info$modified))
        dinfo$mod_date <- info$modified
    dinfo
}

#' @rdname docinfo
#' @export
get_docinfo_pdftk <- function(filename) {
    info <- get_pdftk_metadata(filename)
    dinfo <- docinfo()
    if (length(id <- grep("^InfoKey: Author", info)))
        dinfo$author <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: CreationDate", info))) {
        dinfo$creation_date <- from_date_pdfmark(gsub("^InfoValue: ", "", info[id + 1]))
    }
    if (length(id <- grep("^InfoKey: Creator", info)))
        dinfo$creator <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: Producer", info)))
        dinfo$producer <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: Title", info)))
        dinfo$title <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: Subject", info)))
        dinfo$subject <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: Keywords", info)))
        dinfo$keywords <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: ModDate", info))) {
        dinfo$mod_date <- from_date_pdfmark(gsub("^InfoValue: ", "", info[id + 1]))
    }
    dinfo
}

#' @rdname docinfo
#' @export
set_docinfo <- function(docinfo, input, output = input) {
    if (has_gs()) {
        set_docinfo_gs(docinfo, input, output)
    } else if (has_cmd("pdftk")) {
        set_docinfo_pdftk(docinfo, input, output)
    } else {
        msg <- c("You'll need to install a command to use 'set_docinfo'.",
                 i = "Install `pdftk` command"
        )
        abort(msg, class = "piecepackr_suggested_package")
    }
}

#' @rdname docinfo
#' @export
set_docinfo_gs <- function(docinfo, input, output = input) {
    cmd <- gs()
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    if (input == output) {
        target <- tempfile(fileext = ".pdf")
        on.exit(unlink(target))
    } else {
        target <- output
    }
    metafile <- tempfile(fileext = ".txt")
    on.exit(unlink(metafile))
    writeLines(docinfo$pdfmark(), metafile)
    metafile <- normalizePath(metafile, mustWork = TRUE)
    args <- c("-q", "-o", shQuote(target), "-sDEVICE=pdfwrite", "-sAutoRotatePages=None",
              shQuote(input), shQuote(metafile))
    system2(cmd, args, stdout=TRUE)
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}

#' @rdname docinfo
#' @export
set_docinfo_pdftk <- function(docinfo, input, output = input) {
    cmd <- get_cmd("pdftk")
    meta <- get_pdftk_metadata(input)
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    if (input == output) {
        target <- tempfile(fileext = ".pdf")
        on.exit(unlink(target))
    } else {
        target <- output
    }
    id_info <- grep("^Info", meta)
    if (length(id_info))
        meta <- meta[-id_info]
    meta <- append(docinfo$pdftk(), meta)
    metafile <- tempfile(fileext = ".txt")
    on.exit(unlink(metafile))
    writeLines(meta, metafile)
    metafile <- normalizePath(metafile, mustWork = TRUE)
    args <- c(shQuote(input),
              "update_info_utf8", shQuote(metafile),
              "output", shQuote(target))
    system2(cmd, args, stdout=TRUE)
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}

DocInfo <- R6Class("docinfo",
    public = list(
        initialize = function(author = NULL, creation_date = NULL,
                              creator = NULL, producer = NULL,
                              title = NULL, subject = NULL,
                              keywords = NULL, mod_date = NULL,
                              filename = NULL) {
            if (!is.null(filename))
                private$get_docinfo(filename)
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
        pdfmark = function() {
            tags <- "["
            tags <- append(tags, sprintf(" /Author (%s)\n", self$author %||% ""))
            tags <- append(tags, sprintf(" /CreationDate (%s)\n",
                                         to_date_pdfmark(self$creation_date %||% "")))
            tags <- append(tags, sprintf(" /Creator (%s)\n", self$creator %||% ""))
            tags <- append(tags, sprintf(" /Producer (%s)\n", self$producer %||% ""))
            tags <- append(tags, sprintf(" /Title (%s)\n", self$title %||% ""))
            tags <- append(tags, sprintf(" /Subject (%s)\n", self$subject %||% ""))
            tags <- append(tags, sprintf(" /Keywords (%s)\n",
                                         paste(self$keywords, collapse = ", ") %||% ""))
            tags <- append(tags, sprintf(" /ModDate (%s)\n",
                                         to_date_pdfmark(self$mod_date %||% Sys.Date())))
            tags <- append(tags, " /DOCINFO pdfmark\n")
            paste(tags, collapse="")
        },
        pdftk = function() {
            tags <- character()
            tags <- append(tags, entry_pdftk("Author", self$author %||% ""))
            tags <- append(tags, entry_pdftk("CreationDate",
                                             to_date_pdfmark(self$creation_date %||% "")))
            tags <- append(tags, entry_pdftk("Creator", self$creator %||% ""))
            tags <- append(tags, entry_pdftk("Producer", self$producer %||% ""))
            tags <- append(tags, entry_pdftk("Title", self$title %||% ""))
            tags <- append(tags, entry_pdftk("Subject", self$subject %||% ""))
            tags <- append(tags, entry_pdftk("Keywords",
                                             paste(self$keywords, collapse = ", ") %||% ""))
            tags <- append(tags, entry_pdftk("ModDate",
                                             to_date_pdfmark(self$mod_date %||% Sys.Date())))
            tags
        },
        set_item = function(key, value) {
            if (key == "Author") {
                self$author <- value
            } else if (key == "CreationDate") {
                self$creation_date <- value
            } else if (key == "Creator") {
                self$creator <- value
            } else if (key == "Producer") {
                self$producer <- value
            } else if (key == "Title") {
                self$title <- value
            } else if (key == "Subject") {
                self$subject <- value
            } else if (key == "Keywords") {
                self$keywords <- value
            } else if (key == "ModDate") {
                self$mod_date <- value
            } else {
                msg <- sprintf("We don't support key '%s' yet.", key)
                abort(msg)
            }
        }
    ),
    active = list(
        author = function(value) {
            if (missing(value)) {
                private$val$author
            } else {
                private$val$author <- as.character(value)
            }
        },
        creation_date = function(value) {
            if (missing(value)) {
                private$val$creation_date
            } else {
                private$val$creation_date <- as.Date(value)
            }
        },
        creator = function(value) {
            if (missing(value)) {
                private$val$creator
            } else {
                private$val$creator <- as.character(value)
            }
        },
        producer = function(value) {
            if (missing(value)) {
                private$val$producer
            } else {
                private$val$producer <- as.character(value)
            }
        },
        title = function(value) {
            if (missing(value)) {
                private$val$title
            } else {
                private$val$title <- as.character(value)
            }
        },
        subject = function(value) {
            if (missing(value)) {
                private$val$subject
            } else {
                private$val$subject <- as.character(value)
            }
        },
        keywords = function(value) {
            if (missing(value)) {
                private$val$keywords
            } else {
                private$val$keywords <- keyword_split(as.character(value))
            }
        },
        mod_date = function(value) {
            if (missing(value)) {
                private$val$mod_date
            } else {
                private$val$mod_date <- as.Date(value)
            }
        }
    ),
    private = list(
        val = list(),
        get_docinfo = function(filename) {
            dinfo <- get_docinfo(filename)
            self$author <- dinfo$author
            self$creation_date <- dinfo$creation_date
            self$creator <- dinfo$creator
            self$producer <- dinfo$producer
            self$title <- dinfo$title
            self$subject <- dinfo$subject
            self$keywords <- dinfo$keywords
            self$mod_date <- dinfo$mod_date
        }
    )
)

keyword_split <- function(x) {
    if (length(x) > 1) {
        x
    } else if (length(x) == 0 || x == "") {
        ""
    } else {
        strsplit(x, ",[[:blank:]]*")[[1]]
    }
}
