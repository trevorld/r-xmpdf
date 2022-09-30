#' Set/get pdf document info dictionary
#'
#' `docinfo()` creates an object of pdf document info dictionary information from scratch.
#' `get_docinfo()` gets pdf document info from a file.
#' `set_docinfo()` sets pdf document info for a file.
#'
#' `get_docinfo()` will try to use the following helper functions in the following order:
#'
#' 1. `get_docinfo_pdftools()` which wraps [pdftools::pdf_info()]
#' 2. `get_docinfo_exiftool()` which wraps `exiftool` command-line tool
#' 3. `get_docinfo_pdftk()` which wraps `pdftk` command-line tool
#'
#' `set_docinfo()` will try to use the following helper functions in the following order:
#'
#' 1. `set_docinfo_gs()` which wraps `ghostscript` command-line tool
#' 2. `set_docinfo_exiftool()` which wraps `exiftool` command-line tool
#' 3. `set_docinfo_pdftk()` which wraps `pdftk` command-line tool
#'
#' @param author The document's author.
#' @param creation_date The date the document was created. Will be coerced by `as.POSIXlt()`.
#' @param creator The name of the application that originally created the document (if converted to pdf).
#' @param producer The name of the application that converted the document to pdf.
#' @param title The document's title.
#' @param subject The document's subject.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#' @param mod_date The date the document was last modified. Will be coerced by `as.POSIXlt()`.
#'                 If left `NULL` will default to `Sys.Date()` when used to set documentation info entry.
#' @param filename Filename (pdf) to extract info dictionary entries from.
#'                 Any such entries will be overridden by any manually set entries in [docinfo()].
#' @param use_names If `TRUE` (default) use `filename` as the names of the result.
#' @param docinfo A "docinfo" object (as returned by [docinfo()] or [get_docinfo()]).
#' @param input Input pdf filename.
#' @param output Output pdf filename.
#' @return `docinfo()` returns a "docinfo" R6 class.
#'         `get_docinfo()` returns a list of "docinfo" R6 classes.
#'         `set_docinfo()` returns the (output) filename invisibly.
#' @section `docinfo` R6 Class Methods:\describe{
#'     \item{`print()`}{Print out the documentation info entries.}
#'     \item{`set_item(key, value)`}{Set documentation info key `key` with value `value`.
#'           Can also use the relevant active bindings to set documentation info keys.}
#' }
#' @section `docinfo` R6 Active Bindings:\describe{
#'    \item{`author`}{The document's author.}
#'    \item{`creation_date`}{The date the document was created.}
#'    \item{`creator`}{The name of the application that originally created the document (if converted to pdf).}
#'    \item{`producer`}{The name of the application that converted the document to pdf.}
#'    \item{`title`}{The document's title.}
#'    \item{`subject`}{The document's subject.}
#'    \item{`keywords`}{Character vector of keywords for this document (for cross-document searching).}
#'    \item{`mod_date`}{The date the document was last modified.}
#' }
#' @section Known limitations:
#'
#'   * Currently does not support arbitrary info dictionary entries.
#'   * `set_docinfo_gs()` probably doesn't work with Unicode input.
#'   * As a side effect `set_docinfo_gs()` seems to also update in previously set matching XPN metadata
#'     while `set_docinfo_exiftool()` and `set_docinfo_pdftk()` don't update
#'     any previously set matching XPN metadata.
#'     Some pdf viewers will preferentially use the previously set document title from XPN metadata
#'     if it exists instead of using the title set in documentation info dictionary entry.
#'     Consider also manually setting this XPN metadata using [set_xmp()].
#'   * Old metadata information is usually not deleted from the pdf file by these operations.
#'     If deleting the old metadata is important one may want to try
#'     `qpdf::pdf_compress(input, linearize = TRUE)`.
#'
#' @examples
#' if (supports_set_docinfo() && supports_get_docinfo() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   di_get1 <- get_docinfo(f)[[1]]
#'   print(di_get1)
#'   \dontshow{cat("\n")}
#'
#'   di_set <- docinfo(author = "John Doe",
#'                     title = "Two Boring Pages",
#'                     keywords = c("R", "xmpdf"),
#'                     filename = f)
#'   set_docinfo(di_set, f)
#'
#'   di_get2 <- get_docinfo(f)[[1]]
#'   print(di_get2)
#'   unlink(f)
#' }
#' @seealso [supports_get_docinfo()], [supports_set_docinfo()], [supports_gs()], and [supports_pdftk()] to detect support for these features. For more info about the pdf document info dictionary see
#'   <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo>.
#' @export
docinfo <- function(author = NULL, creation_date = NULL, creator = NULL, producer = NULL,
                    title = NULL, subject = NULL, keywords = NULL, mod_date = NULL,
                    filename = NULL) {

    DocInfo$new(author = author, creation_date = creation_date,
                creator = creator, producer = producer,
                title = title, subject = subject, keywords = keywords, mod_date = mod_date,
                filename = filename)
}

tryFormats <- c("%Y-%m-%dT%H:%M:%S%z",
                "%Y-%m-%d %H:%M:%OS",
                "%Y/%m/%d %H:%M:%OS",
                "%Y-%m-%d %H:%M",
                "%Y/%m/%d %H:%M",
                "%Y-%m-%d",
                "%Y/%m/%d")

to_date_pdfmark <- function(date) {
    if (is.null(date)) {
        NULL
    } else if (is.character(date)) {
        ""
    } else {
        val <- format(date, format = "D:%Y%m%d%H%M%S%z")
        paste0(substr(val, 1, 19), "'", substr(val, 20, 21), "'")
    }
}

from_date_pdfmark <- function(string) {
    destring <- gsub("^(D:)*", "\\2", string)
    destring <- gsub("'", "", destring) # GMT offset
    date <- if (nchar(destring) == 4) {
        destring <- paste0(destring, "0101")
        as.POSIXlt(destring, format = "%Y%m%d")
    } else if (nchar(destring) == 6) {
        destring <- paste0(destring, "01")
        as.POSIXlt(destring, format = "%Y%m%d")
    } else if (nchar(destring) == 8) {
        as.POSIXlt(destring, format = "%Y%m%d")
    } else if (nchar(destring) == 10) {
        as.POSIXlt(destring, format = "%Y%m%d%H")
    } else if (nchar(destring) == 12) {
        as.POSIXlt(destring, format = "%Y%m%d%H%M")
    } else if (nchar(destring) == 14) {
        as.POSIXlt(destring, format = "%Y%m%d%H%M%S")
    } else if (nchar(destring) == 17) {
        destring <- paste0(destring, "00")
        as.POSIXlt(destring, format = "%Y%m%d%H%M%S%z")
    } else if (nchar(destring) == 19) {
        as.POSIXlt(destring, format = "%Y%m%d%H%M%S%z")
    } else {
        NA
    }
    if (is.na(date)) {
        abort(paste("Couldn't parse pdfmark date", string))
    }
    date
}

entry_pdftk <- function(key, value) {
    c("InfoBegin",
      paste("InfoKey:", key),
      paste("InfoValue:", value))
}

#' @rdname docinfo
#' @export
get_docinfo <- function(filename, use_names = TRUE) {
    if (supports_pdftools()) {
        get_docinfo_pdftools(filename, use_names = use_names)
    } else if (supports_exiftool()) {
        get_docinfo_exiftool(filename, use_names = use_names)
    } else if (supports_pdftk()) {
        get_docinfo_pdftk(filename, use_names = use_names)
    } else {
        msg <- c(need_to_install_str("get_docinfo()"),
                 install_package_str("pdftools"),
                 install_exiftool_str(),
                 install_pdftk_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname docinfo
#' @export
get_docinfo_pdftools <- function(filename, use_names = TRUE) {
    assert_suggested("pdftools")
    l <- lapply(filename, get_docinfo_pdftools_helper)
    if (use_names)
        names(l) <- filename
    else
        names(l) <- NULL
    l
}

get_docinfo_pdftools_helper <- function(filename) {
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
get_docinfo_exiftool <- function(filename, use_names = TRUE) {
    l <- lapply(filename, get_docinfo_exiftool_helper)
    if (use_names)
        names(l) <- filename
    else
        names(l) <- NULL
    l
}
get_docinfo_exiftool_helper <- function(filename) {
    md <- get_exiftool_metadata(filename, tags="-PDF:all")
    md <- md[grep("^PDF:", names(md))]
    names(md) <- gsub("^PDF:", "", names(md))
    dinfo <- docinfo()

    for (i in seq_along(md)) {
        key <- names(md)[i]
        if (key %in% c("Author", "Creator", "Producer", "Title", "Subject", "Keywords")) {
            dinfo$set_item(names(md)[i], md[[i]])
        } else if (key %in% c("PDFVersion", "Linearized", "PageCount", "CreateDate", "ModifyDate")) {
            next
        } else {
            msg <- sprintf("We don't support key '%s' yet.", key)
            warn(msg)
        }
    }
    if (!is.null(md$CreateDate))
        dinfo$creation_date <- md$CreateDate
    if (!is.null(md$ModifyDate))
        dinfo$mod_date <- md$ModifyDate
    dinfo
}

#' @rdname docinfo
#' @export
set_docinfo_exiftool <- function(docinfo, input, output = input) {
    stopifnot(inherits(docinfo, "docinfo"))
    tags <- docinfo$exiftool_tags()
    set_exiftool_metadata(tags, input, output)
}

#' @rdname docinfo
#' @export
get_docinfo_pdftk <- function(filename, use_names = TRUE) {
    l <- lapply(filename, get_docinfo_pdftk_helper)
    if (use_names)
        names(l) <- filename
    else
        names(l) <- NULL
    l
}

get_docinfo_pdftk_helper <- function(filename) {
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
    if (supports_gs()) {
        set_docinfo_gs(docinfo, input, output)
    } else if (supports_exiftool()) {
        set_docinfo_exiftool(docinfo, input, output)
    } else if (supports_pdftk()) {
        set_docinfo_pdftk(docinfo, input, output)
    } else {
        msg <- c(need_to_install_str("set_docinfo()"),
                 install_gs_str(),
                 install_exiftool_str(),
                 install_pdftk_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname docinfo
#' @export
set_docinfo_gs <- function(docinfo, input, output = input) {
    stopifnot(inherits(docinfo, "docinfo"))
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
    stopifnot(inherits(docinfo, "docinfo"))
    cmd <- pdftk()
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
        print = function() {
            text <- c(paste("Author:", self$author),
                      paste("CreationDate:", self$creation_date),
                      paste("Creator:", self$creator),
                      paste("Producer:", self$producer),
                      paste("Title:", self$title),
                      paste("Subject:", self$subject),
                      paste("Keywords:",  paste(self$keywords, collapse = ", ")),
                      paste("ModDate:", self$mod_date))
            invisible(cat(text, sep="\n"))
        },
        exiftool_tags = function() {
            # We're using a date format equivalent to R's "%Y-%m-%dT%H:%M:%S%z"
            tags <- list()
            if (!is.null(self$author))
                tags[["PDF:Author"]] <- self$author
            if (!is.null(self$creation_date))
                tags[["PDF:CreateDate"]] <- format(self$creation_date,
                                                     format = "%Y-%m-%dT%H:%M:%S%z")
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
                tags[["PDF:ModifyDate"]] <-  format(self$mod_date,
                                                    format = "%Y-%m-%dT%H:%M:%S%z")
            tags
        },
        pdfmark = function() {
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
                                                 paste(self$keywords, collapse = ", ")))
            if (!is.null(self$mod_date))
                tags <- append(tags, entry_pdftk("ModDate",
                                                 to_date_pdfmark(self$mod_date)))
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
                if (is.null(value))
                    private$val$creation_date <- NULL
                else
                    private$val$creation_date <- as.POSIXlt(value, tryFormats = tryFormats)
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
                if (is.null(value))
                    private$val$mod_date <- NULL
                else
                    private$val$mod_date <- as.POSIXlt(value, tryFormats = tryFormats)
            }
        }
    ),
    private = list(
        val = list(),
        get_docinfo = function(filename) {
            dinfo <- get_docinfo(filename)[[1]]
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
