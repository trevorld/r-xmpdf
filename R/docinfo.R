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
#' @param filename Filename(s) (pdf) to extract info dictionary entries from.
#' @param use_names If `TRUE` (default) use `filename` as the names of the result.
#' @param docinfo A "docinfo" object (as returned by [docinfo()] or [get_docinfo()]).
#' @param input Input pdf filename.
#' @param output Output pdf filename.
#' @return `docinfo()` returns a "docinfo" R6 class.
#'         `get_docinfo()` returns a list of "docinfo" R6 classes.
#'         `set_docinfo()` returns the (output) filename invisibly.
#' @section Known limitations:
#'
#'   * Currently does not support arbitrary info dictionary entries.
#'   * `set_docinfo_gs()` doesn't work with Unicode input.
#'   * As a side effect `set_docinfo_gs()` seems to also update in previously set matching XPN metadata
#'     while `set_docinfo_exiftool()` and `set_docinfo_pdftk()` don't update
#'     any previously set matching XPN metadata.
#'     Some pdf viewers will preferentially use the previously set document title from XPN metadata
#'     if it exists instead of using the title set in documentation info dictionary entry.
#'     Consider also manually setting this XPN metadata using [set_xmp()].
#'   * Old metadata information is usually not deleted from the pdf file by these operations.
#'     If deleting the old metadata is important one may want to try
#'     `qpdf::pdf_compress(input, linearize = TRUE)`.
#'   * Datetimes are often converted to UTC time.
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
#'   print(get_docinfo(f)[[1]])
#'   \dontshow{cat("\n")}
#'
#'   d <- docinfo(author = "John Doe",
#'                title = "Two Boring Pages",
#'                keywords = c("R", "xmpdf"),
#'                filename = f)
#'   set_docinfo(d, f)
#'
#'   print(get_docinfo(f)[[1]])
#'
#'   unlink(f)
#' }
#' @seealso [docinfo()] for more information about the documentation info objects.  [supports_get_docinfo()], [supports_set_docinfo()], [supports_gs()], and [supports_pdftk()] to detect support for these features. For more info about the pdf document info dictionary see
#'   <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo>.
#' @name edit_docinfo
NULL

#' PDF documentation info dictionary object
#'
#' `docinfo()` creates a PDF documentation info dictionary object
#' Such objects can be used with [set_docinfo()] to edit PDF documentation info dictionary entries
#' and such objects are returned by [get_docinfo()].
#' @param author The document's author.  Matching xmp metadata tag is `dc:Creator`.
#' @param creation_date The date the document was created. Will be coerced by `as.POSIXlt(tz = "GMT")`.
#'                Matching xmp metadata tag is `xmp:CreateDate`.
#' @param creator The name of the application that originally created the document (if converted to pdf).
#'                Matching xmp metadata tag is `xmp:CreatorTool`.
#' @param producer The name of the application that converted the document to pdf.
#'                Matching xmp metadata tag is `pdf:Producer`.
#' @param title The document's title.  Matching xmp metadata tag is `dc:Title`.
#' @param subject The document's subject.  Matching xmp metadata tag is `dc:Description`.
#' @param keywords Character vector of keywords for this document (for cross-document searching).
#'                 Matching xmp metadata tag is `pdf:Keywords`.
#' @param mod_date The date the document was last modified. Will be coerced by `as.POSIXlt(tz = "GMT")`.
#'                 If left `NULL` will default to `Sys.Date()` when used to set documentation info entry.
#'                 Matching xmp metadata tag is `xmp:ModifyDate`.
#' @param filename A pdf filename to extract info dictionary entries from:
#'                 any such entries will be overridden by any manually set entries in [docinfo()] call.
#' @seealso [get_docinfo()] and [set_docinfo()] for getting/setting such information from/to PDF files.
#'          [as_docinfo()] for coercing to this object.
#'    [as_xmp()] can be used to coerce `docinfo()` objects into [xmp()] objects.
#' @section Known limitations:
#'
#'   * Currently does not support arbitrary info dictionary entries.
#'
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
#' @examples
#' if (supports_set_docinfo() && supports_get_docinfo() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   print(get_docinfo(f)[[1]])
#'   \dontshow{cat("\n")}
#'
#'   d <- docinfo(author = "John Doe",
#'                title = "Two Boring Pages",
#'                keywords = c("R", "xmpdf"),
#'                filename = f)
#'   set_docinfo(d, f)
#'
#'   print(get_docinfo(f)[[1]])
#'
#'   unlink(f)
#' }
#' @export
docinfo <- function(author = NULL, creation_date = NULL, creator = NULL, producer = NULL,
                    title = NULL, subject = NULL, keywords = NULL, mod_date = NULL,
                    filename = NULL) {

    DocInfo$new(author = author, creation_date = creation_date,
                creator = creator, producer = producer,
                title = title, subject = subject, keywords = keywords, mod_date = mod_date,
                filename = filename)
}

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
    do.call(docinfo, l)
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

tryFormats <- c("%FT%T%z",
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
        val <- as.character(date, format = "D:%Y%m%d%H%M%S%z")
        paste0(substr(val, 1, 19), "'", substr(val, 20, 21), "'")
    }
}

d_format <- function(value) {
    if (is.null(value)) {
        "NULL"
    } else if (is.character(value)) {
        value
    } else if (length(value) > 1) {
        paste(value, collapse = ", ")
    } else {
        strftime(value, format = "%F %T%z")
    }
}

from_date_pdfmark <- function(string) {
    destring <- gsub("^(D:)*", "\\2", string)
    destring <- gsub("'", "", destring) # GMT offset
    date <- if (nchar(destring) == 4) {
        destring <- paste0(destring, "0101000000+0000")
    } else if (nchar(destring) == 6) {
        destring <- paste0(destring, "01000000+0000")
    } else if (nchar(destring) == 8) {
        destring <- paste0(destring, "000000+0000")
    } else if (nchar(destring) == 10) {
        destring <- paste0(destring, "0000+0000")
    } else if (nchar(destring) == 12) {
        destring <- paste0(destring, "00+0000")
    } else if (nchar(destring) == 14) {
        destring <- paste0(destring, "+0000")
    } else if (nchar(destring) == 17) {
        destring <- paste0(destring, "00")
    }
    if (nchar(destring) == "19") {
        tz <- substr(destring, 15, 19)
        date <- strptime(destring, tz = "GMT", format = "%Y%m%d%H%M%S%z")
    } else {
        date <- NA
    }
    if (is.na(date)) {
        abort(paste("Couldn't parse pdfmark date", sQuote(string)))
    }
    date
}

entry_pdftk <- function(key, value) {
    c("InfoBegin",
      paste("InfoKey:", key),
      paste("InfoValue:", value))
}

#' @rdname edit_docinfo
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

#' @rdname edit_docinfo
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

#' @rdname edit_docinfo
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

#' @rdname edit_docinfo
#' @export
set_docinfo_exiftool <- function(docinfo, input, output = input) {
    docinfo <- as_docinfo(docinfo)
    tags <- docinfo$exiftool_tags()
    set_exiftool_metadata(tags, input, output)
}

#' @rdname edit_docinfo
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

#' @rdname edit_docinfo
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

#' @rdname edit_docinfo
#' @export
set_docinfo_gs <- function(docinfo, input, output = input) {
    docinfo <- as_docinfo(docinfo)
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

#' @rdname edit_docinfo
#' @export
set_docinfo_pdftk <- function(docinfo, input, output = input) {
    docinfo <- as_docinfo(docinfo)
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
                tags[["PDF:Keywords"]] <- paste(self$keywords, collapse = ", ")
            if (!is.null(self$mod_date))
                tags[["PDF:ModifyDate"]] <-  format(self$mod_date,
                                                    format = "%Y-%m-%dT%H:%M:%S%z")
            tags
        },
        xmp = function() {
            # these are the XMP tags that `ghostscript` chooses as equivalent
            # to the eight documentation info dictionary entries
            # With `exiftool` we're using a date format equivalent to R's "%Y-%m-%dT%H:%M:%S%z"
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
                tags[["pdf:Keywords"]] <- paste(self$keywords, collapse = ", ")

            if (!is.null(self$creation_date))
                tags[["xmp:CreateDate"]] <- self$creation_date
            if (!is.null(self$creator))
                tags[["xmp:CreatorTool"]] <- self$creator
            if (!is.null(self$mod_date))
                tags[["xmp:ModifyDate"]] <-  self$mod_date

            as_xmp(tags)
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
                    private$val$creation_date <- as.POSIXlt(value, tz = "GMT", tryFormats = tryFormats)
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
                    private$val$mod_date <- as.POSIXlt(value, tz = "GMT", tryFormats = tryFormats)
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
