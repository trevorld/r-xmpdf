# SPDX-License-Identifier: MIT

#' Set/get pdf document info dictionary
#'
#' `get_docinfo()` gets pdf document info from a file.
#' `set_docinfo()` sets pdf document info for a file.
#'
#' `get_docinfo()` will try to use the following helper functions in the following order:
#'
#' 1. `get_docinfo_exiftool()` which wraps `exiftool` command-line tool
#' 2. `get_docinfo_pdftk()` which wraps `pdftk` command-line tool
#' 3. `get_docinfo_pdftools()` which wraps [pdftools::pdf_info()]
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
#'   cat("\nInitial documentation info:\n\n")
#'   d <- get_docinfo(f)[[1]]
#'   print(d)
#'
#'   d <- update(d,
#'               author = "John Doe",
#'               title = "Two Boring Pages",
#'               keywords = c("R", "xmpdf"))
#'   set_docinfo(d, f)
#'
#'   cat("\nDocumentation info after setting it:\n\n")
#'   print(get_docinfo(f)[[1]])
#'
#'   unlink(f)
#' }
#' @seealso [docinfo()] for more information about the documentation info objects.  [supports_get_docinfo()], [supports_set_docinfo()], [supports_gs()], and [supports_pdftk()] to detect support for these features. For more info about the pdf document info dictionary see
#'   <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#document-info-dictionary-docinfo>.
#' @name edit_docinfo
NULL

#' @rdname edit_docinfo
#' @export
get_docinfo <- function(filename, use_names = TRUE) {
    if (supports_exiftool()) {
        get_docinfo_exiftool(filename, use_names = use_names)
    } else if (supports_pdftk()) {
        get_docinfo_pdftk(filename, use_names = use_names)
    } else if (supports_pdftools()) {
        get_docinfo_pdftools(filename, use_names = use_names)
    } else {
        msg <- c(need_to_install_str("get_docinfo()"),
                 install_exiftool_str(),
                 install_pdftk_str(),
                 install_package_str("pdftools")
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname edit_docinfo
#' @export
get_docinfo_pdftools <- function(filename, use_names = TRUE) {
    assert_suggested("pdftools")
    l <- lapply(filename, get_docinfo_pdftools_helper)
    use_filenames(l, use_names, filename)
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
    use_filenames(l, use_names, filename)
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
    use_filenames(l, use_names, filename)
}

get_docinfo_pdftk_helper <- function(filename) {
    info <- get_pdftk_metadata(filename)
    dinfo <- docinfo()
    if (length(id <- grep("^InfoKey: Author", info)))
        dinfo$author <- gsub("^InfoValue: ", "", info[id + 1])
    if (length(id <- grep("^InfoKey: CreationDate", info))) {
        dinfo$creation_date <- datetimeoffset::as_datetimeoffset(gsub("^InfoValue: ", "", info[id + 1]))
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
        dinfo$mod_date <- datetimeoffset::as_datetimeoffset(gsub("^InfoValue: ", "", info[id + 1]))
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
    metafile <- tempfile(fileext = ".bin")
    on.exit(unlink(metafile))
    pmc <- docinfo$pdfmark(raw = FALSE)
    pmc_l1 <- iconv(pmc, to = "latin1")
    if (is.na(pmc_l1)) { # Has non-Latin-1 characters
        writeBin(docinfo$pdfmark(raw = TRUE),
                 metafile,
                 endian = "big")
    } else { # Just Latin-1 characters
        f <- file(metafile, encoding = "latin1")
        open(f, "w")
        writeLines(pmc_l1, f)
        close(f)
    }
    metafile <- normalizePath(metafile, mustWork = TRUE)
    args <- c("-q", "-o", shQuote(target), "-sDEVICE=pdfwrite", "-sAutoRotatePages=None",
              shQuote(input), shQuote(metafile))
    xmpdf_system2(cmd, args)
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
    xmpdf_system2(cmd, args)
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}
