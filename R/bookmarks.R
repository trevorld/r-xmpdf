#' Set/get pdf bookmarks
#'
#' `get_bookmarks()` gets pdf bookmarks from a file.
#' `set_bookmarks()` sets pdf bookmarks for a file.
#'
#' `get_bookmarks()` will try to use the following helper functions in the following order:
#'
#' 1. `get_bookmarks_pdftk()` which wraps `pdftk` command-line tool
#'
#' `set_bookmarks()` will try to use the following helper functions in the following order:
#'
#' 1. `set_bookmarks_gs()` which wraps `ghostscript` command-line tool
#' 2. `set_bookmarks_pdftk()` which wraps `pdftk` command-line tool
#'
#' @param filename Filename (pdf) to extract bookmarks from.
#' @param bookmarks A data frame with bookmark information with the following columns:\describe{
#'   \item{title}{Title for bookmark (mandatory, character)}
#'   \item{page}{Page number for bookmark (mandatory, integer)}
#'   \item{level}{Level of bookmark e.g. 1 top level, 2 second level, etc. (optional, integer).
#'                If missing will be inferred from `count` column else will be assumed to be `1L`.}
#'   \item{count}{Number of bookmarks immediately subordinate (optional, integer).
#'                If missing will be inferred from `level` column else will be assumed do be `0L`.}
#' }
#' @param input Input pdf filename.
#' @param output Output pdf filename.
#' @return `get_bookmarks()` returns a data frame with bookmark info (see `bookmarks` parameter for details about columns).
#'         `set_bookmarks()` returns the (output) filename invisibly.
#' @section Known limitations:
#'
#'   * Currently only allows setting bookmarks for page numbers within the pdf.
#'   * Currently only supports setting the title, page number, and level of bookmarks.
#'
#' @seealso [supports_get_bookmarks()], [supports_set_bookmarks()], [supports_gs()], and [supports_pdftk()] to detect support for these features.  For more info about the pdf bookmarks feature see <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#bookmarks-out>.
#' @examples
#' if (supports_set_bookmarks() && supports_get_bookmarks() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   print(get_bookmarks(f))
#'
#'   bookmarks <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
#'
#'   set_bookmarks(bookmarks, f)
#'   print(get_bookmarks(f))
#' }
#' @name bookmarks
NULL

#' @rdname bookmarks
#' @export
get_bookmarks <- function(filename) {
    if (supports_pdftk()) {
        get_bookmarks_pdftk(filename)
    } else {
        msg <- c("You'll need to install a suggested package or command to use 'get_docinfo'.",
                 i = "Install `pdftk` command"
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname bookmarks
#' @export
get_bookmarks_pdftk <- function(filename) {
    meta <- get_pdftk_metadata(filename)
    id_bookmark <- grep("^BookmarkBegin", meta)
    if (length(id_bookmark)) {
        title <- gsub("^BookmarkTitle: ", "", meta[id_bookmark + 1L])
        level <- gsub("^BookmarkLevel: ", "", meta[id_bookmark + 2L])
        page <- gsub("^BookmarkPageNumber: ", "", meta[id_bookmark + 3L])
        data.frame(title = title,
                   level = as.integer(level),
                   page = as.integer(page),
                   count = get_count(as.integer(level)),
                   stringsAsFactors = FALSE)
    } else {
        data.frame(title = character(0),
                   level = integer(0),
                   page = integer(0),
                   count = integer(0),
                   stringsAsFactors = FALSE)
    }
}

#' @rdname bookmarks
#' @export
set_bookmarks <- function(bookmarks, input, output = input) {
    if (supports_gs()) {
        set_bookmarks_gs(bookmarks, input, output)
    } else if (supports_pdftk()) {
        set_bookmarks_pdftk(bookmarks, input, output)
    } else {
        msg <- c("You'll need to install a suggested package or command to use 'get_docinfo'.",
                 i = "Install `ghostscript` command",
                 i = "Or install `pdftk` command"
        )
        abort(msg, class = "piecepackr_suggested_package")
    }
}

#' @rdname bookmarks
#' @export
set_bookmarks_pdftk <- function(bookmarks, input, output = input) {
    bookmarks <- as_bookmarks(bookmarks)
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
    id_info <- grep("^Bookmark", meta)
    if (length(id_info))
        meta <- meta[-id_info]
    bookmarks_pdftk <- unlist(purrr::pmap(bookmarks, bookmark_pdftk))
    meta <- append(bookmarks_pdftk, meta)
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

#' @rdname bookmarks
#' @export
set_bookmarks_gs <- function(bookmarks, input, output = input) {
    bookmarks <- as_bookmarks(bookmarks)
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
    bookmarks_gs <- unlist(purrr::pmap(bookmarks, bookmark_gs))
    writeLines(bookmarks_gs, metafile)
    metafile <- normalizePath(metafile, mustWork = TRUE)
    args <- c("-q", "-o", shQuote(target), "-sDEVICE=pdfwrite", "-sAutoRotatePages=None",
              shQuote(input), shQuote(metafile))
    system2(cmd, args, stdout=TRUE)
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}

#### open
#### color
#### style
#### count
as_bookmarks <- function(bookmarks) {
    bookmarks <- as.data.frame(bookmarks)
    stopifnot(hasName(bookmarks, "title"),
              hasName(bookmarks, "page"))
    bookmarks[["title"]] <- as.character(bookmarks[["title"]])
    bookmarks[["page"]] <- as.integer(bookmarks[["page"]])
    if (hasName(bookmarks, "level") && hasName(bookmarks, "count")) {
        bookmarks[["level"]] <- as.integer(bookmarks[["level"]])
        bookmarks[["count"]] <- as.integer(bookmarks[["count"]])
    } else if (hasName(bookmarks, "level")) {
        bookmarks[["level"]] <- as.integer(bookmarks[["level"]])
        bookmarks[["count"]] <- get_count(bookmarks[["level"]])
    } else if (hasName(bookmarks, "count")) {
        bookmarks[["count"]] <- as.integer(bookmarks[["count"]])
        bookmarks[["level"]] <- get_level(bookmarks[["count"]])
    } else {
        bookmarks[["level"]] <- 1L
        bookmarks[["count"]] <- 0L
    }
    if (hasName(bookmarks, "style"))
        bookmarks[["style"]] <- as.integer(bookmarks[["style"]])
    else
        bookmarks[["style"]] <- 0L
    bookmarks
}

get_count <- function(levels) {
    levels <- as.integer(levels)
    n <- length(levels)
    counts <- integer(n)
    for (i in seq_len(n)) {
        if (i < n) {
            count <- 0
            for (j in seq(i + 1L, n)) {
                if (levels[j] > levels[i])
                    count <- count + 1L
                else
                    break
            }
            if (count > 0) counts[i] <- count
        }
    }
    counts
}

get_level <- function(counts) {
    counts <- as.integer(abs(counts))
    n <- length(counts)
    levels <- rep_len(1L, n)
    for (i in seq_len(n)) {
        count <- counts[i]
        if (count > 0) {
            indices <- seq(i + 1L, i + count)
            levels[indices] <- levels[indices] + 1L
        }
    }
    levels
}

bookmark_pdftk <- function(title, level, page, ...) {
    c("BookmarkBegin",
      paste("BookmarkTitle:", title),
      paste("BookmarkLevel:", level),
      paste("BookmarkPageNumber:", page))
}

bookmark_gs <- function(title, page, count, ...) {
    if (count == 0)
        count_str <- ""
    else
        count_str <- sprintf(" /Count %d", count)
    sprintf("[%s /Page %d /View [/XYZ null null null] /Title (%s) /OUT pdfmark",
            count_str, page, title)
}
