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
#'                Positive count indicates bookmark should start open while
#'                negative count indicates that this bookmark should start closed.
#'                If missing will be inferred from `level` column else will be assumed to be `0L`.
#'                Note some pdf viewers quietly ignore the initially open/closed feature.}
#'   \item{style}{Style of the bookmark (optional, integer).
#'                If `NA_integer_` will be unset (defaults to "plain").
#'                0 is Plain, 1 is Italic, 2 is Bold, and 3 is Bold and italic.
#'                Note many pdf viewers quietly ignore this feature.}
#'   \item{color}{Color of the bookmark (optional, character).
#'                If `NA_character_` will be unset (presumably defaults to "black").
#'                Note many pdf viewers quietly ignore this feature.}
#' }
#' @param input Input pdf filename.
#' @param output Output pdf filename.
#' @return `get_bookmarks()` returns a data frame with bookmark info (see `bookmarks` parameter for details about columns).
#'         An `NA` indicates that the backend doesn't report information about this pdf feature.
#'         `set_bookmarks()` returns the (output) filename invisibly.
#' @section Known limitations:
#'
#'   * `set_bookmarks_gs()` only supports setting the title, page number, level of bookmarks, and whether open/closed.
#'   * `set_bookmarks_gs()` also probably doesn't work with Unicode input.
#'   * `set_bookmarks_pdftk()` only supports setting the title, page number, and level of bookmarks.
#'
#' @seealso [supports_get_bookmarks()], [supports_set_bookmarks()], [supports_gs()], and [supports_pdftk()] to detect support for these features.  For more info about the pdf bookmarks feature see <https://opensource.adobe.com/dc-acrobat-sdk-docs/library/pdfmark/pdfmark_Basic.html#bookmarks-out>.
#' @examples
#' # Create 2-page pdf using `pdf)` and add some bookmarks to it
#' if (supports_set_bookmarks() && supports_get_bookmarks() && require("grid", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".pdf")
#'   pdf(f, onefile = TRUE)
#'   grid.text("Page 1")
#'   grid.newpage()
#'   grid.text("Page 2")
#'   invisible(dev.off())
#'
#'   print(get_bookmarks(f))
#'   \dontshow{cat("\n")}
#'
#'   bookmarks <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
#'
#'   set_bookmarks(bookmarks, f)
#'   print(get_bookmarks(f))
#'   unlink(f)
#' }
#' @name bookmarks
NULL

#' @rdname bookmarks
#' @export
get_bookmarks <- function(filename) {
    if (supports_pdftk()) {
        get_bookmarks_pdftk(filename)
    } else {
        msg <- c(need_to_install_str("get_bookmarks()"),
                 install_pdftk_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname bookmarks
#' @export
get_bookmarks_pdftk <- function(filename) {
    meta <- get_pdftk_metadata(filename)
    id_bookmark <- grep("^BookmarkBegin", meta)
    df <- if (length(id_bookmark)) {
        title <- gsub("^BookmarkTitle: ", "", meta[id_bookmark + 1L])
        level <- gsub("^BookmarkLevel: ", "", meta[id_bookmark + 2L])
        page <- gsub("^BookmarkPageNumber: ", "", meta[id_bookmark + 3L])
        data.frame(title = title,
                   page = as.integer(page),
                   level = as.integer(level),
                   count = NA_integer_,
                   color = NA_character_,
                   style = NA_integer_,
                   stringsAsFactors = FALSE)
    } else {
        data.frame(title = character(0),
                   page = integer(0),
                   level = integer(0),
                   count = integer(0),
                   color = character(),
                   style = integer(0),
                   stringsAsFactors = FALSE)
    }
    df
}

#' @rdname bookmarks
#' @export
set_bookmarks <- function(bookmarks, input, output = input) {
    if (supports_gs()) {
        set_bookmarks_gs(bookmarks, input, output)
    } else if (supports_pdftk()) {
        set_bookmarks_pdftk(bookmarks, input, output)
    } else {
        msg <- c(need_to_install_str("set_bookmarks()"),
                 install_gs_str(),
                 install_pdftk_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

should_pdftk_message <- function(bookmarks) {
    any(bookmarks$count < 0) || any(!is.na(bookmarks$color)) || any(!is.na(bookmarks$style))
}

#' @rdname bookmarks
#' @export
set_bookmarks_pdftk <- function(bookmarks, input, output = input) {
    bookmarks <- as_bookmarks(bookmarks)
    if (should_pdftk_message(bookmarks)) {
        msg <- c("!" = paste(sQuote("set_bookmarks_pdftk()"),
                            "will ignore certain requested bookmarks features:"))
        if (any(bookmarks$count < 0))
            msg <- c(msg, "*" = paste(sQuote("set_bookmarks_pdftk()"), "treats negative",
                                      sQuote("count"), "values as positive ones."))
        if (any(!is.na(bookmarks$color)))
            msg <- c(msg, "*" = paste(sQuote("set_bookmarks_pdftk()"),
                                      "ignores non-missing", sQuote("color"), "values."))
        if (any(!is.na(bookmarks$style)))
            msg <- c(msg, "*" = paste(sQuote("set_bookmarks_pdftk()"),
                                      "ignores non-missing", sQuote("style"), "values."))
        msg <- c(msg, "i" = paste(sQuote("set_bookmarks_gs()"),
                                  "can handle these features (but not Unicode)"),
                 "i" = paste("You can suppress these messages with",
                             sQuote('suppressMessages(expr, classes = "xmpdf_inform")')))
        inform(msg, class = "xmpdf_inform")
    }

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
    if (nrow(bookmarks) == 0)
        return (data.frame(title = character(),
                           page = integer(),
                           level = integer(),
                           count = integer(),
                           color = character(),
                           style = integer()))
    stopifnot(hasName(bookmarks, "title"),
              hasName(bookmarks, "page"))
    bookmarks[["title"]] <- as.character(bookmarks[["title"]])
    bookmarks[["page"]] <- as.integer(bookmarks[["page"]])
    if (hasName(bookmarks, "level") && hasName(bookmarks, "count")) {
        bookmarks[["level"]] <- as.integer(bookmarks[["level"]])
        bookmarks[["count"]] <- as.integer(bookmarks[["count"]])
        if (any(is.na(bookmarks[["count"]])))
            bookmarks[["count"]] <- get_count(bookmarks[["level"]])
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
    if (hasName(bookmarks, "color"))
        bookmarks[["color"]] <- as.character(bookmarks[["color"]])
    else
        bookmarks[["color"]] <- NA_character_
    if (hasName(bookmarks, "style"))
        bookmarks[["style"]] <- as.integer(bookmarks[["style"]])
    else
        bookmarks[["style"]] <- NA_integer_
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

bookmark_gs <- function(title, page, count, style, color, ...) {
    if (count == 0)
        count_str <- ""
    else
        count_str <- paste(" /Count", count)
    if (is.na(style))
        style_str <- ""
    else
        style_str <- paste0(" /F ", style, "\n")
    if (is.na(color)) {
        color_str <- ""
    } else {
        rgb <- grDevices::col2rgb(color)
        color_str <- sprintf("/C [%f %f %f]\n",
                             rgb[1] / 255,
                             rgb[2] / 255,
                             rgb[3] / 255)
    }
    sprintf("[%s /Page %d /View [/XYZ null null null]\n /Title (%s)\n%s%s /OUT pdfmark",
            count_str, page, title, color_str, style_str)
}
