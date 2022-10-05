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
#' @param filename Filename(s) (pdf) to extract bookmarks from.
#' @param use_names If `TRUE` (default) use `filename` as the names of the result.
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
#' @return `get_bookmarks()` returns a list of data frames with bookmark info (see `bookmarks` parameter for details about columns) plus "total_pages", "filename", and "title" attributes.
#'         `NA` values in the data frame indicates that the backend doesn't report information about this pdf feature.
#'         `set_bookmarks()` returns the (output) filename invisibly.
#' @section Known limitations:
#'
#'   * `get_bookmarks_pdftk()` doesn't report information about bookmarks color, style, and whether the bookmarks
#'     should start open or closed.
#'   * `set_bookmarks_gs()` supports most bookmarks features including color and style but only action supported is to view a particular page.
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
#'   print(get_bookmarks(f)[[1]])
#'   \dontshow{cat("\n")}
#'
#'   bookmarks <- data.frame(title = c("Page 1", "Page 2"), page = c(1, 2))
#'
#'   set_bookmarks(bookmarks, f)
#'   print(get_bookmarks(f)[[1]])
#'   unlink(f)
#' }
#' @name bookmarks
NULL

#' @rdname bookmarks
#' @export
get_bookmarks <- function(filename, use_names = TRUE) {
    if (supports_pdftk()) {
        get_bookmarks_pdftk(filename, use_names = use_names)
    } else {
        msg <- c(need_to_install_str("get_bookmarks()"),
                 install_pdftk_str()
        )
        abort(msg, class = "xmpdf_suggested_package")
    }
}

#' @rdname bookmarks
#' @export
get_bookmarks_pdftk <- function(filename, use_names = TRUE) {
    l <- lapply(filename, get_bookmarks_pdftk_helper)
    l <- use_filenames(l, use_names, filename)
}

#' Concatenate pdf bookmarks
#'
#' `cat_bookmarks()` concatenates a list of bookmarks
#' into a single bookmarks data frame while updating the page numbers.
#' Useful if wanting to concatenate multiple pdf files together and
#' would like to preserve the bookmarks information.
#' @param l A list of bookmark data frames as returned by [get_bookmarks()].
#'          Each data frame should have a "total_pages" attribute.
#'          If `method = "filename"` each data frame should have a "filename" attribute.
#'          If `method = "title"` each data frame should have a "title" attribute.
#' @param method If "flat" simply concatenate the bookmarks while updating page numbers.
#'               If "filename" place each file's bookmarks a level under a new bookmark matching
#'               the (base)name of the filename and then concatenate the bookmarks while updating page numbers.
#'               If "title" place each file's bookmarks a level under a new bookmark matching
#'               the title of the file and then concatenate the bookmarks while updating page numbers.
#' @return A data frame of bookmark data (as suitable for use with [set_bookmarks()]).
#'         A "total_pages" attribute will be set for the theoretical total pages of
#'         the concatenated document represented by the concatenated bookmarks.
#' @examples
#' if (supports_get_bookmarks() && supports_set_bookmarks() && require("grid", quietly = TRUE)) {
#'  # Create two different two-page pdf files
#'  make_pdf <- function(f, title) {
#'    pdf(f, onefile = TRUE, title = title)
#'    grid.text(paste(title, "Page 1"))
#'    grid.newpage()
#'    grid.text(paste(title, "Page 2"))
#'    invisible(dev.off())
#'  }
#'  f1 <- tempfile(fileext = "_doc1.pdf")
#'  on.exit(unlink(f1))
#'  make_pdf(f1, "Document 1")
#'
#'  f2 <- tempfile(fileext = "_doc2.pdf")
#'  on.exit(unlink(f2))
#'  make_pdf(f2, "Document 2")
#'
#'  # Add bookmarks to the two two-page pdf files
#'  bookmarks <- data.frame(title = c("Page 1", "Page 2"),
#'                          page = c(1L, 2L))
#'  set_bookmarks(bookmarks, f1)
#'  set_bookmarks(bookmarks, f2)
#'  l <- get_bookmarks(c(f1, f2))
#'  print(l)
#'
#'  bm <- cat_bookmarks(l, method = "flat")
#'  cat('\nmethod = "flat":\n')
#'  print(bm)
#'
#'  bm <- cat_bookmarks(l, method = "filename")
#'  cat('\nmethod = "filename":\n')
#'  print(bm)
#'
#'  bm <- cat_bookmarks(l, method = "title")
#'  cat('\nmethod = "title":\n')
#'  print(bm)
#'
#'  # `cat_bookmarks()` is useful for setting concatenated pdf files
#'  # created with `cat_pages()`
#'  if (supports_cat_pages()) {
#'     fc <- tempfile(fileext = "_cat.pdf")
#'     on.exit(unlink(fc))
#'     cat_pages(c(f1, f2), fc)
#'     set_bookmarks(bm, fc)
#'     unlink(fc)
#'  }
#'
#'  unlink(f1)
#'  unlink(f2)
#' }
#' @seealso [get_bookmarks()] and [set_bookmarks()] for setting bookmarks.
#'          [cat_pages()] for concatenating pdf files together.
#' @export
cat_bookmarks <- function(l, method = c("flat", "filename", "title")) {
    #### Add styling options for new high level bookmarks open, color, style
    stopifnot(length(l) > 0L)
    method <- match.arg(method, c("flat", "filename", "title"))
    l <- lapply(l, as_bookmarks)

    v_total_pages <- vapply(l, function(x) attr(x, "total_pages"), integer(1L), USE.NAMES = FALSE)
    cum_pages <- cumsum(v_total_pages)
    n_docs <- length(l)
    if (method == "filename") {
        titles <- vapply(l, function(x) basename(attr(x, "filename")), character(1L), USE.NAMES = FALSE)
    } else if (method == "title") {
        titles <- vapply(l, function(x) attr(x, "title"), character(1L), USE.NAMES = FALSE)
    }

    if (method %in% c("filename", "title")) {
        if (hasName(l[[1]], "level"))
            l[[1]]$level <- l[[1]]$level + 1L
        l[[1]] <- rbind(data.frame(title = basename(titles[1L]),
                                   page = 1L,
                                   level = 1L,
                                   count = nrow(l[[1]]),
                                   color = NA_character_,
                                   style = NA_integer_,
                                   stringsAsFactors = FALSE),
                        l[[1]])
    }
    if (n_docs == 1L) {
        return(l[[1L]])
    }
    for (i in seq.int(2L, n_docs)) {
        if (hasName(l[[i]], "page"))
            l[[i]]$page <- l[[i]]$page + cum_pages[i - 1L]
        if (method %in% c("filename", "title")) {
            if (hasName(l[[i]], "level"))
                l[[i]]$level <- l[[i]]$level + 1L
            l[[i]] <- rbind(data.frame(title = basename(titles[i]),
                                       page = cum_pages[i - 1L] + 1L,
                                       level = 1L,
                                       count = nrow(l[[i]]),
                                       color = NA_character_,
                                       style = NA_integer_,
                                       stringsAsFactors = FALSE),
                            l[[i]])
        }
    }

    df <- do.call(function(...) rbind(..., make.row.names = FALSE), l)
    attr(df, "total_pages") <- sum(v_total_pages)
    df
}

df_bookmarks_empty <- data.frame(title = character(0),
                                 page = integer(0),
                                 level = integer(0),
                                 count = integer(0),
                                 color = character(),
                                 style = integer(0),
                                 stringsAsFactors = FALSE)

get_bookmarks_pdftk_helper <- function(filename) {
    meta <- get_pdftk_metadata(filename)
    n_bookmarks <- length(grep("^BookmarkBegin", meta))
    stopifnot(length(grep("^BookmarkTitle", meta)) == n_bookmarks,
              length(grep("^BookmarkLevel", meta)) == n_bookmarks,
              length(grep("^BookmarkPageNumber", meta)) == n_bookmarks)
    df <- if (n_bookmarks > 0) {
        title <- gsub("^BookmarkTitle: ", "", grep("^BookmarkTitle", meta, value = TRUE))
        level <- gsub("^BookmarkLevel: ", "", grep("^BookmarkLevel", meta, value = TRUE))
        page <- gsub("^BookmarkPageNumber: ", "", grep("^BookmarkPageNumber", meta, value = TRUE))
        data.frame(title = title,
                   page = as.integer(page),
                   level = as.integer(level),
                   count = NA_integer_,
                   color = NA_character_,
                   style = NA_integer_,
                   stringsAsFactors = FALSE)
    } else {
        df_bookmarks_empty
    }
    tot_pages <- grep("^NumberOfPages:", meta, value=TRUE)
    if (length(id <- grep("^InfoKey: Title", meta)))
        title <- gsub("^InfoValue: ", "", meta[id + 1])
    else
        title <- NULL
    attr(df, "filename") <- filename
    attr(df, "title") <- title
    attr(df, "total_pages") <- as.integer(strsplit(tot_pages, ":")[[1]][2])
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
        msg <- c(msg, "i" = paste(sQuote("set_bookmarks_gs()"), "can handle these features"),
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
    metafile <- normalizePath(metafile, mustWork = FALSE)
    on.exit(unlink(metafile))
    f <- file(metafile, encoding = "UTF-8")
    open(f, "w")
    writeLines(meta, metafile)
    close(f)

    args <- c(shQuote(input),
              "update_info_utf8", shQuote(metafile),
              "output", shQuote(target))
    xmpdf_system2(cmd, args)
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
    metafile <- tempfile(fileext = ".bin")
    metafile <- normalizePath(metafile, mustWork = FALSE)
    on.exit(unlink(metafile))

    if (any(is.na(iconv(bookmarks$title, to = "latin1")))) { # Has non-Latin-1 characters
        bookmarks_raw <- unlist(purrr::pmap(bookmarks, bookmark_gs_raw))
        writeBin(bookmarks_raw,
                 metafile,
                 endian = "big")
    } else { # Latin-1
        bookmarks_gs <- unlist(purrr::pmap(bookmarks, bookmark_gs))
        f <- file(metafile, encoding = "latin1")
        open(f, "w")
        writeLines(bookmarks_gs, metafile)
        close(f)
    }

    args <- c("-q", "-o", shQuote(target), "-sDEVICE=pdfwrite", "-sAutoRotatePages=None",
              shQuote(input), shQuote(metafile))
    xmpdf_system2(cmd, args)
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}

#### open/closed
as_bookmarks <- function(bookmarks) {
    bookmarks <- as.data.frame(bookmarks)
    if (nrow(bookmarks) == 0) {
        bookmarks$title <- character()
        bookmarks$page <- integer()
        bookmarks$level <- integer()
        bookmarks$count <- integer()
        bookmarks$color <- character()
        bookmarks$style <- integer()
        return(bookmarks)
    }
    stopifnot(hasName(bookmarks, "title"), hasName(bookmarks, "page"))
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
    otc <- bookmark_gs_helper(title, page, count, style, color)
    paste(unlist(otc), collapse="")
}
bookmark_gs_raw <- function(title, page, count, style, color, ...) {
    otc <- bookmark_gs_helper(title, page, count, style, color)
    do.call(raw_pdfmark_entry, otc)
}
bookmark_gs_helper <- function(title, page, count, style, color) {
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
    open <- sprintf("[%s /Page %d /View [/XYZ null null null]\n /Title (",
                    count_str, page)
    close <- sprintf(")\n%s%s /OUT pdfmark",
                     color_str, style_str)

    list(open = open, value = title, close = close)
}
