# SPDX-License-Identifier: MIT

get_exiftool_metadata <- function(filename, tags=NULL) {
    stopifnot(supports_exiftool())
    filename <- normalizePath(filename, mustWork = TRUE)

    # Date format equivalent to R's "%Y-%m-%dT%H:%M:%S%z"
    if (requireNamespace("exiftoolr", quietly = TRUE)) {
        args <- c(tags, "-G1", "-a", "-n", "-j", filename)
        output <- exiftoolr::exif_call(args, quiet = TRUE)
    } else {
        cmd <- exiftool()
        f <- tempfile(fileext = ".txt")
        on.exit(unlink(f))
        args <- c(tags, "-G1", "-a", "-n", "-j", filename)
        writeLines(args, f)
        args <- c("-@", shQuote(f))
        if (length(cmd) == 2L) { # i.e. c("/path/to/perl", "path/to/exiftool")
            args <- c(cmd[-1L], args)
            cmd <- cmd[1L]
        }
        output <- xmpdf_system2(cmd, args)
    }
    jsonlite::fromJSON(output, simplifyDataFrame = FALSE)[[1]]
}

as_exif_value <- function(x) {
    if (inherits(x, c("datetimeoffset", "POSIXt"))) {
        datetimeoffset::format_exiftool(datetimeoffset::as_datetimeoffset(x))
    } else if (is.logical(x)) {
        n <- length(x)
        ifelse(x, rep_len("True", n), rep_len("False", n))
    } else if (is.character(x)) {
        x
    } else {
        as.character(x)
    }
}

#' @param tags Named list of metadata tags to set
#' @noRd
set_exiftool_metadata <- function(tags, input, output = input) {
    stopifnot(supports_exiftool())
    input <- normalizePath(input, mustWork = TRUE)
    output <- normalizePath(output, mustWork = FALSE)
    if (input == output) {
        target <- tempfile(fileext = tools::file_ext(input))
        on.exit(unlink(target))
    } else {
        target <- output
    }
    nms <- character(0)
    values <- character(0)
    ops <- character(0)
    for (name in names(tags)) {
        value <- as_exif_value(tags[[name]])
        #### lang-alt?
        values <- append(values, value)
        if (length(value) > 1) {
            n <- length(value)
            nms <- append(nms, rep_len(name, n))
            ops <- c(ops, rep_len("=", n))
        } else {
            nms <- append(nms, name)
            ops <- append(ops, "=")
        }
    }
    args <- paste0("-", nms, ops, values)
    if (requireNamespace("exiftoolr", quietly = TRUE)) {
        args <- c(args, "-n", "-o", target, input)
        results <- exiftoolr::exif_call(args, quiet = TRUE)
    } else {
        cmd <- exiftool()
        f <- tempfile(fileext = ".txt")
        on.exit(unlink(f))
        args <- c(args, "-n", "-o", target, input)
        writeLines(args, f)
        args <- c("-@", shQuote(f))
        if (length(cmd) == 2L) { # i.e. c("/path/to/perl", "path/to/exiftool")
            args <- c(cmd[-1L], args)
            cmd <- cmd[1L]
        }
        results <- xmpdf_system2(cmd, args)
    }
    if (input == output)
        file.copy(target, output, overwrite = TRUE)
    invisible(output)
}

# get_exiftool_metadata_json <- function(filename, tags=NULL) {
#     assert_suggested("jsonlite")
#     cmd <- exiftool()
#     filename <- shQuote(normalizePath(filename, mustWork = TRUE))
#
#     args <- c(tags, "-G1", "-a", "-j", filename)
#     output <- xmpdf_system2(cmd, args)
#     jsonlite::fromJSON(output, simplifyDataFrame = FALSE)[[1]]
# }
