# SPDX-License-Identifier: MIT

get_exiftool_metadata <- function(filename, tags=NULL) {
    stopifnot(supports_exiftool())
    filename <- normalizePath(filename, mustWork = TRUE)

    # Date format equivalent to R's "%Y-%m-%dT%H:%M:%S%z"
    if (requireNamespace("exiftoolr", quietly = TRUE)) {
        args <- c(tags, "-G1", "-a", "-d", "%FT%T%z", "-csv", filename)
        output <- exiftoolr::exif_call(args, quiet = TRUE)
    } else {
        cmd <- exiftool()
        f <- tempfile(fileext = ".txt")
        on.exit(unlink(f))
        args <- c(tags, "-G1", "-a", "-d", "%FT%T%z", "-csv", filename)
        writeLines(args, f)
        args <- c("-@", shQuote(f))
        if (length(cmd) == 2L) { # i.e. c("/path/to/perl", "path/to/exiftool")
            args <- c(cmd[-1L], args)
            cmd <- cmd[1L]
        }
        output <- xmpdf_system2(cmd, args)
    }
    df <- utils::read.csv(textConnection(output),
                          check.names = FALSE, stringsAsFactors = FALSE)
    as.list(df)
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
    for (name in names(tags)) {
        # We're using a date format equivalent to R's "%FTT%z"
        tags[[name]] <- as.character(tags[[name]], format = "%FT%T%z")
    }
    nms <- names(tags)
    values <- unlist(tags)
    tags <- paste0("-", nms, "=", values)
    # Date format equivalent to R's "%FT%T%z"
    if (requireNamespace("exiftoolr", quietly = TRUE)) {
        args <- c(tags, "-d", "%FT%T%z", "-o", target, input)
        results <- exiftoolr::exif_call(args, quiet = TRUE)
    } else {
        cmd <- exiftool()
        f <- tempfile(fileext = ".txt")
        on.exit(unlink(f))
        args <- c(tags, "-d", "%FT%T%z", "-o", target, input)
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
