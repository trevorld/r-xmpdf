# SPDX-License-Identifier: MIT

get_pdftk_metadata <- function(filename) {
    f <- tempfile(fileext = ".txt")
    on.exit(unlink(f))
    cmd <- pdftk()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    args <- c(filename, "dump_data_utf8", "output", f)
    results <- xmpdf_system2(cmd, args)
    brio::read_lines(f)
}
