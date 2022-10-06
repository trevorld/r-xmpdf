# SPDX-License-Identifier: MIT

get_pdftk_metadata <- function(filename) {
    cmd <- pdftk()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    args <- c(filename, "dump_data_utf8")
    xmpdf_system2(cmd, args)
}
