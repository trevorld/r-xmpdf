get_pdftk_metadata <- function(filename) {
    cmd <- pdftk()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    args <- c(filename, "dump_data_utf8")
    system2(cmd, args, stdout=TRUE)
}
