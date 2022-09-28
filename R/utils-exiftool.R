get_exiftool_metadata <- function(filename, tags=NULL) {
    cmd <- exiftool()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))
    args <- c(tags, "-csv", filename)
    output <- system2(cmd, args, stdout=TRUE)
    df <- read.csv(textConnection(output),
             stringsAsFactors = FALSE)
    as.list(df)
}
