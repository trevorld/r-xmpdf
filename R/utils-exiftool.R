get_exiftool_metadata <- function(filename, tags=NULL) {
    cmd <- exiftool()
    filename <- shQuote(normalizePath(filename, mustWork = TRUE))

    args <- c(tags, "-G1", "-a", "-d", shQuote("%F %T"), "-csv", filename)
    output <- system2(cmd, args, stdout=TRUE)
    df <- utils::read.csv(textConnection(output),
                          check.names = FALSE, stringsAsFactors = FALSE)
    as.list(df)
}

# get_exiftool_metadata_json <- function(filename, tags=NULL) {
#     assert_suggested("jsonlite")
#     cmd <- exiftool()
#     filename <- shQuote(normalizePath(filename, mustWork = TRUE))
#
#     args <- c(tags, "-G1", "-a", "-j", filename)
#     output <- system2(cmd, args, stdout=TRUE)
#     jsonlite::fromJSON(output, simplifyDataFrame = FALSE)[[1]]
# }
