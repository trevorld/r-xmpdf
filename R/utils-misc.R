assert_suggested <- function(package) {
    calling_fn <- deparse(sys.calls()[[sys.nframe()-1]])
    if (!requireNamespace(package, quietly = TRUE)) {
        msg <- c(sprintf("You need to install the suggested package %s to use %s.",
                         sQuote(package), sQuote(calling_fn)),
                 i = sprintf("Use %s.", sQuote(sprintf('install.packages("%s")', package))))
        abort(msg, class = "piecepackr_suggested_package")
    }
}
