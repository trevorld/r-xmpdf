has_gs <- function() {
    has_cmd("ghostscript", tools::find_gs_cmd)
}

gs <- function() {
    get_cmd("ghostscript", tools::find_gs_cmd)
}

has_cmd <- function(name, cmd_fn = function() Sys.which(name)) {
    cmd_fn() != ""
}

get_cmd <- function(name, cmd_fn = function() Sys.which(name)) {
    cmd <- cmd_fn()
    if (cmd == "")
        abort(sprintf("Can't find system dependency `%s` on PATH", name))
    cmd
}

assert_suggested <- function(package) {
    calling_fn <- deparse(sys.calls()[[sys.nframe()-1]])
    if (!requireNamespace(package, quietly = TRUE)) {
        msg <- c(sprintf("You need to install the suggested package %s to use %s.",
                         sQuote(package), sQuote(calling_fn)),
                 i = sprintf("Use %s.", sQuote(sprintf('install.packages("%s")', package))))
        abort(msg, class = "piecepackr_suggested_package")
    }
}
