assert_suggested <- function(package) {
    calling_fn <- deparse(sys.calls()[[sys.nframe()-1]])
    if (!requireNamespace(package, quietly = TRUE)) {
        msg <- c(sprintf("You need to install the suggested package %s to use %s.",
                         sQuote(package), sQuote(calling_fn)),
                 install_package_str(package))
        abort(msg, class = "xmpdf_suggested_package")
    }
}

need_to_install_str <- function(fn_call) {
    c("!" = sprintf("You must install (only) one suggested R package or system command to use %s",
                    sQuote(fn_call)))
}

install_package_str <- function(package) {
     c(x = sprintf("The suggested package %s is not installed", sQuote(package)),
       i = sQuote(sprintf('install.packages("%s")', package))
       )
}

install_cmd_str <- function(cmd) {
    c(x = sprintf("The system command %s is not installed (or detected)", sQuote(cmd)))
}

install_pdftk_str <- function() {
    c(install_cmd_str("pdftk"),
      i = "<https://gitlab.com/pdftk-java/pdftk> (Official)",
      i = paste(sQuote("sudo apt-get install pdftk-java"), "(Debian/Ubuntu)"),
      i = paste(sQuote("brew install pdftk-java"), "(Homebrew)"),
      i = paste(sQuote("choco install pdftk-java"), "(Chocolately)"),
      i = paste(sQuote('Sys.setenv(PDFTK_PATH = "/path/to/pdftk")'),
                "if installed but not detected on PATH")
    )
}

install_gs_str <- function() {
    c(install_cmd_str("ghostscript"),
      i = "<https://www.ghostscript.com/releases/gsdnld.html> (Official)",
      i = paste(sQuote("sudo apt-get install ghostscript"), "(Debian/Ubuntu)"),
      i = paste(sQuote("brew install ghostscript"), "(Homebrew)"),
      i = paste(sQuote("choco install ghostscript"), "(Chocolately)"),
      i = paste(sQuote('Sys.setenv(R_GSCMD = "/path/to/gs")'),
                "if installed but not detected on PATH")
    )
}

install_exiftool_str <- function() {
    c(install_cmd_str("exiftool"),
      i = "<https://exiftool.org/index.html> (Official)",
      i = paste(sQuote('install.packages("exiftoolr"); exiftoolr::install_exiftool()'),
                "(Cross-Platform)"),
      i = paste(sQuote("sudo apt-get install libimage-exiftool-perl"), "(Debian/Ubuntu)"),
      i = paste(sQuote("brew install exiftool"), "(Homebrew)"),
      i = paste(sQuote("choco install exiftool"), "(Chocolately)"),
      i = paste(sQuote('Sys.setenv(ET_EXIFTOOL_PATH = "/path/to/exiftool")'),
                "if installed but not detected on PATH")
    )
}

use_filenames <- function(l, use_names, filename) {
    if (use_names)
        names(l) <- filename
    else
        names(l) <- NULL
    l
}

xmpdf_system2 <- function(cmd, args) {
    output <- system2(cmd, args, stdout = TRUE)
    if (!is.null(attr(output, "status"))) {
        msg <- c(paste(sQuote("system2()"), "command failed."))
        abort(msg)
    }
    invisible(output)
}
