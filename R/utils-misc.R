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
    sprintf("You must install (only) one suggested R package or system command to use %s.",
            sQuote(fn_call))
}

install_package_str <- function(package) {
     c(x = sprintf("The suggested package %s is not installed.", sQuote(package)),
       i = sQuote(sprintf('install.packages("%s")', package))
       )
}

install_cmd_str <- function(cmd) {
    c(x = sprintf("The system command %s is not installed (or detected).", sQuote(cmd)))
}

install_pdftk_str <- function(cmd) {
    c(install_cmd_str("pdftk"),
      i = paste(sQuote("sudo apt-get install pdftk-java"), "(Debian/Ubuntu)"))
}

install_gs_str <- function(cmd) {
    c(install_cmd_str("ghostscript"),
      i = paste(sQuote("sudo apt-get install ghostscript"), "(Debian/Ubuntu)"))
}

install_exiftool_str <- function(cmd) {
    c(install_cmd_str("exiftool"),
      i = paste(sQuote("sudo apt-get install libimage-exiftool-perl"), "(Debian/Ubuntu)"),
      i = paste(sQuote('install.packages("exiftoolr"); exiftoolr::install_exiftool()'),
                "(Cross-Platform)")
    )
}
