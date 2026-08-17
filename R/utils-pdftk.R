# SPDX-License-Identifier: MIT

get_pdftk_metadata <- function(filename, timeout = getOption("xmpdf_pdftk_timeout", 60)) {
	f <- tempfile(fileext = ".txt")
	on.exit(unlink(f), add = TRUE)
	cmd <- pdftk()
	filename <- shQuote(normalizePath(filename, mustWork = TRUE))
	args <- c(filename, "dump_data_utf8", "output", f)
	results <- xmpdf_system2(cmd, args, timeout = timeout)
	brio::read_lines(f)
}
