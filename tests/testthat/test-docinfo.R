skip_if_not_installed("grid")
library("grid")

f1 <- tempfile(fileext = ".pdf")
on.exit(unlink(f1), add = TRUE)
pdf(f1, onefile = TRUE)
grid.text("Page 1")
grid.newpage()
grid.text("Page 2")
invisible(dev.off())

test_that("get_docinfo", {
	skip_if_not(supports_get_docinfo())
	expect_equal(get_docinfo(f1)[[1]]$title, "R Graphics Output")
})
test_that("get_docinfo_pdftools", {
	skip_if_not_installed("pdftools")
	expect_equal(get_docinfo_pdftools(f1)[[1]]$title, "R Graphics Output")
})
test_that("get_docinfo_pdftools arbitrary keys", {
	skip_if_not_installed("pdftools")
	skip_if_not(supports_pdftk())

	f2 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f2), add = TRUE)
	di_set <- docinfo(author = "John Doe", ADBETest_MyKey = "My private information")
	set_docinfo_pdftk(di_set, f1, f2)

	di_get <- get_docinfo_pdftools(f2)[[1]]
	expect_equal(di_get$author, "John Doe")
	expect_equal(di_get$get_item("ADBETest_MyKey"), "My private information")
})
test_that("docinfo_pdftk", {
	skip_if_not(supports_pdftk())

	expect_equal(get_docinfo_pdftk(f1)[[1]]$title, "R Graphics Output")

	f2 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f2), add = TRUE)

	di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
	set_docinfo_pdftk(di_set, f1, f2)
	di_get <- get_docinfo_pdftk(f2)[[1]]
	expect_equal(di_get$title, "Two Boring Pages")
	expect_equal(di_get$author, "John Doe")

	di_set <- get_docinfo(f2)[[1]]
	set_docinfo_pdftk(di_set, f1, f2)
	di_get <- get_docinfo_pdftk(f2)[[1]]
	expect_equal(di_get$title, "Two Boring Pages")
	expect_equal(di_get$author, "John Doe")

	# Arbitrary (non-standard) info dictionary entries
	di_set <- docinfo(author = "John Doe", ADBETest_MyKey = "My private information")
	set_docinfo_pdftk(di_set, f1, f2)
	di_get <- get_docinfo_pdftk(f2)[[1]]
	expect_equal(di_get$author, "John Doe")
	expect_equal(di_get$get_item("ADBETest_MyKey"), "My private information")

	# Arbitrary key that is a prefix-collision with a standard key name
	di_set <- docinfo(author = "John Doe")
	di_set$set_item("AuthorX", "custom value")
	set_docinfo_pdftk(di_set, f1, f2)
	di_get <- get_docinfo_pdftk(f2)[[1]]
	expect_equal(di_get$author, "John Doe")
	expect_equal(di_get$get_item("AuthorX"), "custom value")

	# Only partial update
	f4 <- tempfile(fileext = ".pdf")
	pdf(f4)
	plot(0, 0)
	dev.off()
	expect_equal(get_docinfo_pdftk(f4)[[1]]$title, "R Graphics Output")
	set_docinfo_pdftk(docinfo(author = "John Doe"), f4)
	expect_equal(get_docinfo_pdftk(f4)[[1]]$title, "R Graphics Output")
	expect_equal(get_docinfo_pdftk(f4)[[1]]$author, "John Doe")

	# Unicode works?
	skip_if_not(l10n_info()[["UTF-8"]])
	skip_on_os("mac") # CRAN checks on macOS 14
	set_docinfo_pdftk(docinfo(subject = "R\u5f88\u68d2\uff01"), f4)
	expect_equal(get_docinfo_pdftk(f4)[[1]]$subject, "R\u5f88\u68d2\uff01")

	skip_if_not(supports_exiftool())
	f5 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f5), add = TRUE)
	di_set <- docinfo(subject = "A subject\nwith a newline")
	set_docinfo_exiftool(di_set, f1, f5)

	expect_equal(get_docinfo_exiftool(f5)[[1]]$subject, "A subject\nwith a newline")
	expect_equal(get_docinfo_pdftk(f5)[[1]]$subject, "A subject\nwith a newline")

	di_set$creation_date <- "2010"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010")
	di_set$creation_date <- "2010-01"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01")
	di_set$creation_date <- "2010-01-02"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02")
	di_set$creation_date <- "2010-01-02T03"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02T03")
	di_set$creation_date <- "2010-01-02T03:04"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02T03:04")
	di_set$creation_date <- "2010-01-02T03:04:05"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02T03:04:05")
	di_set$creation_date <- "2010-01-02T03:04:05-03"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02T03:04:05-03")
	di_set$creation_date <- "2010-01-02T03:04:05-03:00"
	set_docinfo_exiftool(di_set, f1, f5)
	expect_equal(format(get_docinfo_pdftk(f5)[[1]]$creation_date), "2010-01-02T03:04:05-03:00")
})

test_that("set_docinfo_gs", {
	skip_if_not(supports_gs())

	f3 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f3), add = TRUE)

	di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
	set_docinfo_gs(di_set, f1, f3)

	skip_if_not(supports_get_docinfo())

	di_get <- get_docinfo(f3)[[1]]
	expect_equal(di_get$title, "Two Boring Pages")
	expect_equal(di_get$author, "John Doe")

	set_docinfo_gs(di_set, f1, f3)
	di_get <- get_docinfo(f3)[[1]]
	expect_equal(di_get$title, "Two Boring Pages")
	expect_equal(di_get$author, "John Doe")

	# Only partial update
	f4 <- tempfile(fileext = ".pdf")
	pdf(f4)
	plot(0, 0)
	dev.off()
	expect_equal(get_docinfo(f4)[[1]]$title, "R Graphics Output")
	set_docinfo_gs(docinfo(author = "John Doe"), f4)
	expect_equal(get_docinfo(f4)[[1]]$title, "R Graphics Output")
	expect_equal(get_docinfo(f4)[[1]]$author, "John Doe")

	# Unicode works?
	skip_if_not(l10n_info()[["UTF-8"]])
	skip_on_os("mac") # CRAN checks on macOS 14
	set_docinfo_gs(docinfo(title = "Test title", subject = "R\u5f88\u68d2\uff01"), f4)
	d <- get_docinfo(f4)[[1]]
	expect_equal(d$subject, "R\u5f88\u68d2\uff01")
	expect_equal(d$title, "Test title")

	# Arbitrary (non-standard) info dictionary entries
	di_set <- docinfo(author = "John Doe", ADBETest_MyKey = "My private information")
	set_docinfo_gs(di_set, f1, f3)
	di_get <- get_docinfo(f3)[[1]]
	expect_equal(di_get$author, "John Doe")
	expect_equal(di_get$get_item("ADBETest_MyKey"), "My private information")

	# Arbitrary entry with non-Latin-1 characters
	di_set <- docinfo()
	di_set$set_item("CustomUnicodeKey", "R\u5f88\u68d2\uff01")
	set_docinfo_gs(di_set, f1, f3)
	di_get <- get_docinfo(f3)[[1]]
	expect_equal(di_get$get_item("CustomUnicodeKey"), "R\u5f88\u68d2\uff01")
})

test_that("docinfo_exiftool", {
	skip_if_not(supports_exiftool())

	expect_equal(get_docinfo_exiftool(f1)[[1]]$title, "R Graphics Output")

	f3 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f3), add = TRUE)

	di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
	set_docinfo_exiftool(di_set, f1, f3)

	di_get <- get_docinfo(f3)[[1]]
	expect_equal(di_get$title, "Two Boring Pages")
	expect_equal(di_get$author, "John Doe")

	# Only partial update
	f4 <- tempfile(fileext = ".pdf")
	pdf(f4)
	plot(0, 0)
	dev.off()
	expect_equal(get_docinfo_exiftool(f4)[[1]]$title, "R Graphics Output")
	set_docinfo_exiftool(docinfo(author = "John Doe"), f4)
	expect_equal(get_docinfo_exiftool(f4)[[1]]$title, "R Graphics Output")
	expect_equal(get_docinfo_exiftool(f4)[[1]]$author, "John Doe")

	# Unicode works?
	skip_if_not(l10n_info()[["UTF-8"]])
	skip_on_os("mac") # CRAN checks on macOS 14
	set_docinfo_exiftool(docinfo(subject = "R\u5f88\u68d2\uff01"), f4)
	expect_equal(get_docinfo_exiftool(f4)[[1]]$subject, "R\u5f88\u68d2\uff01")

	# Arbitrary (non-standard) info dictionary entries
	di_set <- docinfo(author = "John Doe", ADBETest_MyKey = "My private information")
	set_docinfo_exiftool(di_set, f1, f3)
	di_get <- get_docinfo_exiftool(f3)[[1]]
	expect_equal(di_get$author, "John Doe")
	expect_equal(di_get$get_item("ADBETest_MyKey"), "My private information")

	# Arbitrary entry with a quote/backslash (exercises `-config` escaping)
	di_set <- docinfo()
	di_set$set_item("WeirdKey", "value with a 'quote' and a \\backslash")
	set_docinfo_exiftool(di_set, f1, f3)
	di_get <- get_docinfo_exiftool(f3)[[1]]
	expect_equal(di_get$get_item("WeirdKey"), "value with a 'quote' and a \\backslash")
})

test_that("get_docinfo_exiftool() sanitizes arbitrary key names", {
	# Pins down a current `exiftool` quirk (not our own validation logic), so skip on
	# CRAN in case a future `exiftool` release changes this tag-name sanitization behavior.
	skip_on_cran()
	skip_if_not(supports_exiftool())
	skip_if_not(supports_pdftk())

	f3 <- tempfile(fileext = ".pdf")
	on.exit(unlink(f3), add = TRUE)

	# non-word characters become `_`, and a leading non-letter gets a `Tag` prefix.
	di_set <- docinfo()
	di_set$set_item("PTEX.Fullbanner", "dot value")
	di_set$set_item("1LeadingDigit", "digit value")
	set_docinfo_pdftk(di_set, f1, f3) # pdftk writes these keys verbatim
	di_get <- get_docinfo_exiftool(f3)[[1]]
	expect_equal(di_get$get_item("PTEX_Fullbanner"), "dot value")
	expect_equal(di_get$get_item("Tag1LeadingDigit"), "digit value")
	expect_false("PTEX.Fullbanner" %in% di_get$arbitrary_keys())
	expect_false("1LeadingDigit" %in% di_get$arbitrary_keys())
})

test_that("docinfo() positional arguments", {
	d <- docinfo("John Doe")
	expect_equal(d$author, "John Doe")
})

test_that("docinfo() skips (with a warning) arbitrary keys unsupported by a backend", {
	d <- docinfo()
	d$set_item("My Key", "val")
	expect_warning(pm <- d$pdfmark(), class = "rlang_warning")
	expect_false(grepl("My Key", pm, fixed = TRUE))
	expect_warning(tags <- d$exiftool_tags(), class = "rlang_warning")
	expect_false("PDF:My Key" %in% names(tags))

	# `pdftk` is much more permissive: spaces (and most other punctuation) are fine
	expect_no_warning(tags <- d$pdftk())
	expect_true(any(grepl("My Key", tags, fixed = TRUE)))
})

test_that("docinfo() rejects keys with embedded control characters for pdftk", {
	d <- docinfo()
	d$set_item("Key\nWithNewline", "val")
	expect_warning(pm <- d$pdfmark(), class = "rlang_warning")
	expect_false(grepl("WithNewline", pm, fixed = TRUE))
	expect_warning(tags <- d$exiftool_tags(), class = "rlang_warning")
	expect_false(any(grepl("WithNewline", names(tags), fixed = TRUE)))
	expect_warning(tags <- d$pdftk(), class = "rlang_warning")
	expect_false(any(grepl("WithNewline", tags, fixed = TRUE)))
})

test_that("docinfo() rejects non-Latin-1 keys for pdftk (silent corruption risk)", {
	d <- docinfo()
	d$set_item("K€y", "val") # K€y contains U+20AC EURO SIGN
	expect_warning(tags <- d$pdftk(), class = "rlang_warning")
	expect_false(any(grepl("K€y", tags, fixed = TRUE)))
})

test_that("docinfo() allows hyphens in arbitrary keys", {
	d <- docinfo()
	d$set_item("My-Key", "val")
	expect_no_warning(d$pdfmark())
	expect_no_warning(d$pdftk())
	expect_no_warning(d$exiftool_tags())
})

test_that("docinfo() allows dots in arbitrary keys for pdftk/gs but not exiftool", {
	d <- docinfo()
	d$set_item("PTEX.Fullbanner", "val")
	expect_no_warning(pm <- d$pdfmark())
	expect_true(grepl("PTEX.Fullbanner", pm, fixed = TRUE))
	expect_no_warning(tags <- d$pdftk())
	expect_true(any(grepl("PTEX.Fullbanner", tags, fixed = TRUE)))
	expect_warning(tags <- d$exiftool_tags(), class = "rlang_warning")
	expect_false("PDF:PTEX.Fullbanner" %in% names(tags))
})

test_that("docinfo()", {
	d <- docinfo(
		author = "John Doe",
		creation_date = "2022-11-11 11:11:11",
		creator = "Generic Creator",
		producer = "Generic Producer",
		title = "Generic Title",
		subject = "Generic Subject",
		keywords = c("Key", "Word"),
		mod_date = "2022-11-11 11:11:11"
	)

	d2 <- docinfo()
	d2$update(d)
	expect_equal(d2$mod_date, d$mod_date)
})

test_that("conversion to/from docinfo()", {
	d <- docinfo(
		author = "John Doe",
		creation_date = "2022-11-11 11:11:11",
		creator = "Generic Creator",
		producer = "Generic Producer",
		title = "Generic Title",
		subject = "Generic Subject",
		keywords = c("Key", "Word"),
		mod_date = "2022-11-11 11:11:11"
	)
	expect_snapshot(print(d))
	dl <- as.list(d)
	expect_equal(dl$author, "John Doe")
	expect_true(is.list(dl))

	x <- as_xmp(d)
	expect_snapshot(print(x))
	expect_equal(x$creator, "John Doe")

	d2 <- as_docinfo(x)
	expect_equal(d2$title, "Generic Title")

	x2 <- as.list(x)
	names(x2) <- gsub("^[[:alpha:]]+:", "", names(x2))
	d3 <- as_docinfo(as_xmp(x2))
	expect_equal(d3$subject, "Generic Subject")
})

test_that("docinfo() arbitrary keys", {
	d <- docinfo(author = "John Doe", ADBETest_MyKey = "My private information")
	expect_equal(d$get_item("ADBETest_MyKey"), "My private information")
	expect_equal(d$get_nonnull_keys(), c("Author", "ADBETest_MyKey"))

	dl <- as.list(d)
	expect_equal(dl$ADBETest_MyKey, "My private information")

	expect_snapshot(print(d))

	d2 <- docinfo()
	d2$update(d)
	expect_equal(d2$get_item("ADBETest_MyKey"), "My private information")

	d$set_item("ADBETest_MyKey", "Updated information")
	expect_equal(d$get_item("ADBETest_MyKey"), "Updated information")
})

test_that("update", {
	d <- docinfo(author = "John Doe")
	expect_equal(d$author, "John Doe")
	d2 <- update(d, author = "Jane Doe")
	expect_equal(d$author, "John Doe")
	expect_equal(d2$author, "Jane Doe")
})
