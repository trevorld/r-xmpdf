skip_if_not_installed("grid")
library("grid")

f1 <- tempfile(fileext = ".pdf")
on.exit(unlink(f1))
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
test_that("docinfo_pdftk", {
    skip_if_not(supports_pdftk())

    expect_equal(get_docinfo_pdftk(f1)[[1]]$title, "R Graphics Output")

    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
    set_docinfo_pdftk(di_set, f1, f2)
    di_get <- get_docinfo_pdftk(f2)[[1]]
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")

    di_set <- docinfo(filename = f2)
    set_docinfo_pdftk(di_set, f1, f2)
    di_get <- get_docinfo_pdftk(f2)[[1]]
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")

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
    set_docinfo_pdftk(docinfo(subject = "R\u5f88\u68d2\uff01"), f4)
    expect_equal(get_docinfo_pdftk(f4)[[1]]$subject, "R\u5f88\u68d2\uff01")
})

test_that("set_docinfo_gs", {
    skip_if_not(supports_gs() && supports_set_docinfo())

    f3 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f3))

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
    set_docinfo_gs(docinfo(title = "Test title", subject = "R\u5f88\u68d2\uff01"), f4)
    d <- get_docinfo(f4)[[1]]
    expect_equal(d$subject, "R\u5f88\u68d2\uff01")
    expect_equal(d$title, "Test title")
})

test_that("docinfo_exiftool", {
    skip_if_not(supports_exiftool())

    expect_equal(get_docinfo_exiftool(f1)[[1]]$title, "R Graphics Output")

    f3 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f3))

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
    set_docinfo_exiftool(docinfo(subject = "R\u5f88\u68d2\uff01"), f4)
    expect_equal(get_docinfo_exiftool(f4)[[1]]$subject, "R\u5f88\u68d2\uff01")
})

test_that("conversion to/from docinfo()", {
    d <- docinfo(author = "John Doe",
                 creation_date = "2022-11-11 11:11:11",
                 creator = "Generic Creator",
                 producer = "Generic Producer",
                 title = "Generic Title",
                 subject = "Generic Subject",
                 keywords = c("Key", "Word"),
                 mod_date = "2022-11-11 11:11:11")
    expect_snapshot(print(d))
    dl <- as.list(d)
    expect_equal(dl$author, "John Doe")
    expect_true(is.list(dl))

    x <- as_xmp(d)
    expect_snapshot(print(x))
    expect_equal(x[["dc:Creator"]], "John Doe")

    d2 <- as_docinfo(x)
    expect_equal(d2$title, "Generic Title")

    x2 <- as.list(x)
    names(x2) <- gsub("^[[:alpha:]]+:", "", names(x2))
    d3 <- as_docinfo(as_xmp(x2))
    expect_equal(d3$subject, "Generic Subject")
})
