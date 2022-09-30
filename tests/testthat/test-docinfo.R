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
})

test_that("set_docinfo_gs", {
    skip_if_not(supports_gs())

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
})

test_that("get_docinfo_exiftool", {
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
})

test_that("from_date_pdfmark()", {
    expect_equal(format(from_date_pdfmark("D:20081206"), format = "%Y-%m-%d"),
                 "2008-12-06")
    expect_equal(format(from_date_pdfmark("20081206"), format = "%Y-%m-%d"),
                 "2008-12-06")
    expect_equal(format(from_date_pdfmark("D:2008"), format = "%Y"),
                 "2008")
})
