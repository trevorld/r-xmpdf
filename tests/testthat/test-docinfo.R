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
    expect_equal(get_docinfo(f1)$title, "R Graphics Output")
})
test_that("get_docinfo_pdftools", {
    skip_if_not_installed("pdftools")
    expect_equal(get_docinfo_pdftools(f1)$title, "R Graphics Output")
})
test_that("docinfo_pdftk", {
    skip_if_not(supports_pdftk())

    expect_equal(get_docinfo_pdftk(f1)$title, "R Graphics Output")

    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
    set_docinfo_pdftk(di_set, f1, f2)
    di_get <- get_docinfo_pdftk(f2)
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")

    di_set <- docinfo(filename = f2)
    set_docinfo_pdftk(di_set, f1, f2)
    di_get <- get_docinfo_pdftk(f2)
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")
})

test_that("set_docinfo_gs", {
    skip_if_not(supports_gs())

    f3 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f3))

    di_set <- docinfo(author = "John Doe", title = "Two Boring Pages")
    set_docinfo_gs(di_set, f1, f3)

    skip_if_not(supports_get_docinfo())
    di_get <- get_docinfo(f3)
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")

    set_docinfo(di_set, f1, f3)
    di_get <- get_docinfo(f3)
    expect_equal(di_get$title, "Two Boring Pages")
    expect_equal(di_get$author, "John Doe")
})
