skip_if_not_installed("grid")
library("grid")

f <- tempfile(fileext = ".pdf")
on.exit(unlink(f))
pdf(f, onefile = TRUE)
grid.text("Page 1")
grid.newpage()
grid.text("Page 2")
invisible(dev.off())

test_that("n_pages", {
    skip_if_not(supports_n_pages())
    expect_equal(n_pages(f, use_names = FALSE), 2L)
})
test_that("n_pages_qpdf", {
    skip_if_not_installed("qpdf")
    expect_equal(n_pages_qpdf(f, use_names = FALSE), 2L)
})
test_that("n_pages_pdftk", {
    skip_if_not(supports_pdftk())
    expect_equal(n_pages_pdftk(f, use_names = FALSE), 2L)
})
test_that("n_pages_ghostscript", {
    skip_if_not(supports_gs())
    expect_equal(n_pages_gs(f, use_names = FALSE), 2L)
})
test_that("n_pages_exiftool", {
    skip_if_not(supports_exiftool())
    expect_equal(n_pages_exiftool(f, use_names = FALSE), 2L)
})
