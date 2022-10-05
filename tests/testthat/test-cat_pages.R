skip_if_not_installed("grid")
library("grid")

# Create two different two-page pdf files
make_pdf <- function(f, title) {
  pdf(f, onefile = TRUE, title = title)
  grid.text(paste(title, "Page 1"))
  grid.newpage()
  grid.text(paste(title, "Page 2"))
  invisible(dev.off())
}
f1 <- tempfile(fileext = "_doc1.pdf")
on.exit(unlink(f1))
make_pdf(f1, "Document 1")

f2 <- tempfile(fileext = "_doc2.pdf")
on.exit(unlink(f2))
make_pdf(f2, "Document 2")

test_that("cat_pages()", {
    skip_if_not(supports_cat_pages())
    fc <- tempfile(fileext = "_cat.pdf")
    on.exit(unlink(fc))
    cat_pages(c(f1, f2), fc)

    expect_equal(n_pages(fc, use_names = FALSE), 4L)
})

test_that("cat_pages_gs()", {
    skip_if_not(supports_qpdf())
    fc <- tempfile(fileext = "_cat.pdf")
    on.exit(unlink(fc))
    cat_pages_gs(c(f1, f2), fc)

    expect_equal(n_pages(fc, use_names = FALSE), 4L)
})

test_that("cat_pages_pdftk()", {
    skip_if_not(supports_pdftk())
    fc <- tempfile(fileext = "_cat.pdf")
    on.exit(unlink(fc))
    cat_pages_pdftk(c(f1, f2), fc)

    expect_equal(n_pages(fc, use_names = FALSE), 4L)
})

test_that("cat_pages_qpdf()", {
    skip_if_not(supports_qpdf())
    fc <- tempfile(fileext = "_cat.pdf")
    on.exit(unlink(fc))
    cat_pages_qpdf(c(f1, f2), fc)

    expect_equal(n_pages(fc, use_names = FALSE), 4L)
})
