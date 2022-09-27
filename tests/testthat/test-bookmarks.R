skip_if_not_installed("grid")
library("grid")

f1 <- tempfile(fileext = ".pdf")
on.exit(unlink(f1))
pdf(f1, onefile = TRUE)
grid.text("Page 1")
grid.newpage()
grid.text("Page 2")
invisible(dev.off())

test_that("get_bookmarks", {
    skip_if_not(supports_get_bookmarks())
    expect_equal(nrow(get_bookmarks(f1)), 0L)
})

test_that("set_bookmarks", {
    skip_if_not(supports_set_bookmarks())
    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- data.frame(title = "Page 1", "Page 2",
                            page = c(1L, 2L))
    set_bookmarks(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)), 2L)

    bookmarks <- data.frame(title = "Page 2",
                            page = c(2L))
    set_bookmarks(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)), 1L)
})

test_that("bookmarks_pdftk", {
    skip_if_not(has_cmd("pdftk"))

    expect_equal(nrow(get_bookmarks(f1)), 0L)

    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- data.frame(title = "Page 1", "Page 2",
                            page = c(1L, 2L))
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)), 2L)

    bookmarks <- data.frame(title = "Page 2",
                            page = c(2L))
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)), 1L)
})
