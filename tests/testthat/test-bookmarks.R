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
    expect_equal(nrow(get_bookmarks(f1)[[1]]), 0L)
})

test_that("set_bookmarks", {
    skip_if_not(supports_get_bookmarks())
    skip_if_not(supports_set_bookmarks())
    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- data.frame(title = c("Page 1", "Page 2"),
                            page = c(1L, 2L))
    set_bookmarks(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 2L)

    bookmarks <- data.frame(title = "Page 2",
                            page = c(2L))
    set_bookmarks(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 1L)
})

test_that("set_bookmarks_gs", {
    skip_if_not(supports_get_bookmarks())
    skip_if_not(supports_gs())
    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- data.frame(title = c("Page 1", "Page 2"),
                            page = c(1L, 2L))
    set_bookmarks_gs(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 2L)

    bookmarks <- data.frame(title = "Page 2",
                            page = c(2L))
    set_bookmarks_gs(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 1L)

    bookmarks <- data.frame(title = c("Front", "Page 1", "Page 2"),
                            level = c(1, 2, 2),
                            page = c(1L, 1L, 2L))
    set_bookmarks_gs(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 3L)

    # Negative count (closed)
    bookmarks <- data.frame(title = c("Front", "Page 1", "Page 2"),
                            page = c(1L, 1L, 2L),
                            count = c(2L, -1L, 0),
                            style = c(1L, 2L, 3L),
                            color = c("black", "red", "blue"))
    set_bookmarks_gs(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 3L)
})

test_that("bookmarks_pdftk", {
    skip_if_not(supports_pdftk())

    expect_equal(nrow(get_bookmarks(f1)[[1]]), 0L)

    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- data.frame(title = c("Page 1", "Page 2"),
                            page = c(1L, 2L))
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 2L)

    bookmarks <- data.frame(title = "Page 2",
                            page = c(2L))
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 1L)

    bookmarks <- data.frame(title = c("Front", "Page 1", "Page 2"),
                            level = c(1, 2, 2),
                            page = c(1L, 1L, 2L))
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 3L)
})

test_that("`get_count()` and `get_level()`", {
    expect_equal(get_count(c(1, 2, 2)), c(2, 0, 0))
    expect_equal(get_level(c(2, 0, 0)), c(1, 2, 2))
    expect_equal(get_level(c(-2, 0, 0)), c(1, 2, 2))
})
