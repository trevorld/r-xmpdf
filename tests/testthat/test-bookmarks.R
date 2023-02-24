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

    bookmarks <- get_bookmarks(f1)[[1]]
    set_bookmarks(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 0L)

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

    bookmarks <- get_bookmarks(f1)[[1]]
    set_bookmarks_gs(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 0L)

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
                            count = c(1L, -1L, 0),
                            fontface = c("italic", "bold", "bold.italic"),
                            color = c("black", "red", "blue"))
    set_bookmarks_gs(bookmarks, f1, f2)
    bm <- get_bookmarks(f2)[[1]]
    expect_equal(nrow(bm), 3L)
    expect_equal(attr(bm, "total_pages"), 2L)

    # Does Unicode work
    skip_if_not(l10n_info()[["UTF-8"]])
    bookmarks <- data.frame(title = c("R\u5f88\u68d2\uff01", "Page 1", "Page 2"),
                            level = c(1, 2, 2),
                            page = c(1L, 1L, 2L))
    set_bookmarks_gs(bookmarks, f1, f2)
    bm <- get_bookmarks(f2)[[1]]
    expect_equal(bm$title[1], "R\u5f88\u68d2\uff01")

    # input = output
    bookmarks <- data.frame(title = c("R\u5f88\u68d2\uff01", "Page 1", "Page 2"),
                            level = c(1, 2, 2),
                            page = c(1L, 1L, 2L))
    set_bookmarks_gs(bookmarks, f2, f2)
    bm <- get_bookmarks(f2)[[1]]
    expect_equal(bm$title[1], "R\u5f88\u68d2\uff01")
})

test_that("bookmarks_pdftk", {
    skip_if_not(supports_pdftk())

    expect_equal(nrow(get_bookmarks(f1)[[1]]), 0L)

    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))

    bookmarks <- get_bookmarks(f1)[[1]]
    set_bookmarks_pdftk(bookmarks, f1, f2)
    expect_equal(nrow(get_bookmarks(f2)[[1]]), 0L)

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

    # Does Unicode work
    skip_if_not(l10n_info()[["UTF-8"]])
    bookmarks <- data.frame(title = c("R\u5f88\u68d2\uff01", "Page 1", "Page 2"),
                            level = c(1, 2, 2),
                            page = c(1L, 1L, 2L))
    set_bookmarks_pdftk(bookmarks, f2, f2)
    bm <- get_bookmarks_pdftk(f2)[[1]]
    expect_equal(bm$title[1], "R\u5f88\u68d2\uff01")

    # Test unsupported feature messages
    bookmarks <- data.frame(title = c("Front", "Page 1", "Page 2"),
                            count = c(-1L, 1L, 0L),
                            page = c(1L, 1L, 2L),
                            fontface = c("italic", "bold", "plain"),
                            color = c("black", "blue", "red"))
    expect_snapshot(set_bookmarks_pdftk(bookmarks, f1, f2))
})

test_that("`get_count()` and `get_level()`", {
    expect_equal(get_count(c(1, 2, 3, 2), c(TRUE, FALSE, NA, NA)),
                           c(2, -1, 0, 0))
    expect_equal(get_count(c(1, 2, 3, 2, 3, 1, 2), c(TRUE, FALSE, NA, TRUE, NA, FALSE, NA)),
                 c(2, -1, 0, 1, 0, -1, 0))
    expect_error(get_level(c(2, 1, 0)), "mis-specified")
    expect_error(get_level(c(0, 0, 2)), "mis-specified")
    expect_equal(get_level(c(2, 1, 0, 0)), c(1, 2, 3, 2))
    expect_equal(get_level(c(-2, 1, 0, 0)), c(1, 2, 3, 2))
})

test_that("`cat_bookmarks()` works", {
    skip_if_not(supports_get_bookmarks() && supports_set_bookmarks())
    bookmarks <- data.frame(title = c("Page 1", "Page 2"),
                            page = c(1L, 2L))
    f2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f2))
    set_bookmarks(bookmarks, f1, f2)

    f3 <- tempfile(fileext = ".pdf")
    on.exit(unlink(f3))
    set_bookmarks(bookmarks, f1, f3)

    l <- get_bookmarks(c(f2, f3))
    bm <- cat_bookmarks(l, method = "flat")
    expect_equal(bm$page, 1:4)
    expect_equal(bm$title, c("Page 1", "Page 2",
                             "Page 1", "Page 2"))

    bm <- cat_bookmarks(l, method = "filename")
    expect_equal(bm$page, c(1L, 1L, 2L, 3L, 3L, 4L))
    expect_equal(bm$level, c(1L, 2L, 2L, 1L, 2L, 2L))
    expect_equal(bm$count, c(2L, 0L, 0L, 2L, 0L, 0L))
    expect_equal(bm$title, c(basename(f2), "Page 1", "Page 2",
                             basename(f3), "Page 1", "Page 2"))

    bm <- cat_bookmarks(l, method = "title")
    expect_equal(bm$page, c(1L, 1L, 2L, 3L, 3L, 4L))
    expect_equal(bm$level, c(1L, 2L, 2L, 1L, 2L, 2L))
    expect_equal(bm$count, c(2L, 0L, 0L, 2L, 0L, 0L))
    expect_equal(bm$title, c("R Graphics Output", "Page 1", "Page 2",
                             "R Graphics Output", "Page 1", "Page 2"))
})
