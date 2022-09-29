skip_if_not_installed("grid")
library("grid")


test_that("get_xmp() / set_xmp()", {
    skip_if_not(supports_get_xmp() && supports_set_xmp())
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f))
    pdf(f, onefile = TRUE)
    grid.text("Page 1")
    grid.newpage()
    grid.text("Page 2")
    invisible(dev.off())

    expect_equal(length(get_xmp(f)[[1]]), 0L)

    xmp <- list(Title = "An XMP title")
    set_xmp(xmp, f)
    expect_equal(get_xmp(f)[[1]][["dc:Title"]], "An XMP title")
})

test_that("get_xmp_exiftool() / set_xmp_exiftool()", {
    skip_if_not(supports_exiftool())
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f))
    pdf(f, onefile = TRUE)
    grid.text("Page 1")
    grid.newpage()
    grid.text("Page 2")
    invisible(dev.off())

    expect_equal(length(get_xmp_exiftool(f)[[1]]), 0L)

    xmp <- list(Title = "An XMP title")
    set_xmp_exiftool(xmp, f)
    expect_equal(get_xmp_exiftool(f)[[1]][["dc:Title"]], "An XMP title")
})
