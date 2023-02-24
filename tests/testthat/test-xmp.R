skip_if_not_installed("grid")
library("grid")

test_that("get_xmp() / set_xmp()", {
    skip_if_not(supports_get_xmp() && supports_set_xmp())
    skip_if_not(l10n_info()[["UTF-8"]])
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f))
    pdf(f)
    grid.text("Page 1")
    invisible(dev.off())

    expect_snapshot(print(xmp()))

    set_xmp(xmp(), f)
    expect_snapshot(print(get_xmp(f)[[1]]))

    x <- xmp(alt_text = "An alternative image text",
             attribution_url = "https://example.com/attribution",
             creator = "A creator", #### Vector
             create_date = "2020-10-10", # Digital document creation date
             creator_tool = "A creator tool",
             date_created = "2020",
             ext_description = "An extended description (for accessibility)\nWith newline",
             headline = "A headline",
             keywords = "R, xmpdf",
             modify_date = "2023-01-27T13:37:27.909812682-08:00[America/Los_Angeles]",
             more_permissions = "https://example.com/more-permissions",
             producer = "R",
             description = "A description",
             title = "An XMP title", #### alt-lang
             spdx_id = "CC-BY-4.0",
             `Iptc4xmpCore:Location` = "A sublocation (legacy)",
             `dc:contributor` = "A contributor"
    )
    x <- update(x, `dc:contributor` = "An updated contributor")
    expect_snapshot(print(x))
    expect_snapshot(print(x, mode = "creative_commons"))
    expect_snapshot(print(x, mode = "google_images"))
    expect_snapshot(print(x, mode = "all"))

    expect_true(x$marked)
    expect_equal(x$get_item("dc:contributor"),
                 "An updated contributor")
    expect_equal(x$get_item("Iptc4xmpCore:Location"),
                 "A sublocation (legacy)")
    expect_equal(x$subject, c("R", "xmpdf"))

    xc <- x$clone()
    x$update(xc)
    set_xmp(x, f)
    x2 <- get_xmp(f)[[1]]
    expect_equal(x2$alt_text[["x-default"]], "An alternative image text")
    expect_equal(x2$ext_description[["x-default"]],
                 "An extended description (for accessibility)\nWith newline")
    expect_equal(x2$subject, c("R", "xmpdf"))
    expect_equal(x2$title[["x-default"]], "An XMP title")
    expect_equal(x2$get_item("dc:contributor"),
                 "An updated contributor")
    expect_equal(x2$get_item("Iptc4xmpCore:Location"),
                 "A sublocation (legacy)")
    x2$set_item("x:XMPToolkit", NULL) # make reproducible
    expect_snapshot(print(x2))

    x <- xmp(creator = c("Creator 1", "Creator 2"))
    set_xmp(x, f)
    x3 <- get_xmp(f)[[1]]
    expect_equal(x3$creator, c("Creator 1", "Creator 2"))

})

test_that("auto_xmp", {
    x <- xmp(spdx_id = "CC-BY-SA-4.0")
    expect_true(x$marked)
    x$auto_xmp <- base::setdiff(x$auto_xmp, "xmpRights:Marked")
    expect_null(x$marked)

    x <- update(x, creator = "A creator", date_created = "2020-10-04")
    expect_equal(x$rights, "\u00a9 2020 A creator. Some rights reserved.")
    x$spdx_id <- "CC0-1.0"
    expect_equal(x$rights, "In the public domain. No rights reserved.")
    x$spdx_id <- NULL
    expect_equal(x$rights, "\u00a9 2020 A creator. All rights reserved.")
})

test_that("get_xmp_exiftool() / set_xmp_exiftool()", {
    skip_if_not(supports_exiftool())
    f <- tempfile(fileext = ".pdf")
    on.exit(unlink(f))
    pdf(f)
    grid.text("Page 1")
    invisible(dev.off())

    x <- xmp(creator = "An XMP creator")
    x$title <- as_lang_alt(c("en" = "An English Title", "fr" = "A French Title"), default_lang = "en")
    set_xmp_exiftool(x, f)
    x <- get_xmp_exiftool(f, use_names = FALSE)[[1]]
    expect_equal(x$title[["x-default"]], "An English Title")
    expect_equal(x$title[["fr"]], "A French Title")
})

test_that("as_xmp()", {
    df <- data.frame(`dc.Creator` = "John Doe", `dc:Title` = "A Title",
                     check.names = FALSE, stringsAsFactors = FALSE)
    x <- as_xmp(df)
    expect_equal(x$title[["x-default"]], "A Title")
})
