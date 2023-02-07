test_that("messages", {
    rlang::with_options({
        f <- tempfile(fileext = ".pdf")
        on.exit(unlink(f))
        pdf(f)
        plot(0, 0)
        invisible(dev.off())

        expect_snapshot_error(get_bookmarks(f), class = "xmpdf_suggested_package")
        bm <- data.frame(title=character(), page=integer())
        expect_snapshot_error(set_bookmarks(bm, f), class = "xmpdf_suggested_package")
        expect_snapshot_error(get_docinfo(f), class = "xmpdf_suggested_package")
        expect_snapshot_error(set_docinfo(docinfo(), f), class = "xmpdf_suggested_package")
        expect_snapshot_error(get_xmp(f), class = "xmpdf_suggested_package")
        expect_snapshot_error(set_xmp(xmp(), f), class = "xmpdf_suggested_package")
        expect_snapshot_error(cat_pages(c(f, f)), class = "xmpdf_suggested_package")
        expect_snapshot_error(n_pages(f), class = "xmpdf_suggested_package")

        expect_snapshot_error(gs(), class = "xmpdf_suggested_package")
        expect_snapshot_error(exiftool(), class = "xmpdf_suggested_package")
        expect_snapshot_error(pdftk(), class = "xmpdf_suggested_package")

        expect_snapshot_error({
            fn <- function() assert_suggested("foobar")
            fn()
        }, class = "xmpdf_suggested_package")

    },
    xmpdf_disable_pdftk = TRUE,
    xmpdf_disable_gs = TRUE,
    xmpdf_disable_exiftool = TRUE,
    xmpdf_disable_qpdf = TRUE,
    xmpdf_disable_pdftools = TRUE
    )
})

test_that("enable_feature_message()", {
        expect_snapshot(rlang::inform(enable_feature_message("cat_pages")))
        expect_snapshot(rlang::inform(enable_feature_message("get_bookmarks")))
        expect_snapshot(rlang::inform(enable_feature_message("get_docinfo")))
        expect_snapshot(rlang::inform(enable_feature_message("get_xmp")))
        expect_snapshot(rlang::inform(enable_feature_message("n_pages")))
        expect_snapshot(rlang::inform(enable_feature_message("set_bookmarks")))
        expect_snapshot(rlang::inform(enable_feature_message("set_docinfo")))
        expect_snapshot(rlang::inform(enable_feature_message("set_xmp")))
})
