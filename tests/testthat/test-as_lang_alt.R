test_that("as_lang_alt()", {
    la <- as_lang_alt("A single title")
    expect_true(inherits(la, "lang_alt"))
    expect_true(is.list(la))
    expect_equal(names(la), "x-default")
    expect_equal(length(la), 1L)

    la <- as_lang_alt(c(en = "An English Title", fr = "A French Title"))
    expect_equal(names(la), c("en", "fr"))
    expect_equal(length(la), 2L)

    la <- as_lang_alt(c(en = "An English Title", fr = "A French Title"),
                      default_lang = "en")
    expect_equal(names(la), c("x-default", "en", "fr"))
    expect_equal(length(la), 3L)

    la <- as_lang_alt(c(en = "An English Title", fr = "A French Title",
                        "x-default" = "A default title"),
                      default_lang = "en")
    expect_equal(names(la), c("x-default", "en", "fr"))
    expect_equal(length(la), 3L)
})
