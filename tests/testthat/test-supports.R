test_that("`cmd_found()`", {
	expect_true(cmd_found("/usr/bin/exiftool"))
	expect_true(cmd_found(c("/usr/bin/perl", "/path/to/exiftool")))
	expect_false(cmd_found(""))
	expect_false(cmd_found(character(0)))
	expect_false(cmd_found(NA_character_))
	expect_false(cmd_found(c("/usr/bin/perl", NA_character_)))
})

test_that("`find_exiftool_cmd()` caches successful detections", {
	fake <- withr::local_tempfile(lines = character())
	cmd <- c("/usr/bin/perl", fake) # i.e. `perl` + script command
	n <- 0L
	local_mocked_bindings(
		.xmpdf_cache = new.env(parent = emptyenv()),
		find_exiftool_cmd_uncached = function() {
			n <<- n + 1L
			cmd
		}
	)
	expect_true(supports_exiftool())
	expect_equal(exiftool(), cmd)
	expect_true(supports_exiftool())
	expect_equal(n, 1L)

	# `xmpdf_disable_exiftool` option is honored even when detection is cached
	rlang::with_options(expect_false(supports_exiftool()), xmpdf_disable_exiftool = TRUE)
	expect_true(supports_exiftool())
	expect_equal(n, 1L)

	# changing `ET_EXIFTOOL_PATH` or `ET_PERL_PATH` invalidates the cache
	withr::local_envvar(ET_EXIFTOOL_PATH = "/some/other/exiftool")
	expect_true(supports_exiftool())
	expect_equal(n, 2L)
	withr::local_envvar(ET_PERL_PATH = "/some/other/perl")
	expect_true(supports_exiftool())
	expect_equal(n, 3L)
	expect_true(supports_exiftool())
	expect_equal(n, 3L)

	# a cached command whose script no longer exists is re-detected
	unlink(fake)
	expect_true(supports_exiftool())
	expect_equal(n, 4L)
})

test_that("`find_exiftool_cmd()` does not cache failed detections", {
	n <- 0L
	local_mocked_bindings(
		.xmpdf_cache = new.env(parent = emptyenv()),
		find_exiftool_cmd_uncached = function() {
			n <<- n + 1L
			""
		}
	)
	expect_false(supports_exiftool())
	expect_false(supports_exiftool())
	expect_error(exiftool(), class = "xmpdf_suggested_package")
	expect_equal(n, 3L)
})
