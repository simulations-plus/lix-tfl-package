# Direct unit tests for the pure validators in R/PlotChecks.R.
# .checkSplit / .checkFilter call the connector and are covered indirectly by the
# plot functions; only the three pure helpers are tested here.

test_that(".check_list validates list shape and mandatory/optional fields", {
	expect_null(.check_list(NULL, argname = "group"))                 # NULL passthrough
	expect_error(.check_list("notalist", argname = "group"), "must be a list")

	# valid: all mandatory present, no extras -> invisible TRUE
	expect_true(.check_list(list(name = "SEX"), argname = "group",
													listNames = "name", optionalNames = "groups"))

	# missing mandatory field -> error naming it
	expect_error(.check_list(list(groups = 1), argname = "group", listNames = "name"),
							 "Missing mandatory fields")

	# extra (non-allowed) field -> warning, still ignored
	expect_warning(.check_list(list(name = "SEX", bogus = 1), argname = "group",
														 listNames = "name", optionalNames = "groups"),
								 "will be ignored")
})

test_that(".check_in_vector accepts allowed elements and rejects others", {
	expect_null(.check_in_vector(NULL, "arg", allowed_args = c("a", "b")))        # NULL passthrough
	expect_true(.check_in_vector(c("a", "b"), "arg", allowed_args = c("a", "b", "c")))

	# offending value named in the error
	expect_error(.check_in_vector(c("a", "z"), "arg", allowed_args = c("a", "b")), "z")
})

test_that(".check_vector_of_double accepts numeric vectors and rejects non-numeric", {
	expect_null(.check_vector_of_double(NULL, "breaks"))              # NULL passthrough
	expect_equal(.check_vector_of_double(c(1.5, 2.5), "breaks"), c(1.5, 2.5))
	expect_equal(.check_vector_of_double(1:3, "breaks"), 1:3)         # integer accepted
	expect_error(.check_vector_of_double(c("a", "b"), "breaks"), "must be a vector of doubles")
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-PlotChecks.R")
