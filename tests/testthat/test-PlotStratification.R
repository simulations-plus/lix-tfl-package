# Direct unit tests for the pure helpers in R/PlotStratification.R.
# Connector-backed helpers (.stratifyCovariates, .getCovariates, .getOccasions,
# .matchToData, .software, ...) are exercised indirectly by the plot functions.
# The categorical-without-groups path of .splitCovariates/.filter calls
# .matchToData (connector), so only the continuous / categorical-with-groups
# paths are tested here.

test_that(".parseStratify returns an empty spec for empty input", {
	res <- .parseStratify(list())
	expect_null(res$split_specs)
	expect_null(res$color_specs)
	expect_false(res$mergedSplits)
})

test_that(".parseStratify builds split specs from groups definitions", {
	# categorical: definition is a list of category vectors
	cat <- .parseStratify(list(split = "SEX",
		groups = list(list(name = "SEX", definition = list(c("0"), c("1"))))))
	expect_equal(cat$split_specs[[1]]$name, "SEX")
	expect_equal(cat$split_specs[[1]]$groups,
							 stats::setNames(list(c("0"), c("1")), c("0", "1")))

	# continuous: definition is numeric breaks
	cont <- .parseStratify(list(split = "WT",
		groups = list(list(name = "WT", definition = c(70)))))
	expect_equal(cont$split_specs[[1]]$breaks, 70)
})

test_that(".parseStratify moves split to color when mergedSplits is TRUE", {
	res <- .parseStratify(list(split = "SEX", mergedSplits = TRUE))
	expect_null(res$split_specs)
	expect_equal(res$color_specs[[1]]$name, "SEX")
	expect_true(res$mergedSplits)
})

test_that(".parseStratify passes filter, colors and ids through", {
	res <- .parseStratify(list(
		filter              = list(list("AGE", c(1, 3))),
		colors              = c("red", "blue"),
		individualSelection = list(ids = c(1, 2))))
	expect_equal(res$filter_specs, list(list("AGE", c(1, 3))))
	expect_equal(res$colors, c("red", "blue"))
	expect_equal(res$ids, c(1, 2))
})

test_that(".renameColumns renames present columns and guards size mismatch", {
	df <- data.frame(a = 1, b = 2, c = 3)
	expect_equal(names(.renameColumns(df, c("a", "c"), c("x", "z"))), c("x", "b", "z"))
	expect_equal(names(.renameColumns(df, "nope", "x")), names(df))       # absent -> skipped
	expect_message(res <- .renameColumns(df, "a", c("x", "y")), "must match in size")
	expect_identical(res, df)
})

test_that(".splitCovariates groups a continuous covariate by breaks", {
	covData     <- data.frame(id = 1:4, WT = c(60, 70, 80, 90), stringsAsFactors = FALSE)
	covtypes    <- list(WT = "continuous")
	stratifiers <- list(list(name = "WT", breaks = c(60, 75, 90)))

	out <- .splitCovariates(covData, covtypes, stratifiers)
	expect_true(all(c("WT", "split") %in% names(out)))
	expect_equal(length(unique(as.character(out$WT))), 2)   # [60,75) and [75,90]
})

test_that(".splitCovariates relabels a categorical covariate via group definitions", {
	covData  <- data.frame(id = 1:4, SEX = c("0", "0", "1", "1"), stringsAsFactors = FALSE)
	covtypes <- list(SEX = "categorical")
	strat    <- list(list(name = "SEX",
												groups = stats::setNames(list(c("0"), c("1")), c("F", "M"))))

	out <- .splitCovariates(covData, covtypes, strat)
	expect_equal(length(unique(as.character(out$SEX))), 2)
	expect_true(all(grepl("^SEX: ", levels(out$SEX))))
})

test_that(".filter flags rows outside a continuous interval", {
	covData  <- data.frame(id = 1:4, WT = c(60, 70, 80, 90), stringsAsFactors = FALSE)
	covtypes <- list(WT = "continuous")
	filters  <- list(list(name = "WT", breaks = c(60, 75, 90), min = 60, max = 75))

	out <- .filter(covData, covtypes, filters)
	expect_true("filter" %in% names(out))
	expect_equal(out$filter, c(FALSE, FALSE, TRUE, TRUE))   # 80, 90 fall outside [60,75]
})

test_that(".applyStratifyFilter is a no-op without filter specs", {
	df <- data.frame(id = 1:3, eta = c(0.1, 0.2, 0.3))
	expect_identical(.applyStratifyFilter(df, NULL,   NULL), df)
	expect_identical(.applyStratifyFilter(df, list(), NULL), df)
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-PlotStratification.R")
