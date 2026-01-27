pkx_project_1 = "test-projects/pkanalix/project_remifentanil_excl.pkx"
pkx_project_2 = "test-projects/pkanalix/project_occasions2.pkx"
pkx_project_3 = "test-projects/pkanalix/project_aPCSK9_SAD_route.pkx"
pkx_project_4 = "test-projects/pkanalix/project_steadystate_infusion.pkx"

test_that("tableNCAConcentration basic (no stratification)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test basic functionality with no covariates
	table <- tableNCAConcentration(projectFile = pkx_project_1)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Time" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify metrics columns exist (at least some standard ones)
	expect_true(any(c("mean", "SD", "CV", "geoMean", "geoCV", "median", "Nobs") %in% names(table)))

	# Verify Time column is sorted
	expect_true(all(diff(suppressWarnings(as.numeric(table$Time))) >= 0, na.rm = TRUE))
})

test_that("tableNCAConcentration with stratification (stratify_columns = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates but no column stratification
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		stratify_columns = FALSE)

	# Verify covariate column exists
	expect_true("TINFCAT" %in% names(table))
	expect_true("Time" %in% names(table))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAConcentration with column stratification (stratify_columns = TRUE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates and column stratification
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		stratify_columns = TRUE)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Time" %in% names(table))

	# Verify "Overall" column exists (always included in stratified layout)
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))

	# Verify no TINFCAT column (covariates become headers when stratify_columns=TRUE)
	expect_false("TINFCAT" %in% names(table))

	# Verify at least one stratified column exists (besides Time and Overall)
	stratified_cols <- setdiff(names(table), c("Time", "Statistic"))
	expect_gt(length(stratified_cols), 1)
})

test_that("tableNCAConcentration with specific metrics (single metric)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with single metric
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		stratify_columns = TRUE,
		metrics          = "MeanSD")

	# Verify "Statistic" column does NOT exist for single metric
	expect_false("Statistic" %in% names(table))
	expect_true("Time" %in% names(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAConcentration with specific metrics (multiple metrics)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple metrics
	table <- tableNCAConcentration(
		projectFile = pkx_project_1,
		covariates = "TINFCAT",
		stratify_columns = TRUE,
		metrics = c("MeanSD", "GeomCV"))

	# Verify "Statistic" column EXISTS for multiple metrics
	expect_true("Statistic" %in% names(table))
	expect_true("Time" %in% names(table))

	# Verify statistics are as requested
	expect_true(all(c("MeanSD", "GeomCV") %in% table$Statistic |
									all(grepl("Mean.*SD|Geometric.*CV", table$Statistic))))
})

test_that("tableNCAConcentration with useNominalTime", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with nominal time (project_occasions2 likely has nominal time)
	# If no nominal time available, should fall back to actual time
	table <- tableNCAConcentration(
		projectFile    = pkx_project_2,
		useNominalTime = TRUE)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Time" %in% names(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAConcentration with covariate_labels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with custom covariate labels
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		covariate_labels = "Infusion Category",
		stratify_columns = TRUE)

	# Verify custom labels appear in column headers
	header_names <- names(table)
	expect_true(any(grepl("Infusion Category", header_names)))
})

test_that("tableNCAConcentration rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test different rounding methods
	table_monolix <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundDigits = 2,
		roundMethod = "monolix")

	table_signif <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundDigits = 2,
		roundMethod = "signif")

	table_none <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundMethod = "none")

	# Verify all tables are valid
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_none))

	# Verify they have the same structure
	expect_equal(nrow(table_monolix), nrow(table_signif))
	expect_equal(nrow(table_monolix), nrow(table_none))
})

test_that("tableNCAConcentration prettyLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with pretty labels enabled
	table_pretty <- tableNCAConcentration(
		projectFile = pkx_project_1,
		prettyLabels = TRUE
	)

	# Test with pretty labels disabled
	table_raw <- tableNCAConcentration(
		projectFile = pkx_project_1,
		prettyLabels = FALSE
	)

	# Verify both tables are valid
	expect_true(is.data.frame(table_pretty))
	expect_true(is.data.frame(table_raw))

	# Column names should differ (pretty labels should transform them)
	# But both should have same number of rows
	expect_equal(nrow(table_pretty), nrow(table_raw))
})

test_that("tableNCAConcentration errors", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with invalid metrics
	expect_error(
		tableNCAConcentration(
			projectFile = pkx_project_1,
			metrics = c("InvalidMetric1", "InvalidMetric2")
		),
		"metric"
	)

	# Test with invalid covariates (this should error or warn)
	expect_error(
		tableNCAConcentration(
			projectFile = pkx_project_1,
			covariates = "NONEXISTENT_COVARIATE"
		)
	)
})

# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("tests/testthat/test-PKanalixHelpers.R")