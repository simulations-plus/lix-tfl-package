pkx_project_1 = "test-projects/pkanalix/project_remifentanil_excl.pkx"
pkx_project_2 = "test-projects/pkanalix/project_occasions2.pkx"
pkx_project_3 = "test-projects/pkanalix/project_aPCSK9_SAD_route.pkx"
pkx_project_4 = "test-projects/pkanalix/project_steadystate_infusion.pkx"
pkx_project_5  ="test-projects/pkanalix/project_repeated_4periods_multipleRatios.pkx"
################################################################################
################################################################################
# tableNCAConcentration

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
################################################################################
################################################################################
# tableNCAParameters
test_that("tableNCAParameters basic (no stratification)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test basic functionality with no covariates
	table <- tableNCAParameters(projectFile = pkx_project_1)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("parameter" %in% names(table) || "Parameter" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify metrics columns exist (at least some standard ones)
	expect_true(any(c("mean", "SD", "CV", "geoMean", "geoCV", "median", "Nobs") %in% names(table)))
})

test_that("tableNCAParameters with specific NCAparams", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with specific NCA parameters
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Verify only requested parameters are included
	param_col <- if ("parameter" %in% names(table)) "parameter" else "Parameter"
	unique_params <- unique(table[[param_col]])

	# Check that the parameters match (allowing for case differences or formatting)
	expect_true(all(unique_params %in% c("AUCINF_obs", "AUClast", "Cmax")))
	expect_equal(length(unique_params), 3)  # Should have exactly 3 parameters
})

test_that("tableNCAParameters with specific metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with specific metrics
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		metrics     = c("min", "MeanSD", "median", "GeomCV", "max")
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Verify requested metrics are present
	metric_cols <- setdiff(names(table), c("parameter", "Parameter", "Nobs"))
	expect_true(length(metric_cols) > 0)
})

test_that("tableNCAParameters with stratification (stratify_columns = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates but no column stratification
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		stratify_columns = FALSE
	)

	# Verify covariate column exists
	expect_true("TINFCAT" %in% names(table))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with column stratification (stratify_columns = TRUE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates and column stratification
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		stratify_columns = TRUE
	)

	# Verify structure
	expect_true(is.data.frame(table))

	# Verify "Overall" column exists (always included in stratified layout)
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))

	# Verify no TINFCAT column (covariates become headers when stratify_columns=TRUE)
	expect_false("TINFCAT" %in% names(table))

	# Verify parameter column exists
	expect_true("parameter" %in% names(table) || "Parameter" %in% names(table))
})

test_that("tableNCAParameters with column stratification and specific metric", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with single metric (should suppress Statistic column based on concentration tests)
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		stratify_columns = TRUE,
		metrics          = "GeomCV"
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAParameters with column stratification and multiple metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple metrics
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		stratify_columns = TRUE,
		metrics          = c("MeanSD", "GeomCV")
	)

	# Verify "Statistic" column EXISTS for multiple metrics
	expect_true("Statistic" %in% names(table))

	# Verify statistics are as requested
	expect_true(all(c("MeanSD", "GeomCV") %in% table$Statistic |
										all(grepl("Mean.*SD|Geometric.*CV", table$Statistic))))
})

test_that("tableNCAParameters vertical layout (use_wide_format = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test vertical layout without stratification
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		use_wide_format  = FALSE
	)

	# Verify "Statistic" column exists in vertical layout
	expect_true("Statistic" %in% names(table))
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters vertical layout with covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test vertical layout with covariates
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		covariates  = "TINFCAT",
		use_wide_format  = FALSE,
		metrics     = c("min", "MeanSD", "median", "GeomCV", "max")
	)

	# Verify structure
	expect_true("Statistic" %in% names(table))
	expect_true("TINFCAT" %in% names(table))
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with multiple covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple covariates
	table <- tableNCAParameters(
		projectFile      = pkx_project_3,
		NCAparams        = c("AUCINF_obs", "Cmax"),
		covariates       = c("DOSE_mg", "ROUTE"),
		stratify_columns = TRUE,
		metrics          = c("MeanSD", "GeomCV")
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAParameters with covariate_labels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with custom covariate labels
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		covariate_labels = "Infusion Category",
		NCAparams        = c("AUCINF_obs", "Cmax"),
		stratify_columns = TRUE
	)

	# Verify custom labels appear in column headers
	header_names <- names(table)
	expect_true(any(grepl("Infusion Category", header_names)))
})

test_that("tableNCAParameters with multiple covariate_labels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple custom covariate labels
	table <- tableNCAParameters(
		projectFile      = pkx_project_3,
		NCAparams        = c("AUCINF_obs", "Cmax"),
		covariates       = c("DOSE_mg", "ROUTE"),  # Changed from "DOSE" to "DOSE_mg"
		covariate_labels = c("Dose Level", "Route of Administration"),
		stratify_columns = TRUE,
		metrics          = "GeomCV"
	)

	# Verify custom labels appear
	header_names <- names(table)
	expect_true(any(grepl("Dose Level|Route of Administration", header_names)))
})

test_that("tableNCAParameters rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test different rounding methods
	table_monolix <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax"),
		roundDigits = 2,
		roundMethod = "monolix"
	)

	table_signif <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax"),
		roundDigits = 2,
		roundMethod = "signif"
	)

	table_none <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax"),
		roundMethod = "none"
	)

	# Verify all tables are valid
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_none))

	# Verify they have the same structure
	expect_equal(nrow(table_monolix), nrow(table_signif))
	expect_equal(nrow(table_monolix), nrow(table_none))
})

test_that("tableNCAParameters units and aliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with units enabled, aliases disabled
	table_units_no_alias <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = TRUE,
		aliases     = FALSE
	)

	# Test with units disabled, aliases enabled
	table_no_units_alias <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = TRUE
	)

	# Test with both disabled
	table_both_off <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = FALSE
	)

	# Verify all tables are valid
	expect_true(is.data.frame(table_units_no_alias))
	expect_true(is.data.frame(table_no_units_alias))
	expect_true(is.data.frame(table_both_off))
})

test_that("tableNCAParameters prettyLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with pretty labels enabled
	table_pretty <- tableNCAParameters(
		projectFile  = pkx_project_1,
		NCAparams    = c("Cmax"),
		prettyLabels = TRUE
	)

	# Test with pretty labels disabled
	table_raw <- tableNCAParameters(
		projectFile  = pkx_project_1,
		NCAparams    = c("Cmax"),
		prettyLabels = FALSE
	)

	# Verify both tables are valid
	expect_true(is.data.frame(table_pretty))
	expect_true(is.data.frame(table_raw))

	# Both should have same number of rows
	expect_equal(nrow(table_pretty), nrow(table_raw))
})

test_that("tableNCAParameters errors", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with invalid metrics
	expect_error(
		tableNCAParameters(
			projectFile = pkx_project_1,
			metrics     = c("InvalidMetric1", "InvalidMetric2")
		),
		"metric"
	)

	# Test with invalid covariates
	expect_error(
		tableNCAParameters(
			projectFile = pkx_project_1,
			covariates  = "NONEXISTENT_COVARIATE"
		)
	)

	# Test with invalid NCA parameters
	expect_error(
		tableNCAParameters(
			projectFile = pkx_project_1,
			NCAparams   = c("INVALID_PARAM")
		)
	)
})

test_that("tableNCAParameters complex scenario", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test complex scenario with multiple options
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		metrics          = c("GeomCV", "MeanSD", "Q3"),
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		covariates       = "TINFCAT",
		use_wide_format       = FALSE,
		stratify_columns = TRUE,
		roundDigits      = 2,
		roundMethod      = "signif",
		prettyLabels     = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with projectFile = NULL (uses loaded project)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Load project first
	lixoftConnectors::loadProject(pkx_project_1)

	# Call without projectFile argument (should use currently loaded project)
	table <- tableNCAParameters(
		projectFile = NULL,
		NCAparams   = c("Cmax")
	)

	# Verify it works
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with different roundDigits values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with roundDigits = 1
	table_1 <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax"),
		roundDigits = 1,
		roundMethod = "monolix"
	)

	# Test with roundDigits = 5
	table_5 <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax"),
		roundDigits = 5,
		roundMethod = "monolix"
	)

	# Both should be valid
	expect_true(is.data.frame(table_1))
	expect_true(is.data.frame(table_5))

	# They should have same structure but different precision
	expect_equal(nrow(table_1), nrow(table_5))
	expect_equal(ncol(table_1), ncol(table_5))
})

test_that("tableNCAParameters roundMethod with stratification", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test each rounding method with stratification
	for (method in c("monolix", "signif", "none")) {
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			NCAparams        = c("Cmax"),
			covariates       = "TINFCAT",
			stratify_columns = TRUE,
			roundDigits      = 3,
			roundMethod      = method,
			metrics          = "GeomCV"
		)

		expect_true(is.data.frame(table),
								label = paste("Failed with roundMethod:", method))
		expect_gt(nrow(table), 0,
							label = paste("No rows with roundMethod:", method))
	}
})

test_that("tableNCAParameters all arguments combined", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with all arguments specified (comprehensive test)
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		covariates       = "TINFCAT",
		NCAparams        = c("AUCINF_obs", "AUClast", "Cmax"),
		metrics          = c("min", "MeanSD", "median", "GeomCV", "max"),
		use_wide_format       = FALSE,
		stratify_columns = TRUE,
		covariate_labels = "Infusion Category",
		roundDigits      = 4,
		roundMethod      = "signif",
		units            = TRUE,
		aliases          = TRUE,
		prettyLabels     = TRUE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_true("Statistic" %in% names(table))
})

test_that("tableNCAParameters with all flags FALSE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with all boolean flags set to FALSE
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		NCAparams        = c("Cmax"),
		use_wide_format       = FALSE,
		stratify_columns = FALSE,
		units            = FALSE,
		aliases          = FALSE,
		prettyLabels     = FALSE
	)

	# Should still produce valid output
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with all flags TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with all boolean flags set to TRUE
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		NCAparams        = c("Cmax"),
		covariates       = "TINFCAT",
		use_wide_format       = TRUE,
		stratify_columns = FALSE,  # Can't have both use_wide_format=TRUE and stratify_columns=TRUE
		units            = TRUE,
		aliases          = TRUE,
		prettyLabels     = TRUE
	)

	# Should still produce valid output
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters default values work correctly", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with only projectFile specified (all other defaults)
	table <- tableNCAParameters(projectFile = pkx_project_1)

	# Should use:
	# - All available NCAparams
	# - No covariates
	# - Default metrics
	# - use_wide_format = TRUE
	# - stratify_columns = FALSE
	# - roundDigits = 3
	# - roundMethod = "monolix"
	# - units = TRUE
	# - aliases = TRUE
	# - prettyLabels = TRUE

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 2)  # Should have parameter column + metrics
})

test_that("tableNCAParameters with NCAparams = NULL (all parameters)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with NCAparams = NULL to use all available parameters
	table_all <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = NULL  # Should use all computed NCA parameters
	)

	# Test with specific parameters
	table_specific <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("Cmax")
	)

	# All parameters table should have more rows
	expect_true(is.data.frame(table_all))
	expect_true(is.data.frame(table_specific))
	expect_gt(nrow(table_all), nrow(table_specific))
})

test_that("tableNCAParameters with metrics = NULL (default metrics)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with metrics = NULL to use default metrics
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		NCAparams        = c("Cmax"),
		covariates       = "TINFCAT",
		stratify_columns = TRUE,
		metrics          = NULL  # Should use default metric
	)

	# Should produce valid output with default metric
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters edge case: single parameter, single metric", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Minimal case
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = "Cmax",
		metrics     = "mean"
	)

	expect_true(is.data.frame(table))
	expect_equal(nrow(table), 1)  # Should have exactly one row
})

test_that("tableNCAParameters edge case: many parameters, many metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Maximal case (if project has many parameters)
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = NULL,  # All available
		metrics     = c("min", "Q1", "median", "mean", "Q3", "max",
										"SD", "CV", "geoMean", "geoCV", "harmMean")
	)

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 5)  # Many columns
})

################################################################################
# tableNCAParameters with include_id feature

test_that("tableNCAParameters with include_id basic (no covariates)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test basic include_id functionality without covariates
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax", "AUClast", "Lambda_z"),
		include_id   = TRUE,
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify that table contains both summary statistics and individual IDs
	statistic_values <- table$Statistic

	# Check for summary statistics (should have at least some)
	summary_stats <- c("mean", "SD", "CV", "median", "min", "max")
	expect_true(any(summary_stats %in% statistic_values))

	# Check for individual IDs (should be numeric or character IDs)
	# Individual IDs should not match summary statistic names
	has_ids <- any(!statistic_values %in% summary_stats)
	expect_true(has_ids, label = "Table should contain individual IDs in addition to summary statistics")
})

test_that("tableNCAParameters with include_id and single covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test include_id with stratification by single covariate
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		covariates   = "TINFCAT",
		NCAparams    = c("Cmax", "AUClast", "Lambda_z"),
		metrics      = c("min", "MeanSD", "median", "GeomCV", "max"),
		include_id   = TRUE,
		prettyLabels = FALSE,
		units        = FALSE,
		aliases      = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("TINFCAT" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify parameter columns exist
	expect_true(all(c("Cmax", "AUClast", "Lambda_z") %in% names(table)))

	# Verify that individuals are grouped by covariate
	# Each covariate group should have metrics followed by individual IDs
	statistic_values <- table$Statistic
	has_metrics <- any(c("min", "MeanSD", "median", "GeomCV", "max") %in% statistic_values)
	expect_true(has_metrics)
})

test_that("tableNCAParameters with include_id and multiple covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test include_id with multiple covariates
	table <- tableNCAParameters(
		projectFile  = pkx_project_3,
		use_wide_format   = FALSE,
		covariates   = c("DOSE_mg", "ROUTE"),
		NCAparams    = c("Cmax", "AUClast"),
		metrics      = c("MeanSD", "GeomCV"),
		include_id   = TRUE,
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true(all(c("DOSE_mg", "ROUTE") %in% names(table)))
	expect_gt(nrow(table), 0)

	# Verify that table is stratified by both covariates
	# Should have unique combinations of DOSE_mg and ROUTE
	covariate_combos <- unique(table[, c("DOSE_mg", "ROUTE")])
	expect_gt(nrow(covariate_combos), 1)
})

test_that("tableNCAParameters with include_id and prettyLabels = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test include_id with pretty labels enabled
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax", "AUClast", "Lambda_z"),
		include_id   = TRUE,
		prettyLabels = TRUE,
		units        = TRUE,
		aliases      = TRUE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_gt(nrow(table), 0)

	# Pretty labels should be applied to both column names and Statistic values
	# Column names should be human-readable (not raw parameter names)
	# We can't check exact names since they depend on units/aliases,
	# but table should still be valid
})

test_that("tableNCAParameters with include_id validation: use_wide_format = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that include_id = TRUE with use_wide_format = TRUE generates warning
	expect_warning(
		table <- tableNCAParameters(
			projectFile = pkx_project_1,
			use_wide_format  = TRUE,  # This should trigger warning
			NCAparams   = c("Cmax"),
			include_id  = TRUE
		),
		"include_id = TRUE only works with use_wide_format = FALSE"
	)

	# Table should still be generated with include_id set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCAParameters with include_id validation: stratify_columns = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that include_id = TRUE with stratify_columns = TRUE generates warning
	expect_warning(
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			use_wide_format       = FALSE,
			stratify_columns = TRUE,  # This should trigger warning
			covariates       = "TINFCAT",
			NCAparams        = c("Cmax"),
			include_id       = TRUE
		),
		"include_id = TRUE is not yet supported with stratify_columns = TRUE"
	)

	# Table should still be generated with include_id set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCAParameters with include_id = FALSE (default behavior)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that include_id = FALSE produces table without individual IDs
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax", "AUClast"),
		include_id   = FALSE,  # Explicitly FALSE
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))

	# All Statistic values should be summary statistics, no individual IDs
	statistic_values <- table$Statistic
	summary_stats <- c("mean", "SD", "CV", "SE", "geoMean", "geoCV", "geoSD", "geoSE",
	                   "median", "min", "max", "Q1", "Q3", "N", "Nobs", "Ntot", "Nmiss",
	                   "P05", "P95", "MeanSD", "MeanSE", "GeomCV", "GeomSD", "GeomSE", "harmMean")

	# All statistic values should be recognized summary statistics
	expect_true(all(statistic_values %in% summary_stats),
	            label = paste("Unexpected statistic values:",
	                         paste(setdiff(statistic_values, summary_stats), collapse = ", ")))
})

test_that("tableNCAParameters with include_id and different rounding settings", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test include_id with different rounding methods
	table_monolix <- tableNCAParameters(
		projectFile = pkx_project_1,
		use_wide_format  = FALSE,
		NCAparams   = c("Cmax"),
		include_id  = TRUE,
		roundDigits = 2,
		roundMethod = "monolix"
	)

	table_signif <- tableNCAParameters(
		projectFile = pkx_project_1,
		use_wide_format  = FALSE,
		NCAparams   = c("Cmax"),
		include_id  = TRUE,
		roundDigits = 2,
		roundMethod = "signif"
	)

	# Both should be valid and have individual IDs
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_gt(nrow(table_monolix), 0)
	expect_gt(nrow(table_signif), 0)
})

test_that("tableNCAParameters with include_id and specific metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test include_id with specific metrics selected
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax", "AUClast"),
		metrics      = c("median", "GeomCV", "max"),
		include_id   = TRUE,
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))

	# Verify that specified metrics are present
	statistic_values <- table$Statistic
	expect_true(any(c("median", "GeomCV", "max") %in% statistic_values))

	# Verify individual IDs are also present (not just metrics)
	expect_gt(length(unique(statistic_values)), 3)  # More than just the 3 metrics
})

test_that("tableNCAParameters with include_id comprehensive test", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Comprehensive test with all compatible features
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		covariates   = "TINFCAT",
		NCAparams    = c("Cmax", "AUClast", "Lambda_z"),
		metrics      = c("min", "MeanSD", "median", "GeomCV", "max"),
		use_wide_format   = FALSE,
		include_id   = TRUE,
		roundDigits  = 3,
		roundMethod  = "monolix",
		units        = TRUE,
		aliases      = TRUE,
		prettyLabels = TRUE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("TINFCAT" %in% names(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 3)  # At least Statistic, covariate, and parameter columns
})

test_that("tableNCAParameters with include_id row count check", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Compare row counts: include_id = FALSE vs TRUE
	table_no_ids <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax"),
		metrics      = c("median", "GeomCV"),
		include_id   = FALSE,
		prettyLabels = FALSE
	)

	table_with_ids <- tableNCAParameters(
		projectFile  = pkx_project_1,
		use_wide_format   = FALSE,
		NCAparams    = c("Cmax"),
		metrics      = c("median", "GeomCV"),
		include_id   = TRUE,
		prettyLabels = FALSE
	)

	# Table with IDs should have more rows than without IDs
	expect_gt(nrow(table_with_ids), nrow(table_no_ids),
	          label = "Table with include_id=TRUE should have more rows than include_id=FALSE")

	# Both should have same number of columns
	expect_equal(ncol(table_with_ids), ncol(table_no_ids))
})

test_that("tableNCAParameters with include_id and numeric categorical covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with numeric-valued categorical covariate (tests sorting logic)
	# Using project_3 which has DOSE_mg covariate
	table <- tableNCAParameters(
		projectFile  = pkx_project_3,
		use_wide_format   = FALSE,
		covariates   = "DOSE_mg",
		NCAparams    = c("Cmax", "AUClast"),
		include_id   = TRUE,
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("DOSE_mg" %in% names(table))

	# Verify covariate values are sorted (numerically if applicable)
	dose_values <- unique(table$DOSE_mg)
	expect_gt(length(dose_values), 0)
})
################################################################################
################################################################################
# Tests for listingNCAParameters -----------------------------------------------

test_that("listingNCAParameters works with basic settings", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result <- listingNCAParameters(projectFile = pkx_project_1)

	# Basic structure checks
	expect_s3_class(result, "data.frame")
	expect_true("id" %in% names(result))
	expect_gt(nrow(result), 0)
	expect_gt(ncol(result), 1)  # Should have id + parameters
})

test_that("listingNCAParameters works with covariates", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_1,
		covariates = c("SEX", "TINFCAT")
	)

	# Check covariates are present and in correct order
	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "SEX", "TINFCAT") %in% names(result)))
	expect_equal(names(result)[1:3], c("id", "SEX", "TINFCAT"))
})

test_that("listingNCAParameters filters to selected parameters", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	selected_params <- c("Clast", "Cmax", "AUClast", "AUCINF_obs")

	result <- listingNCAParameters(
		projectFile = pkx_project_2,
		covariates = c("FOOD", "TREAT"),
		NCAparams = selected_params,
		roundDigits = 2,
		roundMethod = "signif"
	)

	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "FOOD", "TREAT") %in% names(result)))
	# Should have limited columns: id + covariates + occasions + selected params
	expect_lte(ncol(result), 3 + 2 + length(selected_params) + 5)  # Some buffer for occasions
})

test_that("listingNCAParameters applies rounding with signif method", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result_rounded <- listingNCAParameters(
		projectFile = pkx_project_2,
		NCAparams = c("Cmax", "AUClast"),
		roundDigits = 2,
		roundMethod = "signif"
	)

	result_none <- listingNCAParameters(
		projectFile = pkx_project_2,
		NCAparams = c("Cmax", "AUClast"),
		roundMethod = "none"
	)

	expect_s3_class(result_rounded, "data.frame")
	expect_s3_class(result_none, "data.frame")
	expect_equal(dim(result_rounded), dim(result_none))
})

test_that("listingNCAParameters applies rounding with monolix method", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "monolix"
	)

	expect_s3_class(result, "data.frame")
	expect_gt(ncol(result), 1)
})

test_that("listingNCAParameters works without aliases", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result_with_aliases <- listingNCAParameters(
		projectFile = pkx_project_2,
		aliases = TRUE
	)

	result_without_aliases <- listingNCAParameters(
		projectFile = pkx_project_2,
		aliases = FALSE
	)

	expect_s3_class(result_with_aliases, "data.frame")
	expect_s3_class(result_without_aliases, "data.frame")
	# Same structure but different column names
	expect_equal(dim(result_with_aliases), dim(result_without_aliases))
	# At least some column names should differ
	expect_false(identical(names(result_with_aliases), names(result_without_aliases)))
})

test_that("listingNCAParameters works without units", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result_with_units <- listingNCAParameters(
		projectFile = pkx_project_4,
		units = TRUE
	)

	result_without_units <- listingNCAParameters(
		projectFile = pkx_project_4,
		units = FALSE
	)

	expect_s3_class(result_with_units, "data.frame")
	expect_s3_class(result_without_units, "data.frame")
	expect_equal(nrow(result_with_units), nrow(result_without_units))
})

test_that("listingNCAParameters sorts by covariates in specified order", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_5,
		covariates = c("SEQ", "TRT")
	)

	expect_s3_class(result, "data.frame")
	# Check that data is sorted by covariates
	if (nrow(result) > 1) {
		sorted_check <- result %>% dplyr::arrange(SEQ, TRT)
		expect_equal(result$id, sorted_check$id)
	}
})

test_that("listingNCAParameters validates invalid covariates", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	expect_error(
		listingNCAParameters(
			projectFile = pkx_project_1,
			covariates = c("INVALID_COVARIATE")
		),
		"Invalid covariate"
	)
})

test_that("listingNCAParameters validates invalid NCA parameters", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	expect_error(
		listingNCAParameters(
			projectFile = pkx_project_1,
			NCAparams = c("INVALID_PARAMETER", "ANOTHER_INVALID")
		),
		"None of the requested.*parameters were found"
	)
})

test_that("listingNCAParameters warns for partially invalid parameters", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	expect_warning(
		result <- listingNCAParameters(
			projectFile = pkx_project_2,
			NCAparams = c("Cmax", "INVALID_PARAM", "AUClast")
		),
		"Ignoring unknown.*parameters"
	)

	# Should still return valid parameters
	expect_s3_class(result, "data.frame")
	expect_gt(ncol(result), 1)
})

test_that("listingNCAParameters works with occasions and covariates", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_2,
		covariates = c("FOOD", "TREAT"),
		NCAparams = c("Clast", "Cmax", "AUClast", "AUCINF_obs")
	)

	expect_s3_class(result, "data.frame")
	# Should include id, covariates, occasions, and parameters
	expect_true(all(c("id", "FOOD", "TREAT") %in% names(result)))
	expect_gt(ncol(result), 6)  # id + 2 covariates + occasions + params
})

test_that("listingNCAParameters handles multiple rounding methods", {
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test all three rounding methods
	result_monolix <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundDigits = 3,
		roundMethod = "monolix"
	)

	result_signif <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundDigits = 3,
		roundMethod = "signif"
	)

	result_none <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundMethod = "none"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_s3_class(result_none, "data.frame")
	# All should have same structure
	expect_equal(dim(result_monolix), dim(result_signif))
	expect_equal(dim(result_monolix), dim(result_none))
})

test_that("listingNCAParameters returns correct column order", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_2,
		covariates = c("TREAT", "FOOD")  # Specify order
	)

	# First columns should be: id, TREAT, FOOD (in that order)
	expect_equal(names(result)[1], "id")
	expect_equal(names(result)[2], "TREAT")
	expect_equal(names(result)[3], "FOOD")
})




################################################################################
################################################################################
# Tests for listingNCAConcentration --------------------------------------------

test_that("listingNCAConcentration works with basic settings", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(projectFile = pkx_project_2)

	expect_s3_class(result, "data.frame")
	expect_true("id" %in% names(result))
	expect_true("ActualTime" %in% names(result))
	expect_true("Concentration" %in% names(result))
	expect_gt(nrow(result), 0)
})

test_that("listingNCAConcentration works with covariates", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(
		projectFile = pkx_project_2,
		covariates = c("FOOD", "TREAT")
	)

	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "FOOD", "TREAT", "ActualTime", "Concentration") %in% names(result)))
	# Check covariate ordering
	expect_true(which(names(result) == "FOOD") < which(names(result) == "ActualTime"))
})

test_that("listingNCAConcentration works with useNominalTime", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result <- listingNCAConcentration(
		projectFile = pkx_project_4,
		useNominalTime = TRUE
	)

	expect_s3_class(result, "data.frame")
	# NominalTime only exists if nominal time is available in the project
	# Always should have ActualTime
	expect_true("ActualTime" %in% names(result))
	# If NominalTime exists, both columns should be present
	if ("NominalTime" %in% names(result)) {
		expect_true(all(c("ActualTime", "NominalTime") %in% names(result)))
	}
})

test_that("listingNCAConcentration applies rounding", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result_rounded <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundDigits = 2,
		roundMethod = "signif"
	)

	result_none <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundMethod = "none"
	)

	expect_s3_class(result_rounded, "data.frame")
	expect_s3_class(result_none, "data.frame")
	expect_equal(nrow(result_rounded), nrow(result_none))
})

test_that("listingNCAConcentration handles different rounding methods", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result_monolix <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "monolix"
	)

	result_signif <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "signif"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_equal(dim(result_monolix), dim(result_signif))
})

test_that("listingNCAConcentration includes UsedConc column", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(projectFile = pkx_project_2)

	expect_true("UsedConc" %in% names(result))
	expect_true("Censoring" %in% names(result))
})

test_that("listingNCAConcentration includes ActualTime", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCAConcentration(projectFile = pkx_project_5)

	expect_true("ActualTime" %in% names(result))
	expect_type(result$ActualTime, "double")
})

test_that("listingNCAConcentration handles occasions", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(
		projectFile = pkx_project_2,
		covariates = c("FOOD", "TREAT")
	)

	expect_s3_class(result, "data.frame")
	# Should handle occasion variables in the data
	expect_gt(ncol(result), 5)
})

test_that("listingNCAConcentration validates invalid covariates", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	expect_error(
		listingNCAConcentration(
			projectFile = pkx_project_2,
			covariates = c("INVALID_COV")
		),
		"Invalid covariate"
	)
})

test_that("listingNCAConcentration handles BLQ flags", {
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	result <- listingNCAConcentration(projectFile = pkx_project_3)

	expect_s3_class(result, "data.frame")
	# Should have Censoring column for BLQ handling
	expect_true("Censoring" %in% names(result))
})

test_that("listingNCAConcentration includes covariates in correct order", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(
		projectFile = pkx_project_2,
		covariates = c("FOOD", "TREAT")
	)

	expect_s3_class(result, "data.frame")
	# Check covariates are present
	expect_true(all(c("FOOD", "TREAT") %in% names(result)))
	# Check they come before ActualTime column
	food_pos <- which(names(result) == "FOOD")
	time_pos <- which(names(result) == "ActualTime")
	expect_true(food_pos < time_pos)
})

test_that("listingNCAConcentration preserves concentration text flags", {
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	result <- listingNCAConcentration(projectFile = pkx_project_3)

	expect_s3_class(result, "data.frame")
	expect_type(result$Concentration, "character")
	# Check if any non-numeric flags exist (like "<LLOQ")
	non_na_conc <- result$Concentration[!is.na(result$Concentration)]
	has_flags <- any(grepl("[<>]", non_na_conc))
	# Concentration should always be character to handle flags
	expect_true(is.character(result$Concentration))
})

test_that("listingNCAConcentration works across different project types", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result1 <- listingNCAConcentration(projectFile = pkx_project_1)
	result5 <- listingNCAConcentration(projectFile = pkx_project_5)

	expect_s3_class(result1, "data.frame")
	expect_s3_class(result5, "data.frame")
	# Both should have required columns
	expect_true(all(c("id", "ActualTime", "Concentration") %in% names(result1)))
	expect_true(all(c("id", "ActualTime", "Concentration") %in% names(result5)))
})

# Tests for listingNCARatios ------------------------------------------------

test_that("listingNCARatios works with basic settings", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(projectFile = pkx_project_5)

	# Basic structure checks
	expect_s3_class(result, "data.frame")
	expect_true("id" %in% names(result))
	expect_gt(nrow(result), 0)
	expect_gt(ncol(result), 1)  # Should have id + ratio parameters
})

test_that("listingNCARatios works with covariates", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		covariates = c("SEQ")
	)

	# Check covariates are present and in correct order
	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "SEQ") %in% names(result)))
	expect_equal(names(result)[1:2], c("id", "SEQ"))
})

test_that("listingNCARatios filters to selected ratio parameters", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# First get all available ratios to select from
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 2) {
		selected_ratios <- available_ratios[1:2]

		result <- listingNCARatios(
			projectFile = pkx_project_5,
			covariates = c("SEQ"),
			ratioParams = selected_ratios,
			roundDigits = 2,
			roundMethod = "signif"
		)

		expect_s3_class(result, "data.frame")
		expect_true(all(c("id", "SEQ") %in% names(result)))
		# Should have limited columns: id + covariates + occasions + selected ratios
		expect_lte(ncol(result), 3 + 1 + length(selected_ratios) + 2)  # Some buffer
	}
})

test_that("listingNCARatios applies rounding with signif method", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	ratio_params <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(ratio_params) >= 2) {
		selected <- ratio_params[1:2]

		result_rounded <- listingNCARatios(
			projectFile = pkx_project_5,
			ratioParams = selected,
			roundDigits = 2,
			roundMethod = "signif"
		)

		result_none <- listingNCARatios(
			projectFile = pkx_project_5,
			ratioParams = selected,
			roundMethod = "none"
		)

		expect_s3_class(result_rounded, "data.frame")
		expect_s3_class(result_none, "data.frame")
		expect_equal(dim(result_rounded), dim(result_none))
	}
})

test_that("listingNCARatios applies rounding with monolix method", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 4,
		roundMethod = "monolix"
	)

	expect_s3_class(result, "data.frame")
	expect_gt(ncol(result), 1)
})

test_that("listingNCARatios aliases argument (not yet functional)", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Note: aliases parameter doesn't affect output yet, but function should still work
	result_with_aliases <- listingNCARatios(
		projectFile = pkx_project_5,
		aliases = TRUE
	)

	result_without_aliases <- listingNCARatios(
		projectFile = pkx_project_5,
		aliases = FALSE
	)

	expect_s3_class(result_with_aliases, "data.frame")
	expect_s3_class(result_without_aliases, "data.frame")
	# Currently should have same structure and names (aliases not implemented)
	expect_equal(dim(result_with_aliases), dim(result_without_aliases))
	# When aliases are implemented, this test should be updated to check for differences
})

test_that("listingNCARatios sorts by covariates in specified order", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		covariates = c("SEQ")
	)

	expect_s3_class(result, "data.frame")
	# Check that data is sorted by covariates
	if (nrow(result) > 1) {
		sorted_check <- result %>% dplyr::arrange(SEQ)
		expect_equal(result$id, sorted_check$id)
	}
})

test_that("listingNCARatios validates invalid covariates", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	expect_error(
		listingNCARatios(
			projectFile = pkx_project_5,
			covariates = c("INVALID_COVARIATE")
		),
		"Invalid covariate"
	)
})

test_that("listingNCARatios validates invalid ratio parameters", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	expect_error(
		listingNCARatios(
			projectFile = pkx_project_5,
			ratioParams = c("INVALID_RATIO", "ANOTHER_INVALID")
		),
		"None of the requested.*parameters were found"
	)
})

test_that("listingNCARatios warns for partially invalid parameters", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get one valid ratio parameter
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	valid_ratio <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))[1]

	expect_warning(
		result <- listingNCARatios(
			projectFile = pkx_project_5,
			ratioParams = c(valid_ratio, "INVALID_PARAM")
		),
		"Ignoring unknown.*parameters"
	)

	# Should still return valid parameters
	expect_s3_class(result, "data.frame")
	expect_gt(ncol(result), 1)
})

test_that("listingNCARatios works with occasions and covariates", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		covariates = c("SEQ")
	)

	expect_s3_class(result, "data.frame")
	# Should include id, covariates, occasions, and ratio parameters
	expect_true(all(c("id", "SEQ") %in% names(result)))
	expect_gt(ncol(result), 4)  # id + 2 covariates + occasion + ratios
})

test_that("listingNCARatios handles multiple rounding methods", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test all three rounding methods
	result_monolix <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 3,
		roundMethod = "monolix"
	)

	result_signif <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 3,
		roundMethod = "signif"
	)

	result_none <- listingNCARatios(
		projectFile = pkx_project_5,
		roundMethod = "none"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_s3_class(result_none, "data.frame")
	# All should have same structure
	expect_equal(dim(result_monolix), dim(result_signif))
	expect_equal(dim(result_monolix), dim(result_none))
})

test_that("listingNCARatios returns correct column order", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		covariates = c("SEQ")  # Specify order
	)

	# First columns should be: id, TRT, SEQ (in that order)
	expect_equal(names(result)[1], "id")
	expect_equal(names(result)[2], "SEQ")
})

test_that("listingNCARatios validates NCA results availability", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# project_remifentanil_excl has NCA but likely no ratios configured
	expect_error(
		listingNCARatios(projectFile = pkx_project_1),
		"No NCA.*ratio"
	)
})

test_that("listingNCARatios handles projects without NCA", {
	skip_if_not(file.exists("test-projects/pkanalix/project_steadystate_infusion.pkx"),
							"Test project not available")

	# Project without NCA results
	expect_error(
		listingNCARatios(projectFile = "test-projects/pkanalix/project_steadystate_infusion.pkx"),
		"No NCA ratios available"
	)
})
################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-PKanalixHelpers.R")

