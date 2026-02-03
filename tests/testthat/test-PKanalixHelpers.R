pkx_project_1 = "test-projects/pkanalix/project_remifentanil_excl.pkx"
pkx_project_2 = "test-projects/pkanalix/project_occasions2.pkx"
pkx_project_3 = "test-projects/pkanalix/project_aPCSK9_SAD_route.pkx"
pkx_project_4 = "test-projects/pkanalix/project_steadystate_infusion.pkx"
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

test_that("tableNCAParameters vertical layout (horizontal = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test vertical layout without stratification
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		NCAparams   = c("AUCINF_obs", "AUClast", "Cmax"),
		horizontal  = FALSE
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
		horizontal  = FALSE,
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
		horizontal       = FALSE,
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
								info = paste("Failed with roundMethod:", method))
		expect_gt(nrow(table), 0,
							info = paste("No rows with roundMethod:", method))
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
		horizontal       = FALSE,
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
		horizontal       = FALSE,
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
		horizontal       = TRUE,
		stratify_columns = FALSE,  # Can't have both horizontal=TRUE and stratify_columns=TRUE
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
	# - horizontal = TRUE
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
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-PKanalixHelpers.R")

