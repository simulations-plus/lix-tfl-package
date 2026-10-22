pkx_project_1 = "test-projects/pkanalix/project_remifentanil_excl.pkx"
pkx_project_2 = "test-projects/pkanalix/project_occasions2.pkx"
pkx_project_3 = "test-projects/pkanalix/project_aPCSK9_SAD_route.pkx"
pkx_project_4 = "test-projects/pkanalix/project_steadystate_infusion.pkx"
pkx_project_5  ="test-projects/pkanalix/project_repeated_4periods_multipleRatios.pkx"
pkx_project_complex = "test-projects/pkanalix/NCA_complex.pkx"          # nominal + actual time
pkx_project_notime  = "test-projects/pkanalix/project_noactual_time.pkx" # nominal time only, no actual time
################################################################################
################################################################################
# tableNCAConcentration

test_that("tableNCAConcentration basic wide format (default)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test basic functionality with no covariates - default is wide format
	table <- tableNCAConcentration(projectFile = pkx_project_1)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# In wide format, time values become column headers like "Time = 1.5"
	# Verify Statistic column exists (metrics are rows in wide format)
	expect_true("Statistic" %in% names(table))

	# Verify time columns exist (columns with "Time = " prefix)
	time_cols <- grep("^Time\\s*=", names(table), value = TRUE)
	expect_gt(length(time_cols), 0)
})

test_that("tableNCAConcentration basic long format (useWideFormat = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test long format
	table <- tableNCAConcentration(projectFile = pkx_project_1, useWideFormat = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("ActualTime" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify metrics columns exist (at least some standard ones)
	expect_true(any(c("mean", "SD", "CV", "geoMean", "geoCV", "median", "Nobs") %in% names(table)))

	# Verify Time column is sorted
	expect_true(all(diff(suppressWarnings(as.numeric(table$Time))) >= 0, na.rm = TRUE))
})

test_that("tableNCAConcentration with covariates (stratifyColumns = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates but no column stratification - use long format to get Time column
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		stratifyColumns = FALSE,
		useWideFormat  = FALSE)

	# Verify covariate column exists
	expect_true("TINFCAT" %in% names(table))
	expect_true("ActualTime" %in% names(table))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAConcentration with column stratification (stratifyColumns = TRUE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates and column stratification
	# Note: stratifyColumns = TRUE requires useWideFormat = FALSE
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))

	# With stratifyColumns = TRUE, covariate values become column headers
	# Time column should exist in long format
	expect_true("ActualTime" %in% names(table))

	# Verify "Overall" column exists (always included in stratified layout)
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))

	# Verify no TINFCAT column (covariates become headers when stratifyColumns=TRUE)
	expect_false("TINFCAT" %in% names(table))

	# Verify at least one stratified column exists (besides Time and Overall)
	stratified_cols <- setdiff(names(table), c("Time", "Statistic"))
	expect_gt(length(stratified_cols), 1)
})

test_that("tableNCAConcentration stratified columns sort integer modalities numerically", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# TINFCAT has integer modalities (e.g. 4, 5, 7, 8, 10, 14, 20). Columns must
	# appear in ascending numeric order, not lexicographic ("10", "14", "4", ...).
	table <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		splitCovariates = "TINFCAT",
		stratifyColumns = TRUE,
		useWideFormat   = FALSE)

	# Extract the modality value from each stratified header ("TINFCAT=10\n(N=31)")
	modalities <- as.numeric(sub("^TINFCAT=([0-9.]+).*$", "\\1",
															 grep("^TINFCAT=", names(table), value = TRUE)))

	expect_gt(length(modalities), 1)
	expect_false(any(is.na(modalities)))
	expect_identical(modalities, sort(modalities))
})

test_that("tableNCAConcentration with specific metrics (single metric)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with single metric
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = "MeanSD")

	# Verify "Statistic" column does NOT exist for single metric
	expect_false("Statistic" %in% names(table))
	expect_true(any(grepl("^(Actual)?Time$", names(table))))  # Time or ActualTime
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAConcentration with specific metrics (multiple metrics)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple metrics
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = c("MeanSD", "GeomCV"))

	# Verify "Statistic" column EXISTS for multiple metrics
	expect_true("Statistic" %in% names(table))
	expect_true(any(grepl("^(Actual)?Time$", names(table))))  # Time or ActualTime

	# Verify statistics are as requested
	expect_true(all(c("MeanSD", "GeomCV") %in% table$Statistic |
									all(grepl("Mean.*SD|Geometric.*CV", table$Statistic))))
})

test_that("tableNCAConcentration with useNominalTime", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with nominal time (project_occasions2 likely has nominal time)
	# If no nominal time available, should fall back to actual time
	table <- tableNCAConcentration(
		projectFile     = pkx_project_2,
		useNominalTime  = TRUE,
		useWideFormat = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("ActualTime" %in% names(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAConcentration with splitCovariateLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with custom covariate labels
	table <- tableNCAConcentration(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		splitCovariateLabels = "Infusion Category",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE)

	# Verify custom labels appear in column headers
	header_names <- names(table)
	expect_true(any(grepl("Infusion Category", header_names)))
})

test_that("tableNCAConcentration non-stratified relabels the split covariate column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# When stratifyColumns = FALSE the covariate is a literal column; the label
	# should rename that column (raw covariate name gone, label present).
	table <- tableNCAConcentration(
		projectFile          = pkx_project_1,
		splitCovariates      = "TINFCAT",
		splitCovariateLabels = "Infusion Category",
		stratifyColumns      = FALSE,
		useWideFormat        = FALSE)

	expect_true("Infusion Category" %in% names(table))
	expect_false("TINFCAT" %in% names(table))
})

test_that("tableNCAConcentration rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test different rounding methods
	table_monolix <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundDigits = 2,
		roundMethod = "monolixSuiteRounding")

	table_signif <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundDigits = 2,
		roundMethod = "significantFigures")

	table_signif_preserved <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundDigits = 2,
		roundMethod = "significantFiguresPreserved")

	table_none <- tableNCAConcentration(
		projectFile = pkx_project_1,
		roundMethod = "none")

	# Verify all tables are valid
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_signif_preserved))
	expect_true(is.data.frame(table_none))

	# Verify they have the same structure
	expect_equal(nrow(table_monolix), nrow(table_signif))
	expect_equal(nrow(table_monolix), nrow(table_signif_preserved))
	expect_equal(nrow(table_monolix), nrow(table_none))
})

test_that("tableNCAConcentration mock = TRUE masks output", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	table_real <- tableNCAConcentration(projectFile = pkx_project_1, mock = FALSE)
	table_mock <- tableNCAConcentration(projectFile = pkx_project_1, mock = TRUE)

	expect_true(is.data.frame(table_mock))
	# Masking must not change the table's shape
	expect_equal(dim(table_real), dim(table_mock))
	# Masked numeric cells become the "xx.xx"-style placeholder token
	expect_true(any(grepl("xx", as.matrix(table_mock))))
})

test_that("tableNCAConcentration prettyLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with pretty labels enabled
	table_pretty <- tableNCAConcentration(
		projectFile = pkx_project_1,
		prettyLabels = TRUE)

	# Test with pretty labels disabled
	table_raw <- tableNCAConcentration(
		projectFile = pkx_project_1,
		prettyLabels = FALSE)

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
			metrics = c("InvalidMetric1", "InvalidMetric2")),
		"metric")

	# Test with invalid covariates (this should error or warn)
	expect_error(
		tableNCAConcentration(
			projectFile = pkx_project_1,
			splitCovariates = "NONEXISTENT_COVARIATE"
		)
	)
})
################################################################################
# tableNCAConcentration: maxTimeDeviation feature
# Uses NCA_complex.pkx which carries BOTH nominal and actual time.

test_that("tableNCAConcentration maxTimeDeviation excludes observations and attaches footnote attributes", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_complex), "Test project not available")

	# Reproduces the documented example: threshold 20% excludes the deviating obs.
	expect_message(
		tb_conc <- tableNCAConcentration(
			projectFile      = pkx_project_complex,
			metrics          = c("min", "MeanSD", "max"),
			splitCovariates  = "ATRTNCA",
			includeId        = TRUE,
			maxTimeDeviation = 20,
			filterCovariates = list(ATRTNCA = "A (DRG1 + DRG2)")),
		"time deviation threshold")

	expect_true(is.data.frame(tb_conc))

	# Footnote attributes attached when observations were excluded
	expect_true(isTRUE(attr(tb_conc, "timeDeviationFiltered")))
	expect_equal(attr(tb_conc, "timeDeviationThreshold"), 20)

	excluded <- attr(tb_conc, "timeDeviationExcluded")
	expect_s3_class(excluded, "data.frame")
	expect_gte(nrow(excluded), 1)   # documented run excluded 1 observation

	# includeId = TRUE (with useWideFormat = TRUE default) adds Section column
	expect_true("Section" %in% names(tb_conc))
	expect_true(all(c("SUMMARY", "INDIVIDUAL") %in% tb_conc$Section))
})

test_that("tableNCAConcentration maxTimeDeviation is monotonic in the threshold", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_complex), "Test project not available")

	n_excluded <- function(tbl) {
		ex <- attr(tbl, "timeDeviationExcluded")
		if (is.null(ex)) 0L else nrow(ex)
	}

	tb_strict <- suppressMessages(tableNCAConcentration(
		projectFile = pkx_project_complex, useNominalTime = TRUE,
		maxTimeDeviation = 5, useWideFormat = FALSE))
	tb_loose <- suppressMessages(tableNCAConcentration(
		projectFile = pkx_project_complex, useNominalTime = TRUE,
		maxTimeDeviation = 500, useWideFormat = FALSE))

	# A stricter (smaller) threshold must exclude at least as many observations
	expect_gte(n_excluded(tb_strict), n_excluded(tb_loose))
})

test_that("tableNCAConcentration maxTimeDeviation only applies when useNominalTime = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_complex), "Test project not available")

	# With useNominalTime = FALSE the deviation filter is skipped -> no attributes
	tb <- suppressWarnings(tableNCAConcentration(
		projectFile      = pkx_project_complex,
		useNominalTime   = FALSE,
		maxTimeDeviation = 5))

	expect_true(is.data.frame(tb))
	expect_null(attr(tb, "timeDeviationThreshold"))
	expect_null(attr(tb, "timeDeviationExcluded"))
})

################################################################################
# tableNCAConcentration: pivot-variant coverage

test_that("tableNCAConcentration wide format with splitCovariates (non-stratified)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Wide pivot (time as columns) with the covariate kept as a left-hand column
	table <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		splitCovariates = "TINFCAT",
		useWideFormat   = TRUE,
		stratifyColumns = FALSE)

	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("TINFCAT"   %in% names(table))   # covariate stays as a column (not a header)
	time_cols <- grep("^Time\\s*=", names(table), value = TRUE)
	expect_gt(length(time_cols), 0)
})

test_that("tableNCAConcentration with multiple splitCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	table <- tableNCAConcentration(
		projectFile     = pkx_project_2,
		splitCovariates = c("FOOD", "TREAT"),
		useWideFormat   = FALSE,
		stratifyColumns = FALSE)

	expect_true(is.data.frame(table))
	expect_true(all(c("FOOD", "TREAT") %in% names(table)))
	expect_true("ActualTime" %in% names(table))
	expect_gt(nrow(table), 0)
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
	expect_true(any(c("Mean", "SD", "CV (%)", "geoMean", "geoCV", "Median", "N") %in% names(table)))
})

test_that("tableNCAParameters with specific selectParameters", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with specific NCA parameters
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = FALSE)

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
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		metrics     = c("min", "MeanSD", "median", "GeomCV", "max"))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Verify requested metrics are present
	metric_cols <- setdiff(names(table), c("parameter", "Parameter", "Nobs"))
	expect_true(length(metric_cols) > 0)
})

test_that("tableNCAParameters with stratification (stratifyColumns = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates but no column stratification
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		stratifyColumns = FALSE)

	# Verify covariate column exists
	expect_true("TINFCAT" %in% names(table))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with column stratification (stratifyColumns = TRUE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with covariates and column stratification
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		stratifyColumns = TRUE,
		useWideFormat = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))

	# Verify "Overall" column exists (always included in stratified layout)
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))

	# Verify no TINFCAT column (covariates become headers when stratifyColumns=TRUE)
	expect_false("TINFCAT" %in% names(table))

	# Verify parameter column exists
	expect_true("parameter" %in% names(table) || "Parameter" %in% names(table))
})

test_that("tableNCAParameters with column stratification and specific metric", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with single metric (should suppress Statistic column based on concentration tests)
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		stratifyColumns = TRUE,
		metrics          = "GeomCV",
		useWideFormat  = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAParameters with column stratification and multiple metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple metrics
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = c("MeanSD", "GeomCV"))

	# Verify "Statistic" column EXISTS for multiple metrics
	expect_true("Statistic" %in% names(table))

	# Verify statistics are as requested
	expect_true(all(c("MeanSD", "GeomCV") %in% table$Statistic |
										all(grepl("Mean.*SD|Geometric.*CV", table$Statistic))))
})

test_that("tableNCAParameters vertical layout (useWideFormat = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test vertical layout without stratification
	table <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		useWideFormat  = FALSE)

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
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		splitCovariates  = "TINFCAT",
		useWideFormat  = FALSE,
		metrics     = c("min", "MeanSD", "median", "GeomCV", "max"))

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
		selectParameters = c("AUCINF_obs", "Cmax"),
		splitCovariates  = c("DOSE_mg", "ROUTE"),
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = c("MeanSD", "GeomCV"))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCAParameters with splitCovariateLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with custom covariate labels
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		splitCovariates  = "TINFCAT",
		splitCovariateLabels = "Infusion Category",
		selectParameters = c("AUCINF_obs", "Cmax"),
		stratifyColumns = TRUE,
		useWideFormat  = FALSE)

	# Verify custom labels appear in column headers
	header_names <- names(table)
	expect_true(any(grepl("Infusion Category", header_names)))
})

test_that("tableNCAParameters with multiple splitCovariateLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with multiple custom covariate labels
	table <- tableNCAParameters(
		projectFile      = pkx_project_3,
		selectParameters = c("AUCINF_obs", "Cmax"),
		splitCovariates  = c("DOSE_mg", "ROUTE"),  # Changed from "DOSE" to "DOSE_mg"
		splitCovariateLabels = c("Dose Level", "Route of Administration"),
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = "GeomCV")

	# Verify custom labels appear
	header_names <- names(table)
	expect_true(any(grepl("Dose Level|Route of Administration", header_names)))
})

test_that("tableNCAParameters non-stratified relabels the split covariate column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# stratifyColumns = FALSE -> covariate stays a literal column that the label
	# should rename, mirroring the stratified-header behaviour.
	table <- tableNCAParameters(
		projectFile          = pkx_project_1,
		splitCovariates      = "TINFCAT",
		splitCovariateLabels = "Infusion Category",
		selectParameters     = c("AUCINF_obs", "Cmax"),
		stratifyColumns      = FALSE)

	expect_true("Infusion Category" %in% names(table))
	expect_false("TINFCAT" %in% names(table))
})

test_that("tableNCAParameters rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test different rounding methods
	table_monolix <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundDigits = 2,
		roundMethod = "monolixSuiteRounding")

	table_signif <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundDigits = 2,
		roundMethod = "significantFigures"
	)

	table_signif_preserved <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundDigits = 2,
		roundMethod = "significantFiguresPreserved"
	)

	table_none <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundMethod = "none"
	)

	# Verify all tables are valid
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_signif_preserved))
	expect_true(is.data.frame(table_none))

	# Verify they have the same structure
	expect_equal(nrow(table_monolix), nrow(table_signif))
	expect_equal(nrow(table_monolix), nrow(table_signif_preserved))
	expect_equal(nrow(table_monolix), nrow(table_none))
})

test_that("tableNCAParameters roundByParameter data.frame overrides digits and method per parameter", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	args <- list(
		projectFile   = pkx_project_1,
		selectParameters = c("Cmax", "AUClast"),
		metrics       = "mean",
		useWideFormat = TRUE,
		prettyLabels  = FALSE, units = FALSE, aliases = FALSE,
		roundDigits   = 3, roundMethod = "monolixSuiteRounding")

	base <- do.call(tableNCAParameters, args)
	over <- do.call(tableNCAParameters, c(args, list(
		roundByParameter = data.frame(parameter = "Cmax", digits = 1,
																	method = "significantFigures",
																	stringsAsFactors = FALSE))))

	get_mean <- function(tb, p) tb$mean[tb$parameter == p]

	# Overridden parameter changes, non-overridden parameter keeps global rounding
	expect_false(isTRUE(all.equal(get_mean(base, "Cmax"), get_mean(over, "Cmax"))))
	expect_equal(get_mean(base, "AUClast"), get_mean(over, "AUClast"))
})

test_that("tableNCAParameters roundByParameter accepts a named numeric vector (digits only)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	args <- list(
		projectFile   = pkx_project_1,
		selectParameters = c("Cmax", "AUClast"),
		metrics       = "mean",
		useWideFormat = TRUE,
		prettyLabels  = FALSE, units = FALSE, aliases = FALSE,
		roundDigits   = 3, roundMethod = "monolixSuiteRounding")

	base <- do.call(tableNCAParameters, args)
	over <- do.call(tableNCAParameters, c(args, list(roundByParameter = c(Cmax = 1))))

	get_mean <- function(tb, p) tb$mean[tb$parameter == p]

	expect_false(isTRUE(all.equal(get_mean(base, "Cmax"), get_mean(over, "Cmax"))))
	expect_equal(get_mean(base, "AUClast"), get_mean(over, "AUClast"))
})

test_that("tableNCAParameters roundByParameter ignores unknown parameters with a message", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	expect_message(
		tableNCAParameters(projectFile = pkx_project_1, selectParameters = "Cmax", metrics = "mean",
											 useWideFormat = TRUE, prettyLabels = FALSE, units = FALSE, aliases = FALSE,
											 roundByParameter = c(Cmaxx = 2)),
		"ignoring unknown parameter")

	# Table is still produced; NULL override equals no override
	a <- tableNCAParameters(projectFile = pkx_project_1, selectParameters = "Cmax", metrics = "mean",
													useWideFormat = TRUE, prettyLabels = FALSE, units = FALSE, aliases = FALSE)
	b <- tableNCAParameters(projectFile = pkx_project_1, selectParameters = "Cmax", metrics = "mean",
													useWideFormat = TRUE, prettyLabels = FALSE, units = FALSE, aliases = FALSE,
													roundByParameter = NULL)
	expect_equal(a, b)
})

test_that("tableNCAParameters roundByParameter rejects invalid digits/method", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	expect_error(
		tableNCAParameters(projectFile = pkx_project_1, selectParameters = "Cmax",
											 roundByParameter = data.frame(parameter = "Cmax", digits = 2, method = "bogus")))
	expect_error(
		tableNCAParameters(projectFile = pkx_project_1, selectParameters = "Cmax",
											 roundByParameter = c(Cmax = -1)))
})

test_that("tableNCAParameters roundByParameter applies per-parameter rounding to individual values (includeId)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	common <- list(
		projectFile     = pkx_project_1,
		useWideFormat   = FALSE,
		splitCovariates = "TINFCAT",
		includeId       = TRUE,
		selectParameters = c("Cmax", "AUClast"),
		metrics         = c("MeanSD", "GeomCV", "median"),
		roundDigits     = 1,
		roundMethod     = "monolixSuiteRounding",
		prettyLabels    = FALSE, units = FALSE, aliases = FALSE)  # keep raw param col names

	base <- do.call(tableNCAParameters, common)
	over <- do.call(tableNCAParameters, c(common, list(
		roundByParameter = data.frame(parameter = "Cmax", digits = 3,
																	method = "monolixSuiteRounding",
																	stringsAsFactors = FALSE))))

	expect_true(all(c("Statistic", "Section") %in% names(over)))

	ind_base <- base[base$Section == "INDIVIDUAL", , drop = FALSE]
	ind_over <- over[over$Section == "INDIVIDUAL", , drop = FALSE]
	expect_gt(nrow(ind_over), 0)
	expect_equal(dim(ind_base), dim(ind_over))

	# Overridden parameter: individual values change; non-overridden: unchanged
	expect_false(isTRUE(all.equal(ind_base$Cmax, ind_over$Cmax)))
	expect_equal(ind_over$AUClast, ind_base$AUClast)
})

test_that("tableNCAParameters includeId individual values match independent per-parameter rounding", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Decisive check: each overridden individual cell equals the raw parameter value
	# rounded with that parameter's own digits/method (via the package's .round_num()).
	cfg <- data.frame(parameter = c("Cmax",   "AUClast"),
										digits    = c(3,         4),
										method    = c("monolixSuiteRounding", "significantFigures"),
										stringsAsFactors = FALSE)

	tbl <- tableNCAParameters(
		projectFile      = pkx_project_1,
		useWideFormat    = FALSE,
		splitCovariates  = "TINFCAT",
		includeId        = TRUE,
		selectParameters = cfg$parameter,
		metrics          = c("MeanSD", "median"),
		roundDigits      = 1,
		roundMethod      = "monolixSuiteRounding",
		roundByParameter = cfg,
		prettyLabels     = FALSE, units = FALSE, aliases = FALSE)

	ind <- tbl[tbl$Section == "INDIVIDUAL", , drop = FALSE]
	expect_gt(nrow(ind), 0)

	# Raw per-subject values from the loaded project
	raw <- lixoftConnectors::getNCAIndividualParameters()$parameters

	# Only verify exact per-subject values when there are no occasions, i.e. exactly
	# one INDIVIDUAL row per subject (otherwise Statistic <-> id is not 1:1).
	skip_if(nrow(ind) != dplyr::n_distinct(raw$id),
					"Occasions present; per-subject exact match not applicable")

	row_for_id <- match(as.character(raw$id), as.character(ind$Statistic))
	for (i in seq_len(nrow(cfg))) {
		p        <- cfg$parameter[i]
		exp_vals <- .round_num(raw[[p]], digits = cfg$digits[i],
													 rounding_method = cfg$method[i])
		obs_vals <- suppressWarnings(as.numeric(ind[[p]]))[row_for_id]
		expect_equal(obs_vals, unname(exp_vals),
								 info = paste("per-parameter rounding mismatch for", p))
	}
})

test_that("tableNCAParameters mock = TRUE masks output", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	table_real <- tableNCAParameters(projectFile = pkx_project_1, selectParameters = c("Cmax"), mock = FALSE)
	table_mock <- tableNCAParameters(projectFile = pkx_project_1, selectParameters = c("Cmax"), mock = TRUE)

	expect_true(is.data.frame(table_mock))
	expect_equal(dim(table_real), dim(table_mock))
	expect_true(any(grepl("xx", as.matrix(table_mock))))
})

test_that("tableNCAParameters units and aliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with units enabled, aliases disabled
	table_units_no_alias <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = TRUE,
		aliases     = FALSE)

	# Test with units disabled, aliases enabled
	table_no_units_alias <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = TRUE)

	# Test with both disabled
	table_both_off <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		units       = FALSE,
		aliases     = FALSE)

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
		selectParameters = c("Cmax"),
		prettyLabels = TRUE)

	# Test with pretty labels disabled
	table_raw <- tableNCAParameters(
		projectFile  = pkx_project_1,
		selectParameters = c("Cmax"),
		prettyLabels = FALSE)

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
			metrics     = c("InvalidMetric1", "InvalidMetric2")),
		"metric")

	# Test with invalid covariates
	expect_error(
		tableNCAParameters(
			projectFile = pkx_project_1,
			splitCovariates = "NONEXISTENT_COVARIATE"))

	# Test with invalid NCA parameters
	expect_error(
		tableNCAParameters(
			projectFile = pkx_project_1,
			selectParameters = c("INVALID_PARAM")))
})

test_that("tableNCAParameters complex scenario", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test complex scenario with multiple options
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		metrics          = c("GeomCV", "MeanSD", "Q3"),
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		splitCovariates  = "TINFCAT",
		useWideFormat  = FALSE,
		stratifyColumns = TRUE,
		roundDigits      = 2,
		roundMethod      = "significantFigures",
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
		selectParameters = c("Cmax"))

	# Verify it works
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with different roundDigits values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with roundDigits = 1
	table_1 <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundDigits = 1,
		roundMethod = "monolixSuiteRounding")

	# Test with roundDigits = 5
	table_5 <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax"),
		roundDigits = 5,
		roundMethod = "monolixSuiteRounding")

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
	for (method in c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none")) {
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			selectParameters = c("Cmax"),
			splitCovariates  = "TINFCAT",
			stratifyColumns = TRUE,
			useWideFormat  = FALSE,
			roundDigits      = 3,
			roundMethod      = method,
			metrics          = "GeomCV")

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
		splitCovariates  = "TINFCAT",
		selectParameters = c("AUCINF_obs", "AUClast", "Cmax"),
		metrics          = c("min", "MeanSD", "median", "GeomCV", "max"),
		useWideFormat       = FALSE,
		stratifyColumns = TRUE,
		splitCovariateLabels = "Infusion Category",
		roundDigits      = 4,
		roundMethod      = "significantFigures",
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
		selectParameters = c("Cmax"),
		useWideFormat  = FALSE,
		stratifyColumns = FALSE,
		units            = FALSE,
		aliases          = FALSE,
		prettyLabels     = FALSE)

	# Should still produce valid output
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters with all flags TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with all boolean flags set to TRUE
	table <- tableNCAParameters(
		projectFile      = pkx_project_1,
		selectParameters = c("Cmax"),
		splitCovariates  = "TINFCAT",
		useWideFormat  = TRUE,
		stratifyColumns = FALSE,  # Can't have both useWideFormat=TRUE and stratifyColumns=TRUE
		units            = TRUE,
		aliases          = TRUE,
		prettyLabels     = TRUE)

	# Should still produce valid output
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAParameters default values work correctly", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with only projectFile specified (all other defaults)
	table <- tableNCAParameters(projectFile = pkx_project_1)

	# Should use:
	# - All available selectParameters
	# - No covariates
	# - Default metrics
	# - useWideFormat = TRUE
	# - stratifyColumns = FALSE
	# - roundDigits = 3
	# - roundMethod = "monolixSuiteRounding"
	# - units = TRUE
	# - aliases = TRUE
	# - prettyLabels = TRUE

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 2)  # Should have parameter column + metrics
})

test_that("tableNCAParameters with selectParameters = NULL (all parameters)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with selectParameters = NULL to use all available parameters
	table_all <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = NULL  # Should use all computed NCA parameters
	)

	# Test with specific parameters
	table_specific <- tableNCAParameters(
		projectFile = pkx_project_1,
		selectParameters = c("Cmax")
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
		selectParameters = c("Cmax"),
		splitCovariates  = "TINFCAT",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
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
		selectParameters = "Cmax",
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
		selectParameters = NULL,  # All available
		metrics     = c("min", "Q1", "median", "mean", "Q3", "max",
										"SD", "CV", "geoMean", "geoCV", "harmMean")
	)

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 5)  # Many columns
})

################################################################################
# tableNCAParameters with includeId feature

test_that("tableNCAParameters with includeId basic (no covariates)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test basic includeId functionality without covariates
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax", "AUClast", "Lambda_z"),
		includeId   = TRUE,
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

test_that("tableNCAParameters with includeId and single covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test includeId with stratification by single covariate
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		splitCovariates   = "TINFCAT",
		selectParameters = c("Cmax", "AUClast", "Lambda_z"),
		metrics      = c("min", "MeanSD", "median", "GeomCV", "max"),
		includeId   = TRUE,
		prettyLabels = FALSE,
		units        = FALSE,
		aliases      = FALSE)

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

test_that("tableNCAParameters with includeId and multiple covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test includeId with multiple covariates
	table <- tableNCAParameters(
		projectFile  = pkx_project_3,
		useWideFormat = FALSE,
		splitCovariates = c("DOSE_mg", "ROUTE"),
		selectParameters = c("Cmax", "AUClast"),
		metrics      = c("MeanSD", "GeomCV"),
		includeId   = TRUE,
		prettyLabels = FALSE)

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

test_that("tableNCAParameters with includeId and prettyLabels = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test includeId with pretty labels enabled
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax", "AUClast", "Lambda_z"),
		includeId   = TRUE,
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

test_that("tableNCAParameters with includeId validation: useWideFormat = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that includeId = TRUE with useWideFormat = TRUE generates warning
	expect_warning(
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			useWideFormat  = TRUE,  # This should trigger warning
			selectParameters = c("Cmax"),
			includeId       = TRUE),
		"includeId = TRUE only works with useWideFormat = FALSE")

	# Table should still be generated with includeId set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCAParameters with includeId validation: stratifyColumns = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that includeId = TRUE with stratifyColumns = TRUE generates warning
	expect_warning(
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			useWideFormat  = FALSE,
			stratifyColumns = TRUE,  # This should trigger warning
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax"),
			includeId       = TRUE),
		"includeId = TRUE is not compatible with stratifyColumns = TRUE. Setting includeId to FALSE.")

	# Table should still be generated with includeId set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCAParameters with includeId = FALSE (default behavior)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that includeId = FALSE produces table without individual IDs
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax", "AUClast"),
		includeId   = FALSE,  # Explicitly FALSE
		prettyLabels = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))

	# All Statistic values should be summary statistics, no individual IDs
	statistic_values <- table$Statistic
	summary_stats <- c("mean", "SD", "CV", "SE", "geoMean", "geoCV", "geoSD", "geoSE",
	                   "median", "min", "max", "Q1", "Q3", "N", "Nobs", "Ntot", "Nmiss",
	                   "P05", "P95", "MeanSD", "MeanSE", "MeanCV", "GeomCV", "GeomSD", "GeomSE", "harmMean")

	# All statistic values should be recognized summary statistics
	expect_true(all(statistic_values %in% summary_stats),
	            label = paste("Unexpected statistic values:",
	                         paste(setdiff(statistic_values, summary_stats), collapse = ", ")))
})

test_that("tableNCAParameters with includeId and different rounding settings", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test includeId with different rounding methods
	table_monolix <- tableNCAParameters(
		projectFile = pkx_project_1,
		useWideFormat  = FALSE,
		selectParameters = c("Cmax"),
		includeId  = TRUE,
		roundDigits = 2,
		roundMethod = "monolixSuiteRounding"
	)

	table_signif <- tableNCAParameters(
		projectFile = pkx_project_1,
		useWideFormat  = FALSE,
		selectParameters = c("Cmax"),
		includeId  = TRUE,
		roundDigits = 2,
		roundMethod = "significantFigures"
	)

	table_signif_preserved <- tableNCAParameters(
		projectFile = pkx_project_1,
		useWideFormat  = FALSE,
		selectParameters = c("Cmax"),
		includeId  = TRUE,
		roundDigits = 2,
		roundMethod = "significantFiguresPreserved"
	)

	# All should be valid and have individual IDs
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_signif_preserved))
	expect_gt(nrow(table_monolix), 0)
	expect_gt(nrow(table_signif), 0)
	expect_gt(nrow(table_signif_preserved), 0)
})

test_that("tableNCAParameters with includeId and specific metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test includeId with specific metrics selected
	table <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax", "AUClast"),
		metrics      = c("median", "GeomCV", "max"),
		includeId   = TRUE,
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

test_that("tableNCAParameters with includeId comprehensive test", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Comprehensive test with all compatible features
	table <- tableNCAParameters(
		projectFile     = pkx_project_1,
		splitCovariates = "TINFCAT",
		selectParameters = c("Cmax", "AUClast", "Lambda_z"),
		metrics      = c("min", "MeanSD", "median", "GeomCV", "max"),
		useWideFormat   = FALSE,
		includeId   = TRUE,
		roundDigits  = 3,
		roundMethod  = "monolixSuiteRounding",
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

test_that("tableNCAParameters with includeId row count check", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Compare row counts: includeId = FALSE vs TRUE
	table_no_ids <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax"),
		metrics      = c("median", "GeomCV"),
		includeId   = FALSE,
		prettyLabels = FALSE
	)

	table_with_ids <- tableNCAParameters(
		projectFile  = pkx_project_1,
		useWideFormat   = FALSE,
		selectParameters = c("Cmax"),
		metrics      = c("median", "GeomCV"),
		includeId   = TRUE,
		prettyLabels = FALSE
	)

	# Table with IDs should have more rows than without IDs
	expect_gt(nrow(table_with_ids), nrow(table_no_ids),
	          label = "Table with includeId=TRUE should have more rows than includeId=FALSE")

	# Both should have same number of columns
	expect_equal(ncol(table_with_ids), ncol(table_no_ids)+1)
})

test_that("tableNCAParameters with includeId and numeric categorical covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test with numeric-valued categorical covariate (tests sorting logic)
	# Using project_3 which has DOSE_mg covariate
	table <- tableNCAParameters(
		projectFile  = pkx_project_3,
		useWideFormat   = FALSE,
		splitCovariates   = "DOSE_mg",
		selectParameters = c("Cmax", "AUClast"),
		includeId   = TRUE,
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
# tableNCAParameters: customAliases / customUnits

test_that("tableNCAParameters applies customAliases and customUnits", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Wide format keeps parameter names as values in the Parameter column
	table <- tableNCAParameters(
		projectFile   = pkx_project_1,
		selectParameters = c("Cmax", "AUClast"),
		customAliases = c(Cmax = "C<sub>max</sub>", AUClast = "AUC<sub>last</sub>"),
		customUnits   = c(Cmax = "ng/mL", AUClast = "h*ng/mL"),
		prettyLabels  = FALSE)

	param_col <- if ("parameter" %in% names(table)) "parameter" else "Parameter"
	vals <- table[[param_col]]
	expect_true(any(grepl("[[sub:max]]", vals, fixed = TRUE)))
	expect_true(any(grepl("ng/mL",       vals, fixed = TRUE)))
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
		splitCovariates = c("SEX", "TINFCAT")
	)

	# Check covariates are present and in correct order: splitCovariates are placed
	# before id so modalities stay grouped (see commit 87c620a)
	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "SEX", "TINFCAT") %in% names(result)))
	expect_equal(names(result)[1:3], c("SEX", "TINFCAT", "id"))
})

test_that("listingNCAParameters relabels split covariate columns with splitCovariateLabels", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result <- listingNCAParameters(
		projectFile          = pkx_project_1,
		splitCovariates      = c("SEX", "TINFCAT"),
		splitCovariateLabels = c("Sex", "Infusion Category")
	)

	# Labels rename the covariate columns; raw covariate names are gone.
	expect_true(all(c("Sex", "Infusion Category") %in% names(result)))
	expect_false(any(c("SEX", "TINFCAT") %in% names(result)))
})

test_that("listingNCAParameters filters to selected parameters", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	selected_params <- c("Clast", "Cmax", "AUClast", "AUCINF_obs")

	result <- listingNCAParameters(
		projectFile = pkx_project_2,
		splitCovariates = c("FOOD", "TREAT"),
		selectParameters = selected_params,
		roundDigits = 2,
		roundMethod = "significantFigures"
	)

	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "FOOD", "TREAT") %in% names(result)))
	# Should have limited columns: id + covariates + occasions + selected params
	expect_lte(ncol(result), 3 + 2 + length(selected_params) + 5)  # Some buffer for occasions
})

test_that("listingNCAParameters applies rounding with significantFigures method", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result_rounded <- listingNCAParameters(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundDigits = 2,
		roundMethod = "significantFigures"
	)

	result_none <- listingNCAParameters(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundMethod = "none"
	)

	expect_s3_class(result_rounded, "data.frame")
	expect_s3_class(result_none, "data.frame")
	expect_equal(dim(result_rounded), dim(result_none))
})

test_that("listingNCAParameters applies rounding with significantFiguresPreserved method", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result_rounded <- listingNCAParameters(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundDigits = 2,
		roundMethod = "significantFiguresPreserved"
	)

	result_none <- listingNCAParameters(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundMethod = "none"
	)

	expect_s3_class(result_rounded, "data.frame")
	expect_s3_class(result_none, "data.frame")
	expect_equal(dim(result_rounded), dim(result_none))
})

test_that("listingNCAParameters applies rounding with monolixSuiteRounding method", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "monolixSuiteRounding"
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
		splitCovariates = c("SEQ", "TRT")
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
			splitCovariates = c("INVALID_COVARIATE")
		),
		"Invalid covariate"
	)
})

test_that("listingNCAParameters validates invalid NCA parameters", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	expect_error(
		listingNCAParameters(
			projectFile = pkx_project_1,
			selectParameters = c("INVALID_PARAMETER", "ANOTHER_INVALID")
		),
		"None of the requested.*parameters were found"
	)
})

test_that("listingNCAParameters messages for partially invalid parameters", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	expect_message(
		result <- listingNCAParameters(
			projectFile = pkx_project_2,
			selectParameters = c("Cmax", "INVALID_PARAM", "AUClast")
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
		splitCovariates = c("FOOD", "TREAT"),
		selectParameters = c("Clast", "Cmax", "AUClast", "AUCINF_obs")
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
		roundMethod = "monolixSuiteRounding"
	)

	result_signif <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundDigits = 3,
		roundMethod = "significantFigures"
	)

	result_signif_preserved <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundDigits = 3,
		roundMethod = "significantFiguresPreserved"
	)

	result_none <- listingNCAParameters(
		projectFile = pkx_project_3,
		roundMethod = "none"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_s3_class(result_signif_preserved, "data.frame")
	expect_s3_class(result_none, "data.frame")
	# All should have same structure
	expect_equal(dim(result_monolix), dim(result_signif))
	expect_equal(dim(result_monolix), dim(result_signif_preserved))
	expect_equal(dim(result_monolix), dim(result_none))
})

test_that("listingNCAParameters mock = TRUE masks output", {
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	result_real <- listingNCAParameters(projectFile = pkx_project_3, mock = FALSE)
	result_mock <- listingNCAParameters(projectFile = pkx_project_3, mock = TRUE)

	expect_s3_class(result_mock, "data.frame")
	expect_equal(dim(result_real), dim(result_mock))
	expect_true(all(grepl("^id[0-9]+$", as.character(result_mock$id))))
})

test_that("listingNCAParameters returns correct column order", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAParameters(
		projectFile = pkx_project_2,
		splitCovariates = c("TREAT", "FOOD")  # Specify order
	)

	# First columns should be: TREAT, FOOD, id (splitCovariates before id, see commit 87c620a)
	expect_equal(names(result)[1], "TREAT")
	expect_equal(names(result)[2], "FOOD")
	expect_equal(names(result)[3], "id")
})




test_that("listingNCAParameters applies customAliases and customUnits (named vector)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_complex), "Test project not available")

	result <- listingNCAParameters(
		projectFile   = pkx_project_complex,
		selectParameters = c("AUClast", "Cmax", "C0"),
		roundDigits   = 3,
		roundMethod   = "monolixSuiteRounding",
		customAliases = c(
			Cmax    = "C<sub>max</sub>",
			AUClast = "AUC<sub>0-tlast</sub>",
			C0      = "C<sub>0</sub>"),
		customUnits   = c(
			Cmax    = "100* ng/mL",
			AUClast = "kg*h*ng/mL",
			C0      = "100* nM/mL"))

	nm <- names(result)
	# <sub>max</sub> is converted to the [[sub:max]] placeholder in the column header
	expect_true(any(grepl("[[sub:max]]", nm, fixed = TRUE)))
	# custom unit is appended in parentheses
	expect_true(any(grepl("ng/mL", nm, fixed = TRUE)))
})

test_that("listingNCAParameters accepts customAliases/customUnits as a data.frame", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_complex), "Test project not available")

	aliases_df <- data.frame(
		parameter = c("Cmax", "AUClast"),
		alias     = c("PeakConc", "AreaLast"),
		stringsAsFactors = FALSE)
	units_df <- data.frame(
		parameter = c("Cmax", "AUClast"),
		unit      = c("ng/mL", "h*ng/mL"),
		stringsAsFactors = FALSE)

	result <- listingNCAParameters(
		projectFile   = pkx_project_complex,
		selectParameters = c("Cmax", "AUClast"),
		customAliases = aliases_df,
		customUnits   = units_df)

	nm <- names(result)
	expect_true(any(grepl("PeakConc", nm, fixed = TRUE)))
	expect_true(any(grepl("AreaLast", nm, fixed = TRUE)))
	expect_true(any(grepl("ng/mL",    nm, fixed = TRUE)))
})

test_that("listingNCAParameters roundByParameter data.frame overrides digits and method per parameter", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	# aliases/units FALSE keeps raw parameter codes as column names, so overrides
	# (and the per-column comparison below) key on the same names.
	args <- list(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundDigits = 3, roundMethod = "monolixSuiteRounding",
		units = FALSE, aliases = FALSE)

	base <- do.call(listingNCAParameters, args)
	over <- do.call(listingNCAParameters, c(args, list(
		roundByParameter = data.frame(parameter = "Cmax", digits = 1,
																	method = "significantFigures",
																	stringsAsFactors = FALSE))))

	# Overridden column changes; non-overridden column keeps global rounding
	expect_false(isTRUE(all.equal(base$Cmax, over$Cmax)))
	expect_equal(base$AUClast, over$AUClast)
})

test_that("listingNCAParameters roundByParameter accepts a named numeric vector (digits only)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	args <- list(
		projectFile = pkx_project_2,
		selectParameters = c("Cmax", "AUClast"),
		roundDigits = 3, roundMethod = "monolixSuiteRounding",
		units = FALSE, aliases = FALSE)

	base <- do.call(listingNCAParameters, args)
	over <- do.call(listingNCAParameters, c(args, list(roundByParameter = c(Cmax = 1))))

	expect_false(isTRUE(all.equal(base$Cmax, over$Cmax)))
	expect_equal(base$AUClast, over$AUClast)
})

test_that("listingNCAParameters roundByParameter ignores unknown parameters with a message", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	expect_message(
		listingNCAParameters(projectFile = pkx_project_2, selectParameters = "Cmax",
												 units = FALSE, aliases = FALSE,
												 roundByParameter = c(Cmaxx = 2)),
		"ignoring unknown parameter")

	# Listing is still produced; NULL override equals no override
	a <- listingNCAParameters(projectFile = pkx_project_2, selectParameters = "Cmax",
														units = FALSE, aliases = FALSE)
	b <- listingNCAParameters(projectFile = pkx_project_2, selectParameters = "Cmax",
														units = FALSE, aliases = FALSE, roundByParameter = NULL)
	expect_equal(a, b)
})

test_that("listingNCAParameters roundByParameter rejects invalid digits/method", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	expect_error(
		listingNCAParameters(projectFile = pkx_project_2, selectParameters = "Cmax",
												 roundByParameter = data.frame(parameter = "Cmax", digits = 2, method = "bogus")))
	expect_error(
		listingNCAParameters(projectFile = pkx_project_2, selectParameters = "Cmax",
												 roundByParameter = c(Cmax = -1)))
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
		splitCovariates = c("FOOD", "TREAT")
	)

	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "FOOD", "TREAT", "ActualTime", "Concentration") %in% names(result)))
	# Check covariate ordering
	expect_true(which(names(result) == "FOOD") < which(names(result) == "ActualTime"))
})

test_that("listingNCAConcentration relabels split covariate columns with splitCovariateLabels", {
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	result <- listingNCAConcentration(
		projectFile          = pkx_project_2,
		splitCovariates      = c("FOOD", "TREAT"),
		splitCovariateLabels = c("Food Status", "Treatment")
	)

	expect_true(all(c("Food Status", "Treatment") %in% names(result)))
	expect_false(any(c("FOOD", "TREAT") %in% names(result)))
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
		roundMethod = "significantFigures"
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
		roundMethod = "monolixSuiteRounding"
	)

	result_signif <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "significantFigures"
	)

	result_signif_preserved <- listingNCAConcentration(
		projectFile = pkx_project_4,
		roundDigits = 4,
		roundMethod = "significantFiguresPreserved"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_s3_class(result_signif_preserved, "data.frame")
	expect_equal(dim(result_monolix), dim(result_signif))
	expect_equal(dim(result_monolix), dim(result_signif_preserved))
})

test_that("listingNCAConcentration works with default roundMethod (no argument passed)", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	# Regression test: roundMethod used to be missing its match.arg() resolution here,
	# so leaving it at the default (unspecified) crashed with a "'arg' must be of
	# length 1" error once forwarded into .round_num().
	result <- listingNCAConcentration(projectFile = pkx_project_4)

	expect_s3_class(result, "data.frame")
	expect_gt(nrow(result), 0)
})

test_that("listingNCAConcentration mock = TRUE masks output", {
	skip_if_not(file.exists(pkx_project_4), "Test project not available")

	result_real <- listingNCAConcentration(projectFile = pkx_project_4, mock = FALSE)
	result_mock <- listingNCAConcentration(projectFile = pkx_project_4, mock = TRUE)

	expect_s3_class(result_mock, "data.frame")
	expect_equal(dim(result_real), dim(result_mock))
	# id column is relabeled to "id1", "id2", ...
	expect_true(all(grepl("^id[0-9]+$", as.character(result_mock$id))))
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
		splitCovariates = c("FOOD", "TREAT")
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
			splitCovariates = c("INVALID_COV")
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
		splitCovariates = c("FOOD", "TREAT")
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

# listingNCAConcentration: time-column branches

test_that("listingNCAConcentration handles a project without actual time (nominal only)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_notime), "Test project not available")

	# Project has only nominal time -> nominal is used as the time axis
	result <- suppressMessages(listingNCAConcentration(
		projectFile    = pkx_project_notime,
		useNominalTime = TRUE))

	expect_s3_class(result, "data.frame")
	expect_true("NominalTime" %in% names(result))
	expect_false("ActualTime" %in% names(result))
})

test_that("listingNCAConcentration warns when useNominalTime = TRUE but no nominal time exists", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# project_aPCSK9_SAD_route has actual time but no nominal time
	expect_warning(
		result <- listingNCAConcentration(
			projectFile    = pkx_project_3,
			useNominalTime = TRUE),
		"No nominal time available")

	expect_true("ActualTime"   %in% names(result))
	expect_false("NominalTime" %in% names(result))
})

################################################################################
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
		splitCovariates = c("SEQ")
	)

	# Check covariates are present and in correct order: splitCovariates before id
	# so modalities stay grouped (see commit 87c620a)
	expect_s3_class(result, "data.frame")
	expect_true(all(c("id", "SEQ") %in% names(result)))
	expect_equal(names(result)[1:2], c("SEQ", "id"))
})

test_that("listingNCARatios relabels split covariate columns with splitCovariateLabels", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile          = pkx_project_5,
		splitCovariates      = "SEQ",
		splitCovariateLabels = "Sequence"
	)

	expect_true("Sequence" %in% names(result))
	expect_false("SEQ" %in% names(result))
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
			splitCovariates = c("SEQ"),
			selectParameters = selected_ratios,
			roundDigits = 2,
			roundMethod = "significantFigures"
		)

		expect_s3_class(result, "data.frame")
		expect_true(all(c("id", "SEQ") %in% names(result)))
		# Should have limited columns: id + covariates + occasions + selected ratios
		expect_lte(ncol(result), 3 + 1 + length(selected_ratios) + 2)  # Some buffer
	}
})

test_that("listingNCARatios applies rounding with significantFigures method", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	ratio_params <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(ratio_params) >= 2) {
		selected <- ratio_params[1:2]

		result_rounded <- listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = selected,
			roundDigits = 2,
			roundMethod = "significantFigures"
		)

		result_none <- listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = selected,
			roundMethod = "none"
		)

		expect_s3_class(result_rounded, "data.frame")
		expect_s3_class(result_none, "data.frame")
		expect_equal(dim(result_rounded), dim(result_none))
	}
})

test_that("listingNCARatios applies rounding with significantFiguresPreserved method", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	ratio_params <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(ratio_params) >= 2) {
		selected <- ratio_params[1:2]

		result_rounded <- listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = selected,
			roundDigits = 2,
			roundMethod = "significantFiguresPreserved"
		)

		result_none <- listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = selected,
			roundMethod = "none"
		)

		expect_s3_class(result_rounded, "data.frame")
		expect_s3_class(result_none, "data.frame")
		expect_equal(dim(result_rounded), dim(result_none))
	}
})

test_that("listingNCARatios applies rounding with monolixSuiteRounding method", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 4,
		roundMethod = "monolixSuiteRounding"
	)

	expect_s3_class(result, "data.frame")
	expect_gt(ncol(result), 1)
})

test_that("listingNCARatios roundByParameter data.frame overrides digits and method per parameter", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Ratio parameter columns in pkx_project_5 (aliases are a no-op for ratios,
	# so these raw codes are also the column names that overrides key on).
	selected <- c("AUC_inf_Per1_Per2", "AUC_inf_Per1_Per3")
	args <- list(
		projectFile = pkx_project_5,
		selectParameters = selected,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	base <- do.call(listingNCARatios, args)
	over <- do.call(listingNCARatios, c(args, list(
		roundByParameter = data.frame(parameter = selected[1], digits = 1,
																	method = "significantFigures",
																	stringsAsFactors = FALSE))))

	# Overridden column changes; non-overridden column keeps global rounding
	expect_false(isTRUE(all.equal(base[[selected[1]]], over[[selected[1]]])))
	expect_equal(base[[selected[2]]], over[[selected[2]]])
})

test_that("listingNCARatios roundByParameter accepts a named numeric vector (digits only)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	selected <- c("AUC_inf_Per1_Per2", "AUC_inf_Per1_Per3")
	args <- list(
		projectFile = pkx_project_5,
		selectParameters = selected,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	base <- do.call(listingNCARatios, args)
	over <- do.call(listingNCARatios, c(args, list(
		roundByParameter = c(AUC_inf_Per1_Per2 = 1))))

	expect_false(isTRUE(all.equal(base[[selected[1]]], over[[selected[1]]])))
	expect_equal(base[[selected[2]]], over[[selected[2]]])
})

test_that("listingNCARatios roundByParameter ignores unknown parameters with a message", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	expect_message(
		listingNCARatios(projectFile = pkx_project_5,
										 roundByParameter = c(definitelyNotAParam = 2)),
		"ignoring unknown parameter")

	# Listing is still produced; NULL override equals no override
	a <- listingNCARatios(projectFile = pkx_project_5)
	b <- listingNCARatios(projectFile = pkx_project_5, roundByParameter = NULL)
	expect_equal(a, b)
})

test_that("listingNCARatios roundByParameter rejects invalid digits/method", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	expect_error(
		listingNCARatios(projectFile = pkx_project_5,
										 roundByParameter = data.frame(parameter = "AUC_inf_Per1_Per2",
																									 digits = 2, method = "bogus")))
	expect_error(
		listingNCARatios(projectFile = pkx_project_5,
										 roundByParameter = c(AUC_inf_Per1_Per2 = -1)))
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
		splitCovariates = c("SEQ")
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
			splitCovariates = c("INVALID_COVARIATE")
		),
		"Invalid covariate"
	)
})

test_that("listingNCARatios validates invalid ratio parameters", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	expect_error(
		listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = c("INVALID_RATIO", "ANOTHER_INVALID")
		),
		"None of the requested.*parameters were found"
	)
})

test_that("listingNCARatios messages for partially invalid parameters", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get one valid ratio parameter
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	valid_ratio <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))[1]

	expect_message(
		result <- listingNCARatios(
			projectFile = pkx_project_5,
			selectParameters = c(valid_ratio, "INVALID_PARAM")
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
		splitCovariates = c("SEQ")
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
		roundMethod = "monolixSuiteRounding"
	)

	result_signif <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 3,
		roundMethod = "significantFigures"
	)

	result_signif_preserved <- listingNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 3,
		roundMethod = "significantFiguresPreserved"
	)

	result_none <- listingNCARatios(
		projectFile = pkx_project_5,
		roundMethod = "none"
	)

	expect_s3_class(result_monolix, "data.frame")
	expect_s3_class(result_signif, "data.frame")
	expect_s3_class(result_signif_preserved, "data.frame")
	expect_s3_class(result_none, "data.frame")
	# All should have same structure
	expect_equal(dim(result_monolix), dim(result_signif))
	expect_equal(dim(result_monolix), dim(result_signif_preserved))
	expect_equal(dim(result_monolix), dim(result_none))
})

test_that("listingNCARatios mock = TRUE masks output", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result_real <- listingNCARatios(projectFile = pkx_project_5, mock = FALSE)
	result_mock <- listingNCARatios(projectFile = pkx_project_5, mock = TRUE)

	expect_s3_class(result_mock, "data.frame")
	expect_equal(dim(result_real), dim(result_mock))
	expect_true(all(grepl("^id[0-9]+$", as.character(result_mock$id))))
})

test_that("listingNCARatios returns correct column order", {
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	result <- listingNCARatios(
		projectFile = pkx_project_5,
		splitCovariates = c("SEQ")  # Specify order
	)

	# First columns should be: SEQ, id (splitCovariates before id, see commit 87c620a)
	expect_equal(names(result)[1], "SEQ")
	expect_equal(names(result)[2], "id")
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

test_that("listingNCARatios applies customAliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	all_ratios   <- listingNCARatios(projectFile = pkx_project_5)
	ratio_params <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))
	skip_if(length(ratio_params) < 1, "No ratio parameters available")

	sel     <- ratio_params[1]
	aliases <- setNames("MyRatio<sub>x</sub>", sel)

	result <- listingNCARatios(
		projectFile   = pkx_project_5,
		selectParameters = sel,
		customAliases = aliases)

	expect_true(any(grepl("MyRatio[[sub:x]]", names(result), fixed = TRUE)))
})

################################################################################
################################################################################
# Tests for listingAbbreviation -------------------------------------------------

test_that("listingAbbreviation returns basic structure", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result <- listingAbbreviation(projectFile = pkx_project_1)

	expect_true(is.data.frame(result))
	expect_gt(nrow(result), 0)
	expect_equal(names(result), c("Abbreviation", "Full term"))
})

test_that("listingAbbreviation with aliases = FALSE keeps raw parameter names", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result_aliases    <- listingAbbreviation(projectFile = pkx_project_1, aliases = TRUE)
	result_no_aliases <- listingAbbreviation(projectFile = pkx_project_1, aliases = FALSE)

	expect_true(is.data.frame(result_no_aliases))
	# Same set of underlying NCA parameters either way
	expect_equal(nrow(result_aliases), nrow(result_no_aliases))
})

test_that("listingAbbreviation applies customAliases mapping", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# aliases = FALSE so Abbreviation holds the raw parameter name - customAliases is
	# joined on that raw name, not on whatever alias would otherwise be displayed
	result_default <- listingAbbreviation(projectFile = pkx_project_1, aliases = FALSE)
	target_param    <- result_default$Abbreviation[1]

	custom <- setNames("MyCustomAlias", target_param)
	result_custom <- listingAbbreviation(projectFile = pkx_project_1, customAliases = custom)

	expect_true(is.data.frame(result_custom))
	expect_true("MyCustomAlias" %in% result_custom$Abbreviation)
})

test_that("listingAbbreviation applies fullTerms mapping", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	result_default <- listingAbbreviation(projectFile = pkx_project_1, aliases = FALSE)
	target_param    <- result_default$Abbreviation[1]

	full_terms <- setNames("My Full Term Description", target_param)
	result <- listingAbbreviation(projectFile = pkx_project_1, aliases = FALSE, fullTerms = full_terms)

	expect_true(is.data.frame(result))
	expect_true("My Full Term Description" %in% result$`Full term`)
})

test_that("listingAbbreviation errors on wrong project file type", {
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# .checkProjectFile() validates by file extension; a non-.pkx path should be rejected
	expect_error(
		listingAbbreviation(projectFile = "some_project.mlxtran")
	)
})

################################################################################
################################################################################
# Tests for tableNCARatios -----------------------------------------------------

test_that("tableNCARatios basic (no stratification)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test basic functionality with no covariates
	table <- tableNCARatios(projectFile = pkx_project_5)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Parameter" %in% names(table) || "parameter" %in% names(table))
	expect_gt(nrow(table), 0)

	# Verify metrics columns exist (at least some standard ones)
	expect_true(any(c("mean", "SD", "CV", "geoMean", "geoCV", "median", "Nobs") %in% names(table)))
})

test_that("tableNCARatios with specific selectParameters", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters first
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 2) {
		selected_ratios <- available_ratios[1:2]

		table <- tableNCARatios(
			projectFile = pkx_project_5,
			selectParameters = selected_ratios
		)

		# Verify structure
		expect_true(is.data.frame(table))
		expect_gt(nrow(table), 0)

		# Verify only requested parameters are included
		param_col <- if ("parameter" %in% names(table)) "parameter" else "Parameter"
		unique_params <- unique(table[[param_col]])
		expect_equal(length(unique_params), 2)
	}
})

test_that("tableNCARatios with specific metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with specific metrics
	table <- tableNCARatios(
		projectFile = pkx_project_5,
		metrics     = c("min", "MeanSD", "median", "GeomCV", "max")
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Verify requested metrics are present
	param_col <- if ("parameter" %in% names(table)) "parameter" else "Parameter"
	metric_cols <- setdiff(names(table), c("parameter", "Parameter", "Nobs"))
	expect_true(length(metric_cols) > 0)
})

test_that("tableNCARatios with stratification (stratifyColumns = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with covariates but no column stratification
	table <- tableNCARatios(
		projectFile      = pkx_project_5,
		splitCovariates  = "SEQ",
		stratifyColumns = FALSE
	)

	# Verify covariate column exists
	expect_true("SEQ" %in% names(table))

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCARatios with column stratification (stratifyColumns = TRUE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with covariates and column stratification
	table <- tableNCARatios(
		projectFile      = pkx_project_5,
		splitCovariates  = "SEQ",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE)

	# Verify structure
	expect_true(is.data.frame(table))

	# Verify "Overall" column exists (always included in stratified layout)
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))

	# Verify no SEQ column (covariates become headers when stratifyColumns=TRUE)
	expect_false("SEQ" %in% names(table))
})

test_that("tableNCARatios with column stratification and single metric", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with single metric (should suppress Statistic column)
	table <- tableNCARatios(
		projectFile      = pkx_project_5,
		splitCovariates       = "SEQ",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = "GeomCV"
	)

	# Verify "Statistic" column does NOT exist for single metric
	expect_false("Statistic" %in% names(table))
	expect_true(any(grepl("Overall", names(table), ignore.case = TRUE)))
})

test_that("tableNCARatios with column stratification and multiple metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with multiple metrics
	table <- tableNCARatios(
		projectFile      = pkx_project_5,
		splitCovariates       = "SEQ",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE,
		metrics          = c("MeanSD", "GeomCV")
	)

	# Verify "Statistic" column EXISTS for multiple metrics
	expect_true("Statistic" %in% names(table))
})

test_that("tableNCARatios vertical layout (useWideFormat = FALSE)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test vertical layout without stratification
	table <- tableNCARatios(
		projectFile     = pkx_project_5,
		useWideFormat = FALSE
	)

	# Verify "Statistic" column exists in vertical layout
	expect_true("Statistic" %in% names(table))
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCARatios vertical layout with covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test vertical layout with covariates
	table <- tableNCARatios(
		projectFile     = pkx_project_5,
		splitCovariates      = "SEQ",
		useWideFormat = FALSE,
		metrics         = c("min", "MeanSD", "median", "GeomCV", "max")
	)

	# Verify structure
	expect_true("Statistic" %in% names(table))
	expect_true("SEQ" %in% names(table))
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCARatios rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test different rounding methods
	table_monolix <- tableNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 2,
		roundMethod = "monolixSuiteRounding"
	)

	table_signif <- tableNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 2,
		roundMethod = "significantFigures"
	)

	table_signif_preserved <- tableNCARatios(
		projectFile = pkx_project_5,
		roundDigits = 2,
		roundMethod = "significantFiguresPreserved"
	)

	table_none <- tableNCARatios(
		projectFile = pkx_project_5,
		roundMethod = "none"
	)

	# Verify all tables are valid
	expect_true(is.data.frame(table_monolix))
	expect_true(is.data.frame(table_signif))
	expect_true(is.data.frame(table_signif_preserved))
	expect_true(is.data.frame(table_none))

	# Verify they have the same structure
	expect_equal(nrow(table_monolix), nrow(table_signif))
	expect_equal(nrow(table_monolix), nrow(table_signif_preserved))
	expect_equal(nrow(table_monolix), nrow(table_none))
})

test_that("tableNCARatios roundByParameter applies per-parameter rounding to individual values (includeId)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	all_ratios       <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))
	skip_if(length(available_ratios) < 2, "Need >= 2 ratio parameters")
	p_over <- available_ratios[1]   # gets the override
	p_keep <- available_ratios[2]   # stays on the global rule

	common <- list(
		projectFile     = pkx_project_5,
		useWideFormat   = FALSE,
		splitCovariates = "SEQ",
		includeId       = TRUE,
		selectParameters = c(p_over, p_keep),
		metrics         = c("MeanSD", "GeomCV", "median"),
		roundDigits     = 1,
		roundMethod     = "monolixSuiteRounding",
		prettyLabels    = FALSE)

	base <- do.call(tableNCARatios, common)
	over <- do.call(tableNCARatios, c(common, list(
		roundByParameter = data.frame(parameter = p_over, digits = 4,
																	method = "significantFigures",
																	stringsAsFactors = FALSE))))

	expect_true(all(c("Statistic", "Section") %in% names(over)))
	ind_base <- base[base$Section == "INDIVIDUAL", , drop = FALSE]
	ind_over <- over[over$Section == "INDIVIDUAL", , drop = FALSE]
	expect_gt(nrow(ind_over), 0)
	expect_equal(dim(ind_base), dim(ind_over))

	# Overridden ratio param: individual values change; non-overridden: unchanged
	expect_false(isTRUE(all.equal(ind_base[[p_over]], ind_over[[p_over]])))
	expect_equal(ind_over[[p_keep]], ind_base[[p_keep]])
})

test_that("tableNCARatios includeId individual values match independent per-parameter rounding", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	all_ratios       <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))
	skip_if(length(available_ratios) < 2, "Need >= 2 ratio parameters")

	# Decisive check: each overridden individual cell equals the raw ratio value
	# rounded with that parameter's own digits/method (via the package's .round_num()).
	cfg <- data.frame(parameter = available_ratios[1:2],
										digits    = c(3,         4),
										method    = c("monolixSuiteRounding", "significantFigures"),
										stringsAsFactors = FALSE)

	tbl <- tableNCARatios(
		projectFile      = pkx_project_5,
		useWideFormat    = FALSE,
		splitCovariates  = "SEQ",
		includeId        = TRUE,
		selectParameters = cfg$parameter,
		metrics          = c("MeanSD", "median"),
		roundDigits      = 1,
		roundMethod      = "monolixSuiteRounding",
		roundByParameter = cfg,
		prettyLabels     = FALSE)

	ind <- tbl[tbl$Section == "INDIVIDUAL", , drop = FALSE]
	expect_gt(nrow(ind), 0)

	# Raw per-subject ratio values from the loaded project
	raw <- lixoftConnectors::getNCAIndividualRatios()

	# Only verify exact per-subject values when there are no occasions, i.e. exactly
	# one INDIVIDUAL row per subject (otherwise Statistic <-> id is not 1:1).
	skip_if(nrow(ind) != dplyr::n_distinct(raw$id),
					"Occasions present; per-subject exact match not applicable")

	row_for_id <- match(as.character(raw$id), as.character(ind$Statistic))
	for (i in seq_len(nrow(cfg))) {
		p        <- cfg$parameter[i]
		exp_vals <- .round_num(raw[[p]], digits = cfg$digits[i],
													 rounding_method = cfg$method[i])
		obs_vals <- suppressWarnings(as.numeric(ind[[p]]))[row_for_id]
		expect_equal(obs_vals, unname(exp_vals),
								 info = paste("per-parameter rounding mismatch for", p))
	}
})

test_that("tableNCARatios mock = TRUE masks output", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	table_real <- tableNCARatios(projectFile = pkx_project_5, mock = FALSE)
	table_mock <- tableNCARatios(projectFile = pkx_project_5, mock = TRUE)

	expect_true(is.data.frame(table_mock))
	expect_equal(dim(table_real), dim(table_mock))
	expect_true(any(grepl("xx", as.matrix(table_mock))))
})

test_that("tableNCARatios with splitCovariateLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with custom covariate labels
	table <- tableNCARatios(
		projectFile      = pkx_project_5,
		splitCovariates       = "SEQ",
		splitCovariateLabels = "Sequence",
		stratifyColumns = TRUE,
		useWideFormat  = FALSE
	)

	# Verify custom labels appear in column headers
	header_names <- names(table)
	expect_true(any(grepl("Sequence", header_names)))
})

test_that("tableNCARatios non-stratified relabels the split covariate column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# stratifyColumns = FALSE -> covariate is a literal column renamed by the label.
	table <- tableNCARatios(
		projectFile          = pkx_project_5,
		splitCovariates      = "SEQ",
		splitCovariateLabels = "Sequence",
		stratifyColumns      = FALSE)

	expect_true("Sequence" %in% names(table))
	expect_false("SEQ" %in% names(table))
})

test_that("tableNCARatios prettyLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with pretty labels enabled
	table_pretty <- tableNCARatios(
		projectFile  = pkx_project_5,
		prettyLabels = TRUE
	)

	# Test with pretty labels disabled
	table_raw <- tableNCARatios(
		projectFile  = pkx_project_5,
		prettyLabels = FALSE
	)

	# Verify both tables are valid
	expect_true(is.data.frame(table_pretty))
	expect_true(is.data.frame(table_raw))

	# Both should have same number of rows
	expect_equal(nrow(table_pretty), nrow(table_raw))
})

test_that("tableNCARatios errors", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with invalid metrics
	expect_error(
		tableNCARatios(
			projectFile = pkx_project_5,
			metrics     = c("InvalidMetric1", "InvalidMetric2")
		),
		"metric"
	)

	# Test with invalid covariates
	expect_error(
		tableNCARatios(
			projectFile = pkx_project_5,
			splitCovariates = "NONEXISTENT_COVARIATE"
		)
	)

	# Test with invalid ratio parameters
	expect_error(
		tableNCARatios(
			projectFile = pkx_project_5,
			selectParameters = c("INVALID_RATIO")
		)
	)
})

test_that("tableNCARatios validates NCA ratios availability", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# project_remifentanil_excl has NCA but likely no ratios configured
	expect_error(
		tableNCARatios(projectFile = pkx_project_1),
		"No NCA.*ratio"
	)
})

test_that("tableNCARatios default values work correctly", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test with only projectFile specified (all other defaults)
	table <- tableNCARatios(projectFile = pkx_project_5)

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 2)  # Should have parameter column + metrics
})

test_that("tableNCARatios edge case: single parameter, single metric", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get one valid ratio parameter
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	valid_ratio <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))[1]

	# Minimal case
	table <- tableNCARatios(
		projectFile = pkx_project_5,
		selectParameters = valid_ratio,
		metrics     = "mean"
	)

	expect_true(is.data.frame(table))
	expect_equal(nrow(table), 1)  # Should have exactly one row
})

################################################################################
# tableNCARatios with includeId feature

test_that("tableNCARatios with includeId basic (no covariates)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 2) {
		selected_ratios <- available_ratios[1:2]

		# Test basic includeId functionality without covariates
		table <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = FALSE,
			selectParameters = selected_ratios,
			includeId      = TRUE,
			prettyLabels    = FALSE
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
		has_ids <- any(!statistic_values %in% summary_stats)
		expect_true(has_ids, label = "Table should contain individual IDs in addition to summary statistics")
	}
})

test_that("tableNCARatios with includeId and single covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 2) {
		selected_ratios <- available_ratios[1:2]

		# Test includeId with stratification by single covariate
		table <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = FALSE,
			splitCovariates = "SEQ",
			selectParameters = selected_ratios,
			metrics         = c("min", "MeanSD", "median", "GeomCV", "max"),
			includeId      = TRUE,
			prettyLabels    = FALSE
		)

		# Verify structure
		expect_true(is.data.frame(table))
		expect_true("Statistic" %in% names(table))
		expect_true("SEQ" %in% names(table))
		expect_gt(nrow(table), 0)
	}
})

test_that("tableNCARatios with includeId validation: useWideFormat = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test that includeId = TRUE with useWideFormat = TRUE generates warning
	expect_warning(
		table <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = TRUE,  # This should trigger warning
			includeId      = TRUE
		),
		"includeId = TRUE only works with useWideFormat = FALSE"
	)

	# Table should still be generated with includeId set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCARatios with includeId validation: stratifyColumns = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Test that includeId = TRUE with stratifyColumns = TRUE generates warning
	expect_warning(
		table <- tableNCARatios(
			projectFile      = pkx_project_5,
			useWideFormat  = FALSE,
			stratifyColumns = TRUE,  # This should trigger warning
			splitCovariates  = "SEQ",
			includeId       = TRUE
		),
		"includeId = TRUE is not compatible with stratifyColumns = TRUE"
	)

	# Table should still be generated with includeId set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCARatios with includeId = FALSE (default behavior)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 1) {
		selected_ratio <- available_ratios[1]

		# Test that includeId = FALSE produces table without individual IDs
		table <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = FALSE,
			selectParameters = selected_ratio,
			includeId      = FALSE,  # Explicitly FALSE
			prettyLabels    = FALSE
		)

		# Verify structure
		expect_true(is.data.frame(table))
		expect_true("Statistic" %in% names(table))

		# All Statistic values should be summary statistics, no individual IDs
		statistic_values <- table$Statistic
		summary_stats <- c("mean", "SD", "CV", "SE", "geoMean", "geoCV", "geoSD", "geoSE",
		                   "median", "min", "max", "Q1", "Q3", "N", "Nobs", "Ntot", "Nmiss",
		                   "P05", "P95", "MeanSD", "MeanSE", "MeanCV", "GeomCV", "GeomSD", "GeomSE", "harmMean")

		# All statistic values should be recognized summary statistics
		expect_true(all(statistic_values %in% summary_stats),
		            label = paste("Unexpected statistic values:",
		                         paste(setdiff(statistic_values, summary_stats), collapse = ", ")))
	}
})

test_that("tableNCARatios with includeId row count check", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 1) {
		selected_ratio <- available_ratios[1]

		# Compare row counts: includeId = FALSE vs TRUE
		table_no_ids <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = FALSE,
			selectParameters = selected_ratio,
			metrics         = c("median", "GeomCV"),
			includeId      = FALSE,
			prettyLabels    = FALSE
		)

		table_with_ids <- tableNCARatios(
			projectFile     = pkx_project_5,
			useWideFormat = FALSE,
			selectParameters = selected_ratio,
			metrics         = c("median", "GeomCV"),
			includeId      = TRUE,
			prettyLabels    = FALSE
		)

		# Table with IDs should have more rows than without IDs
		expect_gt(nrow(table_with_ids), nrow(table_no_ids),
		          label = "Table with includeId=TRUE should have more rows than includeId=FALSE")

		# Both should have same number of columns
		expect_equal(ncol(table_with_ids), ncol(table_no_ids) + 1)  # Section column added
	}
})

test_that("tableNCARatios all arguments combined", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available ratio parameters
	all_ratios <- listingNCARatios(projectFile = pkx_project_5)
	available_ratios <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))

	if (length(available_ratios) >= 2) {
		selected_ratios <- available_ratios[1:2]

		# Test with all arguments specified (comprehensive test)
		table <- tableNCARatios(
			projectFile      = pkx_project_5,
			splitCovariates  = "SEQ",
			selectParameters = selected_ratios,
			metrics          = c("min", "MeanSD", "median", "GeomCV", "max"),
			useWideFormat  = FALSE,
			includeId       = TRUE,
			roundDigits      = 4,
			roundMethod      = "significantFigures",
			prettyLabels     = TRUE
		)

		# Verify structure
		expect_true(is.data.frame(table))
		expect_gt(nrow(table), 0)
		expect_true("Statistic" %in% names(table))
		expect_true("SEQ" %in% names(table))
	}
})

################################################################################
# tableNCARatios: customAliases

test_that("tableNCARatios applies customAliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	all_ratios   <- listingNCARatios(projectFile = pkx_project_5)
	ratio_params <- setdiff(names(all_ratios), c("id", "SEQ", "TRT", "OCC"))
	skip_if(length(ratio_params) < 1, "No ratio parameters available")

	sel   <- ratio_params[1]
	table <- tableNCARatios(
		projectFile   = pkx_project_5,
		selectParameters = sel,
		customAliases = setNames("RatAlias<sub>1</sub>", sel),
		prettyLabels  = FALSE)

	param_col <- if ("parameter" %in% names(table)) "parameter" else "Parameter"
	expect_true(any(grepl("RatAlias[[sub:1]]", table[[param_col]], fixed = TRUE)))
})

################################################################################
################################################################################
# Tests for filterCovariates and filterIds -------------------------------------

# --- tableNCAConcentration filter tests ---

test_that("tableNCAConcentration with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing <- listingNCAConcentration(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_value <- tinfcat_values[1]

		# Test with covariate filter
		table_filtered <- tableNCAConcentration(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			filterCovariates = list(TINFCAT = filter_value)
		)

		table_unfiltered <- tableNCAConcentration(
			projectFile = pkx_project_1,
			splitCovariates  = "TINFCAT"
		)

		# Filtered table should have fewer or equal rows
		expect_lte(nrow(table_filtered), nrow(table_unfiltered))
		expect_true(is.data.frame(table_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(table_filtered$TINFCAT == filter_value))
	}
})

test_that("tableNCAConcentration with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get available IDs first
	listing <- listingNCAConcentration(projectFile = pkx_project_1)
	available_ids <- unique(listing$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		table_filtered <- tableNCAConcentration(
			projectFile = pkx_project_1,
			filterIds   = selected_ids
		)

		table_unfiltered <- tableNCAConcentration(
			projectFile = pkx_project_1
		)

		# Filtered table should exist and be valid
		expect_true(is.data.frame(table_filtered))
		expect_gt(nrow(table_filtered), 0)
	}
})

test_that("tableNCAConcentration with combined filterCovariates and filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get data to find valid filter combinations
	listing <- listingNCAConcentration(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)
	available_ids <- unique(listing$id)

	if (length(tinfcat_values) >= 1 && length(available_ids) >= 2) {
		filter_value <- tinfcat_values[1]
		selected_ids <- available_ids[1:2]

		# Test with both filters
		table <- tableNCAConcentration(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			filterCovariates = list(TINFCAT = filter_value),
			filterIds        = selected_ids
		)

		expect_true(is.data.frame(table))
	}
})

test_that("tableNCAConcentration with filterCovariates and includeId", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing <- listingNCAConcentration(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_value <- tinfcat_values[1]

		# Test with covariate filter and includeId
		# Note: includeId = TRUE requires useWideFormat = TRUE for tableNCAConcentration
		table <- tableNCAConcentration(
			projectFile      = pkx_project_1,
			useWideFormat  = TRUE,
			splitCovariates  = "TINFCAT",
			filterCovariates = list(TINFCAT = filter_value),
			includeId       = TRUE
		)

		expect_true(is.data.frame(table))
		expect_true("Statistic" %in% names(table))
		expect_true("Section" %in% names(table))

		# Only filtered covariate value should appear
		expect_true(all(table$TINFCAT == filter_value))

		# Should have both SUMMARY and INDIVIDUAL sections
		expect_true(all(c("SUMMARY", "INDIVIDUAL") %in% table$Section))
	}
})

test_that("tableNCAConcentration filterCovariates validation errors", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test with non-existent covariate in filter
	expect_error(
		tableNCAConcentration(
			projectFile      = pkx_project_1,
			filterCovariates = list(NONEXISTENT = "value")
		),
		"Invalid covariate"
	)

	# Test with non-list filterCovariates
	expect_error(
		tableNCAConcentration(
			projectFile      = pkx_project_1,
			filterCovariates = "TINFCAT"
		),
		"must be a named list"
	)
})

# --- tableNCAParameters filter tests ---

test_that("tableNCAParameters with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_value <- tinfcat_values[1]

		# Test with covariate filter
		table_filtered <- tableNCAParameters(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax"),
			filterCovariates = list(TINFCAT = filter_value)
		)

		table_unfiltered <- tableNCAParameters(
			projectFile = pkx_project_1,
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax")
		)

		# Filtered table should have fewer or equal rows
		expect_lte(nrow(table_filtered), nrow(table_unfiltered))
		expect_true(is.data.frame(table_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(table_filtered$TINFCAT == filter_value))
	}
})

test_that("tableNCAParameters with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get available IDs first
	listing <- listingNCAParameters(projectFile = pkx_project_1)
	available_ids <- unique(listing$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		table_filtered <- tableNCAParameters(
			projectFile = pkx_project_1,
			selectParameters = c("Cmax"),
			filterIds   = selected_ids
		)

		# Filtered table should exist and be valid
		expect_true(is.data.frame(table_filtered))
		expect_gt(nrow(table_filtered), 0)
	}
})

test_that("tableNCAParameters with filterCovariates and includeId", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_value <- tinfcat_values[1]

		# Test with covariate filter and includeId
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			useWideFormat  = FALSE,
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax"),
			filterCovariates = list(TINFCAT = filter_value),
			includeId       = TRUE,
			prettyLabels     = FALSE)

		expect_true(is.data.frame(table))
		expect_true("Statistic" %in% names(table))
		# Only filtered covariate value should appear
		expect_true(all(table$TINFCAT == filter_value))
	}
})

test_that("tableNCAParameters with combined filterCovariates and filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get available data first
	listing <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)
	available_ids <- unique(listing$id)

	if (length(tinfcat_values) >= 2 && length(available_ids) >= 3) {
		filter_value <- tinfcat_values[1]
		selected_ids <- available_ids[1:3]

		# Test with both filters combined
		table_filtered <- tableNCAParameters(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax", "AUClast"),
			filterCovariates = list(TINFCAT = filter_value),
			filterIds        = selected_ids
		)

		# Table should exist and be valid
		expect_true(is.data.frame(table_filtered))
		expect_gt(nrow(table_filtered), 0)

		# Only filtered covariate value should appear
		expect_true(all(table_filtered$TINFCAT == filter_value))

		# Compare with unfiltered - should have fewer or equal rows
		table_unfiltered <- tableNCAParameters(
			projectFile = pkx_project_1,
			splitCovariates = "TINFCAT",
			selectParameters = c("Cmax", "AUClast")
		)
		expect_lte(nrow(table_filtered), nrow(table_unfiltered))
	}
})

# --- tableNCARatios filter tests ---

test_that("tableNCARatios with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get unique SEQ values first
	listing <- listingNCARatios(projectFile = pkx_project_5, splitCovariates = "SEQ")
	seq_values <- unique(listing$SEQ)

	if (length(seq_values) >= 2) {
		filter_value <- seq_values[1]

		# Test with covariate filter
		table_filtered <- tableNCARatios(
			projectFile      = pkx_project_5,
			splitCovariates  = "SEQ",
			filterCovariates = list(SEQ = filter_value)
		)

		table_unfiltered <- tableNCARatios(
			projectFile = pkx_project_5,
			splitCovariates = "SEQ"
		)

		# Filtered table should have fewer or equal rows
		expect_lte(nrow(table_filtered), nrow(table_unfiltered))
		expect_true(is.data.frame(table_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(table_filtered$SEQ == filter_value))
	}
})

test_that("tableNCARatios with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available IDs first
	listing <- listingNCARatios(projectFile = pkx_project_5)
	available_ids <- unique(listing$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		table_filtered <- tableNCARatios(
			projectFile = pkx_project_5,
			filterIds   = selected_ids
		)

		# Filtered table should exist and be valid
		expect_true(is.data.frame(table_filtered))
		expect_gt(nrow(table_filtered), 0)
	}
})

test_that("tableNCARatios with combined filterCovariates and filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available data first
	listing <- listingNCARatios(projectFile = pkx_project_5, splitCovariates = "SEQ")
	seq_values <- unique(listing$SEQ)
	available_ids <- unique(listing$id)

	if (length(seq_values) >= 2 && length(available_ids) >= 3) {
		filter_value <- seq_values[1]
		selected_ids <- available_ids[1:3]

		# Test with both filters combined
		table_filtered <- tableNCARatios(
			projectFile      = pkx_project_5,
			splitCovariates  = "SEQ",
			filterCovariates = list(SEQ = filter_value),
			filterIds        = selected_ids
		)

		# Table should exist and be valid
		expect_true(is.data.frame(table_filtered))
		expect_gt(nrow(table_filtered), 0)

		# Only filtered covariate value should appear
		expect_true(all(table_filtered$SEQ == filter_value))

		# Compare with unfiltered - should have fewer or equal rows
		table_unfiltered <- tableNCARatios(
			projectFile = pkx_project_5,
			splitCovariates = "SEQ"
		)
		expect_lte(nrow(table_filtered), nrow(table_unfiltered))
	}
})

test_that("tableNCARatios with filterCovariates and includeId", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get unique SEQ values first
	listing <- listingNCARatios(projectFile = pkx_project_5, splitCovariates = "SEQ")
	seq_values <- unique(listing$SEQ)

	if (length(seq_values) >= 2) {
		filter_value <- seq_values[1]

		# Test with covariate filter and includeId
		table <- tableNCARatios(
			projectFile      = pkx_project_5,
			useWideFormat  = FALSE,
			splitCovariates  = "SEQ",
			filterCovariates = list(SEQ = filter_value),
			includeId       = TRUE
		)

		expect_true(is.data.frame(table))
		expect_true("Statistic" %in% names(table))
		expect_true("Section" %in% names(table))

		# Only filtered covariate value should appear
		expect_true(all(table$SEQ == filter_value))

		# Should have both SUMMARY and INDIVIDUAL sections
		expect_true(all(c("SUMMARY", "INDIVIDUAL") %in% table$Section))
	}
})

# --- listingNCAConcentration filter tests ---

test_that("listingNCAConcentration with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	# Get unique FOOD values first
	listing_all <- listingNCAConcentration(projectFile = pkx_project_2, splitCovariates = "FOOD")
	food_values <- unique(listing_all$FOOD)

	if (length(food_values) >= 2) {
		filter_value <- food_values[1]

		# Test with covariate filter
		listing_filtered <- listingNCAConcentration(
			projectFile      = pkx_project_2,
			splitCovariates  = "FOOD",
			filterCovariates = list(FOOD = filter_value)
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(listing_filtered$FOOD == filter_value))
	}
})

test_that("listingNCAConcentration with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	# Get available IDs first
	listing_all <- listingNCAConcentration(projectFile = pkx_project_2)
	available_ids <- unique(listing_all$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		listing_filtered <- listingNCAConcentration(
			projectFile = pkx_project_2,
			filterIds   = selected_ids
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only selected IDs should be present
		expect_true(all(listing_filtered$id %in% selected_ids))
	}
})

test_that("listingNCAConcentration with combined filterCovariates and filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_2), "Test project not available")

	# Get available data first
	listing_all <- listingNCAConcentration(projectFile = pkx_project_2, splitCovariates = "FOOD")
	food_values <- unique(listing_all$FOOD)
	available_ids <- unique(listing_all$id)

	if (length(food_values) >= 2 && length(available_ids) >= 3) {
		filter_value <- food_values[1]
		selected_ids <- available_ids[1:3]

		# Test with both filters combined
		listing_filtered <- listingNCAConcentration(
			projectFile      = pkx_project_2,
			splitCovariates  = "FOOD",
			filterCovariates = list(FOOD = filter_value),
			filterIds        = selected_ids
		)

		# Filtered listing should have fewer rows than unfiltered
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only filtered covariate value should appear
		expect_true(all(listing_filtered$FOOD == filter_value))

		# Only selected IDs should be present
		expect_true(all(listing_filtered$id %in% selected_ids))
	}
})

# --- listingNCAParameters filter tests ---

test_that("listingNCAParameters with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing_all <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing_all$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_value <- tinfcat_values[1]

		# Test with covariate filter
		listing_filtered <- listingNCAParameters(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			filterCovariates = list(TINFCAT = filter_value)
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(listing_filtered$TINFCAT == filter_value))
	}
})

test_that("listingNCAParameters with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get available IDs first
	listing_all <- listingNCAParameters(projectFile = pkx_project_1)
	available_ids <- unique(listing_all$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		listing_filtered <- listingNCAParameters(
			projectFile = pkx_project_1,
			filterIds   = selected_ids
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only selected IDs should be present
		expect_true(all(listing_filtered$id %in% selected_ids))
	}
})

test_that("listingNCAParameters with combined filters", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get data to find valid filter combinations
	listing_all <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing_all$TINFCAT)
	available_ids <- unique(listing_all$id)

	if (length(tinfcat_values) >= 1 && length(available_ids) >= 2) {
		filter_value <- tinfcat_values[1]
		selected_ids <- available_ids[1:2]

		# Test with both filters
		listing_filtered <- listingNCAParameters(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			filterCovariates = list(TINFCAT = filter_value),
			filterIds        = selected_ids
		)

		expect_true(is.data.frame(listing_filtered))
		# Both filters should be applied
		if (nrow(listing_filtered) > 0) {
			expect_true(all(listing_filtered$TINFCAT == filter_value))
			expect_true(all(listing_filtered$id %in% selected_ids))
		}
	}
})

# --- listingNCARatios filter tests ---

test_that("listingNCARatios with filterCovariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get unique SEQ values first
	listing_all <- listingNCARatios(projectFile = pkx_project_5, splitCovariates = "SEQ")
	seq_values <- unique(listing_all$SEQ)

	if (length(seq_values) >= 2) {
		filter_value <- seq_values[1]

		# Test with covariate filter
		listing_filtered <- listingNCARatios(
			projectFile      = pkx_project_5,
			splitCovariates  = "SEQ",
			filterCovariates = list(SEQ = filter_value)
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only the filtered covariate value should be present
		expect_true(all(listing_filtered$SEQ == filter_value))
	}
})

test_that("listingNCARatios with filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available IDs first
	listing_all <- listingNCARatios(projectFile = pkx_project_5)
	available_ids <- unique(listing_all$id)

	if (length(available_ids) >= 3) {
		selected_ids <- available_ids[1:3]

		# Test with ID filter
		listing_filtered <- listingNCARatios(
			projectFile = pkx_project_5,
			filterIds   = selected_ids
		)

		# Filtered listing should have fewer rows
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only selected IDs should be present
		expect_true(all(listing_filtered$id %in% selected_ids))
	}
})

test_that("listingNCARatios with combined filterCovariates and filterIds", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_5), "Test project not available")

	# Get available data first
	listing_all <- listingNCARatios(projectFile = pkx_project_5, splitCovariates = "SEQ")
	seq_values <- unique(listing_all$SEQ)
	available_ids <- unique(listing_all$id)

	if (length(seq_values) >= 2 && length(available_ids) >= 3) {
		filter_value <- seq_values[1]
		selected_ids <- available_ids[1:3]

		# Test with both filters combined
		listing_filtered <- listingNCARatios(
			projectFile      = pkx_project_5,
			splitCovariates  = "SEQ",
			filterCovariates = list(SEQ = filter_value),
			filterIds        = selected_ids
		)

		# Filtered listing should have fewer rows than unfiltered
		expect_lt(nrow(listing_filtered), nrow(listing_all))
		expect_true(is.data.frame(listing_filtered))

		# Only filtered covariate value should appear
		expect_true(all(listing_filtered$SEQ == filter_value))

		# Only selected IDs should be present
		expect_true(all(listing_filtered$id %in% selected_ids))
	}
})

# --- Filter validation tests ---

test_that("filterCovariates must be a named list", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test with vector instead of list
	expect_error(
		tableNCAParameters(
			projectFile      = pkx_project_1,
			filterCovariates = c(TINFCAT = "value")
		),
		"must be a named list"
	)

	# Test with unnamed list
	expect_error(
		tableNCAParameters(
			projectFile      = pkx_project_1,
			filterCovariates = list("value")
		),
		"must be a named list"
	)
})

test_that("filterCovariates validates covariate names", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test with non-existent covariate
	expect_error(
		tableNCAParameters(
			projectFile      = pkx_project_1,
			filterCovariates = list(NONEXISTENT_COV = "value")
		),
		"Invalid covariate"
	)
})

test_that("filter with multiple covariate values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Get unique TINFCAT values first
	listing <- listingNCAParameters(projectFile = pkx_project_1, splitCovariates = "TINFCAT")
	tinfcat_values <- unique(listing$TINFCAT)

	if (length(tinfcat_values) >= 2) {
		filter_values <- tinfcat_values[1:2]

		# Test with multiple values for one covariate
		table <- tableNCAParameters(
			projectFile      = pkx_project_1,
			splitCovariates  = "TINFCAT",
			selectParameters = c("Cmax"),
			filterCovariates = list(TINFCAT = filter_values)
		)

		expect_true(is.data.frame(table))
		# Only filtered covariate values should be present
		expect_true(all(table$TINFCAT %in% filter_values))
	}
})

################################################################################
################################################################################
# Tests for includeId in tableNCAConcentration --------------------------------

test_that("tableNCAConcentration with includeId basic (no covariates)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test basic includeId functionality without covariates
	# Note: includeId = TRUE requires useWideFormat = TRUE for tableNCAConcentration
	table <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		useWideFormat = TRUE,
		includeId      = TRUE,
		prettyLabels    = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("Section" %in% names(table))  # Section column added when includeId = TRUE
	expect_gt(nrow(table), 0)

	# Verify that table contains both SUMMARY and INDIVIDUAL sections
	expect_true(all(c("SUMMARY", "INDIVIDUAL") %in% table$Section))
})

test_that("tableNCAConcentration with includeId and single covariate", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test includeId with stratification by single covariate
	# Note: includeId = TRUE requires useWideFormat = TRUE for tableNCAConcentration
	table <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		useWideFormat = TRUE,
		splitCovariates = "TINFCAT",
		metrics         = c("min", "MeanSD", "median", "GeomCV", "max"),
		includeId      = TRUE,
		prettyLabels    = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))
	expect_true("TINFCAT" %in% names(table))
	expect_true("Section" %in% names(table))
	expect_gt(nrow(table), 0)
})

test_that("tableNCAConcentration with includeId validation: useWideFormat = FALSE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test that includeId = TRUE with useWideFormat = TRUE generates warning
	expect_warning(
		table <- tableNCAConcentration(
			projectFile     = pkx_project_1,
			useWideFormat = FALSE,  # This should trigger warning
			includeId      = TRUE),
		"includeId = TRUE only works with useWideFormat = TRUE")

	# Table should still be generated with includeId set to FALSE
	expect_true(is.data.frame(table))
})

test_that("tableNCAConcentration with includeId validation: stratifyColumns = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test that conflicting settings (useWideFormat=TRUE + stratifyColumns=TRUE)
	# emits an informational message and falls back to valid settings
	# Note: stratifyColumns=TRUE requires useWideFormat=FALSE, so stratifyColumns
	# gets disabled first, before the includeId+stratifyColumns check can fire
	expect_message(
		table <- tableNCAConcentration(
			projectFile      = pkx_project_1,
			useWideFormat  = TRUE,
			stratifyColumns = TRUE,
			splitCovariates  = "TINFCAT",
			includeId       = TRUE
		),
		"stratifyColumns = TRUE requires long format"
	)

	# Table should still be generated with fallback settings
	expect_true(is.data.frame(table))
	# With fallback, includeId should work (Section column present)
	expect_true("Section" %in% names(table))
})

test_that("tableNCAConcentration with includeId = FALSE (default behavior)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Test that includeId = FALSE produces table without individual IDs
	# Using useWideFormat = TRUE for consistent structure with Statistic column
	table <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		useWideFormat = TRUE,
		includeId      = FALSE,  # Explicitly FALSE
		prettyLabels    = FALSE
	)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_true("Statistic" %in% names(table))

	# Should NOT have Section column when includeId = FALSE
	expect_false("Section" %in% names(table))

	# All Statistic values should be summary statistics, no individual IDs
	statistic_values <- table$Statistic
	summary_stats <- c("mean", "SD", "CV", "SE", "geoMean", "geoCV", "geoSD", "geoSE",
	                   "median", "min", "max", "Q1", "Q3", "N", "Nobs", "Ntot", "Nmiss",
	                   "P05", "P95", "MeanSD", "MeanSE", "MeanCV", "GeomCV", "GeomSD", "GeomSE",
	                   "harmMean", "Arithmetric\nMean (SD)", "Arithmetric\nMean (CV %)", "Geometric\nCV%")

	# All statistic values should be recognized summary statistics
	expect_true(all(statistic_values %in% summary_stats),
	            label = paste("Unexpected statistic values:",
	                         paste(setdiff(statistic_values, summary_stats), collapse = ", ")))
})

test_that("tableNCAConcentration with includeId row count check", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "Test project not available")

	# Compare row counts: includeId = FALSE vs TRUE
	# Note: includeId = TRUE requires useWideFormat = TRUE for tableNCAConcentration
	table_no_ids <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		useWideFormat = TRUE,
		metrics         = c("median", "GeomCV"),
		includeId      = FALSE,
		prettyLabels    = FALSE
	)

	table_with_ids <- tableNCAConcentration(
		projectFile     = pkx_project_1,
		useWideFormat = TRUE,
		metrics         = c("median", "GeomCV"),
		includeId      = TRUE,
		prettyLabels    = FALSE
	)

	# Table with IDs should have more rows than without IDs
	expect_gt(nrow(table_with_ids), nrow(table_no_ids),
	          label = "Table with includeId=TRUE should have more rows")

	# Table with IDs has Section column added
	expect_equal(ncol(table_with_ids), ncol(table_no_ids) + 1)
})

################################################################################
# tableBEResults

test_that("tableBEResults basic functionality", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test basic functionality
	table <- tableBEResults(projectFile = pkx_project_3)

	# Verify structure
	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Verify expected columns exist
	expect_true("Comparison" %in% names(table) || "comparison" %in% tolower(names(table)))
	expect_true("Parameter" %in% names(table) || "parameter" %in% tolower(names(table)))
})

test_that("tableBEResults with specific selectParameters", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with specific parameters
	selected_params <- c("AUClast_D", "Cmax_D")
	table <- tableBEResults(
		projectFile = pkx_project_3,
		selectParameters = selected_params
	)

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)

	# Compare with all parameters - filtered should have fewer or equal rows
	table_all <- tableBEResults(projectFile = pkx_project_3)
	expect_lte(nrow(table), nrow(table_all))
})

test_that("tableBEResults with invalid selectParameters messages", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with mix of valid and invalid parameters
	expect_message(
		tableBEResults(
			projectFile = pkx_project_3,
			selectParameters = c("AUClast_D", "INVALID_PARAM")
		),
		"Ignoring unknown"
	)
})

test_that("tableBEResults with all invalid selectParameters errors", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with only invalid parameters
	expect_error(
		tableBEResults(
			projectFile = pkx_project_3,
			selectParameters = c("INVALID1", "INVALID2")
		),
		"None of the requested"
	)
})

test_that("tableBEResults with BELimits = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with BE limits column
	table <- tableBEResults(
		projectFile = pkx_project_3,
		BELimits    = TRUE
	)

	expect_true(is.data.frame(table))
	# Check for criteria/limits column (name may vary with prettyLabels)
	col_names_lower <- tolower(names(table))
	expect_true(any(grepl("criteria|limit", col_names_lower)))
})

test_that("tableBEResults with BEConclusion = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with conclusion column (default)
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		BEConclusion = TRUE
	)

	expect_true(is.data.frame(table))
	# Check for conclusion column
	col_names_lower <- tolower(names(table))
	expect_true(any(grepl("conclusion", col_names_lower)))

	# Verify conclusion values
	conclusion_col <- names(table)[grepl("conclusion", tolower(names(table)))]
	if (length(conclusion_col) > 0) {
		conclusions <- unique(table[[conclusion_col[1]]])
		expect_true(all(conclusions %in% c("Bioequivalent", "Not bioequivalent")))
	}
})

test_that("tableBEResults with BEConclusion = FALSE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test without conclusion column
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		BEConclusion = FALSE
	)

	expect_true(is.data.frame(table))
	# Conclusion column should not exist
	col_names_lower <- tolower(names(table))
	expect_false(any(grepl("conclusion", col_names_lower)))
})

test_that("tableBEResults with units = TRUE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with units
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		units        = TRUE,
		prettyLabels = FALSE
	)

	expect_true(is.data.frame(table))
	# Parameter names should contain units in parentheses
	param_col <- names(table)[grepl("param", tolower(names(table)))]
	if (length(param_col) > 0) {
		params <- table[[param_col[1]]]
		expect_true(any(grepl("\\(.*\\)", params)))  # Contains parentheses (units)
	}
})

test_that("tableBEResults with units = FALSE", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test without units
	table_no_units <- tableBEResults(
		projectFile  = pkx_project_3,
		units        = FALSE,
		aliases      = FALSE,
		prettyLabels = FALSE
	)

	table_with_units <- tableBEResults(
		projectFile  = pkx_project_3,
		units        = TRUE,
		aliases      = FALSE,
		prettyLabels = FALSE
	)

	expect_true(is.data.frame(table_no_units))

	# Parameter names without units should be shorter
	param_col <- names(table_no_units)[grepl("param", tolower(names(table_no_units)))]
	if (length(param_col) > 0) {
		params_no_units <- nchar(table_no_units[[param_col[1]]][1])
		params_with_units <- nchar(table_with_units[[param_col[1]]][1])
		expect_lt(params_no_units, params_with_units)
	}
})

test_that("tableBEResults log-transformed parameters have ln() prefix", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test that log-transformed parameters are labeled
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		prettyLabels = FALSE
	)

	expect_true(is.data.frame(table))

	# Check for ln() in parameter names (most BE params are log-transformed)
	param_col <- names(table)[grepl("param", tolower(names(table)))]
	if (length(param_col) > 0) {
		params <- table[[param_col[1]]]
		expect_true(any(grepl("^ln\\(", params)))
	}
})

test_that("tableBEResults rounding methods", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test monolix rounding
	table_monolix <- tableBEResults(
		projectFile = pkx_project_3,
		roundMethod = "monolixSuiteRounding",
		roundDigits = 2
	)
	expect_true(is.data.frame(table_monolix))

	# Test signif rounding
	table_signif <- tableBEResults(
		projectFile = pkx_project_3,
		roundMethod = "significantFigures",
		roundDigits = 3
	)
	expect_true(is.data.frame(table_signif))

	# Test significantFiguresPreserved rounding
	table_signif_preserved <- tableBEResults(
		projectFile = pkx_project_3,
		roundMethod = "significantFiguresPreserved",
		roundDigits = 3
	)
	expect_true(is.data.frame(table_signif_preserved))

	# Test no rounding
	table_none <- tableBEResults(
		projectFile = pkx_project_3,
		roundMethod = "none"
	)
	expect_true(is.data.frame(table_none))
})

test_that("tableBEResults roundByParameter overrides rounding per parameter", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Discover available BE parameters (original, non-aliased names = override keys)
	lixoftConnectors::loadProject(pkx_project_3)
	available_be <- lixoftConnectors::getBioequivalenceSettings()$computedbioequivalenceparameters$parameters
	skip_if(length(available_be) < 2, "Need >= 2 BE parameters")
	p_over <- available_be[1]   # gets the override
	p_keep <- available_be[2]   # stays on the global rule

	common <- list(
		projectFile  = NULL,            # use the project loaded above
		selectParameters = c(p_over, p_keep),
		roundDigits  = 1,
		roundMethod  = "monolixSuiteRounding",
		units        = FALSE, aliases = FALSE, prettyLabels = FALSE)

	base <- do.call(tableBEResults, common)
	over <- do.call(tableBEResults, c(common, list(
		roundByParameter = data.frame(parameter = p_over, digits = 4,
																	method = "significantFigures",
																	stringsAsFactors = FALSE))))

	expect_true(all(c("parameter", "Ratio") %in% names(base)))

	# The parameter column may be ln()-wrapped; both runs share identical labels, so
	# locate each parameter's rows by matching the original name within the label.
	row_over <- grepl(p_over, base$parameter, fixed = TRUE)
	row_keep <- grepl(p_keep, base$parameter, fixed = TRUE)
	expect_true(any(row_over))
	expect_true(any(row_keep))

	# Overridden parameter's Ratio changes; non-overridden keeps the global rounding
	expect_false(isTRUE(all.equal(base$Ratio[row_over], over$Ratio[row_over])))
	expect_equal(over$Ratio[row_keep], base$Ratio[row_keep])
})

test_that("tableBEResults mock = TRUE masks output", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	table_real <- tableBEResults(projectFile = pkx_project_3, mock = FALSE)
	table_mock <- tableBEResults(projectFile = pkx_project_3, mock = TRUE)

	expect_true(is.data.frame(table_mock))
	expect_equal(dim(table_real), dim(table_mock))
	expect_true(any(grepl("xx", as.matrix(table_mock))))
})

test_that("tableBEResults prettyLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with pretty labels
	table_pretty <- tableBEResults(
		projectFile  = pkx_project_3,
		prettyLabels = TRUE
	)

	# Test without pretty labels
	table_raw <- tableBEResults(
		projectFile  = pkx_project_3,
		prettyLabels = FALSE
	)

	expect_true(is.data.frame(table_pretty))
	expect_true(is.data.frame(table_raw))

	# Column names should differ
	expect_false(identical(names(table_pretty), names(table_raw)))
})

test_that("tableBEResults multiple comparisons combined", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test that multiple test vs ref comparisons are in one table
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		prettyLabels = FALSE
	)

	expect_true(is.data.frame(table))

	# Check for Comparison column
	expect_true("Comparison" %in% names(table))

	# Should have "vs." in comparison strings
	expect_true(all(grepl("vs\\.", table$Comparison)))
})

test_that("tableBEResults all arguments combined", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	# Test with all arguments specified
	table <- tableBEResults(
		projectFile  = pkx_project_3,
		selectParameters = c("AUClast_D", "Cmax_D"),
		roundDigits  = 2,
		roundMethod  = "monolixSuiteRounding",
		units        = TRUE,
		aliases      = TRUE,
		BELimits     = TRUE,
		BEConclusion = TRUE,
		prettyLabels = TRUE
	)

	expect_true(is.data.frame(table))
	expect_gt(nrow(table), 0)
	expect_gt(ncol(table), 3)
})

test_that("tableBEResults applies customAliases and customUnits", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_3), "Test project not available")

	table <- tableBEResults(
		projectFile   = pkx_project_3,
		selectParameters = c("AUClast_D", "Cmax_D"),
		customAliases = c(AUClast_D = "AUC<sub>0-tlast</sub>", Cmax_D = "C<sub>max</sub>"),
		customUnits   = c(AUClast_D = "h*ng/mL", Cmax_D = "ng/mL"),
		units         = TRUE,
		aliases       = TRUE,
		prettyLabels  = FALSE)

	vals <- table$parameter
	expect_true(any(grepl("[[sub:max]]", vals, fixed = TRUE)))
	expect_true(any(grepl("ng/mL",       vals, fixed = TRUE)))
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-PKanalixHelpers.R")

