mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"

test_that("tableContinuousCovariateSummary default arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE)
	expect_equal(dim(table), c(3,9))
	expect_equal(table$Covariate, c("AGE", "HEIGHT", "WEIGHT"))
	expect_equal(table$Min, c(36, 165, 80))
})

test_that("tableCategoricalCovariateSummary default arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableCategoricalCovariateSummary(mono_project_1)
	expect_equal(dim(table), c(5,4))
	expect_contains(table$Covariate, c("DOSE", "SEX"))
	expect_contains(table$Category, c("150", "300", "800", "M", "F"))
	expect_equal(table$Frequency, c(6, 6, 6, 9, 9))
})

test_that("tableContinuousCovariateSummary extra aliases in df", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	cov = c("AGE", "HEIGHT", "WEIGHT")
	lab = c("Age", "Height", "Weight")
	table = tableContinuousCovariateSummary(mono_project_1,
																					customAliases = data.frame(covariate = c(cov, "EXTRA"), label = c(lab, "Extra")))
	expect_equal(table$Covariate, lab)
})

test_that("tableContinuousCovariateSummary on some with new aliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	cov = c("HEIGHT", "WEIGHT")
	lab = c("Height", "Weight")
	table = tableContinuousCovariateSummary(mono_project_1,
																					customAliases = data.frame(covariate = cov, label = lab))
	expect_equal(table$Covariate, c("AGE", lab))
})

test_that("tableContinuousCovariateSummary customAliases and customUnits combine as 'alias (unit)'", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableContinuousCovariateSummary(mono_project_1,
																					customAliases = c(HEIGHT = "Height", WEIGHT = "Weight"),
																					customUnits   = c(HEIGHT = "cm", WEIGHT = "kg"))
	expect_equal(table$Covariate, c("AGE", "Height (cm)", "Weight (kg)"))
})

test_that("tableContinuousCovariateSummary customUnits alone appends unit without alias", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableContinuousCovariateSummary(mono_project_1,
																					customUnits = c(HEIGHT = "cm", WEIGHT = "kg"))
	expect_equal(table$Covariate, c("AGE", "HEIGHT (cm)", "WEIGHT (kg)"))
})

test_that("tableCategoricalCovariateSummary label df", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	cov = c("DOSE", "SEX")
	lab = c("Dose (mg)", "Sex")
	catcov = c("DOSE", "DOSE", "DOSE", "SEX", "SEX")
	catcat = c("150", "300", "800", "M", "F")
	catlab = c("150 mg", "300 mg", "800 mg", "Male", "Female")
	table = tableCategoricalCovariateSummary(mono_project_1,
																					 data.frame(covariate = cov, label = lab),
																					 data.frame(covariate = catcov, category = catcat, label = catlab))
	expect_equal(dim(table), c(5,4))
	expect_contains(table$Covariate, lab)
	expect_equal(table$Category, catlab)
})

test_that("tableContinuousCovariateSummary digits and rounding arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableContinuousCovariateSummary(mono_project_1, digits = 3, roundMethod = "significantFigures")
	expect_equal(dim(table), c(3,9))
	expect_equal(table$Mean, c(50.3, 180, 93.6))

	table = tableContinuousCovariateSummary(mono_project_1, digits = 1, roundMethod = "monolixSuiteRounding")
	expect_equal(dim(table), c(3,9))
	expect_equal(table$Mean, c(50.3, 180.2, 93.6))
})

test_that("tableCategoricalCovariateSummary digits arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableCategoricalCovariateSummary(mono_project_1, digits = 4)
	expect_equal(table$Percentage, c(0.3333, 0.3333, 0.3333, 0.5, 0.5))

	table = tableCategoricalCovariateSummary(mono_project_1, digits = 3)
	expect_equal(table$Percentage, c(0.333, 0.333, 0.333, 0.5, 0.5))
})

test_that("tableContinuousCovariateSummary non-stratified relabels the split covariate column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# stratifyColumns = FALSE (default): the split covariate appears as a literal
	# column; splitCovariateLabels renames it (raw name gone, label present).
	table = tableContinuousCovariateSummary(mono_project_1,
																					splitCovariates      = "SEX",
																					splitCovariateLabels = "Sex",
																					prettyLabels         = FALSE)
	expect_true("Sex" %in% names(table))
	expect_false("SEX" %in% names(table))
})

test_that("tableCategoricalCovariateSummary non-stratified relabels the split covariate column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# stratifyColumns = FALSE (default): SEX becomes a left-side grouping column
	# that splitCovariateLabels renames to "Sex".
	table = tableCategoricalCovariateSummary(mono_project_1,
																					 splitCovariates      = "SEX",
																					 splitCovariateLabels = "Sex")
	expect_true("Sex" %in% names(table))
	expect_false("SEX" %in% names(table))
})

test_that("listingDosingRegimen renames the time and amount columns to Time/Amount", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# The dataset time/amount headers (TIME/AMT) are always renamed to Time/Amount.
	listing = listingDosingRegimen(mono_project_1)
	expect_s3_class(listing, "data.frame")
	expect_true(all(c("Time", "Amount") %in% names(listing)))
	expect_false(any(c("TIME", "AMT") %in% names(listing)))
})

test_that("listingDosingRegimen relabels split covariate columns with splitCovariateLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	listing = listingDosingRegimen(mono_project_1,
																 splitCovariates      = "SEX",
																 splitCovariateLabels = "Sex")
	expect_true("Sex" %in% names(listing))
	expect_false("SEX" %in% names(listing))
	# Time/Amount rename still applies alongside the covariate relabel.
	expect_true(all(c("Time", "Amount") %in% names(listing)))
})
