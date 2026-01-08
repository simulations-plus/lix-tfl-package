mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"

test_that("tableContinuousCovariateSummary default arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table = tableContinuousCovariateSummary(mono_project_1)
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

test_that("tableContinuousCovariateSummary label df", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	cov = c("AGE", "HEIGHT", "WEIGHT")
	lab = c("Age (yrs)", "Height (cm)", "Weight (kg)")
	table = tableContinuousCovariateSummary(mono_project_1,
																					data.frame(covariate = c(cov, "EXTRA"), label = c(lab, "Extra")))
	expect_equal(dim(table), c(3,9))
	expect_equal(table$Covariate, lab)
})

test_that("tableContinuousCovariateSummary label df", {
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
	table = tableContinuousCovariateSummary(mono_project_1, digits = 3, roundingMethod = "signif")
	expect_equal(dim(table), c(3,9))
	expect_equal(table$Mean, c(50.3, 180, 93.6))

	table = tableContinuousCovariateSummary(mono_project_1, digits = 1, roundingMethod = "monolix")
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
