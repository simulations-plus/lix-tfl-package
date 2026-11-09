mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
sim_project = "test-projects/simulx/sim_forest.smlx"

# For code coverage (or for R CMD check to pass), need to comment out the following tests
# For unknown reasons, switching software causes the test to fail when run in certain contexts,
# even when all tests pass
#-----------------------------------------------------------------------------------------------

test_that("ensureProjectLoaded change software", {
	suppressMessages(.ensureProjectLoaded(mono_project_1))
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	# note can only get project for Monolix, not Simulx
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	suppressMessages(expect_message(.ensureProjectLoaded(sim_project), "Loading new project"))
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$directory),
							 normalizePath("test-projects/simulx/sim_forest"))

	# switch back to Monolix (avoid messages)
	suppressMessages(.ensureProjectLoaded(mono_project_1))

})

test_that("ensureProjectLoaded no project loaded", {
	# switch software to make sure no project loaded
	suppressMessages(lixoftConnectors::initializeLixoftConnectors(software = "simulx", force = TRUE))
	suppressMessages(lixoftConnectors::initializeLixoftConnectors(software = "monolix", force = TRUE))

	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	suppressMessages(expect_message(.ensureProjectLoaded(mono_project_1), "Loading new project"))
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
})

test_that("ensureProjectLoaded no project loaded, error to pass NULL", {
	# switch software to make sure no project loaded
	suppressMessages(lixoftConnectors::initializeLixoftConnectors(software = "simulx", force = TRUE))
	suppressMessages(lixoftConnectors::initializeLixoftConnectors(software = "monolix", force = TRUE))

	expect_error(.ensureProjectLoaded(NULL), "no project")
})

#---------------------------------------------------------------------------------------------------
# end of tests to comment out for code coverage

test_that("ensureProjectLoaded same project loaded", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	expect_message(.ensureProjectLoaded(mono_project_1), "project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(mono_project_1), "Project already loaded")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
})

test_that("ensureProjectLoaded different project loaded", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	expect_message(.ensureProjectLoaded(mono_project_1), "project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(mono_project_2), "Loading new project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_2))
})

test_that("ensureProjectLoaded NULL keeps same project loaded", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	expect_message(.ensureProjectLoaded(mono_project_1), "project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(NULL), "No project specified, using project results")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
})


test_that("ensureProjectLoaded bad input", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	expect_message(
		expect_message(.ensureProjectLoaded("fileDoesNotExist.mlxtran"), "ERROR"),
		"new project")
	expect_error(.ensureProjectLoaded("badExtension.mlx"), "Unknown file type")
	expect_error(.ensureProjectLoaded("noExtension"), "Unknown file type")
	expect_error(.ensureProjectLoaded(""), "Unknown file type")
	expect_error(.ensureProjectLoaded(123), "Unknown file type")
})

test_that("checkProjectFile basic", {
	expect_no_error(.checkProjectFile("testfile.mlxtran", "monolix"))
	expect_no_error(.checkProjectFile("testfile.pkx", "pkanalix"))
	expect_no_error(.checkProjectFile("testfile.mlxtran", c("pkanalix", "monolix")))

	expect_error(.checkProjectFile("wrongsoftware.mlxtran", "pkanalix"))
	expect_error(.checkProjectFile("badextension.mlx", "monolix"))
	expect_error(.checkProjectFile("noextension", "pkanalix"))
	expect_error(.checkProjectFile("somefile.mlxtran", "badsoftware"))
})

test_that("checkProjectFile NULL projectFile", {
	.ensureProjectLoaded(mono_project_1)
	expect_no_error(.checkProjectFile(NULL, "monolix"))
	expect_error(.checkProjectFile(NULL, "pkanalix"))
})


test_that("round_num method monolix", {
	expect_equal(.round_num(123.456, 2), 123.46)
	expect_equal(.round_num(123.456, 2, rounding_method = "monolixSuiteRounding"), 123.46)
	expect_equal(.round_num(0.123456, 2), 0.12)
	expect_equal(.round_num(0.00123456, 2), 0.0012)
	expect_equal(.round_num(123, 2), 123)
	expect_equal(.round_num(123.456789, 4), 123.4568)
	expect_equal(.round_num(-123.456, 2), -123.46)
})

test_that("round_num rounding_method signif", {
	expect_equal(.round_num(123.456, 2, rounding_method = "significantFigures"), 120)
	expect_equal(.round_num(0.123456, 2, rounding_method = "significantFigures"), 0.12)
	expect_equal(.round_num(0.00123456, 2, rounding_method = "significantFigures"), 0.0012)
	expect_equal(.round_num(123, 2, rounding_method = "significantFigures"), 120)
	expect_equal(.round_num(123.456789, 4, rounding_method = "significantFigures"), 123.5)
})

test_that("round_num rounding_method none", {
	expect_equal(.round_num(123.456, 2, rounding_method = "none"), 123.456)
	expect_equal(.round_num(0.123456, 0, rounding_method = "none"), 0.123456)
	expect_equal(.round_num(1/3, NA, rounding_method = "none"), 1/3)
	expect_equal(.round_num(123, NULL, rounding_method = "none"), 123)
})

test_that("round_num vector", {
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "monolixSuiteRounding"),
							 c(123.46, 0.0012))
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "significantFigures"),
							 c(120, 0.0012))
})

test_that("round_num data.frame", {
	df = data.frame(a = c(123.45678, 0.00123456),
									b = c(-3.4567, -0.0001234),
									c = c(TRUE, FALSE),
									d = c("1.2345", "2.3456"))

	# character columns that parse as numeric are coerced and rounded (unless excluded)
	expect_equal(.round_num(df, 3, rounding_method = "monolixSuiteRounding"),
							 data.frame(a = c(123.457, 0.00123),
							 					 b = c(-3.457, -0.000123),
							 					 c = c(TRUE, FALSE),
							 					 d = c(1.234, 2.346)))
	expect_equal(.round_num(df, 3, rounding_method = "significantFigures"),
							 data.frame(a = c(123, 0.00123),
							 					 b = c(-3.46, -0.000123),
							 					 c = c(TRUE, FALSE),
							 					 d = c(1.23, 2.35)))
	expect_equal(.round_num(df, 3, exclude = "b", rounding_method = "significantFigures"),
							 data.frame(a = c(123, 0.00123),
							 					 b = c(-3.4567, -0.0001234),
							 					 c = c(TRUE, FALSE),
							 					 d = c(1.23, 2.35)))
	expect_equal(.round_num(df, 3, exclude = c("a", "c", "d"), rounding_method = "significantFigures"),
							 data.frame(a = c(123.45678, 0.00123456),
							 					 b = c(-3.46, -0.000123),
							 					 c = c(TRUE, FALSE),
							 					 d = c("1.2345", "2.3456")))
})

test_that("round_num error", {
	expect_error(.round_num(123.456, "2"))
	expect_error(.round_num(0.123456, "two"))
	expect_error(.round_num(12.345, 2, rounding_method = "badmethod"))
	expect_equal(.round_num(123, NULL, rounding_method = "none"), 123)
})

test_that("round_num not number", {
	expect_equal(.round_num("123", 2), "123")
	expect_equal(.round_num("ddd", 2), "ddd")
	expect_equal(.round_num("dd23", 3, rounding_method = "none"), "dd23")
})

test_that("round_num keep NA/NaN as is", {
	expect_equal(.round_num(NA, 2), NA)
	expect_equal(.round_num(NaN, 2), NaN)
	expect_equal(.round_num(NA, 3, rounding_method = "significantFigures"), NA)
	expect_equal(.round_num(NaN, 3, rounding_method = "significantFigures"), NaN)
	expect_equal(.round_num(c(1.2345, NA, 1.2, NaN, -1.2345), 2), c(1.23, NA, 1.2, NaN, -1.23))
})

test_that("applyRounding with NULL overrides equals round_num", {
	df <- data.frame(a = c(1.23456, 2.34567), b = c(0.111111, 0.222222))
	expect_equal(
		.applyRounding(df, 2, "significantFigures"),
		.round_num(df, 2, rounding_method = "significantFigures"))
})

test_that("applyRounding column axis rounds overridden column with its own config", {
	df <- data.frame(Time = c(0, 1), mean = c(1.23456, 2.34567), SD = c(0.111111, 0.222222))
	ov <- data.frame(parameter = "mean", digits = 4, method = "significantFigures",
									 stringsAsFactors = FALSE)

	res <- .applyRounding(df, 2, "significantFigures", exclude = "Time",
												overrides = ov, paramAxis = "column")

	expect_equal(res$mean, c(1.235, 2.346))   # overridden: 4 sig figs
	expect_equal(res$SD,   c(0.11, 0.22))     # global: 2 sig figs
	expect_equal(res$Time, c(0, 1))           # excluded: untouched
})

test_that("applyRounding row axis rounds per parameter and preserves row order", {
	df <- data.frame(parameter = c("Cmax", "AUC", "Tmax"),
									 Value     = c(1.23456, 2.34567, 3.45678),
									 stringsAsFactors = FALSE)
	# override a non-first parameter to exercise the split/re-order path
	ov <- data.frame(parameter = "AUC", digits = 4, method = "significantFigures",
									 stringsAsFactors = FALSE)

	res <- .applyRounding(df, 1, "significantFigures", exclude = "parameter",
												overrides = ov, paramCol = "parameter", paramAxis = "row")

	expect_equal(res$parameter, c("Cmax", "AUC", "Tmax"))  # original order preserved
	expect_equal(res$Value, c(1, 2.346, 3))                # AUC: 4 sig figs, others: 1
})

test_that("applyRounding row axis supports multiple per-parameter overrides", {
	df <- data.frame(parameter = c("Cmax", "AUC", "Tmax"),
									 Value     = c(1.23456, 2.34567, 3.45678),
									 stringsAsFactors = FALSE)
	ov <- data.frame(parameter = c("Cmax", "Tmax"),
									 digits    = c(4, 2),
									 method    = c("significantFigures", "significantFigures"),
									 stringsAsFactors = FALSE)

	res <- .applyRounding(df, 1, "significantFigures", exclude = "parameter",
												overrides = ov, paramCol = "parameter", paramAxis = "row")

	expect_equal(res$Value, c(1.235, 2, 3.5))  # Cmax 4 sig, AUC global 1 sig, Tmax 2 sig
})

test_that("modelBuildingStepGroups separates rounds by (step, reference)", {
	# empty input
	expect_equal(.modelBuildingStepGroups(character(0), character(0)), integer(0))

	# base + two forward rounds against different references + a backward round;
	# same direction but different reference must start a new group
	step <- c("Base", "Forward", "Forward", "Forward", "Forward", "Backward")
	ref  <- c("",     "Model 1", "Model 1", "Model 4", "Model 4", "Model 4")
	expect_equal(.modelBuildingStepGroups(step, ref), c(1, 2, 2, 3, 3, 4))

	# interleaved forward/backward (COSSAC-style): every row is its own round
	step2 <- c("Base", "Backward", "Forward", "Backward")
	ref2  <- c("", "Model 1", "Model 2", "Model 3")
	expect_equal(.modelBuildingStepGroups(step2, ref2), c(1, 2, 3, 4))

	# single group when step and reference never change
	expect_equal(.modelBuildingStepGroups(rep("Forward", 3), rep("Model 1", 3)),
							 c(1, 1, 1))
})

################################################################################
# .formatRounded() - display-layer formatter shared by every table/listing helper
################################################################################

test_that("formatRounded returns character and keeps the NA vs NaN distinction (all methods)", {
	# This is the global contract every helper relies on: NA -> NA_character_
	# (renders blank), NaN -> "NaN" (renders literally), never confused.
	for (m in c("monolixSuiteRounding", "significantFigures",
							"significantFiguresPreserved", "none")) {
		out <- .formatRounded(c(1.2, NA, NaN), digits = 3, method = m, trailingZeros = TRUE)
		expect_type(out, "character")
		expect_true(is.na(out[2]))        # genuine NA stays NA
		expect_false(is.nan(suppressWarnings(as.numeric(out[2]))) && !is.na(out[2]))
		expect_identical(out[2], NA_character_)
		expect_identical(out[3], "NaN")   # NaN is preserved as a literal string
	}
})

test_that("formatRounded trailingZeros pads vs strips but keeps the same value", {
	pad <- .formatRounded(c(1.2, 0.01), digits = 3, method = "monolixSuiteRounding",
												trailingZeros = TRUE)
	mn  <- .formatRounded(c(1.2, 0.01), digits = 3, method = "monolixSuiteRounding",
												trailingZeros = FALSE)
	expect_equal(pad, c("1.200", "0.0100"))
	expect_equal(mn,  c("1.2", "0.01"))
	expect_equal(as.numeric(pad), as.numeric(mn))
})

test_that("formatRounded monolixSuiteRounding is decimal-places for >=1, sig-figs for <1", {
	out <- .formatRounded(c(123.456, 0.012345), digits = 2,
												method = "monolixSuiteRounding", trailingZeros = TRUE)
	expect_equal(out, c("123.46", "0.012"))  # >=1: 2 decimals; <1: 2 significant figures
})

test_that("formatRounded significantFigures counts significant digits and re-derives precision after rounding", {
	out <- .formatRounded(c(123.456, 9.96, 0.012345), digits = 3,
												method = "significantFigures", trailingZeros = TRUE)
	expect_equal(out, c("123", "9.96", "0.0123"))
	# rounding that bumps the magnitude drops the now-spurious decimals (9.96 -> 10, not 10.0)
	expect_equal(.formatRounded(9.96, digits = 2, method = "significantFigures",
															trailingZeros = TRUE), "10")
})

test_that("formatRounded significantFiguresPreserved keeps integer digits (no decimals once int digits >= digits)", {
	out <- .formatRounded(c(123.456, 1234.4), digits = 3,
												method = "significantFiguresPreserved", trailingZeros = TRUE)
	expect_equal(out, c("123", "1234"))
})

test_that("formatRounded none passes values through without rounding", {
	out <- .formatRounded(c(1.23456, 0.01, NA, NaN), digits = 3, method = "none")
	expect_equal(out[1], "1.23456")   # untouched
	expect_identical(out[3], NA_character_)
	expect_identical(out[4], "NaN")
})

test_that("formatRounded leaves genuinely non-numeric input untouched", {
	expect_equal(.formatRounded(c("a", "b"), digits = 3, method = "monolixSuiteRounding"),
							 c("a", "b"))
})

test_that("formatRounded pads zero like a value of 1 and drops the sign of -0", {
	expect_equal(.formatRounded(c(0, 1), digits = 3, method = "monolixSuiteRounding", trailingZeros = TRUE),
							 c("0.000", "1.000"))
	expect_equal(.formatRounded(c(0, 1), digits = 3, method = "significantFigures", trailingZeros = TRUE),
							 c("0.00", "1.00"))
	expect_equal(.formatRounded(c(0, 1), digits = 3, method = "significantFiguresPreserved", trailingZeros = TRUE),
							 c("0.00", "1.00"))
	expect_equal(.formatRounded(0, digits = 3, method = "monolixSuiteRounding", trailingZeros = FALSE), "0")
	expect_equal(.formatRounded(-0.0001, digits = 3, method = "significantFiguresPreserved", trailingZeros = TRUE),
							 "-0.000100")
	expect_equal(.formatRounded(-0.0004, digits = 3, method = "monolixSuiteRounding", trailingZeros = TRUE),
							 "-0.000400")
	expect_false(any(grepl("^-0[.]?0*$", .formatRounded(c(-0, round(-0.0001, 3)), digits = 3,
																											method = "monolixSuiteRounding", trailingZeros = TRUE))))
})

test_that("formatRounded uses scientific notation for |x| >= 1e9 or < 1e-6", {
	x <- c(2.92175983644145e+53, -7.45977e16, 1.23456e9, 34752480.634, 0.0000156, 1.17e-8, 2.22044604925031e-16)
	expect_equal(.formatRounded(x, digits = 3, method = "monolixSuiteRounding", trailingZeros = TRUE),
							 c("2.92e+53", "-7.46e+16", "1.23e+09", "34752480.634", "0.0000156", "1.17e-08", "2.22e-16"))
	expect_equal(.formatRounded(x, digits = 3, method = "significantFigures", trailingZeros = TRUE),
							 c("2.92e+53", "-7.46e+16", "1.23e+09", "34800000", "0.0000156", "1.17e-08", "2.22e-16"))
	# minimal form strips trailing zeros of the mantissa only, never of the exponent
	expect_equal(.formatRounded(c(1.2e10, 1e10), digits = 3, method = "monolixSuiteRounding", trailingZeros = FALSE),
							 c("1.2e+10", "1e+10"))
	expect_equal(.formatRounded(c(1.2e10, 1e10), digits = 3, method = "monolixSuiteRounding", trailingZeros = TRUE),
							 c("1.20e+10", "1.00e+10"))
	# boundaries and non-finite values
	expect_equal(.formatRounded(c(999999999, 1e9), digits = 3, method = "significantFiguresPreserved", trailingZeros = TRUE),
							 c("999999999", "1.00e+09"))
	expect_equal(.formatRounded(c(Inf, NaN, NA), digits = 3, method = "monolixSuiteRounding", trailingZeros = TRUE),
							 c("Inf", "NaN", NA_character_))
	# "none" stays untouched
	expect_equal(.formatRounded(1234567890.5, digits = 3, method = "none"), "1234567890.5")
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-InternalHelpers.R")
