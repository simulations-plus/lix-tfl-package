# Direct unit tests for the pure helpers in R/InternalHelpers_PKanalix.R.
# Synthetic inputs only; no project is loaded (Tier A/B of the test plan).

################################################################################
# Batch 1 — string/label/unit formatting, simple validation
################################################################################

test_that(".formatUnits moves ^-1 terms to the denominator", {
	expect_equal(.formatUnits("h.ng.mL^-1"), "h*ng/mL")
	expect_equal(.formatUnits("ng.mL^-1"),   "ng/mL")
	expect_equal(.formatUnits("mL^-1"),      "1/mL")
	expect_equal(.formatUnits("h"),          "h")
	expect_true(is.na(.formatUnits(NA)))
	expect_equal(.formatUnits(""), "")
})

test_that(".convertHtmlAliasesForFlextable converts <sub> and strips other tags", {
	df  <- data.frame(aliases = c("C<sub>max</sub>", "<b>AUC</b>", "plain"),
										stringsAsFactors = FALSE)
	out <- .convertHtmlAliasesForFlextable(df)
	expect_equal(unname(out$aliases), c("C[[sub:max]]", "AUC", "plain"))
})

test_that(".normalizeAliasMapping accepts named vectors and 2-column data.frames", {
	expect_equal(.normalizeAliasMapping(c(Cmax = "Cmax label", AUC = "AUC label")),
							 data.frame(parameter = c("Cmax", "AUC"),
													aliases   = c("Cmax label", "AUC label"),
													stringsAsFactors = FALSE))
	# value_col controls the second column name
	expect_equal(names(.normalizeAliasMapping(c(Cmax = "ng/mL"), value_col = "units")),
							 c("parameter", "units"))
	# data.frame: first two columns taken and renamed
	expect_equal(.normalizeAliasMapping(data.frame(p = "Cmax", v = "lab", extra = "z")),
							 data.frame(parameter = "Cmax", aliases = "lab", stringsAsFactors = FALSE))
	expect_null(.normalizeAliasMapping(NULL))
	expect_error(.normalizeAliasMapping(data.frame(p = "Cmax")), "at least 2 columns")
	expect_error(.normalizeAliasMapping("Cmax"), "named vector or a 2-column dataframe")
})

test_that(".combinedStats builds the paired display metrics", {
	df  <- data.frame(mean = "5.0", SD = "1.0", CV = "20.0",
										geoMean = "4.8", geoCV = "18.0", geoSD = "1.1",
										stringsAsFactors = FALSE)
	out <- .combinedStats(df)
	expect_equal(out$MeanSD, "5.0 (1.0)")
	expect_equal(out$MeanCV, "5.0 (20.0)")
	expect_equal(out$GeomCV, "4.8 (18.0)")
	expect_equal(out$GeomSD, "4.8 (1.1)")
})

test_that(".isCensoredFlag recognizes string labels and numeric flags", {
	expect_true(.isCensoredFlag("leftCensored"))
	expect_true(.isCensoredFlag("rightCensored"))
	expect_false(.isCensoredFlag("notCensored"))
	expect_true(.isCensoredFlag(1))
	expect_true(.isCensoredFlag("1"))
	expect_false(.isCensoredFlag(0))
	expect_false(.isCensoredFlag(""))
	expect_false(.isCensoredFlag(NA))
})

test_that(".groupableExclusionColumns drops structural columns and respects the listing", {
	hdr <- data.frame(
		col  = c("ID", "TIME", "AMT", "SEX", "WT", "DV", "MDV"),
		type = c("id", "time", "amount", "catcov", "contcov", "observation", "mdv"),
		stringsAsFactors = FALSE)
	expect_equal(.groupableExclusionColumns(hdr), c("SEX", "WT"))
	# restricted to columns present in the listing
	expect_equal(.groupableExclusionColumns(hdr, listing = data.frame(SEX = 1)), "SEX")
})

test_that(".warnUnsuitableGrouping warns only above the ratio threshold", {
	# 4 distinct values / 4 subjects = 1.0 > 0.5
	d <- data.frame(id = 1:4, g = c("a", "b", "c", "d"))
	expect_warning(.warnUnsuitableGrouping(d, "g", id_col = "id"), "might not be a suitable")
	# 2 distinct / 4 subjects = 0.5, not above the default threshold
	d2 <- data.frame(id = 1:4, g = c("a", "a", "b", "b"))
	expect_no_warning(.warnUnsuitableGrouping(d2, "g", id_col = "id"))
	# no-ops
	expect_no_warning(.warnUnsuitableGrouping(d, character(0), id_col = "id"))
	expect_no_warning(.warnUnsuitableGrouping(d, "g", id_col = "nope"))
})

test_that(".validateAndSelectMetrics handles NULL, subsets, unknowns and empties", {
	avail <- c("Covariate", "mean", "SD")
	expect_message(m <- .validateAndSelectMetrics(avail, NULL, descriptors = "Covariate"),
								 "No metrics specified")
	expect_setequal(m, c("mean", "SD"))

	expect_equal(.validateAndSelectMetrics(avail, "mean", descriptors = "Covariate"), "mean")

	expect_message(v <- .validateAndSelectMetrics(avail, c("mean", "bogus"), descriptors = "Covariate"),
								 "Ignoring unknown metrics")
	expect_equal(v, "mean")

	expect_error(.validateAndSelectMetrics(avail, c("x", "y"), descriptors = "Covariate"),
							 "None of the requested metrics")
})

test_that(".validateCovariateLabels fills, truncates and passes through", {
	expect_null(.validateCovariateLabels(NULL, NULL))
	expect_equal(.validateCovariateLabels(c("A", "B"), NULL), c("A", "B"))
	expect_equal(.validateCovariateLabels(c("A", "B"), c("LabA", "LabB")), c("LabA", "LabB"))
	# fewer labels -> fill remaining with covariate names
	expect_equal(.validateCovariateLabels(c("A", "B", "C"), "LabA"), c("LabA", "B", "C"))
	# more labels -> truncate + warn
	expect_warning(r <- .validateCovariateLabels("A", c("LabA", "LabB")), "Extra labels ignored")
	expect_equal(r, "LabA")
})

test_that(".urineNCAConcLabel maps urine concentration columns", {
	expect_equal(.urineNCAConcLabel("ActualTime"),    "Interval Midpoint")
	expect_equal(.urineNCAConcLabel("NominalTime"),   "Nominal Interval Midpoint")
	expect_equal(.urineNCAConcLabel("Concentration"), "Excretion Rate")
	expect_equal(.urineNCAConcLabel("UsedConc"),      "Used Excretion Rate")
	expect_null(.urineNCAConcLabel("Unknown"))
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-InternalHelpers_PKanalix.R")
