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
# Batch 2 — rounding overrides + filters
################################################################################

test_that(".normalizeRoundOverrides accepts a named numeric vector (digits only)", {
	out <- .normalizeRoundOverrides(c(Cmax = 2, AUClast = 4),
																	globalDigits = 3, globalMethod = "significantFigures")
	expect_equal(out$parameter, c("Cmax", "AUClast"))
	expect_equal(out$digits,    c(2, 4))
	expect_equal(out$method,    c("significantFigures", "significantFigures"))  # filled from global
})

test_that(".normalizeRoundOverrides accepts a data.frame and fills missing digits/method", {
	full <- .normalizeRoundOverrides(
		data.frame(parameter = c("Cmax", "AUClast"), digits = c(1, 2),
							 method = c("none", "monolixSuiteRounding"), stringsAsFactors = FALSE),
		globalDigits = 3, globalMethod = "significantFigures")
	expect_equal(full$digits, c(1, 2))
	expect_equal(full$method, c("none", "monolixSuiteRounding"))

	# missing method filled from global
	expect_equal(.normalizeRoundOverrides(data.frame(parameter = "Cmax", digits = 1),
																				3, "none")$method, "none")
	# missing digits filled from global
	expect_equal(.normalizeRoundOverrides(data.frame(parameter = "Cmax", method = "none"),
																				3, "monolixSuiteRounding")$digits, 3)
	# first column treated as parameter when not named "parameter"
	expect_equal(.normalizeRoundOverrides(data.frame(x = "Cmax", digits = 2), 3, "none")$parameter, "Cmax")
})

test_that(".normalizeRoundOverrides de-duplicates by parameter (last wins) and drops empty names", {
	dedup <- .normalizeRoundOverrides(
		data.frame(parameter = c("Cmax", "Cmax"), digits = c(1, 2)), 3, "none")
	expect_equal(nrow(dedup), 1)
	expect_equal(dedup$digits, 2)

	expect_null(.normalizeRoundOverrides(data.frame(parameter = "", digits = 2), 3, "none"))
	expect_null(.normalizeRoundOverrides(NULL, 3, "none"))
})

test_that(".normalizeRoundOverrides rejects malformed input", {
	expect_error(.normalizeRoundOverrides(c(2, 4), 3, "none"), "must be fully named")
	expect_error(.normalizeRoundOverrides(c(Cmax = "x"), 3, "none"),
							 "named numeric vector or a data.frame")
	expect_error(.normalizeRoundOverrides(data.frame(parameter = "Cmax", digits = "abc"), 3, "none"),
							 "digits must be numeric")
	expect_error(.normalizeRoundOverrides(c(Cmax = -1), 3, "none"), "non-negative integers")
	expect_error(.normalizeRoundOverrides(c(Cmax = 1.5), 3, "none"), "non-negative integers")
	expect_error(.normalizeRoundOverrides(data.frame(parameter = "Cmax", digits = 2, method = "bogus"),
																				3, "none"), "Invalid rounding method")
})

test_that(".validateRoundOverrides drops unknown parameters with a message", {
	ov <- data.frame(parameter = c("Cmax", "Bogus"), digits = c(2, 2),
									 method = c("none", "none"), stringsAsFactors = FALSE)
	expect_message(out <- .validateRoundOverrides(ov, available = c("Cmax", "AUClast")),
								 "ignoring unknown parameter")
	expect_equal(out$parameter, "Cmax")

	# all unknown -> NULL
	expect_message(none <- .validateRoundOverrides(
		data.frame(parameter = "X", digits = 2, method = "none"), available = "Cmax"), "ignoring unknown")
	expect_null(none)

	# NULL / empty -> NULL
	expect_null(.validateRoundOverrides(NULL, available = "Cmax"))

	# all valid -> passthrough, no message
	ok <- data.frame(parameter = "Cmax", digits = 2, method = "none", stringsAsFactors = FALSE)
	expect_no_message(out2 <- .validateRoundOverrides(ok, available = c("Cmax", "AUClast")))
	expect_equal(out2$parameter, "Cmax")
})

test_that(".applyFilters subsets by ids and covariates and warns on edge cases", {
	d <- data.frame(id = c(1, 2, 3), x = c("a", "b", "c"), stringsAsFactors = FALSE)

	expect_identical(.applyFilters(d), d)                      # no filters -> unchanged
	expect_equal(.applyFilters(d, filterIds = c(1, 3))$id, c(1, 3))
	expect_equal(.applyFilters(d, filterIds = c("1", "2"))$id, c(1, 2))   # id coercion
	expect_equal(.applyFilters(d, filterCovariates = list(x = c("a", "b")))$x, c("a", "b"))
	# combined: id in {2,3} AND x in {a,b} -> id 2
	expect_equal(.applyFilters(d, filterCovariates = list(x = c("a", "b")), filterIds = c(2, 3))$id, 2)

	expect_warning(.applyFilters(d, filterIds = 999), "empty dataset")
	expect_warning(.applyFilters(data.frame(y = 1:3), filterIds = 1), "ID column 'id' not found")
	expect_warning(.applyFilters(d, filterCovariates = list(nope = "z")), "not found in data")
})

test_that(".applyCovariateColumnLabels renames only present, differing columns", {
	d <- data.frame(SEX = 1, DOSE = 2, x = 3)

	expect_identical(.applyCovariateColumnLabels(d, NULL, "Sex"), d)
	expect_identical(.applyCovariateColumnLabels(d, "SEX", NULL), d)

	one <- .applyCovariateColumnLabels(d, "SEX", "Sex")
	expect_true("Sex" %in% names(one))
	expect_false("SEX" %in% names(one))

	# label equal to raw name -> no-op; column absent -> skipped
	expect_equal(names(.applyCovariateColumnLabels(d, "SEX", "SEX")), names(d))
	expect_equal(names(.applyCovariateColumnLabels(d, "NOPE", "Label")), names(d))

	multi <- .applyCovariateColumnLabels(d, c("SEX", "DOSE"), c("Sex", "Dose"))
	expect_true(all(c("Sex", "Dose") %in% names(multi)))
})

test_that(".fixNCAConcentrationsColumnSwap reverses covariate content and relabels", {
	df <- data.frame(DOSE = c("d1", "d2"), FOOD = c("f1", "f2"), VISIT = c("v1", "v2"),
									 id = c(1, 2), stringsAsFactors = FALSE)
	out <- .fixNCAConcentrationsColumnSwap(df, c("DOSE", "FOOD", "VISIT"))

	expect_equal(names(out), c("DOSE", "FOOD", "VISIT", "id"))   # covariates first, then rest
	# content reversed then relabeled: DOSE<-VISIT, VISIT<-DOSE; odd middle (FOOD) unchanged
	expect_equal(out$DOSE,  c("v1", "v2"))
	expect_equal(out$FOOD,  c("f1", "f2"))
	expect_equal(out$VISIT, c("d1", "d2"))
	expect_equal(out$id,    c(1, 2))

	# even count -> full reversal
	df2 <- data.frame(A = "a1", B = "b1", id = 1, stringsAsFactors = FALSE)
	out2 <- .fixNCAConcentrationsColumnSwap(df2, c("A", "B"))
	expect_equal(out2$A, "b1")
	expect_equal(out2$B, "a1")

	# <= 1 covariate -> no-op
	df1 <- data.frame(A = "a1", id = 1, stringsAsFactors = FALSE)
	expect_identical(.fixNCAConcentrationsColumnSwap(df1, "A"), df1)
	expect_identical(.fixNCAConcentrationsColumnSwap(df1, character(0)), df1)
})

################################################################################
# Batch 3 — frequencies, pivots, stratified tables, misc transforms
################################################################################

test_that(".calculateCategoricalFrequencies counts modalities per variable with its own N", {
	df <- data.frame(
		id       = c(1, 2, 3,  1, 2, 3),
		variable = rep(c("SEX", "RACE"), each = 3),
		value    = c("M", "F", "M",  "A", "A", "B"),
		stringsAsFactors = FALSE)
	out <- .calculateCategoricalFrequencies(df)

	expect_equal(names(out), c("variable", "Modalities", "Count", "Percentage", "N"))
	# N is per-variable (distinct subjects), not global
	expect_equal(unique(out$N[out$variable == "SEX"]),  3)
	expect_equal(unique(out$N[out$variable == "RACE"]), 3)
	# SEX: M=2 (66.67%), F=1 (33.33%)
	expect_equal(out$Count[out$variable == "SEX" & out$Modalities == "M"], 2)
	expect_equal(round(out$Percentage[out$variable == "SEX" & out$Modalities == "M"], 2), 66.67)
	# RACE: A=2, B=1
	expect_equal(out$Count[out$variable == "RACE" & out$Modalities == "A"], 2)
	expect_equal(out$Count[out$variable == "RACE" & out$Modalities == "B"], 1)
})

test_that(".format_frequency_table builds the 'N (%)' column and rounds percentages", {
	freq <- data.frame(variable = "SEX", Modalities = c("M", "F"),
										 Count = c(15, 10), Percentage = c(60, 40), N = c(25, 25),
										 stringsAsFactors = FALSE)
	out <- .format_frequency_table(freq, digits = 1, method = "monolixSuiteRounding",
																 exclude_cols = c("variable", "Modalities", "N"))

	expect_true("N (%)" %in% names(out))
	expect_false(any(c("Count", "Percentage") %in% names(out)))
	expect_equal(out$`N (%)`, c("15 (60.0)", "10 (40.0)"))

	# the method argument actually drives the percentage rounding:
	# 66.667% at 2 digits -> monolix 66.67 vs significantFigures 67 -> "67.00"
	freq2 <- data.frame(variable = "X", Modalities = "a", Count = 2, Percentage = 66.667, N = 3,
											stringsAsFactors = FALSE)
	mono <- .format_frequency_table(freq2, 2, "monolixSuiteRounding", c("variable", "Modalities", "N"))
	sig  <- .format_frequency_table(freq2, 2, "significantFigures",   c("variable", "Modalities", "N"))
	expect_equal(mono$`N (%)`, "2 (66.67)")
	expect_equal(sig$`N (%)`,  "2 (67.00)")
})

test_that(".calc_format_stratum composes frequency calc + formatting", {
	df  <- data.frame(id = 1:4, variable = "SEX", value = c("M", "M", "F", "F"),
										stringsAsFactors = FALSE)
	out <- .calc_format_stratum(df, digits = 2, method = "monolixSuiteRounding",
															exclude_cols = c("variable", "Modalities", "N"))
	expect_true("N (%)" %in% names(out))
	expect_equal(out$`N (%)`[out$Modalities == "M"], "2 (50.00)")
})

test_that(".pivotToVertical transposes metrics to rows keyed by Statistic", {
	ss  <- data.frame(Covariate = c("AGE", "WEIGHT"), mean = c("50", "80"),
										SD = c("8", "10"), stringsAsFactors = FALSE)
	out <- .pivotToVertical(ss, param_col = "Covariate")

	expect_equal(names(out), c("Statistic", "AGE", "WEIGHT"))
	expect_equal(as.character(out$Statistic), c("mean", "SD"))   # input column order kept
	expect_equal(out$AGE[out$Statistic == "mean"],    "50")
	expect_equal(out$WEIGHT[out$Statistic == "SD"],   "10")

	# with splitCovariates: the split column is preserved as a key, one block per stratum
	ss2 <- data.frame(Covariate = c("AGE", "AGE"), SEX = c("0", "1"),
										mean = c("50", "60"), SD = c("5", "6"), stringsAsFactors = FALSE)
	sp  <- .pivotToVertical(ss2, param_col = "Covariate", splitCovariates = "SEX")
	expect_true(all(c("SEX", "Statistic", "AGE") %in% names(sp)))
	expect_equal(sp$AGE[sp$SEX == "0" & sp$Statistic == "mean"], "50")
	expect_equal(sp$AGE[sp$SEX == "1" & sp$Statistic == "SD"],   "6")
})

test_that(".buildStratifiedTable builds per-stratum + Overall columns", {
	summary_stats <- data.frame(
		Covariate = c("AGE", "AGE"), SEX = c("0", "1"), N = c(5, 7),
		mean = c("50", "60"), SD = c("5", "6"), stringsAsFactors = FALSE)
	overall_stats <- data.frame(
		Covariate = "AGE", N = 12, mean = "56", SD = "5.5", stringsAsFactors = FALSE)

	# multiple metrics -> Statistic column + one column per stratum + Overall
	multi <- .buildStratifiedTable(summary_stats, overall_stats, splitCovariates = "SEX",
		selected_metrics = c("mean", "SD"), param_col = "Covariate")
	expect_true("Statistic" %in% names(multi))
	expect_true(any(grepl("SEX=0", names(multi))))
	expect_true(any(grepl("SEX=1", names(multi))))
	sex0 <- grep("SEX=0", names(multi), value = TRUE)
	ovl  <- grep("Overall", names(multi), value = TRUE)
	expect_equal(multi[[sex0]][multi$Statistic == "mean"], "50")
	expect_equal(multi[[ovl]][multi$Statistic == "SD"],    "5.5")

	# single metric -> no Statistic column; stratum columns hold the metric
	single <- .buildStratifiedTable(summary_stats, overall_stats, splitCovariates = "SEX",
		selected_metrics = "mean", param_col = "Covariate")
	expect_false("Statistic" %in% names(single))
	expect_equal(single[[grep("SEX=0", names(single), value = TRUE)]], "50")

	# no splitCovariates -> single Overall column
	ss_only <- data.frame(Covariate = c("AGE", "WEIGHT"), N = c(10, 10), Nobs = c(10, 10),
												mean = c("50", "80"), SD = c("5", "8"), stringsAsFactors = FALSE)
	none <- .buildStratifiedTable(ss_only, overall_stats = NULL, splitCovariates = NULL,
		selected_metrics = c("mean", "SD"), param_col = "Covariate")
	expect_true("Statistic" %in% names(none))
	expect_length(grep("Overall", names(none)), 1)
})

test_that(".createCompositeId leaves unique data alone and builds ids from varying occasions", {
	# no duplicates -> unchanged
	d <- data.frame(id = 1:3, OCC = 1, Parameter = "Cmax", Value = 1:3, stringsAsFactors = FALSE)
	res <- .createCompositeId(d, splitCovariates = NULL, occ_vars = "OCC", value_group_col = "Parameter")
	expect_false(res$composite_id_created)
	expect_identical(res$data, d)

	# duplicates caused by a varying occasion -> composite id "id-OCC"
	d2 <- data.frame(id = c(1, 1, 2, 2), OCC = c(1, 2, 1, 2), Parameter = "Cmax",
									 Value = 1:4, stringsAsFactors = FALSE)
	res2 <- suppressMessages(.createCompositeId(d2, splitCovariates = NULL, occ_vars = "OCC",
																							value_group_col = "Parameter"))
	expect_true(res2$composite_id_created)
	expect_equal(res2$occasion_cols, "OCC")
	expect_equal(res2$data$composite_id, c("1-1", "1-2", "2-1", "2-2"))

	# duplicates but no occasion column -> warn, keep original id
	d3 <- data.frame(id = c(1, 1), Parameter = "Cmax", Value = 1:2, stringsAsFactors = FALSE)
	expect_warning(res3 <- .createCompositeId(d3, splitCovariates = NULL, occ_vars = character(0),
																						value_group_col = "Parameter"), "no occasion columns")
	expect_false(res3$composite_id_created)
})

test_that(".filterByTimeDeviation drops rows over the threshold and keeps t=0", {
	d <- data.frame(id = c(1, 1, 1, 2),
									ActualTime  = c(0, 1.0, 2.5, 1.05),
									NominalTime = c(0, 1,   2,   1),
									stringsAsFactors = FALSE)

	# 2.5 vs 2 = 25% deviation -> excluded; t=0 always kept
	out <- suppressMessages(.filterByTimeDeviation(d, occs = character(0), threshold = 10))
	expect_equal(nrow(out), 3)
	expect_false(2.5 %in% out$ActualTime)
	expect_true(attr(out, "timeDeviationFiltered"))
	expect_equal(attr(out, "timeDeviationThreshold"), 10)
	expect_equal(attr(out, "timeDeviationExcluded")$.pct_deviation, 25)

	# nothing over a high threshold
	out2 <- .filterByTimeDeviation(d, occs = character(0), threshold = 50)
	expect_false(attr(out2, "timeDeviationFiltered"))
	expect_equal(nrow(out2), 4)
})

test_that(".applyLogTransformLabel wraps only log-transformed parameters", {
	df <- data.frame(Original_Parameter = c("AUClast", "Cmax", "Tmax"),
									 parameter = c("AUClast", "Cmax", "Tmax"), stringsAsFactors = FALSE)
	out <- .applyLogTransformLabel(df, c(AUClast = TRUE, Cmax = FALSE, Tmax = TRUE))

	expect_equal(out$parameter, c("ln(AUClast)", "Cmax", "ln(Tmax)"))
	expect_equal(out$Original_Parameter, c("AUClast", "Cmax", "Tmax"))   # retained
	# parameter absent from the map is left unchanged
	expect_equal(.applyLogTransformLabel(
		data.frame(Original_Parameter = "X", parameter = "X lab", stringsAsFactors = FALSE),
		c(AUClast = TRUE))$parameter, "X lab")
})

test_that(".isCensoredObservation flags kept-censored rows only", {
	mr  <- data.frame(id = 1, time = 1, DV = 0.5, CENS = 1, stringsAsFactors = FALSE)
	iObs <- which(names(mr) == "DV")

	expect_true(.isCensoredObservation("BLQ", mr, iObs, censCol = "CENS"))   # tag + active flag + numeric interp
	expect_false(.isCensoredObservation("BLQ", mr, iObs, censCol = NULL))    # no censoring column
	expect_false(.isCensoredObservation("BLQ", mr, iObs, censCol = "NOPE"))
	expect_false(.isCensoredObservation("0.5", mr, iObs, censCol = "CENS"))  # numeric raw -> genuine mismatch

	mr0 <- data.frame(id = 1, time = 1, DV = 0.5, CENS = 0, stringsAsFactors = FALSE)
	expect_false(.isCensoredObservation("BLQ", mr0, iObs, censCol = "CENS")) # flag inactive
})

test_that(".getDelim detects the delimiter yielding the most columns", {
	f_comma <- withr::local_tempfile(fileext = ".csv")
	writeLines(c("a,b,c", "1,2,3"), f_comma)
	expect_equal(.getDelim(f_comma), ",")

	f_semi <- withr::local_tempfile(fileext = ".csv")
	writeLines(c("a;b;c", "1;2;3"), f_semi)
	expect_equal(.getDelim(f_semi), ";")

	f_tab <- withr::local_tempfile(fileext = ".txt")
	writeLines(c("a\tb\tc", "1\t2\t3"), f_tab)
	expect_equal(.getDelim(f_tab), "\t")

	# single column -> message + NULL
	f_one <- withr::local_tempfile(fileext = ".txt")
	writeLines(c("a", "1", "2"), f_one)
	expect_message(res <- .getDelim(f_one), "Cannot read file")
	expect_null(res)
})

################################################################################
# Batch 4 — mock / masking family
################################################################################

test_that(".mockOutputData masks numeric tokens at the given width and protects excludeCols", {
	df <- data.frame(
		Covariate = c("AGE", "WEIGHT"),
		mean      = c("50.33", "93.61"),
		MeanSD    = c("50.33 (8.20)", "93.61 (12.3)"),
		stringsAsFactors = FALSE)

	out <- .mockOutputData(df, digits = 2, excludeCols = "Covariate")
	expect_equal(out$Covariate, c("AGE", "WEIGHT"))                         # protected
	expect_equal(out$mean,   c("xx.xx", "xx.xx"))                           # numbers masked
	expect_equal(out$MeanSD, c("xx.xx (xx.xx)", "xx.xx (xx.xx)"))           # delimiters kept

	# mask width follows `digits`
	out0 <- .mockOutputData(df, digits = 0, excludeCols = "Covariate")
	expect_equal(out0$mean, c("xx", "xx"))
})

test_that(".mockOutputData relabels modality columns and fills incomplete cells", {
	# modality column -> sequential "Category k"; value column masked
	dfm <- data.frame(SEX = c("M", "F", "M"), mean = c("1.0", "2.0", "3.0"),
										stringsAsFactors = FALSE)
	out <- .mockOutputData(dfm, digits = 1, modalityCols = list(SEX = NA_character_))
	expect_equal(out$SEX, c("Category 1", "Category 2", "Category 1"))
	expect_equal(out$mean, c("xx.x", "xx.x", "xx.x"))

	# an NA cell is filled from the column's representative masked pattern (no NA leaks)
	dfna <- data.frame(grp = c("a", "b"), val = c("50 (8)", NA), stringsAsFactors = FALSE)
	outna <- .mockOutputData(dfna, digits = 2, excludeCols = "grp", naFillAxis = "column")
	expect_equal(outna$val, c("xx.xx (xx.xx)", "xx.xx (xx.xx)"))
})

test_that(".mockStratHeaders masks the N count and relabels modality values in headers", {
	df <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
		`SEX=M\n(N=15)` = 1, `SEX=F\n(N=10)` = 2, Overall = 3)
	out <- .mockStratHeaders(df)

	expect_true(any(grepl("N=xx", names(out), fixed = TRUE)))
	expect_false(any(grepl("N=15|N=10", names(out))))
	expect_true(any(grepl("SEX=Category 1", names(out), fixed = TRUE)))
	expect_true(any(grepl("SEX=Category 2", names(out), fixed = TRUE)))
	expect_true("Overall" %in% names(out))   # no (N=) suffix -> untouched
})

test_that(".mockDataListing masks each column according to its header type", {
	df <- data.frame(
		ID   = c(1, 1, 2),
		SEX  = c("M", "M", "F"),
		OCC  = c(1, 2, 1),
		DV   = c(0.5, 1.2, 3.4),
		DATE = c("03/15/2024", "03/16/2024", "04/01/2024"),
		AMT  = c(".", ".", "100"),
		stringsAsFactors = FALSE)
	colTypes <- c(ID = "id", SEX = "catcov", OCC = "occ", DV = "observation",
								DATE = "date", AMT = "amount")
	out <- .mockDataListing(df, colTypes)

	expect_equal(out$ID,   c("id1", "id1", "id2"))                # one label per subject
	expect_equal(out$SEX,  c("Category 1", "Category 1", "Category 2"))
	expect_equal(out$OCC,  c("occ1", "occ2", "occ1"))
	expect_equal(out$DV,   c("xx.xx", "xx.xx", "xx.xx"))
	expect_equal(out$DATE, c("xx/xx/xxxx", "xx/xx/xxxx", "xx/xx/xxxx"))
	expect_equal(out$AMT,  c(".", ".", "xx.xx"))                  # missing markers kept
})

test_that(".mockDataListing maskUntagged masks free-text columns with no digits", {
	df <- data.frame(ID = 1, REASON = "ignore", stringsAsFactors = FALSE)
	# untyped column defaults to numeric mask -> digits-only leaves "ignore" alone
	expect_equal(.mockDataListing(df, colTypes = c(ID = "id"))$REASON, "ignore")
	# maskUntagged replaces the whole value
	expect_equal(.mockDataListing(df, colTypes = c(ID = "id"), maskUntagged = TRUE)$REASON, "xx.xx")
})

test_that(".mockIndividualSection relabels only individual-row ids", {
	df <- data.frame(
		Section   = c("SUMMARY", "SUMMARY", "INDIVIDUAL", "INDIVIDUAL"),
		Statistic = c("mean", "SD", "Subject-1", "Subject-2"),
		Value     = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE)
	out <- .mockIndividualSection(df)

	expect_equal(out$Statistic[out$Section == "SUMMARY"],    c("mean", "SD"))   # metric names kept
	expect_equal(out$Statistic[out$Section == "INDIVIDUAL"], c("id1", "id2"))   # ids relabeled

	# no-op when the section/statistic columns are absent
	expect_identical(.mockIndividualSection(data.frame(x = 1)), data.frame(x = 1))
})

################################################################################
# Batch 5 — Tier B: connector helpers tested via injected cov_info / units_df
################################################################################

# synthetic getCovariateInformation() result: WT varies within id 1 (occasion-
# varying), SEX is constant per subject, OCC is the occasion index column.
.fake_cov_info <- function() {
	list(
		name = c("WT", "SEX"),
		covariate = data.frame(
			id  = c(1, 1, 2, 2),
			OCC = c(1, 2, 1, 2),
			WT  = c(70, 72, 80, 80),
			SEX = c("M", "M", "F", "F"),
			stringsAsFactors = FALSE))
}

test_that(".getOccasionInvariantCovariates splits invariant vs occasion-varying", {
	ci <- .fake_cov_info()

	res <- .getOccasionInvariantCovariates(ci)
	expect_equal(res$invariant, "SEX")
	expect_equal(res$varying,   "WT")

	# adding the occasion to the grouping makes WT invariant too
	res2 <- .getOccasionInvariantCovariates(ci, group_by = "OCC")
	expect_setequal(res2$invariant, c("WT", "SEX"))
	expect_length(res2$varying, 0)

	# covariates filter restricts the candidates considered
	res3 <- .getOccasionInvariantCovariates(ci, covariates = "WT")
	expect_equal(res3$varying, "WT")
	expect_length(res3$invariant, 0)
})

test_that(".getCovariateOccasionDependency reports the resolving occasion per varying covariate", {
	ci  <- .fake_cov_info()
	dep <- .getCovariateOccasionDependency(ci)

	expect_equal(names(dep), "WT")   # only the varying covariate is reported
	expect_equal(dep$WT, "OCC")      # adding OCC makes WT constant
})

test_that(".assertOccasionInvariantForRatio errors only on occasion-varying covariates", {
	ci <- .fake_cov_info()

	expect_null(.assertOccasionInvariantForRatio("SEX", cov_info = ci))
	expect_null(.assertOccasionInvariantForRatio(NULL,  cov_info = ci))
	expect_error(.assertOccasionInvariantForRatio("WT", cov_info = ci),
							 "Occasion-varying covariate")
})

test_that(".addUnitsOnly appends formatted units and drops the original-name column", {
	df <- data.frame(Original_Parameter = c("AUClast", "Cmax", "Tmax"),
									 parameter          = c("AUClast", "Cmax", "Tmax"),
									 stringsAsFactors = FALSE)
	units_df <- data.frame(parameter = c("AUClast", "Cmax"),
												 units     = c("h.ng.mL^-1", "ng.mL^-1"), stringsAsFactors = FALSE)

	out <- .addUnitsOnly(df, units_df = units_df)
	expect_false("Original_Parameter" %in% names(out))
	# units formatted via .formatUnits; parameter without a unit is left bare
	expect_equal(out$parameter, c("AUClast (h*ng/mL)", "Cmax (ng/mL)", "Tmax"))
})

test_that(".addUnitsAliases combines aliases (HTML-converted) and units into display names", {
	df <- data.frame(parameter = c("AUClast", "Cmax", "Tmax"),
									 Value     = c("10", "20", "30"), stringsAsFactors = FALSE)
	units_df   <- data.frame(parameter = c("AUClast", "Cmax"),
													 units     = c("h.ng.mL^-1", "ng.mL^-1"), stringsAsFactors = FALSE)
	aliases_df <- data.frame(parameter = c("AUClast", "Tmax"),
													 aliases   = c("AUC<sub>last</sub>", "T<sub>max</sub>"),
													 stringsAsFactors = FALSE)

	out <- .addUnitsAliases(df, units_df = units_df, aliases_df = aliases_df)
	# alias+unit / unit-only / alias-only; <sub> converted to [[sub:..]]
	expect_equal(out$parameter,
							 c("AUC[[sub:last]] (h*ng/mL)", "Cmax (ng/mL)", "T[[sub:max]]"))
	expect_equal(out$Value, c("10", "20", "30"))   # other columns + order preserved

	# add_units = FALSE suppresses the unit suffix
	out2 <- .addUnitsAliases(df, add_units = FALSE, aliases_df = aliases_df)
	expect_equal(out2$parameter, c("AUC[[sub:last]]", "Cmax", "T[[sub:max]]"))
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-InternalHelpers_PKanalix.R")
