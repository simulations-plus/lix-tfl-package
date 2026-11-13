mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
mono_project_7 = "test-projects/monolix/warfarin_covariate3_project_COSSAC.mlxtran"
pkx_project_1  = "test-projects/pkanalix/project_remifentanil_excl.pkx"

# cell lookup by covariate name (robust to row order)
.covcell <- function(tb, cov, col) tb[[col]][tb$Covariate == cov]

# trailingZeros / character-output contract: both carry the same underlying
# numeric value; padded form is never shorter than the minimal one.
.expect_trailing_contract <- function(pad, minimal) {
	expect_type(pad, "character")
	expect_type(minimal, "character")
	expect_equal(suppressWarnings(as.numeric(pad)),
							 suppressWarnings(as.numeric(minimal)))
	expect_true(all(nchar(pad) >= nchar(minimal), na.rm = TRUE))
}

################################################################################
# tableContinuousCovariateSummary() — values are pinned at roundDigits = 2,
# monolixSuiteRounding, prettyLabels = FALSE (PK_run01: AGE/HEIGHT/WEIGHT, N = 18)
################################################################################

test_that("tableContinuousCovariateSummary default arguments: values are character display strings", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE)

	expect_equal(table$Covariate, c("AGE", "HEIGHT", "WEIGHT"))
	expect_type(table$mean, "character")
	expect_equal(.covcell(table, "AGE", "min"),    "36.00")
	expect_equal(.covcell(table, "AGE", "mean"),   "50.33")
	expect_equal(.covcell(table, "AGE", "SD"),     "8.20")
	expect_equal(.covcell(table, "AGE", "MeanSD"), "50.33 (8.20)")  # combined metric padded
	expect_equal(.covcell(table, "AGE", "N"),      "18")            # count stays integer
})

test_that("tableContinuousCovariateSummary trailingZeros toggles padding including combined metrics", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	pad <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE, trailingZeros = TRUE)
	mn  <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE, trailingZeros = FALSE)

	.expect_trailing_contract(pad$mean, mn$mean)
	.expect_trailing_contract(pad$SD,   mn$SD)
	# padded vs minimal, including inside the "mean (SD)" string
	expect_equal(.covcell(pad, "AGE", "SD"),     "8.20")
	expect_equal(.covcell(mn,  "AGE", "SD"),     "8.2")
	expect_equal(.covcell(pad, "AGE", "MeanSD"), "50.33 (8.20)")
	expect_equal(.covcell(mn,  "AGE", "MeanSD"), "50.33 (8.2)")
})

test_that("tableContinuousCovariateSummary roundDigits/roundMethod drive precision", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	mono <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")
	sig  <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		roundDigits = 3, roundMethod = "significantFigures")
	sigp <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		roundDigits = 3, roundMethod = "significantFiguresPreserved")

	# all values >= 1: monolix -> 3 decimals; sig-figs -> 3 significant figures
	expect_equal(mono$mean, c("50.333", "180.167", "93.611"))
	expect_equal(sig$mean,  c("50.3",   "180",     "93.6"))
	# sigFiguresPreserved matches sigFigures here (no integer part exceeds 3 digits)
	expect_equal(sigp$mean, c("50.3",   "180",     "93.6"))
})

test_that("tableContinuousCovariateSummary roundMethod = none passes values through unrounded", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	none <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE, roundMethod = "none")

	expect_type(none$mean, "character")
	expect_equal(as.numeric(.covcell(none, "AGE", "mean")), 50.33333, tolerance = 1e-4)
	expect_equal(.covcell(none, "AGE", "N"), "18")  # counts still integer
})

test_that("tableContinuousCovariateSummary metrics selects (only) the requested columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	m <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		metrics = c("mean", "SD"))

	expect_setequal(names(m), c("Covariate", "mean", "SD"))
	expect_equal(.covcell(m, "AGE", "mean"), "50.33")
	expect_equal(.covcell(m, "AGE", "SD"),   "8.20")
})

test_that("tableContinuousCovariateSummary long format transposes metrics to rows", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	lng <- tableContinuousCovariateSummary(mono_project_1, useWideFormat = FALSE, prettyLabels = FALSE)

	expect_true("Statistic" %in% names(lng))
	expect_true(all(c("AGE", "HEIGHT", "WEIGHT") %in% names(lng)))
	expect_equal(lng$AGE[lng$Statistic == "mean"], "50.33")
	expect_equal(lng$AGE[lng$Statistic == "N"],    "18")
})

test_that("tableContinuousCovariateSummary splitCovariates computes per-group stats (warfarin: wt by sex)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	split <- tableContinuousCovariateSummary(mono_project_7, splitCovariates = "sex", prettyLabels = FALSE)

	expect_true("sex" %in% names(split))
	expect_equal(split$mean[split$sex == 0], "51.34")
	expect_equal(split$mean[split$sex == 1], "73.46")
	expect_equal(split$N[split$sex == 0],    "5")
	expect_equal(split$N[split$sex == 1],    "27")
})

test_that("tableContinuousCovariateSummary stratifyColumns builds per-stratum + Overall columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# stratifyColumns requires long format; this path formats summary + overall stats
	strat <- tableContinuousCovariateSummary(mono_project_7, splitCovariates = "sex",
		stratifyColumns = TRUE, useWideFormat = FALSE)

	expect_true("Statistic" %in% names(strat))
	sex0_col    <- grep("sex=0",   names(strat), value = TRUE)
	sex1_col    <- grep("sex=1",   names(strat), value = TRUE)
	overall_col <- grep("Overall", names(strat), value = TRUE)
	expect_length(sex0_col, 1)
	expect_length(sex1_col, 1)
	expect_length(overall_col, 1)

	mean_row <- strat[strat$Statistic == "Mean", ]
	expect_equal(mean_row[[sex0_col]],    "51.34")
	expect_equal(mean_row[[sex1_col]],    "73.46")
	expect_equal(mean_row[[overall_col]], "70.00")

	n_row <- strat[strat$Statistic == "N", ]
	expect_equal(n_row[[overall_col]], "32")  # count stays integer in the Overall column
})

test_that("tableContinuousCovariateSummary filterCovariates restricts the computed population", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# PK_run01 has 9 male / 9 female subjects; filtering to males -> N = 9 everywhere
	filt <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		filterCovariates = list(SEX = "M"))
	expect_equal(as.numeric(filt$N), c(9, 9, 9))
})

test_that("tableContinuousCovariateSummary mock masks values but keeps structure", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE)
	mock <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_equal(mock$Covariate, real$Covariate)
	expect_true(all(grepl("x", mock$mean)))  # values masked
})

test_that("tableContinuousCovariateSummary prettyLabels only renames headers; values unchanged", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	raw    <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE)
	pretty <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = TRUE)

	expect_equal(dim(raw), dim(pretty))
	expect_false(identical(names(raw), names(pretty)))
	expect_equal(unname(as.list(raw)), unname(as.list(pretty)))
})

test_that("tableContinuousCovariateSummary works on a PKanalix project", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "pkx project not available")
	tbl <- tableContinuousCovariateSummary(pkx_project_1, prettyLabels = FALSE)

	expect_s3_class(tbl, "data.frame")
	expect_true("mean" %in% names(tbl))
	expect_type(tbl$mean, "character")
	expect_false(any(grepl(".", tbl$N, fixed = TRUE)))  # counts are integers, no decimal point
})

test_that("tableContinuousCovariateSummary rejects an invalid roundMethod", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableContinuousCovariateSummary(mono_project_1, roundMethod = "bogus"))
})

# ---- custom aliases / units (Covariate column is unaffected by rounding) ------

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

################################################################################
# tableCategoricalCovariateSummary() — combined schema: columns
# "Subject characteristics", "Modalities", "Frequency\nN (%)", where the
# frequency value is "Count (Percentage)" with Percentage sprintf-padded to
# roundDigits. No prettyLabels argument. (PK_run01: DOSE 150/300/800, SEX M/F)
################################################################################

.freq_col <- "Frequency\nN (%)"

test_that("tableCategoricalCovariateSummary default arguments", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	table <- tableCategoricalCovariateSummary(mono_project_1)

	expect_equal(dim(table), c(5, 3))
	expect_equal(names(table), c("Subject characteristics", "Modalities", .freq_col))
	expect_equal(table$`Subject characteristics`, c("DOSE", "DOSE", "DOSE", "SEX", "SEX"))
	expect_equal(table$Modalities, c("150", "300", "800", "F", "M"))
	# integer count + percentage padded to roundDigits (default 2); no % sign in the value
	expect_equal(table[[.freq_col]],
							 c("6 (33.33)", "6 (33.33)", "6 (33.33)", "9 (50.00)", "9 (50.00)"))
})

test_that("tableCategoricalCovariateSummary roundDigits controls percentage precision", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	d4 <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 4)
	d1 <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 1)

	expect_equal(d4[[.freq_col]][d4$Modalities == "150"], "6 (33.3333)")
	expect_equal(d4[[.freq_col]][d4$Modalities == "F"],   "9 (50.0000)")
	expect_equal(d1[[.freq_col]][d1$Modalities == "150"], "6 (33.3)")
	expect_equal(d1[[.freq_col]][d1$Modalities == "F"],   "9 (50.0)")
})

test_that("tableCategoricalCovariateSummary roundMethod changes the percentage rounding", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	mono <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 2, roundMethod = "monolixSuiteRounding")
	sig  <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 2, roundMethod = "significantFigures")

	# 33.333%: monolix -> 33.33 (2 decimals); significantFigures -> 33 -> padded "33.00"
	expect_equal(mono[[.freq_col]][mono$Modalities == "150"], "6 (33.33)")
	expect_equal(sig[[.freq_col]][sig$Modalities == "150"],   "6 (33.00)")
})

test_that("tableCategoricalCovariateSummary covDisplayLabels and categoryDisplayLabels relabel rows", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	cov    = c("DOSE", "SEX")
	lab    = c("Dose (mg)", "Sex")
	catcov = c("DOSE", "DOSE", "DOSE", "SEX", "SEX")
	catcat = c("150", "300", "800", "F", "M")
	catlab = c("150 mg", "300 mg", "800 mg", "Female", "Male")
	table  = tableCategoricalCovariateSummary(mono_project_1,
		covDisplayLabels      = data.frame(covariate = cov, label = lab),
		categoryDisplayLabels = data.frame(covariate = catcov, category = catcat, label = catlab))

	expect_equal(dim(table), c(5, 3))
	expect_equal(table$`Subject characteristics`,
							 c("Dose (mg)", "Dose (mg)", "Dose (mg)", "Sex", "Sex"))
	expect_equal(table$Modalities, c("150 mg", "300 mg", "800 mg", "Female", "Male"))
})

test_that("tableCategoricalCovariateSummary splitCovariates adds the stratifier as a left column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# split by SEX -> DOSE summarized within each SEX; splitCovariateLabels renames the column
	table <- tableCategoricalCovariateSummary(mono_project_1,
		splitCovariates = "SEX", splitCovariateLabels = "Sex")

	expect_true("Sex" %in% names(table))
	expect_false("SEX" %in% names(table))
	expect_equal(unique(table$`Subject characteristics`), "DOSE")  # SEX is the splitter
})

test_that("tableCategoricalCovariateSummary stratifyColumns builds per-stratum + Overall columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	strat <- tableCategoricalCovariateSummary(mono_project_1,
		splitCovariates = "SEX", stratifyColumns = TRUE)

	expect_true("Subject characteristics" %in% names(strat))
	overall_col  <- grep("Overall", names(strat), value = TRUE)
	stratum_cols <- grep("SEX=",    names(strat), value = TRUE)
	expect_length(overall_col, 1)
	expect_true(length(stratum_cols) >= 1)
	# Overall ignores the stratifier -> DOSE 150 overall = "6 (33.33)"
	expect_equal(strat[[overall_col]][strat$Modalities == "150"], "6 (33.33)")
})

test_that("tableCategoricalCovariateSummary filterCovariates restricts the population", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# keep males only (9 subjects) -> SEX collapses to a single 'M' row at 100%
	filt <- tableCategoricalCovariateSummary(mono_project_1, filterCovariates = list(SEX = "M"))

	sex_rows <- filt[filt$`Subject characteristics` == "SEX", ]
	expect_equal(sex_rows$Modalities, "M")
	expect_equal(sex_rows[[.freq_col]], "9 (100.00)")
})

test_that("tableCategoricalCovariateSummary mock masks frequencies but keeps covariate names", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- tableCategoricalCovariateSummary(mono_project_1)
	mock <- tableCategoricalCovariateSummary(mono_project_1, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_equal(mock$`Subject characteristics`, real$`Subject characteristics`)  # kept
	expect_true(all(grepl("x", mock[[.freq_col]])))                                # masked
})

test_that("tableCategoricalCovariateSummary works on a PKanalix project", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	skip_if_not(file.exists(pkx_project_1), "pkx project not available")
	tbl <- tableCategoricalCovariateSummary(pkx_project_1)

	expect_s3_class(tbl, "data.frame")
	expect_true("Subject characteristics" %in% names(tbl))
	expect_true(.freq_col %in% names(tbl))
})

test_that("tableCategoricalCovariateSummary rejects an invalid roundMethod", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableCategoricalCovariateSummary(mono_project_1, roundMethod = "bogus"))
})

################################################################################
# listingDosingRegimen()
################################################################################

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

################################################################################
# listingExclusions() / tableExclusions() on Data Formatting projects
# (fixtures: Monolix 2024R1 demos, without results)
################################################################################
df_units_project   = "test-projects/monolix/data_formatting/DoseAndLOQ_byCategory.mlxtran"
df_doseocc_project = "test-projects/monolix/data_formatting/doseIntervals_as_Occ.mlxtran"

test_that(".readRawDataFile skips additional header lines (units row)", {
	units_file <- "test-projects/monolix/data_formatting/data/units_BLQ_tags_data.csv"
	raw <- .readRawDataFile(units_file, headerLines = c(1, 2))
	# 866 lines in the file = names row + units row + 864 data rows
	expect_equal(nrow(raw), 864)
	expect_equal(names(raw)[-1], c("TIME", "CONC", "AGE", "WT", "STUDY", "LLOQ"))
	expect_true(is.numeric(raw$TIME))
	expect_true(is.numeric(raw$LLOQ))

	# a single header line is read exactly as with read.csv()
	plain_file <- "test-projects/monolix/data_formatting/data/data_multidose.csv"
	expect_equal(.readRawDataFile(plain_file, headerLines = 1), utils::read.csv(plain_file))
})

test_that("listingExclusions/tableExclusions handle a units row under the header", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# used to fail with "non-numeric argument to binary operator"
	listing <- suppressMessages(listingExclusions(df_units_project))
	# every line is used by Monolix; BLQ tags are kept as censored, not excluded
	expect_null(listing)
	expect_null(suppressMessages(tableExclusions(df_units_project)))
})

test_that("listingExclusions handles occasions created from dose intervals", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# DOSE_OCC has no source column in the raw file; used to fail with
	# "replacement has length zero"
	listing <- suppressMessages(listingExclusions(df_doseocc_project))
	expect_null(listing)
})

################################################################################
# listingExclusions() data file formats: Excel sheet in use, SAS (.xpt/.sas7bdat)
# (fixtures: PKanalix 2024R1 demos, without results; the Excel file is the
#  project_excel_sheets demo data with the "doses" sheet moved first)
################################################################################
xlsx_sheets_project = "test-projects/pkanalix/excel_sheets/project_excel_sheets_doses_first.pkx"
xpt_project         = "test-projects/pkanalix/sas_formats/project_covariates_xpt.pkx"
sas_project         = "test-projects/pkanalix/sas_formats/project_covariates_sas7bdat.pkx"

test_that(".readRawDataFile reads the requested Excel sheet and SAS files", {
	skip_if_not_installed("readxl")
	skip_if_not_installed("haven")
	xlsx <- "test-projects/pkanalix/excel_sheets/data/data_sheets_doses_first.xlsx"
	expect_equal(names(.readRawDataFile(xlsx)), c("ID", "TIME", "AMT"))           # first sheet = doses
	conc <- .readRawDataFile(xlsx, sheet = "concentrations")
	expect_equal(names(conc), c("ID", "TIME", "DV", "SEX", "WEIGHT"))
	expect_equal(nrow(conc), 650)
	expect_equal(names(.readRawDataFile(xlsx, sheet = "")), c("ID", "TIME", "AMT"))  # "" = first sheet

	xpt <- .readRawDataFile("test-projects/pkanalix/sas_formats/data/data_covariates.xpt")
	sas <- .readRawDataFile("test-projects/pkanalix/sas_formats/data/data_covariates.sas7bdat")
	expect_s3_class(xpt, "data.frame")
	expect_equal(nrow(xpt), 700)
	expect_equal(dim(sas), dim(xpt))
	expect_false(any(vapply(xpt, function(x) inherits(x, "haven_labelled"), logical(1))))
})

test_that("listingExclusions reads the Excel sheet used by the project", {
	skip_if_not_installed("readxl")
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# used to read the first sheet ("doses") and report 50 dose rows as excluded
	expect_null(suppressMessages(listingExclusions(xlsx_sheets_project)))
})

test_that("listingExclusions/tableExclusions support SAS data files (.xpt, .sas7bdat)", {
	skip_if_not_installed("haven")
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# used to fail with "Only CSV, TXT, or Excel (XLSX/XLS) format is currently supported."
	expect_null(suppressMessages(listingExclusions(xpt_project)))
	expect_null(suppressMessages(listingExclusions(sas_project)))
	expect_null(suppressMessages(tableExclusions(xpt_project)))
})

################################################################################
# listingExclusions() / tableExclusions(): observations ignored through MDV = 1
# (pkx_project_1 = project_remifentanil_excl: 8 observations with a DV value and
#  MDV = 1, plus 65 dose rows with MDV = 1, which are not exclusions)
################################################################################
test_that("listingExclusions lists observations ignored through MDV = 1, not dose rows", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	listing <- suppressMessages(listingExclusions(pkx_project_1))
	# no listing at all after the LINE_NUMBER change (the ignored rows are still in
	# getInterpretedData() with DV blanked); dose rows were listed before that
	expect_s3_class(listing, "data.frame")
	expect_equal(nrow(listing), 8)
	expect_true(all(listing$MDV == 1))
	expect_true(all(trimws(as.character(listing$DV)) != "."))     # each row carries an observation
	expect_false(any(trimws(as.character(listing$AMT)) != "."))   # no dose rows
	expect_equal(sort(unique(listing$ID)), c(3, 4, 11))
	expect_true(any(listing$ID == 3 & abs(listing$TIME - 18.07) < 1e-8 & as.numeric(listing$DV) == 41.4))
})

test_that("tableExclusions summarizes the ignored observations", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	tbl <- suppressMessages(tableExclusions(pkx_project_1, prettyLabels = FALSE))
	expect_s3_class(tbl, "data.frame")
	expect_equal(nrow(tbl), 1)
	vals <- unlist(tbl[1, ])
	expect_true(any(as.numeric(vals) == 3, na.rm = TRUE))   # 3 subjects
	expect_true(any(as.numeric(vals) == 8, na.rm = TRUE))   # 8 observations
})
################################################################################
# Argument-coverage additions (Phase 2). .covcell / .expect_trailing_contract
# are defined at the top of this file.
################################################################################

# --- tableContinuousCovariateSummary() ---------------------------------------

test_that("tableContinuousCovariateSummary filterIds restricts the population", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# IDs 1-6 -> N = 6 for every covariate
	filt <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE, filterIds = 1:6)
	expect_equal(as.numeric(filt$N), c(6, 6, 6))
})

test_that("tableContinuousCovariateSummary multi-element splitCovariates groups by every combination", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	multi <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		splitCovariates = c("SEX", "DOSE"))

	expect_true(all(c("SEX", "DOSE") %in% names(multi)))
	# AGE split across all 6 SEX x DOSE cells; per-group N matches the raw data
	age  <- multi[multi$Covariate == "AGE", , drop = FALSE]
	keys <- paste(age$SEX, age$DOSE, sep = ":")
	expected <- c("F:150" = 2, "F:300" = 4, "F:800" = 3,
								"M:150" = 4, "M:300" = 2, "M:800" = 3)
	expect_equal(nrow(age), 6L)
	expect_setequal(keys, names(expected))
	expect_equal(setNames(as.numeric(age$N), keys)[names(expected)], expected)
})

test_that("tableContinuousCovariateSummary stratifyColumns + useWideFormat is ignored with a message", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_message(
		tbl <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
			splitCovariates = "SEX", stratifyColumns = TRUE, useWideFormat = TRUE),
		"requires long format")
	# falls back to wide non-stratified: SEX is a split column, no Statistic rows
	expect_false("Statistic" %in% names(tbl))
	expect_true("SEX" %in% names(tbl))
})

test_that("tableContinuousCovariateSummary splitCovariateLabels appear in stratified headers", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	strat <- tableContinuousCovariateSummary(mono_project_7, splitCovariates = "sex",
		splitCovariateLabels = "Sex", stratifyColumns = TRUE, useWideFormat = FALSE)

	expect_true(any(grepl("Sex=0", names(strat))))
	expect_true(any(grepl("Sex=1", names(strat))))
	expect_false(any(grepl("sex=", names(strat))))   # raw name replaced by label
})

test_that("tableContinuousCovariateSummary significantFiguresPreserved diverges from significantFigures", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# WEIGHT mean ~93.6 (2 integer digits), roundDigits = 1:
	#   significantFigures -> 90 ; significantFiguresPreserved -> 94
	sf  <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		roundDigits = 1, roundMethod = "significantFigures")
	sfp <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		roundDigits = 1, roundMethod = "significantFiguresPreserved")

	expect_equal(.covcell(sf,  "WEIGHT", "mean"), "90")
	expect_equal(.covcell(sfp, "WEIGHT", "mean"), "94")
})

test_that("tableContinuousCovariateSummary mock masks values when combined with split + filter", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		splitCovariates = "SEX", filterIds = 1:12)
	mock <- tableContinuousCovariateSummary(mono_project_1, prettyLabels = FALSE,
		splitCovariates = "SEX", filterIds = 1:12, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_equal(mock$Covariate, real$Covariate)   # covariate names kept
	expect_true(all(grepl("x", mock$mean)))        # values masked
})

# --- tableCategoricalCovariateSummary() --------------------------------------

test_that("tableCategoricalCovariateSummary filterIds restricts the population", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# IDs 1-6 are all DOSE 150 (4 M, 2 F)
	filt <- tableCategoricalCovariateSummary(mono_project_1, filterIds = 1:6)

	dose <- filt[filt$`Subject characteristics` == "DOSE", ]
	expect_equal(dose[[.freq_col]][dose$Modalities == "150"], "6 (100.00)")
	sex <- filt[filt$`Subject characteristics` == "SEX", ]
	expect_equal(sex[[.freq_col]][sex$Modalities == "M"], "4 (66.67)")
	expect_equal(sex[[.freq_col]][sex$Modalities == "F"], "2 (33.33)")
})

test_that("tableCategoricalCovariateSummary roundMethod none and significantFiguresPreserved", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# none: raw percentage, sprintf-padded to roundDigits
	none <- tableCategoricalCovariateSummary(mono_project_1, roundMethod = "none")
	expect_equal(none[[.freq_col]][none$Modalities == "150"], "6 (33.33)")

	# 33.33%, roundDigits = 1: sig-figs -> 30, preserved keeps integer part -> 33
	sig <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 1, roundMethod = "significantFigures")
	sfp <- tableCategoricalCovariateSummary(mono_project_1, roundDigits = 1, roundMethod = "significantFiguresPreserved")
	expect_equal(sig[[.freq_col]][sig$Modalities == "150"], "6 (30.0)")
	expect_equal(sfp[[.freq_col]][sfp$Modalities == "150"], "6 (33.0)")
})

test_that("tableCategoricalCovariateSummary validates covDisplayLabels / categoryDisplayLabels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableCategoricalCovariateSummary(mono_project_1, covDisplayLabels = c(DOSE = "Dose")),
							 "must be a dataframe")
	expect_error(tableCategoricalCovariateSummary(mono_project_1,
		covDisplayLabels = data.frame(cov = "DOSE", lab = "Dose")), "headers 'covariate' and 'label")
	expect_error(tableCategoricalCovariateSummary(mono_project_1, categoryDisplayLabels = "x"),
							 "must be a dataframe")
	expect_error(tableCategoricalCovariateSummary(mono_project_1,
		categoryDisplayLabels = data.frame(covariate = "DOSE", label = "x")), "headers 'covariate")
})

test_that("tableCategoricalCovariateSummary splitCovariateLabels appear in stratified headers", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	strat <- tableCategoricalCovariateSummary(mono_project_1, splitCovariates = "SEX",
		stratifyColumns = TRUE, splitCovariateLabels = "Sex")

	expect_true(any(grepl("Sex=", names(strat))))
	expect_false(any(grepl("SEX=", names(strat))))
})

# --- getDescriptiveStatsMetrics() --------------------------------------------

test_that("getDescriptiveStatsMetrics returns the full metric vocabulary in calculation order", {
	expect_equal(getDescriptiveStatsMetrics(),
		c("N", "Nobs", "min", "P05", "Q1", "mean", "SD", "SE", "CV", "median", "Q3",
			"P95", "max", "geoMean", "geoSD", "geoCV", "harmMean",
			"MeanSD", "MeanCV", "GeomCV", "GeomSD"))
})

# --- listingDosingRegimen() --------------------------------------------------

test_that("listingDosingRegimen filterIds keeps only the requested subjects", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	listing <- listingDosingRegimen(mono_project_1, filterIds = c(1, 2))
	expect_setequal(as.character(listing$id), c("1", "2"))
})

test_that("listingDosingRegimen filterCovariates restricts to matching subjects", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# SEX = M -> the 9 male subjects
	listing <- listingDosingRegimen(mono_project_1, filterCovariates = list(SEX = "M"))
	expect_setequal(as.character(listing$id), c("1", "3", "4", "5", "8", "10", "13", "15", "18"))
})

test_that("listingDosingRegimen mock masks ids and numeric columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- listingDosingRegimen(mono_project_1)
	mock <- listingDosingRegimen(mono_project_1, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_true(all(grepl("^id[0-9]+$", mock$id)))   # id -> "id1", ...
	expect_true(all(grepl("x", as.character(mock$Time))))
	expect_true(all(grepl("x", as.character(mock$Amount))))
})

test_that("listingDosingRegimen surfaces occasion descriptor columns from dose intervals", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# doseIntervals_as_Occ: DOSE_OCC tagged as occasion, AMT1 as amount
	listing <- suppressMessages(listingDosingRegimen(df_doseocc_project))
	expect_true("DOSE_OCC" %in% names(listing))
	expect_true(all(c("Time", "Amount") %in% names(listing)))
})

################################################################################
# PKanalix group (project_remifentanil_excl): load once, act via projectFile=NULL.
# 8 excluded observations across IDs 3, 4, 11 (TINFCAT 20, 20, 14; all SEX 1).
################################################################################
suppressMessages(lixoftConnectors::initializeLixoftConnectors(software = "pkanalix", force = TRUE))
suppressMessages(lixoftConnectors::loadProject(pkx_project_1))

test_that("tableExclusions splitCovariates groups the excluded subjects", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	tbl <- tableExclusions(projectFile = NULL, splitCovariates = "TINFCAT", prettyLabels = FALSE)

	expect_true("TINFCAT" %in% names(tbl))
	expect_equal(nrow(tbl), 2)
	expect_equal(tbl$nbSbj[tbl$TINFCAT == 20],      2)   # IDs 3, 4
	expect_equal(tbl$totalNbObs[tbl$TINFCAT == 20], 5)
	expect_equal(tbl$nbSbj[tbl$TINFCAT == 14],      1)   # ID 11
	expect_equal(tbl$totalNbObs[tbl$TINFCAT == 14], 3)
})

test_that("tableExclusions length-2 splitCovariates groups by every combination", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	tbl <- tableExclusions(projectFile = NULL, splitCovariates = c("SEX", "TINFCAT"), prettyLabels = FALSE)

	expect_true(all(c("SEX", "TINFCAT") %in% names(tbl)))
	expect_equal(nrow(tbl), 2)             # (1,20) and (1,14)
	expect_equal(sum(tbl$nbSbj), 3)
	expect_equal(sum(tbl$totalNbObs), 8)
})

test_that("tableExclusions splitCovariateLabels renames the grouping column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	tbl <- tableExclusions(projectFile = NULL, splitCovariates = "TINFCAT",
		splitCovariateLabels = "Infusion category", prettyLabels = FALSE)
	expect_true("Infusion category" %in% names(tbl))
	expect_false("TINFCAT" %in% names(tbl))
})

test_that("tableExclusions filterIds and filterCovariates restrict the summary", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# only ID 11 -> 1 subject, 3 obs
	f_ids <- tableExclusions(projectFile = NULL, filterIds = 11, prettyLabels = FALSE)
	expect_equal(f_ids$nbSbj, 1)
	expect_equal(f_ids$totalNbObs, 3)
	# TINFCAT = 20 -> IDs 3, 4 -> 2 subjects, 5 obs
	f_cov <- tableExclusions(projectFile = NULL, filterCovariates = list(TINFCAT = 20), prettyLabels = FALSE)
	expect_equal(f_cov$nbSbj, 2)
	expect_equal(f_cov$totalNbObs, 5)
})

test_that("tableExclusions useWideFormat = FALSE transposes stats to rows", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	lng <- tableExclusions(projectFile = NULL, useWideFormat = FALSE, prettyLabels = FALSE)

	expect_true(all(c("Statistic", "Value") %in% names(lng)))
	expect_equal(lng$Value[lng$Statistic == "nbSbj"],      3)
	expect_equal(lng$Value[lng$Statistic == "totalNbObs"], 8)
})

test_that("tableExclusions maxSubjectRatio flags near-unique grouping columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	# LBM is unique per subject (65/65) -> warns at the default ratio
	expect_warning(
		tableExclusions(projectFile = NULL, splitCovariates = "LBM", prettyLabels = FALSE),
		"might not be a suitable grouping")
	# a high threshold suppresses the warning
	expect_no_warning(
		tableExclusions(projectFile = NULL, splitCovariates = "LBM", maxSubjectRatio = 2, prettyLabels = FALSE))
})

test_that("tableExclusions mock masks the summary values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- tableExclusions(projectFile = NULL, prettyLabels = FALSE)
	mock <- tableExclusions(projectFile = NULL, prettyLabels = FALSE, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_true(all(grepl("x", as.character(unlist(mock[1, ], use.names = FALSE)))))
})

test_that("tableExclusions prettyLabels renames headers on the real table", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	pretty <- tableExclusions(projectFile = NULL, prettyLabels = TRUE)
	raw    <- tableExclusions(projectFile = NULL, prettyLabels = FALSE)

	expect_equal(dim(pretty), dim(raw))
	expect_true("Nb. of Subjects" %in% names(pretty))
	expect_false("nbSbj" %in% names(pretty))
})

test_that("tableExclusions rejects an unknown grouping column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableExclusions(projectFile = NULL, splitCovariates = "NOPE"),
							 "Grouping column\\(s\\) not found")
})

test_that("listingExclusions mock masks values by column type", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	real <- suppressMessages(listingExclusions(projectFile = NULL))
	mock <- suppressMessages(listingExclusions(projectFile = NULL, mock = TRUE))

	expect_equal(dim(real), dim(mock))
	expect_true(all(grepl("^id[0-9]+$", as.character(mock$ID))))   # id -> "id1", ...
	expect_true(all(grepl("x", as.character(mock$DV))))            # numeric -> "xx.xx"
})

test_that("listingDosingRegimen surfaces the rate descriptor on a PKanalix project", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	listing <- listingDosingRegimen(projectFile = NULL)   # remifentanil: RATE tagged as rate
	expect_true("RATE" %in% names(listing))
	expect_true(all(c("Time", "Amount") %in% names(listing)))
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-DataHelpers.R")
