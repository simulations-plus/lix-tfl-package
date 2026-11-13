mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
mono_project_3 = "test-projects/monolix/prj08_iov2.mlxtran"
mono_project_4 = "test-projects/monolix/prj9_iov2_bootstrap.mlxtran"
mono_project_5 = "test-projects/monolix/prj08_iov2_noresults.mlxtran"
mono_project_6 = "test-projects/monolix/prj10_iov2_bootstrap.mlxtran"
mono_project_7 = "test-projects/monolix/warfarin_covariate3_project_COSSAC.mlxtran"

syc_project = "test-projects/monolix/PK_models.syc"
################################################################################
# test tableRunHistory()
test_that("tableRunHistory project", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	table = tableRunHistory(syc_project)
	expect_equal(dim(table), c(2,13))
	expect_equal(table$Run, c("PK_run02", "PK_run01"))
	expect_equal(table$`Project Path`, c(basename(mono_project_2), basename(mono_project_1)))
	expect_equal(table$Parent, c("PK_run01", ""))
})

test_that("tableRunHistory prettyLabels renames headers without changing data", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	raw    <- tableRunHistory(syc_project, prettyLabels = FALSE)
	pretty <- tableRunHistory(syc_project, prettyLabels = TRUE)

	expect_equal(dim(raw), dim(pretty))                    # only headers change
	expect_true(all(c("Path", "Parent") %in% names(raw)))  # machine names raw
	expect_false("Path" %in% names(pretty))                # renamed away
	expect_true(all(c("Project Path", "Parent Run") %in% names(pretty)))
	expect_equal(unname(as.list(raw)), unname(as.list(pretty)))  # values identical
})

test_that("tableRunHistory roundDigits/roundMethod are applied to numeric columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	none <- tableRunHistory(syc_project, prettyLabels = FALSE, roundMethod = "none")
	r0   <- tableRunHistory(syc_project, prettyLabels = FALSE,
													roundMethod = "monolixSuiteRounding", roundDigits = 0)

	num_cols <- names(Filter(is.numeric, none))
	skip_if(length(num_cols) == 0, "no numeric columns present")
	# rounding to 0 digits must change at least one column vs. unrounded
	changed <- any(vapply(num_cols,
												function(c) !isTRUE(all.equal(none[[c]], r0[[c]])), logical(1)))
	expect_true(changed)
})

test_that("tableRunHistory sortByCriteria orders rows; 'none' leaves them unsorted", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	none <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "none")
	ll   <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "LL")

	expect_setequal(none$Run, ll$Run)                       # same runs, maybe reordered
	ll_cols <- intersect(c("LLlin", "LLis"), names(ll))
	skip_if(length(ll_cols) == 0, "no LL column to verify sort")
	# LL view is ascending on whichever LL column drove the sort
	expect_true(any(vapply(ll_cols,
												 function(c) !is.unsorted(ll[[c]][!is.na(ll[[c]])]), logical(1))))
})

test_that("tableRunHistory filterCriteria keeps only the requested criterion columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	none <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "none", filterCriteria = "none")
	ll   <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "none", filterCriteria = "LL")
	bicc <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "none", filterCriteria = "BICc")

	expect_false(any(grepl("BICc", names(ll))))    # LL view drops all BICc cols
	expect_false(any(grepl("LL",   names(bicc))))   # BICc view drops all LL cols
	expect_true(all(names(ll)   %in% names(none)))
	expect_true(all(names(bicc) %in% names(none)))
})

test_that("tableRunHistory mock masks values and anonymizes run identifiers", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	real <- tableRunHistory(syc_project, prettyLabels = FALSE, mock = FALSE)
	mock <- tableRunHistory(syc_project, prettyLabels = FALSE, mock = TRUE, roundDigits = 2)

	expect_equal(dim(real), dim(mock))                        # same shape
	expect_true(all(grepl("^project[0-9]+$", mock$Run)))      # Run anonymized
	expect_false(any(mock$Run %in% real$Run))                 # real names gone
	num_cols <- intersect(names(Filter(is.numeric, real)), names(mock))
	skip_if(length(num_cols) == 0, "no numeric columns to mask")
	vals <- mock[[num_cols[1]]]
	expect_true(all(grepl("xx", vals[!is.na(vals)])))         # masked to "xx.xx"
})

test_that("tableRunHistory rejects invalid argument values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableRunHistory(syc_project, roundMethod    = "bogus"))
	expect_error(tableRunHistory(syc_project, sortByCriteria = "bogus"))
	expect_error(tableRunHistory(syc_project, filterCriteria = "bogus"))
})
################################################################################
# test tablePopulationParameterEstimates()
test_that("tablePopulationParameterEstimates applies customAliases to the Parameter column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)

	# alias every available parameter (fixed effects, IIV, residual error)
	aliases <- c(
		Tlag_pop   = "Tlag",      ka_pop   = "ka",   V_pop   = "V",   Cl_pop   = "Cl",
		omega_Tlag = "w_Tlag",    omega_ka = "w_ka", omega_V = "w_V", omega_Cl = "w_Cl",
		a          = "const err", b        = "prop err")

	table <- tablePopulationParameterEstimates(mono_project_1, customAliases = aliases,
		includeIIV = TRUE)

	# every parameter is displayed under its alias; sections are unaffected
	expect_setequal(table$Parameter, unname(aliases))
	expect_setequal(as.character(table$Section), c("FE", "IIV", "RES_ERR"))

	# a parameter left out of the mapping keeps its original name
	partial <- tablePopulationParameterEstimates(mono_project_1,
		customAliases = c(V_pop = "Volume"), includeIIV = TRUE)
	expect_true("Volume" %in% partial$Parameter)   # mapped   -> alias
	expect_true("ka_pop" %in% partial$Parameter)   # unmapped -> original name
})

test_that("tablePopulationParameterEstimates appends customUnits to the Parameter column", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)

	# 2-column data.frame form (columns are positional: name, unit) covering all
	# parameters; blank units are left off, non-blank units are appended in "( )"
	units_df <- data.frame(
		param = c("Tlag_pop", "ka_pop", "V_pop", "Cl_pop",
							"omega_Tlag", "omega_ka", "omega_V", "omega_Cl", "a", "b"),
		unit  = c("h", "1/h", "L", "L/h", "", "", "", "", "", ""),
		stringsAsFactors = FALSE
	)

	table <- tablePopulationParameterEstimates(mono_project_1, customUnits = units_df,
		includeIIV = TRUE)

	# non-blank units appended in parentheses to the original parameter name
	expect_true(all(c("Tlag_pop (h)", "ka_pop (1/h)", "V_pop (L)", "Cl_pop (L/h)")
									%in% table$Parameter))
	# blank-unit parameters are untouched (no parentheses added)
	expect_true(all(c("omega_V", "a", "b") %in% table$Parameter))
	expect_false(any(grepl("(", table$Parameter[as.character(table$Section) != "FE"], fixed = TRUE)))
})

test_that("tablePopulationParameterEstimates reports RSE and stdErrMethod selects the method", {
	# PK_run01 ships with FisherInformation for both linearization and stochasticApproximation,
	# so no estimation is run here.
	withr::local_options(TFLMonolixSuite.verbose = TRUE)
	.ensureProjectLoaded(mono_project_1)
	# RSE column present and populated from the precomputed Fisher information
	table <- tablePopulationParameterEstimates(projectFile = mono_project_1, prettyLabels = FALSE)
	expect_true("RSE" %in% names(table))
	expect_false(any(is.na(table$RSE)))

	# stdErrMethod selects between the two precomputed methods -> different RSE values
	lin <- tablePopulationParameterEstimates(mono_project_1, stdErrMethod = "linearization", prettyLabels = FALSE)
	sa  <- tablePopulationParameterEstimates(mono_project_1, stdErrMethod = "stochasticApproximation", prettyLabels = FALSE)
	expect_false(isTRUE(all.equal(lin$RSE, sa$RSE)))
	expect_equal(as.numeric(lin$RSE[1]), 2.7280)
	expect_equal(as.numeric(sa$RSE[1]),  2.8860)

	# both methods available and none requested -> user is informed, linearization is used
	expect_message(table <- tablePopulationParameterEstimates(mono_project_1, prettyLabels = FALSE),
								 "multiple methods")
	expect_equal(as.numeric(table$RSE[1]), 2.7280)
})

test_that("tablePopulationParameterEstimates include* flags toggle optional columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)

	# includeRSE
	expect_true("RSE" %in% names(
		tablePopulationParameterEstimates(mono_project_1, includeRSE = TRUE, prettyLabels = FALSE)))
	expect_false("RSE" %in% names(
		tablePopulationParameterEstimates(mono_project_1, includeRSE = FALSE, prettyLabels = FALSE)))

	# includeCI -> combined CI column, not the raw percentile columns
	ci <- tablePopulationParameterEstimates(mono_project_1, includeCI = TRUE, prettyLabels = FALSE)
	expect_true("CI_2.5_97.5" %in% names(ci))
	expect_false(any(c("P2.5", "P97.5") %in% names(ci)))
	expect_false("CI_2.5_97.5" %in% names(
		tablePopulationParameterEstimates(mono_project_1, includeCI = FALSE, prettyLabels = FALSE)))

	# includeCV -> CV column populated for omega rows; helper *_CV rows are folded away
	cv <- tablePopulationParameterEstimates(mono_project_1, includeCV = TRUE, prettyLabels = FALSE)
	expect_true("CV" %in% names(cv))
	omega_rows <- grep("omega_", cv$Parameter)
	expect_true(any(!is.na(cv$CV[omega_rows])))
	expect_false(any(paste0(c("omega_Tlag", "omega_ka", "omega_V", "omega_Cl"), "_CV") %in% cv$Parameter))
	expect_false("CV" %in% names(
		tablePopulationParameterEstimates(mono_project_1, includeCV = FALSE, prettyLabels = FALSE)))
})

test_that("tablePopulationParameterEstimates includeBootstrap adds bootstrap columns", {
	# prj9_iov2_bootstrap ships with Bootstrap results; PK_run01 does NOT.
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_4)

	with_boot <- tablePopulationParameterEstimates(mono_project_4, includeBootstrap = TRUE, prettyLabels = FALSE)
	expect_true("Boot_Median" %in% names(with_boot))
	boot_ci_col <- grep("^Boot_CI", names(with_boot), value = TRUE)
	expect_length(boot_ci_col, 1)
	# bootstrap columns are populated from the precomputed runs
	expect_true(any(!is.na(with_boot$Boot_Median)))
	expect_true(any(!is.na(with_boot[[boot_ci_col]])))

	# includeBootstrap = FALSE omits all bootstrap columns
	without_boot <- tablePopulationParameterEstimates(mono_project_4, includeBootstrap = FALSE, prettyLabels = FALSE)
	expect_false(any(grepl("^Boot", names(without_boot))))
})

test_that("tablePopulationParameterEstimates wide format", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test wide format with IIV
	table = tablePopulationParameterEstimates(mono_project_1, useWideFormat = TRUE, includeIIV = TRUE, prettyLabels = FALSE)
	expect_true("FE_Estimate" %in% names(table))
	expect_true("IIV_SD" %in% names(table))
	expect_false("IIV" %in% table$Section)  # omega parameters should not appear as separate rows
	expect_true(any(table$Section == "FE"))

	# Test wide format with CV
	table = tablePopulationParameterEstimates(mono_project_1, useWideFormat = TRUE, includeCV = TRUE, includeIIV = TRUE, , prettyLabels = FALSE)
	expect_true("IIV_CV" %in% names(table))
	expect_true("IIV_SD" %in% names(table))

	# Test wide format with RSE
	table = tablePopulationParameterEstimates(mono_project_1, useWideFormat = TRUE, includeRSE = TRUE, includeIIV = TRUE, , prettyLabels = FALSE)
	expect_true("FE_RSE" %in% names(table))
	expect_true("IIV_RSE" %in% names(table))

	# Test wide format without IIV
	table = tablePopulationParameterEstimates(mono_project_1, useWideFormat = TRUE, includeIIV = FALSE, , prettyLabels = FALSE)
	expect_false("IIV_SD" %in% names(table))
	expect_false("IIV_CV" %in% names(table))
	expect_false("IIV_RSE" %in% names(table))
})

test_that("tablePopulationParameterEstimates CI combined format", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test CI column in long format (should be combined)
	table = tablePopulationParameterEstimates(mono_project_1, includeCI = TRUE, prettyLabels = FALSE)
	expect_true("CI_2.5_97.5" %in% names(table))
	# CI values should have format "(value; value)"
	ci_values = table$CI_2.5_97.5[!is.na(table$CI_2.5_97.5)]
	if (length(ci_values) > 0) {
		expect_true(all(grepl("^\\(.*; .*\\)$", ci_values)))
	}

	# Test CI column in wide format (should be renamed to FE_CI)
	table = tablePopulationParameterEstimates(mono_project_1, includeCI = TRUE, useWideFormat = TRUE, includeIIV = TRUE, prettyLabels = FALSE)
	expect_true("FE_CI_2.5_97.5" %in% names(table))
	expect_true("IIV_CI_2.5_97.5" %in% names(table))
	expect_false("CI_2.5_97.5" %in% names(table))
})

test_that("tablePopulationParameterEstimates section labels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# Test that section labels use abbreviated format
	table = tablePopulationParameterEstimates(mono_project_1)
	unique_sections = unique(table$Section)

	# Should use abbreviated labels: FE, IIV, IOV, CORR, CORR_IIV, CORR_IOV, RES_ERR
	expect_true(all(unique_sections %in% c("FE", "IIV", "IOV", "CORR", "CORR_IIV", "CORR_IOV", "RES_ERR")))

	# Should NOT use verbose labels
	expect_false(any(unique_sections %in% c("Fixed effects", "Random effects", "Residual error")))
})

test_that("tablePopulationParameterEstimates errors", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	# no results
	expect_error(tablePopulationParameterEstimates(mono_project_5))

	.ensureProjectLoaded(mono_project_1)

	expect_error(tablePopulationParameterEstimates(mono_project_5, data.frame(Parameter = "V_pop", Unit = "L")))
	expect_error(tablePopulationParameterEstimates(mono_project_5, "Parameter"))
	expect_error(tablePopulationParameterEstimates(mono_project_5, c(Parameter = "V_pop", Label = "V", Unit = "L")))
})

# ------------------------------------------------------------------------------
# Tests for tablePopulationParameterEstimates() with IOV
test_that("tablePopulationParameterEstimates() includes IOV parameters in long format", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = TRUE,
		useWideFormat = FALSE
	)

	# Check that IOV section exists
	expect_true("IOV" %in% result$Section)

	# Check that gamma parameters are present
	iov_rows <- result[result$Section == "IOV", ]
	expect_gt(nrow(iov_rows), 0)

	# Check attributes
	expect_equal(attr(result, "table_type"), "pop")
	expect_equal(attr(result, "format"), "long")
	expect_false(attr(result, "includeIOV"))  # FALSE in long format
})

test_that("tablePopulationParameterEstimates() wide format with IOV creates IOV columns", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = TRUE,
		useWideFormat = TRUE
	)

	# Check that IOV_SD column exists
	expect_true("IOV\nSD" %in% names(result) | "IOV_SD" %in% names(result))

	# Check that IOV section is NOT present (should be in columns, not rows)
	expect_false("IOV" %in% result$Section)

	# Check attributes
	expect_equal(attr(result, "format"), "wide")
	expect_true(attr(result, "includeIOV"))
})
test_that("tablePopulationParameterEstimates() wide format can exclude IOV", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = FALSE,
		useWideFormat = TRUE
	)

	# Check that IOV columns do NOT exist
	expect_false(any(grepl("IOV", names(result))))

	# Check attributes
	expect_false(attr(result, "includeIOV"))
})

test_that("tablePopulationParameterEstimates() wide format with IOV and CV", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = TRUE,
		includeCV = TRUE,
		useWideFormat = TRUE
	)

	# Check for both IOV_SD and IOV_CV columns
	col_names <- names(result)
	expect_true(any(grepl("IOV.*SD", col_names)))
	expect_true(any(grepl("IOV.*CV", col_names)))
})
test_that("tablePopulationParameterEstimates() wide format with IOV, RSE, and CI", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = TRUE,
		includeRSE = TRUE,
		includeCI = TRUE,
		useWideFormat = TRUE
	)

	# Check for IOV columns with RSE and CI
	col_names <- names(result)
	expect_true(any(grepl("IOV.*RSE", col_names)))
	expect_true(any(grepl("IOV.*CI", col_names)))
})

test_that("tablePopulationParameterEstimates() wide format: includeIIV=FALSE, includeIOV=TRUE", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_3,
		includeIIV = FALSE,
		includeIOV = TRUE,
		useWideFormat = TRUE
	)

	# Should have IOV columns but not IIV columns
	col_names <- names(result)
	expect_false(any(grepl("IIV", col_names)))
	expect_true(any(grepl("IOV", col_names)))

	# Check attributes
	expect_false(attr(result, "includeIIV"))
	expect_true(attr(result, "includeIOV"))
})

# ------------------------------------------------------------------------------
# Tests for tablePopulationParameterEstimates() with Bootstrap
test_that("tablePopulationParameterEstimates() includes bootstrap results in long format", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		useWideFormat = FALSE,
		prettyLabels = FALSE
	)

	# Check for bootstrap columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*Median", col_names)))
	expect_true(any(grepl("Boot.*CI", col_names)))
	expect_true(any(grepl("Boot.*RSE", col_names)))
})

test_that("tablePopulationParameterEstimates() bootstrap CI naming adapts to confidence level", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		useWideFormat = FALSE,
		prettyLabels = FALSE
	)

	# Check that CI column exists with dynamic naming (e.g., Boot_CI_2.5_97.5)
	col_names <- names(result)
	boot_ci_col <- grep("Boot.*CI.*[0-9]", col_names, value = TRUE)
	expect_length(boot_ci_col, 1)
	expect_true(grepl("Boot.*CI.*[0-9]+.*[0-9]+", boot_ci_col))
})

test_that("tablePopulationParameterEstimates() wide format with bootstrap creates bootstrap IIV columns", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		includeIIV = TRUE,
		useWideFormat = TRUE,
		prettyLabels = FALSE
	)

	# Check for bootstrap IIV columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*IIV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IIV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IIV.*RSE", col_names)))
})
test_that("tablePopulationParameterEstimates() wide format with bootstrap and IOV creates bootstrap IOV columns", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,  # Project with both bootstrap and IOV
		includeBootstrap = TRUE,
		includeIOV = TRUE,
		useWideFormat = TRUE,
		prettyLabels = FALSE
	)

	# Check for bootstrap IOV columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*IOV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IOV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IOV.*RSE", col_names)))
})

test_that("tablePopulationParameterEstimates() bootstrap CI values are properly rounded and combined", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "significantFigures",
		useWideFormat = FALSE,
		prettyLabels = FALSE
	)

	# Find the bootstrap CI column
	boot_ci_col <- grep("Boot.*CI", names(result), value = TRUE)

	# Check that CI values are formatted as "(lower; upper)"
	ci_values <- result[[boot_ci_col]]
	non_na_values <- ci_values[!is.na(ci_values)]
	expect_true(all(grepl("^\\([^;]+;\\s[^)]+\\)$", non_na_values)))
})

test_that("tablePopulationParameterEstimates() bootstrap with different rounding methods", {
	result_monolix <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "monolixSuiteRounding",
		prettyLabels = FALSE
	)

	result_signif <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "significantFigures",
		prettyLabels = FALSE
	)

	# Both should have bootstrap columns
	expect_true(any(grepl("Boot", names(result_monolix))))
	expect_true(any(grepl("Boot", names(result_signif))))
})

# ------------------------------------------------------------------------------
# Combined Tests: IOV + Bootstrap + Wide Format

test_that("tablePopulationParameterEstimates() comprehensive: wide format with all options", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeRSE = TRUE,
		includeIIV = TRUE,
		includeIOV = TRUE,
		includeCI = TRUE,
		includeCV = TRUE,
		includeBootstrap = TRUE,
		useWideFormat = TRUE,
		roundDigits = 3,
		roundMethod = "significantFigures",
		prettyLabels = FALSE)

	col_names <- names(result)

	# Fixed effects columns
	expect_true(any(grepl("FE.*Estimate", col_names)))
	expect_true(any(grepl("FE.*RSE", col_names)))
	expect_true(any(grepl("FE.*CI", col_names)))

	# IIV columns
	expect_true(any(grepl("IIV.*SD", col_names)))
	expect_true(any(grepl("IIV.*CV", col_names)))
	expect_true(any(grepl("IIV.*RSE", col_names)))
	expect_true(any(grepl("IIV.*CI", col_names)))

	# IOV columns
	expect_true(any(grepl("IOV.*SD", col_names)))
	expect_true(any(grepl("IOV.*CV", col_names)))
	expect_true(any(grepl("IOV.*RSE", col_names)))
	expect_true(any(grepl("IOV.*CI", col_names)))

	# Bootstrap FE columns
	expect_true(any(grepl("Boot.*Median", col_names)))
	expect_true(any(grepl("Boot.*CI", col_names)))
	expect_true(any(grepl("Boot.*RSE", col_names)))

	# Bootstrap IIV columns
	expect_true(any(grepl("Boot.*IIV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IIV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IIV.*RSE", col_names)))

	# Bootstrap IOV columns
	expect_true(any(grepl("Boot.*IOV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IOV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IOV.*RSE", col_names)))

	# Check attributes
	expect_equal(attr(result, "format"), "wide")
	expect_true(attr(result, "includeIIV"))
	expect_true(attr(result, "includeIOV"))
})

test_that("tablePopulationParameterEstimates() long format with IOV and bootstrap", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeIOV = TRUE,
		includeBootstrap = TRUE,
		useWideFormat = FALSE
	)

	# Check sections
	sections <- unique(result$Section)
	expect_true("FE" %in% sections)
	expect_true("IIV" %in% sections)
	expect_true("IOV" %in% sections)

	# Bootstrap columns should be present for all sections
	col_names <- names(result)
	expect_true(any(grepl("Boot.*Median", col_names)))
	expect_true(any(grepl("Boot.*CI", col_names)))
})

# ------------------------------------------------------------------------------
# Column Order Tests
test_that("tablePopulationParameterEstimates() bootstrap columns appear in correct order", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		useWideFormat = FALSE,
		prettyLabels = FALSE
	)

	col_names <- names(result)
	boot_median_pos <- grep("Boot_Median", col_names)
	boot_ci_pos <- grep("Boot_CI", col_names)
	boot_rse_pos <- grep("Boot_RSE", col_names)

	# Order should be: Median, CI, RSE
	expect_true(boot_median_pos < boot_ci_pos)
	expect_true(boot_ci_pos < boot_rse_pos)
})

test_that("tablePopulationParameterEstimates() wide format bootstrap IIV column order", {
	result <- tablePopulationParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		includeIIV = TRUE,
		useWideFormat = TRUE,
		prettyLabels = FALSE
	)

	col_names <- names(result)
	boot_iiv_median_pos <- grep("Boot_IIV_Median", col_names)
	boot_iiv_ci_pos <- grep("Boot_IIV_CI", col_names)
	boot_iiv_rse_pos <- grep("Boot_IIV_RSE", col_names)

	# Order should be: Median, CI, RSE
	expect_true(boot_iiv_median_pos < boot_iiv_ci_pos)
	expect_true(boot_iiv_ci_pos < boot_iiv_rse_pos)
})

################################################################################
# tableCovariateModelBuilding()

mb_project_cossac <- "test-projects/monolix/warfarin_covariate3_project_COSSAC.mlxtran"
mb_project_scm    <- "test-projects/monolix/warfarin_covariate3_project_SCM_BICc.mlxtran"
mb_project_samba  <- "test-projects/monolix/warfarin_covariate3_project_SAMBA.mlxtran"

test_that("tableCovariateModelBuilding COSSAC (LRT) reports connector best model + LRT columns", {
	table <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE)

	expect_equal(table$Run, paste("Model", 1:6))
	# COSSAC uses LRT: p.value / Significant present, no BICc-only Improved flag
	expect_contains(names(table), c("Tested", "Step", "p.value", "Significant", "BestModel"))
	expect_false("Improved" %in% names(table))
	# best model comes straight from the connector (bestModel = Model 4)
	expect_equal(which(table$BestModel == "Yes"), 4L)
	# start model has no reference / deltas
	expect_equal(table$Reference[1], "")
	expect_true(is.na(table$dLL[1]))
})

test_that("tableCovariateModelBuilding SCM (BICc) reports connector best model + Improved flag", {
	table <- tableCovariateModelBuilding(mb_project_scm, prettyLabels = FALSE)

	expect_equal(nrow(table), 16L)
	# BICc criterion: Improved flag, no LRT columns
	expect_contains(names(table), c("Tested", "Step", "Improved", "BestModel"))
	expect_false(any(c("p.value", "Significant") %in% names(table)))
	# best model comes straight from the connector (bestModel = Model 11)
	expect_equal(which(table$BestModel == "Yes"), 11L)
})

test_that("tableCovariateModelBuilding SAMBA derives best model by minimum BICc", {
	table <- tableCovariateModelBuilding(mb_project_samba, prettyLabels = FALSE)

	expect_equal(table$Run, paste("Model", 1:2))
	# SAMBA proposes whole models: no per-step columns
	expect_false(any(c("Tested", "Step", "p.value", "Significant") %in% names(table)))
	expect_contains(names(table), c("Reference", "LL", "BICc", "dLL", "dBICc",
																	"BestModel", "Improved"))
	# best model is derived (connector returns no flag): lowest BICc
	expect_equal(which(table$BestModel == "Yes"), which.min(table$BICc))
	# deltas are computed against the start model (Model 1)
	expect_equal(table$Reference, c("", "Model 1"))
	expect_true(is.na(table$dBICc[1]))
	expect_equal(table$dBICc[2], round(table$BICc[2] - table$BICc[1], 2))
})

test_that("tableCovariateModelBuilding SAMBA respects filterCriteria and sortBy", {
	# filterCriteria = "BICc" drops the LL/dLL columns
	table_bicc <- tableCovariateModelBuilding(mb_project_samba, filterCriteria = "BICc",
																	 prettyLabels = FALSE)
	expect_false(any(c("LL", "dLL") %in% names(table_bicc)))
	expect_contains(names(table_bicc), c("BICc", "dBICc"))

	# SAMBA has no steps, so sortBy = "BICc" orders the whole table globally and
	# the best model floats to row 1
	table_sorted <- tableCovariateModelBuilding(mb_project_samba, sortBy = "BICc",
																		 prettyLabels = FALSE)
	expect_equal(table_sorted$BICc, sort(table_sorted$BICc))
	expect_equal(table_sorted$BestModel[1], "Yes")
})

test_that("tableCovariateModelBuilding stepwise sorting stays within each step round", {
	# SCM does three forward rounds (vs Model 1, then Model 4, then Model 11).
	# Sorting by LL must order rows *within* each round, never merging rounds.
	table <- tableCovariateModelBuilding(mb_project_scm, sortBy = "LL", prettyLabels = FALSE)

	grp <- .modelBuildingStepGroups(table$Step, table$Reference)
	# each round keeps a single reference and the rounds stay in algorithm order
	expect_equal(tapply(table$Reference, grp, function(x) length(unique(x))),
							 array(1L, dim = length(unique(grp)),
										 dimnames = list(as.character(sort(unique(grp))))))
	# within every round LL is non-decreasing (sorted), even though the global
	# LL sequence is not monotonic across rounds
	for (g in unique(grp)) {
		ll <- table$LL[grp == g]
		expect_false(is.unsorted(ll))
	}
	# the reference of the 2nd forward round is a model from the 1st round, i.e.
	# rounds are genuinely distinct and ordered
	expect_true(length(unique(grp)) >= 3)
})

test_that("tableCovariateModelBuilding errors with a clear message when no COSSAC/SCM/SAMBA results exist", {
	# For a project that never ran a covariate model-building search, the connector
	# returns a bare FALSE (not NULL/length 0) -> must not surface as the internal
	# "$ operator is invalid for atomic vectors" error.
	expect_error(tableCovariateModelBuilding(mono_project_3) , "No model building results found.*COSSAC, SCM, or SAMBA")
})

################################################################################
# trailingZeros + character-output contract (tablePopulationParameterEstimates)
#
# When a rounding method is active, estimate/uncertainty columns are rendered as
# character display strings. trailingZeros = TRUE pads to the precision implied
# by digits/method; FALSE shows the minimal form. Both carry the SAME underlying
# numeric value, so: columns are character, as.numeric() agrees, and the padded
# string is never shorter than the minimal one.
################################################################################

.expect_trailing_contract <- function(pad, minimal) {
	expect_type(pad, "character")
	expect_type(minimal, "character")
	# same underlying numeric value (trailing zeros don't change it)
	expect_equal(suppressWarnings(as.numeric(pad)),
							 suppressWarnings(as.numeric(minimal)))
	# padded form is never shorter than the minimal form (NA cells ignored)
	expect_true(all(nchar(pad) >= nchar(minimal), na.rm = TRUE))
}

test_that("tablePopulationParameterEstimates trailingZeros toggles padding; long-format columns are character", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	args <- list(projectFile = mono_project_1, roundDigits = 3,
							 roundMethod = "monolixSuiteRounding", prettyLabels = FALSE)
	pad <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = TRUE)))
	mn  <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = FALSE)))
	.expect_trailing_contract(pad$Estimate, mn$Estimate)
	.expect_trailing_contract(pad$RSE,      mn$RSE)
})

test_that("tablePopulationParameterEstimates trailingZeros toggles padding; wide IIV columns are character", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	args <- list(projectFile = mono_project_1, useWideFormat = TRUE, includeIIV = TRUE,
							 roundDigits = 3, roundMethod = "monolixSuiteRounding", prettyLabels = FALSE)
	pad <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = TRUE)))
	mn  <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = FALSE)))
	.expect_trailing_contract(pad$FE_Estimate, mn$FE_Estimate)
	.expect_trailing_contract(pad$IIV_SD,      mn$IIV_SD)
})

test_that("tablePopulationParameterEstimates wide-reshape bootstrap columns are character display strings", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	args <- list(projectFile = mono_project_4, includeBootstrap = TRUE, includeIIV = TRUE,
							 useWideFormat = TRUE, roundDigits = 3,
							 roundMethod = "monolixSuiteRounding", prettyLabels = FALSE)
	pad <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = TRUE)))
	mn  <- do.call(tablePopulationParameterEstimates, c(args, list(trailingZeros = FALSE)))
	# collapsed reshape columns start as NA_character_; formatted strings must survive
	.expect_trailing_contract(pad$Boot_IIV_Median, mn$Boot_IIV_Median)
})

################################################################################
# Value-based coverage (prj9_iov2_bootstrap): categorical TREAT covariate + IOV
# + bootstrap. Pinned values are at roundDigits = 3, monolixSuiteRounding, and
# are independent of prettyLabels (which only renames headers).
################################################################################

# cell lookup by parameter name (robust to row order)
.popcell <- function(tb, param, col) tb[[col]][tb$Parameter == param]

test_that("tablePopulationParameterEstimates monolixSuiteRounding: decimals for >=1, sig-figs for <1", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeCV = FALSE, includeCI = FALSE, includeBootstrap = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	expect_type(res$Estimate, "character")
	expect_equal(.popcell(res, "ka_pop", "Estimate"), "1.477")   # >= 1 -> 3 decimal places
	expect_equal(.popcell(res, "V_pop",  "Estimate"), "0.474")   # <  1 -> 3 significant figures
	expect_equal(.popcell(res, "Cl_pop", "Estimate"), "0.0405")  # <  1 -> 3 significant figures
})

test_that("tablePopulationParameterEstimates includeCV populates CV for variability rows only", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeCV = TRUE, includeCI = FALSE, includeBootstrap = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	expect_true("CV" %in% names(res))
	expect_type(res$CV, "character")
	expect_equal(.popcell(res, "omega_ka", "CV"), "27.821")
	expect_equal(.popcell(res, "omega_V",  "CV"), "11.612")
	# fixed effects and residual error carry no CV
	expect_true(is.na(.popcell(res, "ka_pop", "CV")))
	expect_true(is.na(.popcell(res, "a",      "CV")))
})

test_that("tablePopulationParameterEstimates includeCI combines bounds into a (lower; upper) string", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeCV = FALSE, includeCI = TRUE, includeBootstrap = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	expect_true("CI_2.5_97.5" %in% names(res))
	expect_equal(.popcell(res, "ka_pop", "CI_2.5_97.5"), "(1.327; 1.644)")
	expect_equal(.popcell(res, "V_pop",  "CI_2.5_97.5"), "(0.449; 0.501)")
})

test_that("tablePopulationParameterEstimates includeFEcategory expands categorical fixed effects", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	base <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeFEcategory = FALSE, includeCV = FALSE, includeCI = FALSE,
		includeBootstrap = FALSE, roundDigits = 3, roundMethod = "monolixSuiteRounding")
	expect_false("FE_CAT" %in% base$Section)
	expect_false("ka_TREAT_A" %in% base$Parameter)

	fe <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeFEcategory = TRUE, includeCV = FALSE, includeCI = FALSE,
		includeBootstrap = FALSE, roundDigits = 3, roundMethod = "monolixSuiteRounding")
	expect_true("FE_CAT" %in% fe$Section)
	expect_setequal(fe$Parameter[fe$Section == "FE_CAT"],
		c("ka_TREAT_A", "ka_TREAT_B", "V_TREAT_A", "V_TREAT_B", "Cl_TREAT_A", "Cl_TREAT_B"))

	# reference category (A) equals the typical/pop estimate
	expect_equal(.popcell(fe, "ka_TREAT_A", "Estimate"), .popcell(fe, "ka_pop", "Estimate"))
	expect_equal(.popcell(fe, "V_TREAT_A",  "Estimate"), .popcell(fe, "V_pop",  "Estimate"))
	expect_equal(.popcell(fe, "Cl_TREAT_A", "Estimate"), .popcell(fe, "Cl_pop", "Estimate"))
	# the other category differs, with the expected values
	expect_equal(.popcell(fe, "ka_TREAT_B", "Estimate"), "1.586")
	expect_equal(.popcell(fe, "V_TREAT_B",  "Estimate"), "0.396")
	expect_equal(.popcell(fe, "Cl_TREAT_B", "Estimate"), "0.0330")
})

test_that("tablePopulationParameterEstimates FE_CAT rows have no bootstrap, base FE rows do", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeFEcategory = TRUE, includeBootstrap = TRUE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	expect_true(all(is.na(res$Boot_Median[res$Section == "FE_CAT"])))
	expect_equal(.popcell(res, "ka_pop", "Boot_Median"), "1.449")
})

test_that("tablePopulationParameterEstimates roundByParameter overrides digits/method per original name", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeCV = FALSE, includeCI = FALSE, includeBootstrap = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding",
		roundByParameter = data.frame(
			parameter = c("ka_pop", "V_pop"),
			digits    = c(1, 1),
			method    = c("significantFigures", "significantFigures"),
			stringsAsFactors = FALSE))

	expect_equal(.popcell(res, "ka_pop", "Estimate"), "1")      # overridden -> 1 significant figure
	expect_equal(.popcell(res, "V_pop",  "Estimate"), "0.5")    # overridden -> 1 significant figure
	expect_equal(.popcell(res, "Cl_pop", "Estimate"), "0.0405") # not overridden -> global (3 digits)
})

test_that("tablePopulationParameterEstimates roundByParameter ignores unknown parameters with a message", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	expect_message(
		tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
			roundByParameter = c(nonexistent_pop = 2)),
		"ignoring unknown parameter")

	a <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE)
	b <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE, roundByParameter = NULL)
	expect_equal(a, b)
})

test_that("tablePopulationParameterEstimates roundByParameter rejects invalid digits/method", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	expect_error(tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		roundByParameter = data.frame(parameter = "V_pop", digits = 2, method = "bogus")))
	expect_error(tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		roundByParameter = c(V_pop = -1)))
})

test_that("tablePopulationParameterEstimates mock masks values but keeps structure", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	real <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE, includeCI = TRUE)
	mock <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE, includeCI = TRUE, mock = TRUE)

	expect_equal(dim(real), dim(mock))
	expect_equal(mock$Parameter, real$Parameter)                       # identifiers kept
	expect_equal(as.character(mock$Section), as.character(real$Section))
	expect_true(all(grepl("x", mock$Estimate[!is.na(mock$Estimate)]))) # estimates masked
	expect_true(all(grepl("x", mock$CI_2.5_97.5[!is.na(mock$CI_2.5_97.5)])))
})

test_that("tablePopulationParameterEstimates preserves trailing zeros at roundDigits = 4 (integration)", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_6, prettyLabels = FALSE,
		includeCV = FALSE, includeCI = FALSE, includeBootstrap = FALSE,
		roundDigits = 4, roundMethod = "monolixSuiteRounding")

	expect_equal(.popcell(res, "ka_pop", "Estimate"), "1.4770")   # >= 1 -> 4 decimals, trailing zero kept
	expect_equal(.popcell(res, "V_pop",  "Estimate"), "0.4741")   # <  1 -> 4 significant figures
	expect_equal(.popcell(res, "Cl_pop", "Estimate"), "0.04047")  # <  1 -> 4 significant figures

	minimal <- tablePopulationParameterEstimates(mono_project_6, prettyLabels = FALSE,
		includeCV = FALSE, includeCI = FALSE, includeBootstrap = FALSE,
		roundDigits = 4, roundMethod = "monolixSuiteRounding", trailingZeros = FALSE)
	expect_equal(.popcell(minimal, "ka_pop", "Estimate"), "1.477")  # same value, padding stripped
})

test_that("tablePopulationParameterEstimates includeFEcategory with a continuous covariate adds no FE_CAT section", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_2, prettyLabels = FALSE,
		includeFEcategory = TRUE, includeCV = TRUE, includeCI = FALSE,
		includeBootstrap = FALSE, roundDigits = 3, roundMethod = "monolixSuiteRounding")

	# continuous covariate (logtWT) -> no categorical expansion
	expect_false("FE_CAT" %in% res$Section)
	expect_true("beta_V_logtWT" %in% res$Parameter)

	expect_equal(.popcell(res, "Tlag_pop",      "Estimate"), "0.412")  # <  1 -> 3 sig figs
	expect_equal(.popcell(res, "V_pop",         "Estimate"), "8.289")  # >= 1 -> 3 decimals
	expect_equal(.popcell(res, "beta_V_logtWT", "Estimate"), "1.266")
	expect_equal(.popcell(res, "omega_ka",      "CV"),       "78.211")
})

test_that("tablePopulationParameterEstimates customDescriptions adds a Description column keyed on original names", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	res <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		customDescriptions = c(V_pop = "central volume", Cl_pop = "clearance"))

	expect_true("Description" %in% names(res))
	# placed immediately after Parameter
	expect_equal(which(names(res) == "Description"), which(names(res) == "Parameter") + 1L)
	expect_equal(.popcell(res, "V_pop",  "Description"), "central volume")
	expect_equal(.popcell(res, "Cl_pop", "Description"), "clearance")
	expect_true(is.na(.popcell(res, "ka_pop", "Description")))  # unmapped -> NA
})

test_that("tablePopulationParameterEstimates customDescriptions ignores unknown keys with a message", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	expect_message(
		tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
			customDescriptions = c(nonexistent_pop = "x")),
		"did not match any row")
})

test_that("tablePopulationParameterEstimates prettyLabels only renames headers; values are unchanged", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	raw    <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE)
	pretty <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = TRUE)

	expect_equal(dim(raw), dim(pretty))
	expect_false(identical(names(raw), names(pretty)))           # only headers change
	expect_equal(unname(as.list(raw)), unname(as.list(pretty)))  # values identical
})

test_that("tablePopulationParameterEstimates includeFEcategory in wide format keeps category rows with FE estimates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	wide <- tablePopulationParameterEstimates(mono_project_4, prettyLabels = FALSE,
		includeFEcategory = TRUE, useWideFormat = TRUE, includeBootstrap = FALSE,
		includeCV = FALSE, includeCI = FALSE,
		roundDigits = 3, roundMethod = "monolixSuiteRounding")

	expect_true("FE_Estimate" %in% names(wide))
	expect_true("FE_CAT" %in% wide$Section)
	expect_setequal(wide$Parameter[wide$Section == "FE_CAT"],
		c("ka_TREAT_A", "ka_TREAT_B", "V_TREAT_A", "V_TREAT_B", "Cl_TREAT_A", "Cl_TREAT_B"))

	# reference category (A) equals the typical/pop estimate (invariant, same as long format)
	expect_equal(.popcell(wide, "ka_TREAT_A", "FE_Estimate"), .popcell(wide, "ka_pop", "FE_Estimate"))
	expect_equal(.popcell(wide, "V_TREAT_A",  "FE_Estimate"), .popcell(wide, "V_pop",  "FE_Estimate"))
	# estimate values are identical to long format (wide only reshapes the variability params)
	expect_equal(.popcell(wide, "ka_TREAT_B", "FE_Estimate"), "1.586")
	expect_equal(.popcell(wide, "Cl_TREAT_B", "FE_Estimate"), "0.0330")
})

################################################################################
# Argument-coverage additions (Phase 1). Appended so .popcell etc. are in scope.
################################################################################

# --- tableCovariateModelBuilding() -------------------------------------------

test_that("tableCovariateModelBuilding displayBestModelPerStep reduces to one accepted model per step", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	full <- tableCovariateModelBuilding(mb_project_scm, prettyLabels = FALSE)
	best <- tableCovariateModelBuilding(mb_project_scm, displayBestModelPerStep = TRUE,
																			prettyLabels = FALSE)

	expect_lt(nrow(best), nrow(full))
	expect_true(all(best$Run %in% full$Run))
	expect_true(any(best$Step == "Base"))                   # Base always kept
	expect_setequal(best$Run, c(full$Run[full$Step == "Base"], attr(best, "best_per_step")))
	grp <- .modelBuildingStepGroups(best$Step, best$Reference)
	expect_equal(unname(max(table(grp))), 1L)               # one model per round
})

test_that("tableCovariateModelBuilding displayBestModelPerStep is ignored for SAMBA (no steps)", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	expect_message(
		best <- tableCovariateModelBuilding(mb_project_samba, displayBestModelPerStep = TRUE,
																				prettyLabels = FALSE),
		"applies only to stepwise")
	full <- tableCovariateModelBuilding(mb_project_samba, prettyLabels = FALSE)
	expect_equal(nrow(best), nrow(full))
})

test_that("tableCovariateModelBuilding mock masks metrics and anonymizes covariate names", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	real <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE, mock = FALSE)
	mock <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE, mock = TRUE,
																			roundDigits = 2)

	expect_equal(dim(real), dim(mock))
	expect_equal(mock$Run, real$Run)                        # identifiers kept
	expect_equal(mock$BestModel, real$BestModel)
	bicc <- as.character(mock$BICc)
	expect_true(all(grepl("xx", bicc[!is.na(bicc)])))       # metrics masked
	# covariate names anonymized in Tested; parameter names kept
	tested <- mock$Tested[nzchar(mock$Tested)]
	expect_true(all(grepl("^Covariate[0-9]+ on ", tested)))
	expect_false(any(mock$Tested == real$Tested & nzchar(real$Tested)))
})

test_that("tableCovariateModelBuilding customDescriptions adds a Description column after Tested", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	tbl <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE,
		customDescriptions = c("Model 1" = "Base covariate model",
													 "Model 4" = "Selected final model"))

	expect_true("Description" %in% names(tbl))
	expect_equal(which(names(tbl) == "Description"), which(names(tbl) == "Tested") + 1L)
	expect_equal(tbl$Description[tbl$Run == "Model 1"], "Base covariate model")
	expect_equal(tbl$Description[tbl$Run == "Model 4"], "Selected final model")
	expect_true(is.na(tbl$Description[tbl$Run == "Model 2"]))   # unmapped -> NA
})

test_that("tableCovariateModelBuilding customDescriptions ignores unknown keys with a message", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	expect_message(
		tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE,
			customDescriptions = c("Model 999" = "does not exist")),
		"did not match any Run")
})

test_that("tableCovariateModelBuilding roundDigits/roundMethod change the numeric display", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	none <- tableCovariateModelBuilding(mb_project_samba, roundMethod = "none", prettyLabels = FALSE)
	r1   <- tableCovariateModelBuilding(mb_project_samba, roundDigits = 1,
																			roundMethod = "monolixSuiteRounding", prettyLabels = FALSE)
	expect_false(isTRUE(all.equal(none$BICc, r1$BICc)))

	# BICc ~690 (3 integer digits) -> the two methods disagree at roundDigits = 2
	sf  <- tableCovariateModelBuilding(mb_project_samba, roundDigits = 2,
																		 roundMethod = "significantFigures", prettyLabels = FALSE)
	sfp <- tableCovariateModelBuilding(mb_project_samba, roundDigits = 2,
																		 roundMethod = "significantFiguresPreserved", prettyLabels = FALSE)
	expect_false(isTRUE(all.equal(sf$BICc, sfp$BICc)))
	expect_true(all(sf$BICc %% 10 == 0))                    # 2 sig figs -> multiples of 10
	expect_false(all(sfp$BICc %% 10 == 0))                  # preserved keeps the units digit
})

test_that("tableCovariateModelBuilding rejects invalid enum argument values", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableCovariateModelBuilding(mb_project_cossac, filterCriteria = "bogus"))
	expect_error(tableCovariateModelBuilding(mb_project_cossac, sortBy         = "bogus"))
	expect_error(tableCovariateModelBuilding(mb_project_cossac, roundMethod    = "bogus"))
})

test_that("tableCovariateModelBuilding sortBy pValue/index orders rows as documented", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# pValue: sort stays within each round, ascending in p.value
	pv <- tableCovariateModelBuilding(mb_project_cossac, sortBy = "pValue", prettyLabels = FALSE)
	grp <- .modelBuildingStepGroups(pv$Step, pv$Reference)
	for (g in unique(grp)) {
		vals <- pv$p.value[grp == g]
		expect_false(is.unsorted(vals[!is.na(vals)]))
	}

	# index: algorithm order
	idx <- tableCovariateModelBuilding(mb_project_cossac, sortBy = "index", prettyLabels = FALSE)
	expect_equal(idx$Run, paste("Model", 1:6))
})

test_that("tableCovariateModelBuilding filterCriteria = 'LL' drops the BICc columns", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	tbl <- tableCovariateModelBuilding(mb_project_cossac, filterCriteria = "LL", prettyLabels = FALSE)
	expect_false(any(c("BICc", "dBICc") %in% names(tbl)))
	expect_contains(names(tbl), c("LL", "dLL"))
})

test_that("tableCovariateModelBuilding prettyLabels renames headers without changing data", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	raw    <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = FALSE)
	pretty <- tableCovariateModelBuilding(mb_project_cossac, prettyLabels = TRUE)

	expect_equal(dim(raw), dim(pretty))
	expect_true("CovariateModel" %in% names(raw))
	expect_false("CovariateModel" %in% names(pretty))
	expect_true(all(c("Covariate\nModel", "Best\nModel") %in% names(pretty)))
	expect_equal(unname(as.list(raw)), unname(as.list(pretty)))
})

# --- tableRunHistory() -------------------------------------------------------

test_that("tableRunHistory rejects incompatible sortByCriteria x filterCriteria pair", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(
		tableRunHistory(syc_project, sortByCriteria = "LL", filterCriteria = "BICc"),
		"Incompatible arguments")
})

test_that("tableRunHistory sortByCriteria = 'BICc' orders rows by BICc", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	none <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "none")
	bicc <- tableRunHistory(syc_project, prettyLabels = FALSE, sortByCriteria = "BICc")

	expect_setequal(none$Run, bicc$Run)
	bicc_cols <- intersect(c("BICclin", "BICcis"), names(bicc))
	skip_if(length(bicc_cols) == 0, "no BICc column to verify sort")
	expect_true(any(vapply(bicc_cols,
												 function(c) !is.unsorted(bicc[[c]][!is.na(bicc[[c]])]), logical(1))))
})

test_that("tableRunHistory significantFigures vs significantFiguresPreserved diverge on numeric cols", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# BICc/LL ~1700-1800 (4 integer digits): the methods disagree at roundDigits = 2
	sf  <- tableRunHistory(syc_project, prettyLabels = FALSE, roundDigits = 2,
												 roundMethod = "significantFigures")
	sfp <- tableRunHistory(syc_project, prettyLabels = FALSE, roundDigits = 2,
												 roundMethod = "significantFiguresPreserved")

	num_cols <- intersect(names(Filter(is.numeric, sf)), names(sfp))
	skip_if(length(num_cols) == 0, "no numeric columns present")
	expect_true(any(vapply(num_cols,
												 function(c) !isTRUE(all.equal(sf[[c]], sfp[[c]])), logical(1))))
})

test_that("tableRunHistory error paths: missing mandatory arg and missing .syc file", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	expect_error(tableRunHistory(NULL), "mandatory")
	expect_error(tableRunHistory("test-projects/monolix/does_not_exist.syc"),
							 "Could not find Sycomore project")
})

test_that("tableRunHistory higher-precision roundDigits differs from roundDigits = 0", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	r0 <- tableRunHistory(syc_project, prettyLabels = FALSE,
												roundMethod = "monolixSuiteRounding", roundDigits = 0)
	r3 <- tableRunHistory(syc_project, prettyLabels = FALSE,
												roundMethod = "monolixSuiteRounding", roundDigits = 3)

	num_cols <- intersect(names(Filter(is.numeric, r0)), names(r3))
	skip_if(length(num_cols) == 0, "no numeric columns present")
	expect_true(any(vapply(num_cols,
												 function(c) !isTRUE(all.equal(r0[[c]], r3[[c]])), logical(1))))
})

# --- tablePopulationParameterEstimates() -------------------------------------

test_that("tablePopulationParameterEstimates roundMethod 'none' keeps full-precision strings", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_2)

	none    <- tablePopulationParameterEstimates(mono_project_2, prettyLabels = FALSE,
																							 roundMethod = "none")
	rounded <- tablePopulationParameterEstimates(mono_project_2, prettyLabels = FALSE,
																							 roundMethod = "significantFigures", roundDigits = 3)

	expect_type(none$Estimate, "character")
	expect_false(isTRUE(all.equal(none$Estimate, rounded$Estimate)))
})

test_that("tablePopulationParameterEstimates significantFiguresPreserved diverges from significantFigures", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_2)

	# omega_ka CV ~78.2 (2 integer digits), roundDigits = 1:
	#   significantFigures -> 80 ; significantFiguresPreserved -> 78
	sf  <- tablePopulationParameterEstimates(mono_project_2, prettyLabels = FALSE,
		includeCV = TRUE, roundDigits = 1, roundMethod = "significantFigures")
	sfp <- tablePopulationParameterEstimates(mono_project_2, prettyLabels = FALSE,
		includeCV = TRUE, roundDigits = 1, roundMethod = "significantFiguresPreserved")

	expect_equal(.popcell(sf,  "omega_ka", "CV"), "80")
	expect_equal(.popcell(sfp, "omega_ka", "CV"), "78")
})

test_that("tablePopulationParameterEstimates drops RSE/CI when the requested SE method is unavailable", {
	# prj08_iov2 ships only stochasticApproximation SE -> requesting linearization auto-drops
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_3)

	expect_warning(
		tbl <- tablePopulationParameterEstimates(mono_project_3, stdErrMethod = "linearization",
			includeRSE = TRUE, includeCI = TRUE, prettyLabels = FALSE),
		"Setting includeRSE and includeCI to FALSE")

	expect_false("RSE" %in% names(tbl))
	expect_false(any(grepl("^CI", names(tbl))))
})

test_that("tablePopulationParameterEstimates validates customUnits by name, independently of customAliases", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)   # project WITH results, so validation is what fails

	expect_error(
		tablePopulationParameterEstimates(mono_project_1, customUnits = "L"),
		"named vector or a 2-column dataframe")
	expect_error(
		tablePopulationParameterEstimates(mono_project_1,
			customUnits = data.frame(V_pop = "L")),
		"at least 2 columns")
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-MonolixHelpers.R")
