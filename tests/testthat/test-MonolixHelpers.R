mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
mono_project_3 = "test-projects/monolix/prj08_iov2.mlxtran"
mono_project_4 = "test-projects/monolix/prj9_iov2_bootstrap.mlxtran"

syc_project = "test-projects/monolix/PK_models.syc"

test_that("tableRunHistory project", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	table = tableRunHistory(syc_project)
	expect_equal(dim(table), c(2,5))
	expect_equal(table$Run, c("PK_run01", "PK_run02"))
	expect_equal(table$Path, c(basename(mono_project_1), basename(mono_project_2)))
	expect_equal(table$Parent, c("", "PK_run01"))
})

test_that("tableRunHistory run likelihood", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	# first run likelihood tasks for monolix project, linearization for project 1
	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	suppressMessages(lixoftConnectors::runLogLikelihoodEstimation(linearization = TRUE))
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableRunHistory(syc_project)
	expect_equal(dim(table), c(2,7))
	expect_equal(table$Run, c("PK_run01", "PK_run02"))
	expect_contains(names(table), c("LLlin", "BICclin"))
	expect_disjoint(names(table), c("LLis", "BICcis", "dLLlin", "dLLis"))

	# now run additional likelihood tasks, linearization and IS for project 2
	.ensureProjectLoaded(mono_project_2)
	lixoftConnectors::runPopulationParameterEstimation()
	suppressMessages(lixoftConnectors::runLogLikelihoodEstimation(linearization = TRUE))
	lixoftConnectors::runLogLikelihoodEstimation(linearization = FALSE)
	on.exit(unlink(tools::file_path_sans_ext(mono_project_2), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableRunHistory(syc_project)
	expect_equal(dim(table), c(2,11))
	expect_equal(table$Run, c("PK_run01", "PK_run02"))
	expect_contains(names(table), c("LLlin", "LLis", "BICclin", "BICcis", "dLLlin"))
	expect_disjoint(names(table), "dLLis")
	expect_equal(table$dLLlin, c(NA, -20.075))
})

test_that("tableParameterEstimates basic", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	mapping_pop = data.frame(Parameter = c("Tlag_pop", "ka_pop", "V_pop", "Cl_pop"),
													 Label = c("Tlag", "ka", "V", "Cl"),
													 Unit = c("h", "", "L", "L/h"))
	mapping_omega = data.frame(Parameter = c("omega_Tlag", "omega_ka", "omega_V", "omega_Cl"),
														 Label = c("w_Tlag", "w_ka", "w_V", "w_Cl"),
														 Unit = c("-T", "-ka", "-V", "-Cl"))
	mapping_error = data.frame(Parameter = c("a", "b"),
														 Label = c("const err", "prop err"),
														 Unit = c("aaa", "bbb"))

	# verify column RSE appears but is NA without standard errors being run
	# verify only requested parameters included in specified order,
	# but sections of table remain Fixed, Random, Error irrespectively

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table1 = tableParameterEstimates(mono_project_1, mapping_pop)
	expect_equal(table1$Parameter, mapping_pop$Label)
	expect_equal(table1$Unit, mapping_pop$Unit)
	expect_all_equal(table1$RSE, NA)
	expect_all_equal(table1$Section, "FE")
	expect_contains(names(table1), "Estimate")

	table2 = tableParameterEstimates(mono_project_1, rbind(mapping_pop, mapping_omega))
	expect_equal(table2$Parameter, c(mapping_pop$Label, mapping_omega$Label))
	expect_equal(table2$Unit, c(mapping_pop$Unit, mapping_omega$Unit))
	expect_all_equal(table2$RSE, NA)
	expect_equal(table2$Section, c(rep("FE", nrow(mapping_pop)), rep("IIV", nrow(mapping_omega))))

	table3 = tableParameterEstimates(mono_project_1, rbind(mapping_pop, mapping_error, mapping_omega))
	expect_equal(table3$Parameter, c(mapping_pop$Label, mapping_omega$Label, mapping_error$Label))
	expect_equal(table3$Unit, c(mapping_pop$Unit, mapping_omega$Unit, mapping_error$Unit))
	expect_all_equal(table3$RSE, NA)
	expect_equal(table3$Section, c(rep("FE", nrow(mapping_pop)),
																		 rep("IIV", nrow(mapping_omega)),
																		 rep("RES_ERR", nrow(mapping_error))))

	table4 = tableParameterEstimates(mono_project_1, NULL)
	expect_equal(table4$Parameter, c(mapping_pop$Label, mapping_omega$Parameter, mapping_error$Parameter))
	expect_false("Unit" %in% names(table4))
	expect_all_equal(table4$RSE, NA)
})

test_that("tableParameterEstimates with std errs", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	mapping = data.frame(Parameter = c("Tlag_pop", "ka_pop", "omega_Tlag", "omega_ka", "a"),
												 Label = c("Tlag", "ka", "w_Tlag", "w_ka", "a err"),
												 Unit = c("h", "", "-T", "-ka", "aaa"))

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	suppressMessages(lixoftConnectors::runStandardErrorEstimation(linearization = TRUE))
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableParameterEstimates(mono_project_1, mapping)
	expect_equal(table$Parameter, mapping$Label)
	expect_equal(table$Unit, mapping$Unit)
	expect_all_false(is.na(table$RSE))
	expect_contains(names(table), "Estimate")

	# verify can select std err method and defaults to linearization if not specified
	suppressMessages(lixoftConnectors::runStandardErrorEstimation(linearization = FALSE))
	table = tableParameterEstimates(mono_project_1, mapping, stdErrMethod = "linearization")
	expect_all_false(is.na(table$RSE))
	expect_equal(table$RSE[1], 2.795)
	table = tableParameterEstimates(mono_project_1, mapping, stdErrMethod = "stochasticApproximation")
	expect_all_false(is.na(table$RSE))
	expect_equal(table$RSE[1], 2.886)
	expect_message(tableParameterEstimates(mono_project_1, mapping), "multiple methods")
	table = tableParameterEstimates(mono_project_1, mapping)
	expect_all_false(is.na(table$RSE))
	expect_equal(table$RSE[1], 2.795)
})

test_that("tableParameterEstimates include logicals", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	# before bootstrap run (nor std errors)
	table <- tableParameterEstimates(mono_project_1, includeBootstrap = TRUE, prettyLabels = FALSE)
	# Find dynamic bootstrap CI column name
	boot_ci_col <- grep("^Boot_CI", names(table), value = TRUE)

	expect_true(all(is.na(table$RSE)))
	expect_true(all(is.na(table$Boot_Median)))
	expect_true(all(is.na(table[[boot_ci_col]])))  # Dynamic CI column
	expect_true(all(is.na(table$Boot_RSE)))

	# after bootstrap
	lixoftConnectors::runBootstrap(nbRuns = 2)
	table <- tableParameterEstimates(mono_project_1, includeBootstrap = TRUE, prettyLabels = FALSE)

	boot_ci_col <- grep("^Boot_CI", names(table), value = TRUE)

	expect_true(all(is.na(table$RSE)))  # Still no standard errors
	expect_false(any(is.na(table$Boot_Median)))
	expect_false(any(is.na(table[[boot_ci_col]])))  # CI column has values
	expect_false(any(is.na(table$Boot_RSE)))
})

test_that("tableParameterEstimates include logicals", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeRSE = TRUE, prettyLabels = FALSE)
	expect_true("RSE" %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeRSE = FALSE, prettyLabels = FALSE)
	expect_false("RSE" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCI = TRUE, prettyLabels = FALSE)
	expect_true("CI_2.5_97.5" %in% names(table))
	expect_false("P2.5" %in% names(table))
	expect_false("P97.5" %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCI = FALSE, prettyLabels = FALSE)
	expect_false("CI_2.5_97.5" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCV = TRUE, prettyLabels = FALSE)
	expect_true("CV" %in% names(table))
	# CV values should be present for omega parameters (not NA)
	omega_rows = grep("omega_", table$Parameter)
	expect_true(any(!is.na(table$CV[omega_rows])))
	# CV parameter rows should NOT exist anymore
	expect_all_false(paste0(c("omega_Tlag", "omega_ka", "omega_V", "omega_Cl"), "_CV") %in% table$Parameter)

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCV = FALSE, prettyLabels = FALSE)
	expect_false("CV" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeBootstrap = TRUE, prettyLabels = FALSE)
	expect_all_true(c("Bootstrap_P2.5", "Bootstrap_Median", "Bootstrap_P97.5") %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeBootstrap = FALSE, prettyLabels = FALSE)
	expect_all_false(c("Bootstrap_P2.5", "Bootstrap_Median", "Bootstrap_P97.5") %in% names(table))
})

test_that("tableParameterEstimates wide format", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	# Test wide format with IIV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, useWideFormat = TRUE, includeIIV = TRUE, prettyLabels = FALSE)
	expect_true("FE_Estimate" %in% names(table))
	expect_true("IIV_SD" %in% names(table))
	expect_false("IIV" %in% table$Section)  # omega parameters should not appear as separate rows
	expect_true(all(table$Section == "FE"))

	# Test wide format with CV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, useWideFormat = TRUE, includeCV = TRUE, includeIIV = TRUE, , prettyLabels = FALSE)
	expect_true("IIV_CV" %in% names(table))
	expect_true("IIV_SD" %in% names(table))

	# Test wide format with RSE
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, useWideFormat = TRUE, includeRSE = TRUE, includeIIV = TRUE, , prettyLabels = FALSE)
	expect_true("FE_RSE" %in% names(table))
	expect_true("IIV_RSE" %in% names(table))

	# Test wide format without IIV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, useWideFormat = TRUE, includeIIV = FALSE, , prettyLabels = FALSE)
	expect_false("IIV_SD" %in% names(table))
	expect_false("IIV_CV" %in% names(table))
	expect_false("IIV_RSE" %in% names(table))
})

test_that("tableParameterEstimates CI combined format", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	# Test CI column in long format (should be combined)
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCI = TRUE, prettyLabels = FALSE)
	expect_true("CI_2.5_97.5" %in% names(table))
	# CI values should have format "(value; value)"
	ci_values = table$CI_2.5_97.5[!is.na(table$CI_2.5_97.5)]
	if (length(ci_values) > 0) {
		expect_true(all(grepl("^\\(.*; .*\\)$", ci_values)))
	}

	# Test CI column in wide format (should be renamed to FE_CI)
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, includeCI = TRUE, useWideFormat = TRUE, includeIIV = TRUE, prettyLabels = FALSE)
	expect_true("FE_CI" %in% names(table))
	expect_true("IIV_CI" %in% names(table))
	expect_false("CI_2.5_97.5" %in% names(table))
})

test_that("tableParameterEstimates section labels", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	# Test that section labels use abbreviated format
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL)
	unique_sections = unique(table$Section)

	# Should use abbreviated labels: FE, IIV, IOV, CORR, CORR_IIV, CORR_IOV, RES_ERR
	expect_true(all(unique_sections %in% c("FE", "IIV", "IOV", "CORR", "CORR_IIV", "CORR_IOV", "RES_ERR")))

	# Should NOT use verbose labels
	expect_false(any(unique_sections %in% c("Fixed effects", "Random effects", "Residual error")))
})

test_that("tableParameterEstimates errors", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	# no results
	expect_error(tableParameterEstimates(mono_project_2, NULL))

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	expect_error(tableParameterEstimates(mono_project_1, data.frame(Parameter = "V_pop", Unit = "L")))
	expect_error(tableParameterEstimates(mono_project_1, "Parameter"))
	expect_error(tableParameterEstimates(mono_project_1, c(Parameter = "V_pop", Label = "V", Unit = "L")))
})

# ------------------------------------------------------------------------------
# Tests for tableParameterEstimates() with IOV
test_that("tableParameterEstimates() includes IOV parameters in long format", {
	result <- tableParameterEstimates(
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

test_that("tableParameterEstimates() wide format with IOV creates IOV columns", {
	result <- tableParameterEstimates(
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
test_that("tableParameterEstimates() wide format can exclude IOV", {
	result <- tableParameterEstimates(
		projectFile = mono_project_3,
		includeIOV = FALSE,
		useWideFormat = TRUE
	)

	# Check that IOV columns do NOT exist
	expect_false(any(grepl("IOV", names(result))))

	# Check attributes
	expect_false(attr(result, "includeIOV"))
})

test_that("tableParameterEstimates() wide format with IOV and CV", {
	result <- tableParameterEstimates(
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
test_that("tableParameterEstimates() wide format with IOV, RSE, and CI", {
	result <- tableParameterEstimates(
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

test_that("tableParameterEstimates() wide format: includeIIV=FALSE, includeIOV=TRUE", {
	result <- tableParameterEstimates(
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
# Tests for tableParameterEstimates() with Bootstrap
test_that("tableParameterEstimates() includes bootstrap results in long format", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		useWideFormat = FALSE
	)

	# Check for bootstrap columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*Median", col_names)))
	expect_true(any(grepl("Boot.*CI", col_names)))
	expect_true(any(grepl("Boot.*RSE", col_names)))
})

test_that("tableParameterEstimates() bootstrap CI naming adapts to confidence level", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		useWideFormat = FALSE
	)

	# Check that CI column exists with dynamic naming (e.g., Boot_CI_2.5_97.5)
	col_names <- names(result)
	boot_ci_col <- grep("Boot.*CI.*[0-9]", col_names, value = TRUE)
	expect_length(boot_ci_col, 1)
	expect_true(grepl("Boot.*CI.*[0-9]+.*[0-9]+", boot_ci_col))
})

test_that("tableParameterEstimates() wide format with bootstrap creates bootstrap IIV columns", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		includeIIV = TRUE,
		useWideFormat = TRUE
	)

	# Check for bootstrap IIV columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*IIV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IIV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IIV.*RSE", col_names)))
})
test_that("tableParameterEstimates() wide format with bootstrap and IOV creates bootstrap IOV columns", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,  # Project with both bootstrap and IOV
		includeBootstrap = TRUE,
		includeIOV = TRUE,
		useWideFormat = TRUE
	)

	# Check for bootstrap IOV columns
	col_names <- names(result)
	expect_true(any(grepl("Boot.*IOV.*Median", col_names)))
	expect_true(any(grepl("Boot.*IOV.*CI", col_names)))
	expect_true(any(grepl("Boot.*IOV.*RSE", col_names)))
})

test_that("tableParameterEstimates() bootstrap CI values are properly rounded and combined", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "signif",
		useWideFormat = FALSE
	)

	# Find the bootstrap CI column
	boot_ci_col <- grep("Boot.*CI", names(result), value = TRUE)

	# Check that CI values are formatted as "(lower; upper)"
	ci_values <- result[[boot_ci_col]]
	non_na_values <- ci_values[!is.na(ci_values)]
	expect_true(all(grepl("^\\([^;]+;\\s[^)]+\\)$", non_na_values)))
})

test_that("tableParameterEstimates() bootstrap with different rounding methods", {
	result_monolix <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "monolix"
	)

	result_signif <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeBootstrap = TRUE,
		roundDigits = 3,
		roundMethod = "signif"
	)

	# Both should have bootstrap columns
	expect_true(any(grepl("Boot", names(result_monolix))))
	expect_true(any(grepl("Boot", names(result_signif))))
})

# ------------------------------------------------------------------------------
# Combined Tests: IOV + Bootstrap + Wide Format

test_that("tableParameterEstimates() comprehensive: wide format with all options", {
	result <- tableParameterEstimates(
		projectFile = mono_project_4,
		includeRSE = TRUE,
		includeIIV = TRUE,
		includeIOV = TRUE,
		includeCI = TRUE,
		includeCV = TRUE,
		includeBootstrap = TRUE,
		useWideFormat = TRUE,
		roundDigits = 3,
		roundMethod = "signif",
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

test_that("tableParameterEstimates() long format with IOV and bootstrap", {
	result <- tableParameterEstimates(
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
test_that("tableParameterEstimates() bootstrap columns appear in correct order", {
	result <- tableParameterEstimates(
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

test_that("tableParameterEstimates() wide format bootstrap IIV column order", {
	result <- tableParameterEstimates(
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
# tableModelBuilding()

mb_project_cossac <- "test-projects/monolix/warfarin_covariate3_project_COSSAC.mlxtran"
mb_project_scm    <- "test-projects/monolix/warfarin_covariate3_project_SCM_BICc.mlxtran"
mb_project_samba  <- "test-projects/monolix/warfarin_covariate3_project_SAMBA.mlxtran"

test_that("tableModelBuilding COSSAC (LRT) reports connector best model + LRT columns", {
	table <- tableModelBuilding(mb_project_cossac, prettyLabels = FALSE)

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

test_that("tableModelBuilding SCM (BICc) reports connector best model + Improved flag", {
	table <- tableModelBuilding(mb_project_scm, prettyLabels = FALSE)

	expect_equal(nrow(table), 16L)
	# BICc criterion: Improved flag, no LRT columns
	expect_contains(names(table), c("Tested", "Step", "Improved", "BestModel"))
	expect_false(any(c("p.value", "Significant") %in% names(table)))
	# best model comes straight from the connector (bestModel = Model 11)
	expect_equal(which(table$BestModel == "Yes"), 11L)
})

test_that("tableModelBuilding SAMBA derives best model by minimum BICc", {
	table <- tableModelBuilding(mb_project_samba, prettyLabels = FALSE)

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

test_that("tableModelBuilding SAMBA respects filterCriteria and sortBy", {
	# filterCriteria = "BICc" drops the LL/dLL columns
	table_bicc <- tableModelBuilding(mb_project_samba, filterCriteria = "BICc",
																	 prettyLabels = FALSE)
	expect_false(any(c("LL", "dLL") %in% names(table_bicc)))
	expect_contains(names(table_bicc), c("BICc", "dBICc"))

	# SAMBA has no steps, so sortBy = "BICc" orders the whole table globally and
	# the best model floats to row 1
	table_sorted <- tableModelBuilding(mb_project_samba, sortBy = "BICc",
																		 prettyLabels = FALSE)
	expect_equal(table_sorted$BICc, sort(table_sorted$BICc))
	expect_equal(table_sorted$BestModel[1], "Yes")
})

test_that("tableModelBuilding stepwise sorting stays within each step round", {
	# SCM does three forward rounds (vs Model 1, then Model 4, then Model 11).
	# Sorting by LL must order rows *within* each round, never merging rounds.
	table <- tableModelBuilding(mb_project_scm, sortBy = "LL", prettyLabels = FALSE)

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

test_that("tableModelBuilding errors with a clear message when no COSSAC/SCM/SAMBA results exist", {
	# For a project that never ran a covariate model-building search, the connector
	# returns a bare FALSE (not NULL/length 0) -> must not surface as the internal
	# "$ operator is invalid for atomic vectors" error.
	expect_error(
		tableModelBuilding("test-projects/monolix/prj08_iov2.mlxtran"),
		"No model building results found.*COSSAC, SCM, or SAMBA"
	)
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-MonolixHelpers.R")
