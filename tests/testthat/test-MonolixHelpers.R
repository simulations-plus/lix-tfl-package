mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
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
	table = tableParameterEstimates(mono_project_1, mapping, std_err_method = "linearization")
	expect_all_false(is.na(table$RSE))
	expect_equal(table$RSE[1], 2.795)
	table = tableParameterEstimates(mono_project_1, mapping, std_err_method = "stochasticApproximation")
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
	table = tableParameterEstimates(mono_project_1, include_bootstrap = TRUE)
	expect_all_true(is.na(table$RSE))
	expect_all_true(is.na(table$Bootstrap_P2.5))
	expect_all_true(is.na(table$Bootstrap_Median))
	expect_all_true(is.na(table$Bootstrap_P97.5))

	# after bootstrap
	lixoftConnectors::runBootstrap(nbRuns = 2)
	table = tableParameterEstimates(mono_project_1, include_bootstrap = TRUE)
	expect_all_true(is.na(table$RSE))
	expect_all_false(is.na(table$Bootstrap_P2.5))
	expect_all_false(is.na(table$Bootstrap_Median))
	expect_all_false(is.na(table$Bootstrap_P97.5))
})

test_that("tableParameterEstimates include logicals", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_RSE = TRUE)
	expect_true("RSE" %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_RSE = FALSE)
	expect_false("RSE" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CI = TRUE)
	expect_true("CI_2.5_97.5" %in% names(table))
	expect_false("P2.5" %in% names(table))
	expect_false("P97.5" %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CI = FALSE)
	expect_false("CI_2.5_97.5" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CV = TRUE)
	expect_true("CV" %in% names(table))
	# CV values should be present for omega parameters (not NA)
	omega_rows = grep("omega_", table$Parameter)
	expect_true(any(!is.na(table$CV[omega_rows])))
	# CV parameter rows should NOT exist anymore
	expect_all_false(paste0(c("omega_Tlag", "omega_ka", "omega_V", "omega_Cl"), "_CV") %in% table$Parameter)

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CV = FALSE)
	expect_false("CV" %in% names(table))

	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_bootstrap = TRUE)
	expect_all_true(c("Bootstrap_P2.5", "Bootstrap_Median", "Bootstrap_P97.5") %in% names(table))
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_bootstrap = FALSE)
	expect_all_false(c("Bootstrap_P2.5", "Bootstrap_Median", "Bootstrap_P97.5") %in% names(table))
})

test_that("tableParameterEstimates wide format", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)

	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	# Test wide format with IIV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, use_wide_format = TRUE, include_IIV = TRUE)
	expect_true("FE_Estimate" %in% names(table))
	expect_true("IIV_SD" %in% names(table))
	expect_false("IIV" %in% table$Section)  # omega parameters should not appear as separate rows
	expect_true(all(table$Section == "FE"))

	# Test wide format with CV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, use_wide_format = TRUE, include_CV = TRUE, include_IIV = TRUE)
	expect_true("IIV_CV" %in% names(table))
	expect_true("IIV_SD" %in% names(table))

	# Test wide format with RSE
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, use_wide_format = TRUE, include_RSE = TRUE, include_IIV = TRUE)
	expect_true("FE_RSE" %in% names(table))
	expect_true("IIV_RSE" %in% names(table))

	# Test wide format without IIV
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, use_wide_format = TRUE, include_IIV = FALSE)
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
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CI = TRUE)
	expect_true("CI_2.5_97.5" %in% names(table))
	# CI values should have format "(value; value)"
	ci_values = table$CI_2.5_97.5[!is.na(table$CI_2.5_97.5)]
	if (length(ci_values) > 0) {
		expect_true(all(grepl("^\\(.*; .*\\)$", ci_values)))
	}

	# Test CI column in wide format (should be renamed to FE_CI)
	table = tableParameterEstimates(mono_project_1, nameMapping = NULL, include_CI = TRUE, use_wide_format = TRUE, include_IIV = TRUE)
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

# testing
# test logic of columns included with true/false
# test create mapping
# test omitting/reordering params
# test warning for wrong params
test_that("createNameMapping correct", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	expected = data.frame(Parameter = c("V_pop", "Cl_pop", "ka_pop"),
												Label = c("V", "Cl", "ka"),
												Unit = c("L", "L/h", ""))

	expect_equal(createNameMapping(c(Parameter = "V_pop", Label = "V", Unit = "L"),
																 c(Parameter = "Cl_pop", Label = "Cl", Unit = "L/h"),
																 c(Parameter = "ka_pop", Label = "ka")),
							 expected)

	expect_equal(createNameMapping(c(Parameter = "V_pop", Label = "V", Unit = "L"),
																 c(Unit = "L/h", Label = "Cl", Parameter = "Cl_pop"),
																 c(Label = "ka", Parameter = "ka_pop")),
							 expected)
})

test_that("createNameMapping errors", {
	withr::local_options(TFLMonolixSuite.verbose = TRUE)

	expect_error(createNameMapping(c(Parameter = "V_pop", Unit = "L")))
	expect_error(createNameMapping("Parameter"))
	expect_error(createNameMapping(c(Parameter = "V_pop", Unit = "L"), "Cl_pop"))
	expect_error(createNameMapping(c("V_pop", "V", "L")))
})
