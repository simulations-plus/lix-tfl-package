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

	# first run likelihood tasks for monolix projects, delete folders later
	.ensureProjectLoaded(mono_project_1)
	lixoftConnectors::runPopulationParameterEstimation()
	suppressMessages(lixoftConnectors::runLogLikelihoodEstimation(linearization = TRUE))
	on.exit(unlink(tools::file_path_sans_ext(mono_project_1), recursive = TRUE), add = TRUE, after = FALSE)

	table = tableRunHistory(syc_project)
	expect_equal(dim(table), c(2,7))
	expect_equal(table$Run, c("PK_run01", "PK_run02"))
	expect_contains(names(table), c("LLlin", "BICclin"))
	expect_disjoint(names(table), c("LLis", "BICcis", "dLLlin", "dLLis"))

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
#	print(table)
})
