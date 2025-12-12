# alphbetically to be run first and test initialization of lixoftConnectors

# note: tests run alphabetically, thus explicitly test case of connectors not initialized here
# note: running tests directly vs with R CMD CHECK are different contexts in terms of connectors
# being initialized, which is why the specific message is not checked
test_that("ensureProjectLoaded initialize", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	project = "test-projects/monolix/PK_run01.mlxtran"
	.ensureProjectLoaded(project)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))
})
