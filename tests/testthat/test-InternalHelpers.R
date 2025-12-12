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

	expect_message(.ensureProjectLoaded(NULL), "using project already loaded")
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
	expect_equal(.round_num(123.456, 2), "123.46")
	expect_equal(.round_num(123.456, 2, rounding_method = "monolix"), "123.46")
	expect_equal(.round_num(0.123456, 2), "0.12")
	expect_equal(.round_num(0.00123456, 2), "0.0012")
	expect_equal(.round_num(123, 2), "123")
	expect_equal(.round_num(123.456789, 4), "123.4568")
})

test_that("round_num rounding_method signif", {
	expect_equal(.round_num(123.456, 2, rounding_method = "signif"), "120")
	expect_equal(.round_num(0.123456, 2, rounding_method = "signif"), "0.12")
	expect_equal(.round_num(0.00123456, 2, rounding_method = "signif"), "0.0012")
	expect_equal(.round_num(123, 2, rounding_method = "signif"), "120")
	expect_equal(.round_num(123.456789, 4, rounding_method = "signif"), "123.5")
})

test_that("round_num rounding_method none", {
	expect_equal(.round_num(123.456, 2, rounding_method = "none"), 123.456)
	expect_equal(.round_num(0.123456, 0, rounding_method = "none"), 0.123456)
	expect_equal(.round_num(1/3, NA, rounding_method = "none"), 1/3)
	expect_equal(.round_num(123, NULL, rounding_method = "none"), 123)
})

test_that("round_num vector", {
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "monolix"),
							 c("123.46", "0.0012"))
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "signif"),
							 c("120", "0.0012"))
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
