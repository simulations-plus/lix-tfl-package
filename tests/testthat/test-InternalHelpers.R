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

test_that("round_num vector", {
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "monolix"),
							 c("123.46", "0.0012"))
	expect_equal(.round_num(c(123.456, 0.00123456), 2, rounding_method = "signif"),
							 c("120", "0.0012"))
})

# note: each test file run in separate R session, thus connectors not initialized for first test
test_that("ensureProjectLoaded initialize", {
	project = "test-projects/monolix/PK_run01.mlxtran"
	expect_message(.ensureProjectLoaded(project), "initialized")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))
})

test_that("ensureProjectLoaded same project loaded", {
	project = "test-projects/monolix/PK_run01.mlxtran"

	.ensureProjectLoaded(project)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))

	expect_message(.ensureProjectLoaded(project), "Project already loaded")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))
})

test_that("ensureProjectLoaded different project loaded", {
	project1 = "test-projects/monolix/PK_run01.mlxtran"
	project2 = "test-projects/monolix/PK_run02.mlxtran"

	.ensureProjectLoaded(project1)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project1))

	expect_message(.ensureProjectLoaded(project2), "Loading new project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project2))
})

test_that("ensureProjectLoaded NULL keeps same project loaded", {
	project = "test-projects/monolix/PK_run01.mlxtran"

	.ensureProjectLoaded(project)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))

	expect_message(.ensureProjectLoaded(NULL), "using project already loaded")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(project))
})

test_that("ensureProjectLoaded change software", {
	mono_project = "test-projects/monolix/PK_run01.mlxtran"
	sim_project = "test-projects/simulx/sim_forest.smlx"

	.ensureProjectLoaded(mono_project)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project))

	# note can only get project for Monolix
	expect_message(.ensureProjectLoaded(sim_project), "Loading new project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$directory),
							 normalizePath("test-projects/simulx/sim_forest"))
})

test_that("ensureProjectLoaded bad input", {
	expect_message(.ensureProjectLoaded("fileDoesNotExist.mlxtran"), "ERROR")
	expect_message(.ensureProjectLoaded("badExtension.mlx"), "ERROR")
	expect_message(.ensureProjectLoaded("noExtension"), "ERROR")
	expect_message(.ensureProjectLoaded(""), "ERROR")
	expect_message(.ensureProjectLoaded(123), "ERROR")
})

test_that("checkProjectFile", {
	expect_no_error(.checkProjectFile("testfile.mlxtran", "monolix"))
	expect_no_error(.checkProjectFile("testfile.pkx", "pkanalix"))
	expect_no_error(.checkProjectFile("testfile.mlxtran", c("pkanalix", "monolix")))

	expect_error(.checkProjectFile("wrongsoftware.mlxtran", "pkanalix"))
	expect_error(.checkProjectFile("badextension.mlx", "monolix"))
	expect_error(.checkProjectFile("noextension", "pkanalix"))
	expect_error(.checkProjectFile("somefile.mlxtran", "badsoftware"))
})



