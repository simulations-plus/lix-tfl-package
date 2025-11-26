mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"
mono_project_2 = "test-projects/monolix/PK_run02.mlxtran"
sim_project = "test-projects/simulx/sim_forest.smlx"

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

# note: tests run alphabetically, thus cannot explicitly test case of connectors not initialized here
# test_that("ensureProjectLoaded initialize", {
# 	expect_message(.ensureProjectLoaded(mono_project_1), "initialized")
# 	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
# })

test_that("ensureProjectLoaded same project loaded", {
	.ensureProjectLoaded(mono_project_1)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(mono_project_1), "Project already loaded")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
})

test_that("ensureProjectLoaded different project loaded", {
	.ensureProjectLoaded(mono_project_1)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(mono_project_2), "Loading new project")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_2))
})

test_that("ensureProjectLoaded NULL keeps same project loaded", {
	.ensureProjectLoaded(mono_project_1)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

	expect_message(.ensureProjectLoaded(NULL), "using project already loaded")
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))
})

test_that("ensureProjectLoaded change software", {
	.ensureProjectLoaded(mono_project_1)
	expect_equal(normalizePath(lixoftConnectors::getProjectSettings()$project), normalizePath(mono_project_1))

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

