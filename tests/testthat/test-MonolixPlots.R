mono_project_1 = "test-projects/monolix/PK_run01.mlxtran"

################################################################################
# Stratification by a continuous covariate without user-defined groups
################################################################################
test_that("continuous stratifier without groups defaults to a median split", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)
	wt <- lixoftConnectors::getCovariateInformation()$covariate$WEIGHT

	strat <- .stratifyCovariates(split = list(list(name = "WEIGHT")))
	expect_equal(nlevels(strat$split), 2)
	lower <- strat$split == levels(strat$split)[1]
	expect_true(all(wt[lower] < stats::median(wt)))
	expect_true(all(wt[!lower] >= stats::median(wt)))

	# explicit breaks are still honoured
	br <- unname(stats::quantile(wt, 0.25))
	strat_b <- .stratifyCovariates(split = list(list(name = "WEIGHT", breaks = br)))
	expect_equal(sum(strat_b$split == levels(strat_b$split)[1]), sum(wt < br))
})

test_that("a constant continuous covariate gives a single group", {
	# min == median == max --> one break; cut() would read it as a number of
	# intervals and create empty groups
	covData <- data.frame(id = 1:3, WT = c(4, 4, 4))
	strat <- .splitCovariates(covData, covtypes = list(WT = "continuous"),
														stratifiers = list(list(name = "WT", breaks = 4)))
	expect_equal(nlevels(strat$split), 1)
	expect_equal(as.vector(table(strat$split)), 3)
})

test_that("plots accept a continuous covariate in split/color without groups", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)
	# used to fail with "Missing mandatory fields : breaks"
	plots <- list(
		suppressMessages(plotQQStandardizedRandomEffects(stratify = list(split = "WEIGHT"))),
		suppressMessages(plotQQStandardizedRandomEffects(stratify = list(color = "WEIGHT"))),
		suppressMessages(plotAbsIWRESScatterPlot(stratify = list(split = "WEIGHT"))),
		suppressMessages(plotAbsIWRESScatterPlot(stratify = list(color = "WEIGHT"))))
	for (p in plots) {
		expect_s3_class(p, "ggplot")
		expect_no_error(suppressMessages(ggplot2::ggplot_build(p)))
	}
})

################################################################################
# plotQQStandardizedRandomEffects() legend / labels
################################################################################
test_that("plotQQStandardizedRandomEffects shows no shape key when points are coloured", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	.ensureProjectLoaded(mono_project_1)
	shape_mapped <- function(p) any(vapply(p$layers, function(l) "shape" %in% names(l$mapping), logical(1)))

	p_plain <- suppressMessages(plotQQStandardizedRandomEffects())
	expect_true(shape_mapped(p_plain))   # single "Empirical quantiles" key
	expect_equal(p_plain$labels$y, "Empirical quantiles of\nstandardized random effects")

	# coloured: the colour keys identify the points, no black "Empirical quantiles" key
	p_col <- suppressMessages(plotQQStandardizedRandomEffects(stratify = list(color = "SEX")))
	expect_false(shape_mapped(p_col))
	expect_no_message(ggplot2::ggplot_build(p_col))
})

################################################################################
# Stratification by an occasion in a project without covariates
# (fixture: doseIntervals_as_Occ, occasion DOSE_OCC from Data Formatting, no
#  covariates; no results needed for the stratification helpers)
################################################################################
test_that("occasion split works in a project without covariates", {
	withr::local_options(TFLMonolixSuite.verbose = FALSE)
	suppressMessages(.ensureProjectLoaded("test-projects/monolix/data_formatting/doseIntervals_as_Occ.mlxtran"))
	expect_null(lixoftConnectors::getCovariateInformation()$covariate)   # precondition

	# used to fail with 'could not find function ".getSubjocc"'
	split <- .checkSplit(list(list(name = "DOSE_OCC")), "split")
	expect_equal(split[[1]]$name, "DOSE_OCC")

	strat <- .stratifyCovariates(split = list(list(name = "DOSE_OCC")))
	expect_gt(nlevels(strat$split), 1)
	expect_true(all(grepl("^DOSE_OCC: ", levels(strat$split))))

	suppressMessages(.ensureProjectLoaded(mono_project_1))   # back to the project used above
})
