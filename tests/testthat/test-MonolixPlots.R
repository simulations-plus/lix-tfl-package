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

################################################################################
# Pure helpers (no project / ggplot): math + name resolution + binning
################################################################################

test_that(".sdNameForEta maps an eta to its SD parameter name", {
	expect_equal(.sdNameForEta("eta_V", list(indivEstimate = "mode", variabilityLevel = NULL)),
							 "omega_V")
	# simulated at a level strips the level suffix
	expect_equal(.sdNameForEta("eta_V_OCC",
													 list(indivEstimate = "simulated", variabilityLevel = "OCC")), "omega_V")
	expect_equal(.sdNameForEta("eta_Cl", list(indivEstimate = "mode", variabilityLevel = NULL),
													 sd_prefix = "gamma_"), "gamma_Cl")
})

test_that(".sdComponentsForEta returns one component per level, omega+gammas when combined", {
	# selected level -> single component
	expect_equal(.sdComponentsForEta("eta_V_OCC",
		list(indivEstimate = "simulated", variabilityLevel = "OCC"), occ_vars = "OCC"), "omega_V")
	# combined, no occasions -> omega only
	expect_equal(.sdComponentsForEta("eta_V",
		list(indivEstimate = "mode", variabilityLevel = NULL), occ_vars = character(0)), "omega_V")
	# combined, one/two occasions -> omega + gamma_/gamma2_
	expect_equal(.sdComponentsForEta("eta_V",
		list(indivEstimate = "mode", variabilityLevel = NULL), occ_vars = "OCC"),
		c("omega_V", "gamma_V"))
	expect_equal(.sdComponentsForEta("eta_Cl",
		list(indivEstimate = "mode", variabilityLevel = NULL), occ_vars = c("OCC1", "OCC2")),
		c("omega_Cl", "gamma_Cl", "gamma2_Cl"))
})

test_that(".withSeed is reproducible and restores the global RNG stream", {
	expect_equal(.withSeed(42, runif(3)), .withSeed(42, runif(3)))   # same seed -> same draw

	set.seed(1); x1 <- runif(1)
	set.seed(1); invisible(.withSeed(42, runif(5))); x2 <- runif(1)
	expect_equal(x1, x2)                                             # global stream untouched
})

test_that(".halfNormalQuantile and .ordinal compute expected values", {
	expect_equal(.halfNormalQuantile(0.5), stats::qnorm(0.75))
	expect_equal(.ordinal(1),  "1st")
	expect_equal(.ordinal(2),  "2nd")
	expect_equal(.ordinal(3),  "3rd")
	expect_equal(.ordinal(11), "11th")   # teens are all "th"
	expect_equal(.ordinal(21), "21st")
	expect_equal(.ordinal(73), "73rd")
})

test_that(".leastSquaresBins assigns contiguous clusters to separate bins", {
	x <- c(1, 1.1, 10, 10.2, 50, 50.1)
	g <- .leastSquaresBins(x, nbins = 3)
	expect_length(g, 6)
	expect_equal(g[1], g[2]); expect_equal(g[3], g[4]); expect_equal(g[5], g[6])
	expect_equal(length(unique(g)), 3)

	expect_equal(length(unique(.leastSquaresBins(c(1, 2), nbins = 5))), 2)   # capped at n
	expect_true(all(.leastSquaresBins(x, nbins = 1) == 1L))                  # single bin
})

test_that(".makeBins dispatches on criteria and yields the requested bin count", {
	ls <- .makeBins(c(1, 1.1, 10, 10.2),
									binsSettings = list(is.fixedNbBins = TRUE, nbBins = 2))   # default leastsquare
	expect_equal(length(unique(ls)), 2)

	ew <- .makeBins(0:10, binsSettings = list(criteria = "equalwidth",
																						is.fixedNbBins = TRUE, nbBins = 2))
	expect_equal(length(unique(ew)), 2)

	es <- .makeBins(1:10, binsSettings = list(criteria = "equalsize",
																						is.fixedNbBins = TRUE, nbBins = 2))
	expect_equal(length(unique(es)), 2)
})

test_that(".extendToEdges prepends/appends edge rows per group", {
	df <- data.frame(xaxis = "time", split = "all", perc = "median",
									 x = c(1, 2, 3), xlo = c(0.5, 1.5, 2.5), xhi = c(1.5, 2.5, 3.5),
									 stringsAsFactors = FALSE)
	out <- .extendToEdges(df)
	expect_equal(nrow(out), 5)                       # 3 + 2 edges
	expect_equal(out$x[1], 0.5)                      # xlo of first
	expect_equal(out$x[nrow(out)], 3.5)             # xhi of last
	expect_equal(out$x[2:4], c(1, 2, 3))
})

test_that(".fineGrid resamples curves onto a fine x-grid, NULL for <2 points", {
	df <- data.frame(xaxis = "time", split = "all", perc = "median",
									 x = c(0, 1, 2), empirical = c(0, 10, 20),
									 ci_lower = c(0, 5, 10), ci_upper = c(0, 15, 30),
									 stringsAsFactors = FALSE)
	out <- .fineGrid(df, npts = 5)
	expect_equal(nrow(out), 5)
	expect_equal(range(out$x), c(0, 2))
	expect_equal(out$empirical[out$x == 1], 10)      # linear interpolation at midpoint
	expect_equal(out$ci_lower[out$x == 1], 5)

	one <- data.frame(xaxis = "time", split = "all", perc = "median",
										x = 1, empirical = 1, ci_lower = 0, ci_upper = 2, stringsAsFactors = FALSE)
	expect_null(.fineGrid(one))
})
################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-MonolixPlots.R")
