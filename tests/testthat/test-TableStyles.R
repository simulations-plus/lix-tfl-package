# Direct unit tests for the helpers in R/TableStyles.R (no connector anywhere).
# Pure helpers first; flextable-dependent ones are guarded with skip_if_not_installed.

# --- pure helpers ------------------------------------------------------------

test_that(".soften_hex blends a colour toward white by `amount`", {
	expect_equal(.soften_hex("#000000", amount = 0.8), "#CCCCCC")   # 80% toward white
	expect_equal(.soften_hex("#000000", amount = 1),   "#FFFFFF")   # fully white
	expect_equal(toupper(.soften_hex("#336699", amount = 0)), "#336699")  # unchanged at 0
})

test_that(".hex_to_rgba formats rgba with 2-decimal alpha", {
	expect_equal(.hex_to_rgba("#FFD700"),             "rgba(255,215,0,0.20)")  # default alpha
	expect_equal(.hex_to_rgba("#000000", alpha = 0.5), "rgba(0,0,0,0.50)")
})

test_that(".is_html_output is FALSE outside an HTML knitr context", {
	expect_false(.is_html_output())
})

test_that(".generateAutoFootnotes returns nothing for empty attrs", {
	expect_equal(.generateAutoFootnotes(NULL),   character(0))
	expect_equal(.generateAutoFootnotes(list()), character(0))
})

test_that(".generateAutoFootnotes emits source and time-deviation footnotes", {
	src <- .generateAutoFootnotes(list(project_source = "proj.mlxtran", r_source = "script.R"))
	expect_length(src, 1)                                   # both lines in one entry
	expect_match(src, "Project source: proj.mlxtran")
	expect_match(src, "Script source: script.R")

	excluded <- data.frame(id = 3, ActualTime = 2.5, NominalTime = 2, .pct_deviation = 25,
												 check.names = FALSE, stringsAsFactors = FALSE)
	dev <- .generateAutoFootnotes(list(timeDeviationFiltered = TRUE,
																		 timeDeviationThreshold = 10,
																		 timeDeviationExcluded  = excluded))
	expect_length(dev, 1)
	expect_match(dev, "excluded from summary")
	expect_match(dev, "Subject 3")
	expect_match(dev, "t=2.5")

	# both detectors -> two footnote entries
	both <- .generateAutoFootnotes(list(timeDeviationFiltered = TRUE, timeDeviationThreshold = 10,
																			timeDeviationExcluded = excluded, project_source = "proj.mlxtran"))
	expect_length(both, 2)
})

test_that(".add_section_headers inserts headers for model-building tables, passes others through", {
	mb <- data.frame(Run = paste("Model", 1:3), Step = c("Base", "Forward", "Forward"),
									 LL = c(1, 2, 3), stringsAsFactors = FALSE)
	attr(mb, "table_type") <- "modelBuilding"
	res <- .add_section_headers(mb)

	expect_true(length(res$section_header_rows) > 0)
	expect_false("Step" %in% names(res$data.flex))                       # Step consumed into headers
	expect_equal(nrow(res$data.flex), nrow(mb) + length(res$section_header_rows))

	# a table without table_type/Section is returned unchanged
	plain <- data.frame(a = 1:2, b = 3:4)
	res2  <- .add_section_headers(plain)
	expect_equal(res2$data.flex, plain)
	expect_length(res2$section_header_rows, 0)
})

# --- flextable-dependent helpers ---------------------------------------------

test_that("styleTFL and the flextable helpers return flextables", {
	skip_if_not_installed("flextable")
	skip_if_not_installed("officer")

	df <- data.frame(Parameter = c("Cl", "V"), Estimate = c(1.5, 20.3), RSE = c(45, 120))

	expect_s3_class(styleTFL(df), "flextable")
	# colorBy + manual footnote paths still return a flextable
	expect_s3_class(styleTFL(df, colorBy = "RSE", footnote = "note"), "flextable")

	expect_s3_class(my_custom_theme(flextable::flextable(df)), "flextable")

	# subscript markers are consumed without error
	dsub <- data.frame(Parameter = c("C[[sub:max]]", "AUC"), Value = c("1", "2"),
										 stringsAsFactors = FALSE)
	expect_s3_class(.apply_alias_subscripts(flextable::flextable(dsub), dsub), "flextable")
})

################################################################################
# setwd("C:/Users/skollmann/Documents/git/lix-tfl-package")
# testthat::test_file("C:/Users/skollmann/Documents/git/lix-tfl-package/tests/testthat/test-TableStyles.R")
