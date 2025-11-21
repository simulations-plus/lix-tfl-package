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
