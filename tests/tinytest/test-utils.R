source("helpers.R")

exit_if_not(requireNamespace("stringi", quietly = TRUE))

x <- "hi there ; this is a string"

expect_equal(
	{
		y <- x
		astgrepr:::my_stri_sub(y, 1, 1) <- "AAAA"
		y
	},
	{
		y <- x
		stringi::stri_sub(y, 1, 1) <- "AAAA"
		y
	}
)

expect_equal(
	{
		y <- x
		astgrepr:::my_stri_sub(y, 1, 3) <- "AAAA"
		y
	},
	{
		y <- x
		stringi::stri_sub(y, 1, 3) <- "AAAA"
		y
	}
)

expect_equal(
	{
		y <- x
		astgrepr:::my_stri_sub(y, 5, 10) <- "AAAA"
		y
	},
	{
		y <- x
		stringi::stri_sub(y, 5, 10) <- "AAAA"
		y
	}
)
