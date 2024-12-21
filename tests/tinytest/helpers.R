library(tinytest)
library(astgrepr)
register_tinytest_extension(
	"astgrepr",
	"expect_snapshot"
)
