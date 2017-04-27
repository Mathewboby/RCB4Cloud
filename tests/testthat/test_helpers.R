library(RCB)
context("Data checks and manipulation")

test_that("Testing the checkData function", {
  test_data_fields <- list(
    CROP_OBSRVTN_DETAIL_ID = "NUMVALUE",
    FIELD_ID = "FIELDID",
    FACTOR_1 = "FACTOR1",
    REP_ID = "REPID",
    Factortype.name = "EXPERIMENTALUNITID" )
  test_data <- data.frame ( NUMVALUE = 1, FIELDID = 1, FACTOR1 = 1, REPID = 1 )
  missing_factor <- "ERROR: Variable(s) EXPERIMENTALUNITID not in data header"
  expect_equal( checkData(test_data, test_data_fields), missing_factor)
})