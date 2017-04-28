library(RCB)
context("Check that the deltas are correct against known data")

test_that("Checking the ouput against RCBInputExample for path P4", {
  # Prep data
  library(openxlsx)
  data <- read.xlsx ( "data/RCBInputExample.xlsx", sheet = "RCB Input" )
  test_output <- read.xlsx( "data/RCBInputExample.xlsx", sheet = "metric" )
  
  # Compare output deltas with the output in test_output
  package_test <- R.ASReml_RCB_Return(data,"P4") 
  test_output <- test_output[ , c("HEAD_ANALYSIS_ENTRY_CONCAT", 
                                  "COMPARE_ANALYSIS_ENTRY_CONCAT_1", 
                                  "MEAN_DELTA",
                                  "TRIM(TO_CHAR(ROUND(GRP_COMP_ACSS_LOC.P_VALUE,4),999990.9999))",
                                  "SE_DELTA") ]
  comp_table <- merge ( test_output, package_test$Deltas, 
                        by.x = c("HEAD_ANALYSIS_ENTRY_CONCAT", "COMPARE_ANALYSIS_ENTRY_CONCAT_1"),
                        by.y = c( "head", "comp" ),
                        all.x = FALSE,
                        all.y = FALSE )
  comp_table[, c("diff", "p.diff", "sed")] <- round(comp_table[, c("diff", "p.diff", "sed")], 4)
  
  # Expect the differences in rounding to be less than the precision of the rounding
  expect_identical(abs(comp_table$MEAN_DELTA - comp_table$diff ) < 0.00011, rep(TRUE, nrow(comp_table) ) )
})