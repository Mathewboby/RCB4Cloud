##
## The following code is from Christopher Leeds
## https://github.platforms.engineering/TADS/OutlierScreens/blob/master/R/model_select.R

#' Determine the formula strings to be used for modeling in the DSRscreen function.
#'
#' @note This function currenrly only accepts nested model structures.
#'
#' @author Christopher Leeds <christoper.leeds@monsanto.com>
#'
#' @param modeltype the analysis design.
#' @param response_value name of variable that stores the response value in our dataframe.
#' @param factor1 name of the 1st factor in the dataframe.
#' @param factor2 name of the 2nd factor in the dataframe.
#' @param factor3 name of the 3rd factor in the dataframe.
#' @param replicate_name name of the replicate variable in the dataframe.
#'
#' @return The linear model formula needed to calculate DSR for outlier screening.
model_select <- function(modeltype,
                         response_value,
                         factor1,
                         factor2,
                         factor3,
                         replicate_name) {

  # INITIALIZE output_object OBJECT
  output_object <- list(modeltype = modeltype,
                        lm = NULL,
                        error = F,
                        error_message = NULL)

  # store formulas in output_object object
  if (toupper(modeltype) == "CRD") {
    output_object$lm <- paste(response_value, " ~ ", factor1)
  } else if (toupper(modeltype) == "CRD2") {
    output_object$lm <- paste(response_value, " ~ ",
                              factor1, "+",
                              factor1, "/", factor2)
  } else if (toupper(modeltype) == "RCB") {
    output_object$lm <- paste(response_value, " ~ ",
                              factor1, " + ",
                              replicate_name)
  } else if (toupper(modeltype) == "RCB2") {
    output_object$lm <- paste(response_value, " ~ ",
                              factor1, " + ",
                              factor1, "/", factor2, "+",
                              replicate_name)
  } else if (toupper(modeltype) == "GUBD2") {
    output_object$lm <- paste(response_value, " ~ ",
                              factor1, " + ",
                              factor1, "/", factor2, "+",
                              replicate_name, "+",
                              replicate_name, ":", factor1)
  } else if (toupper(modeltype) == "GUBD3") {
    output_object$lm <- paste(response_value, " ~ ",
                              factor1, " + ",
                              factor1, "/", factor2, "+",
                              replicate_name, "+",
                              replicate_name, ":", factor1, "+",
                              replicate_name, ":", factor1, ":", factor2)
  } else {
    output_object$error <- T
    output_object$error_message <- "Valid analysis modeltype not selected.
    Valid Choices are: CRD, CRD2, RCB, RCB2, GUBD2, GUBD3."
  }

  # return output_object object
  return(output_object)
}

##
## The following code is from Steven Johnson
## https://github.platforms.engineering/TADS/SmartModel/blob/master/R/Functions.R

#' Sets the fixed and random formulas based on the analysis type
#' @inheritParams checkAnalysisType
#' @export
setFixedRandomEffects <- function(analysis_type){
  ###Specify the fixed, sparse and random formulas:
  switch(analysis_type,
         P1 = {
           print (" RCB1: Single LOC Multi REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1,  sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", REP_ID, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1"), envir=.GlobalEnv)
           assign("present", list(), envir=.GlobalEnv)
         },
         P2 = {
           print (" RCB1: Multi LOC Single REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~ ", FIELD_ID, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1"), envir=.GlobalEnv)
           assign("present", list(), envir=.GlobalEnv)
         },
         P3 = {
           print (" RCB1: Multi LOC SINGLE REP BLUP")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~ ", FIELD_ID, " + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1"), envir=.GlobalEnv)
           assign("present", list(), envir=.GlobalEnv)
         },
         P4 = {
           print (" RCB1: Multi LOC MULTI REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FIELD_ID, ":", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1"), envir=.GlobalEnv)
           assign("present", list(), envir=.GlobalEnv)
         },
         P5 = {
           print (" RCB1: Multi LOC MULTI REP BLUP")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FACTOR_1, "+", FIELD_ID, ":",
                                FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1"), envir=.GlobalEnv)
           assign("present", list(), envir=.GlobalEnv)
         },
         P6 = {
           print (" RCB2: Single LOC Multi REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, "+", FACTOR_1, ":",FACTOR_2, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", REP_ID, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1", "factor1:factor2"), envir=.GlobalEnv)
           assign("present", c("factor1", "factor2"), envir=.GlobalEnv)
         },
         P7 = {
           print (" RCB2: Multi LOC Multi REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, "+", FACTOR_1, ":", FACTOR_2, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~ ", FIELD_ID, "+",
                                FIELD_ID, ":", REP_ID, "+",
                                FIELD_ID, ":", FACTOR_1, "+",
                                FACTOR_1, ":", FACTOR_2, ":", FIELD_ID, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1", "factor1:factor2"), envir=.GlobalEnv)
           assign("present", c("factor1", "factor2"), envir=.GlobalEnv)
         },
         P8 = {
           print (" GUBD2: Single LOC Multi REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FACTOR_1, "+", FIELD_ID, ":",
                                FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1", "factor1:factor2"), envir=.GlobalEnv)
           assign("present", c("factor1", "factor2"), envir=.GlobalEnv)
         },
         P9 = {
           print (" GUBD2: Multi LOC Multi REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FACTOR_1, "+", FIELD_ID, ":",
                                FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("classify", c("factor1", "factor1:factor2"), envir=.GlobalEnv)
           assign("present", c("factor1", "factor2"), envir=.GlobalEnv)
         }
  )
}
