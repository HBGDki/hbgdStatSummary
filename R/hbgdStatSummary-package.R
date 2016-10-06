if (getRversion() >= "2.15.1") {
  utils::globalVariables(unique(c(
    "type", ".", "ageweeks"
  )))
}

#' @import hbgd dplyr progress
#' @importFrom graphics hist
#' @importFrom stats qt sd
#' @importFrom utils data
#' @importFrom tidyr nest unnest
#' @importFrom magrittr extract2 set_names set_colnames
NULL
