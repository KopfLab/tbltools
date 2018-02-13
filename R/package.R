#' @keywords internal
"_PACKAGE"

#' @importFrom glue glue collapse
#' @importFrom dplyr select rename mutate filter group_by arrange
#' @importFrom stringr str_c
#' @importFrom readxl read_excel
#' @import shiny 
#' @importFrom shinyjs useShinyjs
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# quiets concerns of R CMD check about . that appears in pipelines 
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c("."))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}