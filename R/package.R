#' @keywords internal
"_PACKAGE"

#' @importFrom rlang is_quosure quo !! quo_text
#' @importFrom glue glue collapse
#' @importFrom tibble deframe
#' @importFrom dplyr select rename mutate filter group_by arrange tally bind_rows everything starts_with ends_with left_join do data_frame
#' @importFrom lubridate now ymd_hms
#' @importFrom purrr quietly map2 map2_chr
#' @importFrom stringr str_c str_extract str_replace_na str_trim
#' @importFrom readr write_rds read_rds cols col_character col_logical col_integer
#' @importFrom readxl read_excel
#' @importFrom googlesheets gs_auth gs_title gs_gs gs_ws_ls gs_ws_new gs_add_row gs_read_csv
#' @import shiny 
#' @importFrom shinyjs useShinyjs hidden show hide inlineCSS 
#' @importFrom shinycssloaders withSpinner
#' @importFrom rsconnect deployApp
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