#' @keywords internal
"_PACKAGE"

#' @importFrom rlang is_quosure quo !! quo_text sym enquo eval_tidy quo_text
#' @importFrom methods is
#' @importFrom tibble deframe tibble
#' @importFrom dplyr select rename mutate filter group_by ungroup arrange tally bind_rows everything starts_with ends_with left_join do data_frame summarize tbl_df
#' @importFrom tidyr nest unnest
#' @importFrom lubridate now ymd_hms
#' @importFrom purrr quietly map2 map2_chr quietly is_empty
#' @importFrom stringr str_c str_extract str_replace_na str_trim str_detect fixed str_interp str_replace
#' @importFrom readr write_rds read_rds cols col_character col_logical col_integer read_lines
#' @importFrom readxl read_excel excel_sheets
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData freezePane setColWidths addStyle saveWorkbook
#' @importFrom lifecycle deprecated
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