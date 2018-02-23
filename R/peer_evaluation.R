# peer evaluation app functions =====

#' Set up a peer evaluation
#' 
#' This functions makes it easy to setup a peer evaluation app folder. It creates the app script \code{app.R} and copies the \code{roster_file} (an Excel spreadsheet) as well as the necessary google drive credentials for the \code{data_gsheet} to the same directory. It will ask for google drive credentials using \link[googlesheets]{gs_auth} and the entered credentials must have access to the \code{data_gsheet}. Both \code{app.R} and the \code{roster_file} can be edited and tested in the app directory before uploading the whole app to a shiny server. On the shiny server, the \code{roster_file} will be read only and all actual data will be stored in the google drive data file.
#' 
#' Use \link{tbl_test_peer_evaluation} with the same \code{folder} as parameter to test run the peer evaluation application locally on your computer.
#' 
#' @inheritParams peer_evaluation_server
#' @param folder target folder where to setup the peer evaluation (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @param template_roster_file path to an excel (.xslx) file that contains the student roster information to use as template for the peer evaluation app (can be edited later)  - will use the package template by default
#' @param overwrite whether to overwrite the app in the target directory if it already exists
#' @inheritParams tbl_run_peer_evaluation
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family peer evaluation functions
#' @export
tbl_setup_peer_evaluation <- function(folder = "peer_evaluation", data_gs_title = "Peer Evaluation", 
                                      template_roster_file = system.file(package = "tbltools", "extdata", "roster_template.xlsx"), 
                                      gs_token = file.path(folder, "gs_token.rds"), overwrite = FALSE) {
  
  # check for roster file
  if(!file.exists(template_roster_file))
    glue("roster file '{template_roster_file}' does not exist") %>% stop(call. = FALSE)
  
  # try to read roster file
  tryCatch(roster <- read_excel(template_roster_file), error = function(e) {
    glue("could not read roster Excel file '{template_roster_file}': {e$message}") %>% 
    stop(call. = FALSE)
  })
  
  # check for folder
  if(!dir.exists(folder)) {
    glue("Info: creating app directory '{folder}'") %>% message()
    dir.create(folder, recursive = TRUE)
  } else if (file.exists(file.path(folder, "app.R"))) {
    glue("Info: an app already exists in folder '{folder}' ",
         "{if(overwrite) 'but will be overwritten' else '(use \"overwrite = TRUE\" to overwrite)'}") %>% 
      message()
    if (!overwrite) return(invisible(folder))
  }
  
  # copy roster file
  if (!file.exists(file.path(folder, "roster.xlsx")) || overwrite) {
    glue("Info: copying '{basename(template_roster_file)}' to {folder}/roster.xlsx") %>% message()
    file.copy(template_roster_file, to = file.path(folder, "roster.xlsx"), overwrite = TRUE)
  }
  
  # check roster file
  check_student_roster(roster)

  # google sheets authentication
  if (!is.null(gs_token) && file.exists(gs_token))
    token <- try_to_authenticate(gs_token)
  else 
    token <- try_to_authenticate()
  
  
  # save token
  save_path <- file.path(folder, "gs_token.rds")
  if (!file.exists(save_path) || overwrite) {
    glue("Info: copying authentication token to {folder}/gs_token.rds") %>% message()
    if (!is.null(gs_token) && file.exists(save_path))
      # copy from location
      file.copy(gs_token, to = save_path)
    else
      # copy from fresh authentication
      write_rds(x = token, path = save_path)
  }
  
  # check google sheet presence
  gs <- try_to_fetch_google_spreadsheet(data_gs_title)
  
  # generate function call
  glue("Info: creating {folder}/app.R application file") %>% message()
  
  # generate function call parameters ======
  parameters <- 
    list(
      data_gs_title = data_gs_title,
      roster = quo(readxl::read_excel("roster.xlsx")),
      gs_token = "gs_token.rds",
      app_title = "Peer Evaluation",
      points_per_teammate = 10
    ) %>% {
      map2_chr(names(.), ., function(var, val) {
        val <- 
          if(is_quosure(val)) quo_text(val)
          else if(is.numeric(val) || is.logical(val)) val
          else if (is.character(val)) str_c("\"", val, "\"")
          else stop("don't know how to process ", class(val))
        str_c(var, " = ", val)
      })
    } %>% 
    collapse(sep = ",\n\t")
    
  glue(
    "library(tbltools)",
    "tbl_run_peer_evaluation(",
    "  {parameters}",
    ")", .sep = "\n") %>% 
    cat(file = file.path(folder, "app.R"))
  
  # copy evaluation template
  glue("Info: creating {folder}/evaluation.Rmd evaluation file") %>% message()
  
  system.file(package = "tbltools", "extdata", "evaluation_template.Rmd") %>% 
    read_lines() %>% 
    collapse(sep = "\n") %>% 
    str_interp(list(data_gs_title = data_gs_title)) %>% 
    cat(file = file.path(folder, "evaluation.Rmd"))
  
  glue("Info: set up of tbltools' Peer Evaluation app in directory '{folder}' is complete.\n",
       "Please modify the files in '{folder}' as appropriate before uploading to shiny server.") %>% 
    message()
  return(invisible(folder))
}


#' Start the peer evaluation user interface
#'
#' This function starts the peer evaluation user interface. This function is typically not called directly but indirectly by setting up the peer evalution app using \link{tbl_setup_peer_evaluation}, adjusting the files in the peer evaluation app folder, and test running the app using \link{tbl_test_peer_evaluation}.
#'
#' @inheritParams peer_evaluation_server
#' @inheritParams peer_evaluation_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @family peer evaluation functions
#' @export
tbl_run_peer_evaluation <- function(data_gs_title, roster, gs_token, app_title = "Peer Evaluation", 
                                    points_per_teammate, auto_login_access_code = NULL, ..., launch = FALSE) {
  
  # safety checks
  if (missing(roster)) stop("roster data frame required", call. = FALSE)
 
  # start-up message
  glue(
    "\n***************************************************************",
    "\nInfo: launching Peer Evaluation GUI (version {packageVersion('tbltools')})...",
    "\nInfo: app title: '{app_title}'",
    "\nInfo: roster: {nrow(roster)} students in {length(unique(roster$team))} teams",
    "\nInfo: points per teammate: {points_per_teammate}"
  ) %>% message()

  # generate app
  app <- shinyApp(
    ui = peer_evaluation_ui(app_title = app_title),
    server = peer_evaluation_server(
      roster = roster,
      data_gs_title = data_gs_title,
      gs_token = gs_token,
      points_per_teammate = points_per_teammate,
      auto_login_access_code = auto_login_access_code
    )
  )
  
  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}


#' Test run peer evaluation locally
#' 
#' This function starts a peer evaluation app set up using \link{tbl_setup_peer_evaluation}.
#' 
#' @param folder folder where the peer evaluation app is located (see \link{tbl_setup_peer_evaluation} for details)
#' @param ... parameters passed to \link[shiny]{runApp}
#' @family peer evaluation functions
#' @export
tbl_test_peer_evaluation <- function(folder = "peer_evaluation", ...) {
  
  if(!dir.exists(folder)) {
    glue("peer evaluation app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain a peer evaluation app ('app.R' is missing)") %>% 
      stop(call. = FALSE)
  }
  
  runApp(folder, ...)
}

#' Deploy peer evaluation app
#' 
#' Upload the peer evaluation app to a shiny server via rsconnect. This requires rsconnect credentials to be already set --> see \link[rsconnect]{setAccountInfo}
#' @param folder folder where the peer evaluation app is located (see \link{tbl_setup_peer_evaluation} for details)
#' @param ... passed on to \link[rsconnect]{deployApp}
#' @family peer evaluation functionsde 
#' @export
tbl_deploy_peer_evaluation <- function(folder = "peer_evaluation", ...) {
  
  if(!dir.exists(folder)) {
    glue("peer evaluation app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain a peer evaluation app ('app.R' is missing)") %>% 
      stop(call. = FALSE)
  }
  
  deployApp(folder, ...)
}

# peer evals data functions -----

#' Fetch the peer evaluation data
#' 
#' Fetches the peer evaluation data from the google spreadsheet. For standard installations of the peer evaluation app, all defaults should be sufficient with only parameter \code{data_gs_title} requiring a specification.
#' 
#' @inheritParams tbl_run_peer_evaluation
#' @param folder folder where the peer evaluation app is located (relative to the location of the RMarkdown file if used in the latter context)
#' @export
tbl_fetch_peer_evaluation_data <- function(folder = ".", data_gs_title, 
                                           roster = read_excel(file.path(folder, "roster.xlsx")), 
                                           gs_token = file.path(folder, "gs_token.rds")) {
  
  # safety checks
  if (!is.data.frame(roster)) stop("roster data frame required", call. = FALSE)
  students <- check_student_roster(roster) %>% 
    # get team member
    group_by(team) %>% 
    mutate(n_team_mates = n() - 1) %>% 
    ungroup()
  try_to_authenticate(gs_token)
  gs <- try_to_fetch_google_spreadsheet(data_gs_title)
  
  # downloading data
  message("Info: downloading data... ", appendLF = FALSE)
  local_path <- file.path(folder, "pe_data_downloaded.xlsx")
  result <- quietly(gs_download)(gs, to = local_path, overwrite = TRUE)
  message(result$messages, appendLF = FALSE)
  
  # fetch student info
  access_code_prefix <- "id_"
  students <- students %>% mutate(access_code = str_c(access_code_prefix, access_code)) 
  pe_data <- students %>% 
    group_by(access_code) %>% 
    do({
      read_peer_eval(local_path, .$access_code) %>% 
        rename(evaluatee_access_code = access_code)
    })
  
  # merge students in
  pe_data <- students %>% left_join(pe_data, by = "access_code")
  
  # structure information and nest evaluations
  pe_data <- 
    pe_data %>% 
    mutate(
      started = !is.na(timestamp),
      submitted2 = ifelse(is.na(submitted), FALSE, submitted),
      submitted_timestamp = timestamp,
      self_evaluation = access_code == evaluatee_access_code
    ) %>% 
    # simplify column sorting
    select(-submitted, -timestamp) %>% rename(submitted = submitted2) %>% 
    # next data
    nest(evaluatee_access_code, self_evaluation, plus, minus, score, .key = "evaluations")
  
  # update timestamp (can't do inside mutate, problems with the NA)
  pe_data <- within(pe_data, submitted_timestamp[!submitted] <- NA)
  
  return(pe_data)
}

#' Summarize peer evaluation data
#' 
#' Summarizes the peer evaluation data. Preserves all roster information (last name, first name, team, + any custom fields).
#' 
#' @param data the peer evaluation data frame retrieved by \link{tbl_fetch_peer_evaluation_dat}
#' @param submitted_only only include evaluations that were actually submitted (rather than just saved)
#' @export
tbl_summarize_peer_evaluation_data <- function(data, submitted_only = FALSE) {
  
  # safety
  if (missing(data) || !is.data.frame(data))
    stop("no data frame supplied")
  
  # check for required columns
  req_columns <- c("access_code", "evaluations")
  if (length(missing <- setdiff(req_columns, names(data))) > 0)
    glue("missing column(s) in data frame: {collapse(missing, sep=', ')}") %>% 
    stop(call. = FALSE)
  
  summarize_evals <- . %>% sample() %>% na.omit() %>% collapse(sep = "\n\n") %>% { ifelse(is.null(.), NA_character_, .) }
  
  # make sure to preserve custom roster info
  roster <- data %>% select(-started, -submitted, -submitted_timestamp, -evaluations) %>% 
    select(evaluatee_access_code = access_code, everything())
  
  # summarize information
  data_sum <- data %>% 
    select(access_code, submitted, evaluations) %>%
    unnest(evaluations) %>% 
    filter(submitted | !submitted_only) %>% 
    select(-access_code, -submitted) %>% 
    group_by(evaluatee_access_code) %>%
    summarize(
      n_evaluations = sum(!self_evaluation, na.rm = TRUE),
      score_avg = ifelse(n_evaluations > 0, mean(score[!self_evaluation]), NA_real_),
      self_plus = plus[self_evaluation] %>% summarize_evals,
      self_minus = minus[self_evaluation] %>% summarize_evals,
      team_plus = plus[!self_evaluation] %>% summarize_evals,
      team_minus = minus[!self_evaluation] %>% summarize_evals
    ) 
  
  # return
  left_join(roster, data_sum, by = "evaluatee_access_code") %>% 
    mutate(n_evaluations = ifelse(is.na(n_evaluations), 0, n_evaluations))
}

#' Export peer evaluation data
#' 
#' Exports peer evaluation data - both raw data and data summary (see \link{tbl_summarize_peer_evaluation_data} for details).
#' 
#' @inheritParams tbl_summarize_peer_evaluation_data
#' @param filepath the excel file where to save the peer evaluation data summary
#' @param ... parameters passed on to \code{\link{tbl_summarize_peer_evaluation_data}}
#' @return returns the passed in data invisibly to permit use of this function inside pipelines
#' @export
tbl_export_peer_evaluation_data <- function(data, filepath = "pe_data_summary.xlsx", ...) {
  
  # safety
  if (missing(data) || !is.data.frame(data))
    stop("no data frame supplied")
  
  glue("Info: exporting peer evaluation data and data summary to '{filepath}'... ") %>% 
    message(appendLF = FALSE)
  
  # cell and header styles
  style <- createStyle(valign = "top", wrapText = TRUE)
  header_style <- createStyle(textDecoration = "bold", border="bottom", borderColour = "#000000", borderStyle = "medium")
  
  # data
  data_all <- data %>% unnest(evaluations)
  data_sum <- data %>% tbl_summarize_peer_evaluation_data(...)
  
  # work book
  wb <- createWorkbook()
  
  # summary work sheet
  ws <- addWorksheet(wb, "summary")
  writeData(wb, ws, data_sum, headerStyle = header_style)
  freezePane(wb, ws, firstRow = TRUE)
  setColWidths(wb, ws, cols = 1:ncol(data_sum), widths = "auto")
  addStyle(wb, ws, style, rows = 1:(nrow(data_sum) + 1), cols = 1:ncol(data_sum), gridExpand = TRUE, stack = TRUE)
  
  # data work sheet
  ws <- addWorksheet(wb, "data")  
  writeData(wb, ws, data_all, headerStyle = header_style)
  freezePane(wb, ws, firstRow = TRUE)
  setColWidths(wb, ws, cols = 1:ncol(data_all), widths = "auto")
  addStyle(wb, ws, style, rows = 1:(nrow(data_all) + 1), cols = 1:ncol(data_all), gridExpand = TRUE, stack = TRUE)
  
  # save data sheet
  saveWorkbook(wb, filepath, overwrite = TRUE)
  
  message("completed.")
  
  return(invisible(data))
}

# utility functions ====

#' Generate random access codes
#' 
#' Utility function to generate a list of random access codes that can be copied into an Excel column.
#' @param n how many access codes to generate
#' @param length how many characters for each acccess code
#' @return returns the access codes invisible
#' @export
tbl_generate_access_codes <- function(n, length = 4) {
  codes <- sapply(1:n, function(i) {
    sample(1:36, length, replace = TRUE) %>% 
      sapply(function(x) if (x>26) x-27 else LETTERS[x]) %>% 
      paste(collapse = "")
  }) 
  codes %>% cat(sep = "\n")
  return(invisible(codes))
}

# check functions =====

# safety checks for student roster data frame
check_student_roster <- function(roster) {
  # check for data frame
  if(!is.data.frame(roster))
    stop("student roster must be a data frame", call. = FALSE)
  
  # check for required columns
  req_columns <- c("last", "first", "access_code", "team")
  if (length(missing <- setdiff(req_columns, names(roster))) > 0)
    glue("missing column(s) in student roster: {collapse(missing, sep=', ')}") %>% 
    stop(call. = FALSE)
  
  # check for unique access codes
  not_unique <- roster %>% group_by(access_code) %>% tally() %>% filter(n > 1)
  if (nrow(not_unique) > 0) {
    glue("all access codes must be unique, found not unique access code(s): ",
         "{collapse(not_unique$access_code, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  
  return(mutate(roster, access_code = as.character(access_code)))
}


# authenticate with google server
try_to_authenticate <- function(gs_token = NULL) {
  # google sheets authentication
  automatic <- is.null(getOption('httr_oob_default')) || !getOption('httr_oob_default')
  tryCatch({
    if (!is.null(gs_token)) {
      # authenticat quietly if token is provided
      message("Info: authenticating with google server via token... ", appendLF = FALSE)
      token <- quietly(gs_auth)(token = gs_token, new_user = TRUE, cache=FALSE)
    } else {
      # allow authentication info messages
      glue("Info: authenticating with google server {if(automatic) 'automatically' else 'manually'}... ") %>% 
        message(appendLF = FALSE)
      token <- gs_auth(token = NULL, new_user = TRUE, cache=FALSE)
    }
    message("complete.")
  },
  error = function(e) {
    if (automatic && str_detect(e$message, fixed("Failed to create server"))) {
      glue("could not authenticate automatically, please run 'options(httr_oob_default = TRUE)' and try again to authenticate manually: {e$message}") %>% 
        stop(call. = FALSE)
    } else {
      glue("google spreadsheet authentication failed: {e$message}") %>% 
        stop(call. = FALSE)
    }
  })
  return(token)
}

# find google spreadsheet
try_to_fetch_google_spreadsheet <- function(gs_title) {
  message("Info: looking for spreadsheet... ", appendLF = FALSE)
  tryCatch(gs <- gs_title(gs_title), error = function(e) {
    glue("google spreadsheet with title '{gs_title}' could not be retrieved: {e$message}") %>% 
      stop(call. = FALSE)
  })
  return(gs)
}

# data loading/saving functions ==========

# load peer evaluation
# @param ss - spreadsheet
read_peer_eval <- function(ss, access_code) {
  
  is_gs <- is(ss, "googlesheet")
  
  if (is_gs) {
    # refresh sheet
    gs <- gs_gs(gs)
    worksheets <- gs_ws_ls(gs)
  } else {
    # make sure file exists
    if (!file.exists(ss)) 
      glue("provided spreadsheet is neither a google spreadsheet nor a valid path to a local excel file") %>% 
      stop(call. = FALSE)
    worksheets <- excel_sheets(ss)
  }
  
  # check if tab exists
  if (!access_code %in% worksheets) {
    # does not exist
    return(
      data_frame(
        timestamp = now(),
        submitted = logical(0),
        access_code = character(0),
        plus = character(0),
        minus = character(0),
        score = integer(0)
      )
    )
  } else if (is_gs) {
    # does exist and is a google spreadsheet
    data <- gs %>% 
      # retrieve data
      gs_read_csv(
        ws = access_code, 
        col_types = cols(
          timestamp = col_character(),
          submitted = col_logical(),
          access_code = col_character(),
          plus = col_character(),
          minus = col_character(),
          score = col_integer()
        )) %>% 
      # convert timestamp
      mutate(timestamp = ymd_hms(timestamp))
  } else {
    # does exist and is a local file
    data <- read_excel(ss, sheet = access_code)
  }
  
  # return
  data %>% 
    # make sure score is numeric
    mutate(score = quietly(as.numeric)(score)$result) %>% 
    # filter only most recent entry
    filter(timestamp == max(timestamp)) %>% 
    return()
}

# save peer evaluation
save_peer_eval <- function(gs, access_code, data, submitted = FALSE) {
  
  # refresh sheet
  gs <- gs_gs(gs)
  
  # add timestamp and submitted info
  data <- data %>% 
    mutate(
      timestamp = now("UTC"),
      submitted = submitted
    ) %>% 
    select(timestamp, submitted, everything()) %>% 
    as.data.frame()
  
  # check for spreadsheet
  if (!access_code %in% gs_ws_ls(gs)) {
    glue("Info: creating {nrow(data)} rows in new gs tab '{access_code}'") %>% message()
    gs_ws_new(gs, ws_title = access_code, input = data, trim = TRUE)
  } else {
    # add new rows
    glue("Info: adding {nrow(data)} rows to tab '{access_code}'") %>% message()
    gs_add_row(gs, ws = access_code, data)
  }
  
}