# peer evaluation app functions =====

#' Set up a peer evaluation
#' 
#' This functions makes it easy to setup a peer evaluation app folder. It creates the app script \code{app.R} and copies the \code{roster_file} (an Excel spreadsheet) as well as the necessary google drive credentials for the \code{data_gsheet} to the same directory. It will ask for google drive credentials using \link[googlesheets]{gs_auth} and the entered credentials must have access to the \code{data_gsheet}. Both \code{app.R} and the \code{roster_file} can be edited and tested in the app directory before uploading the whole app to a shiny server. On the shiny server, the \code{roster_file} will be read only and all actual data will be stored in the google drive data file.
#' 
#' Use \link{tbl_test_peer_evaluation} with the same \code{folder} as parameter to test run the peer evaluation application locally on your computer.
#' 
#' @inheritParams peer_evaluation_server
#' @param folder target folder where to setup the peer evaluation (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @param roster_file path to an excel (.xslx) file that contains the student roster information - will use the package template by default
#' @param overwrite whether to overwrite the app in the target directory if it already exists
#' @inheritParams tbl_run_peer_evaluation
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family peer evaluation functions
#' @export
tbl_setup_peer_evaluation <- function(folder = "peer_evaluation", roster_file = system.file(package = "tbltools", "extdata", "roster_template.xlsx"), data_gs_title = "Peer Evaluation", gs_token = NULL, overwrite = FALSE) {
  
  # check for roster file
  if(!file.exists(roster_file))
    glue("roster file '{roster_file}' does not exist") %>% stop(call. = FALSE)
  
  # try to read roster file
  tryCatch(roster <- read_excel(roster_file), error = function(e) {
    glue("could not read roster Excel file '{roster_file}': {e$message}") %>% 
    stop(call. = FALSE)
  })
  
  # check for folder
  if(!dir.exists(folder)) {
    glue("Info: creating app directory '{folder}'") %>% message()
    dir.create(folder, recursive = TRUE)
  } else if (file.exists(file.path(folder, "app.R"))) {
    glue("Info: an app already exists in folder '{folder}' ",
         "{if(overwrite) 'but will be overwritten' else 'and will NOT be overwritten}") %>% 
      message()
    if (!overwrite) return(folder)
  }
  
  # copy roster file
  if (!file.exists(file.path(folder, "roster.xlsx")) || overwrite) {
    glue("Info: copying '{basename(roster_file)}' to {folder}/roster.xlsx") %>% message()
    file.copy(roster_file, to = file.path(folder, "roster.xlsx"), overwrite = TRUE)
  }
  
  # check roster file
  check_student_roster(roster)

  # google sheets authentication
  token <- try_to_authenticate(gs_token)
  
  # save token
  if (!file.exists(file.path(folder, "gs_token.rds")) || overwrite) {
    glue("Info: copying authentication token to {folder}/gs_token.rds") %>% message()
    if (!is.null(gs_token))
      # copy from location
      file.copy(gs_token, to = file.path(folder, "gs_token.rds"))
    else
      # copy from fresh authentication
      write_rds(x = token, path = file.path(folder, "gs_token.rds"))
  }
  
  # check google sheet presence
  gs <- try_to_fetch_google_spreadsheet(data_gs_title)
  
  # generate function call
  glue("Info: creating {folder}/app.R application file") %>% message()
  
  # generate function call parameters ======
  parameters <- 
    list(
      roster = quo(readxl::read_excel("roster.xlsx")),
      data_gs_title = data_gs_title,
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
tbl_run_peer_evaluation <- function(roster, data_gs_title, gs_token, app_title = "Peer Evaluation", points_per_teammate, ..., launch = FALSE) {
  
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
  
  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()
  
  # generate app
  app <- shinyApp(
    ui = peer_evaluation_ui(app_title = app_title),
    server = peer_evaluation_server(
      roster = roster,
      data_gs_title = data_gs_title,
      gs_token = gs_token,
      points_per_teammate = points_per_teammate
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

tbl_upload_peer_evaluation <- function() {
  
}

# maybe
tbl_gather_peer_evaluation_data <- function() {
  
}

#' function to generate random codes for students
#' @export
generate_access_codes <- function(n, length = 4) {
  sapply(1:n, function(i) {
    sample(1:36, length, replace = TRUE) %>% 
      sapply(function(x) if (x>26) x-27 else LETTERS[x]) %>% 
      paste(collapse = "")
  })
}

# safety utilties functions =====

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
  
  return(roster)
}


# authenticate with google server
try_to_authenticate <- function(gs_token) {
  # google sheets authentication
  message("Info: authenticating with google server... ", appendLF = FALSE)
  tryCatch({
    if (!is.null(gs_token))
      # authenticat quietly if token is provided
      token <- quietly(gs_auth)(token = gs_token, new_user = TRUE, cache=FALSE)
    else
      # allow authentication info messages
      token <- gs_auth(new_user = TRUE, cache=FALSE)
    message("complete.")
  },
  error = function(e) {
    glue("google spreadsheet authentication failed: {e$message}") %>% 
      stop(call. = FALSE)
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
load_peer_eval <- function(gs, access_code) {
  # refresh sheet
  gs <- gs_gs(gs)
  
  # check if tab exists
  if (!access_code %in% gs_ws_ls(gs)) {
    # does not exist
    return(NULL)
  } else {
    # does exist
    gs %>% 
      # retrieve data
      gs_read_csv(
        ws = access_code, 
        col_types = cols(
          timestamp = col_character(),
          submitted = col_logical(),
          short = col_character(),
          plus = col_character(),
          minus = col_character(),
          score = col_integer()
        )) %>% 
      # convert timestamp
      mutate(timestamp = ymd_hms(timestamp)) %>% 
      # filter only most recent entry
      filter(timestamp == max(timestamp)) %>% 
      return()
  }
  
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