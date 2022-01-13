# tRAT app functions =====

#' Set up a team readiness assessment test (tRAT) app
#' 
#' This functions makes it easy to setup a team readiness assessment test (tRAT) app folder. It creates the app script \code{app.R} and copies the \code{teams.xslx} (an Excel spreadsheet), \code{questions.xlsx} (an Excel spreadsheet) as well as the necessary google access key file for the \code{data_gs_title} google spreadsheet to the same directory. The provided \code{gs_key_file} must grant access to the google spreadsheet (\code{data_gs_title}). See the \href{https://tbltools.kopflab.org/articles/peer_evaluations.html}{peer evaluations vignette} for details on how to set up google credentials and generate a key file. Unless \code{check_gs_access = FALSE}, this function will call the \code{\link{tbl_check_gs_access}} function to confirm that the key file works and provides access to the requested google spreadsheet. All \code{app.R}, \code{teams.xlsx}, \code{questions.xlsx} can be edited and tested in the app directory before uploading the whole app to a shiny server. On the shiny server, the \code{teams.xlsx} and \code{questions.xlsx} will be read-only and all actual data will be stored in the google spreadsheet data file.
#' 
#' Use \link{tbl_test_team_rat} with the same \code{folder} as parameter to test run the tRAT application locally on your computer. It is recommended to always test the tRAT application locally before uploading it to the shiny application server.
#' 
#' @param folder target folder where to setup the tRAT (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @param data_gs_title name of the google spreadsheet that should be used for storing the tRAT data. This spreadsheet must already exist and the \code{gs_key_file} must grant access to it.
#' @param template_teams_file path to an excel (.xslx) file that contains the team information to use as template for the tRAT app (can be edited later)  - will use the package template by default
#' @param template_questions_file path to an excel (.xslx) file that contains the questions/answer key information to use as template for the tRAT app (can be edited later)  - will use the package template by default
#' @param gs_key_file path to your .json access key file for TBL google spreadsheets. See the \href{https://tbltools.kopflab.org/articles/peer_evaluations.html}{peer evaluations vignette} for details. This key file is safe to use on a secure shiny app server but be careful to never post this file anywhere publicly as it could be used to gain access to your TBL spreadsheets. Make sure to share the google spreadshhet for this tRAT with the \code{client_email} listed in the key file.
#' @param overwrite whether to overwrite the app in the target directory if it already exists
#' @param check_gs_access whether to confirm google spreadsheet access (using the \code{\link{tbl_check_gs_access}} function). Note that if this is set to \code{FALSE}, this function will NOT validate the \code{gs_key_file} and NOT check that the provided \code{data_gs_title} is a valid spreadsheet the key file grants access to.
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family tRAT functions
#' @export
tbl_setup_team_rat <- function(
  folder = "tRAT", data_gs_title = "tRAT", 
  template_teams_file = system.file(package = "tbltools", "extdata", "teams_template.xlsx"),
  template_questions_file = system.file(package = "tbltools", "extdata", "questions_template.xlsx"), 
  gs_key_file = NULL, overwrite = FALSE, check_gs_access = TRUE) {

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
  
  # process key file
  gs_key_file_save_path <- file.path(folder, "gs_key_file.json")
  no_key_file_msg <- "If you don't want to check access now and copy a key file to your tRAT app manually later, set 'gs_key_file = NULL' and 'check_gs_access = FALSE'."
  if (!is.null(gs_key_file)) {
    # check that the file exists
    if (!file.exists(gs_key_file))
      glue("key file does not exist at location '{gs_key_file}', do you have the correct path? ", no_key_file_msg) %>% 
      stop(call. = FALSE)
    
    # check that the file is a valid json file
    tryCatch(
      jsonlite::read_json(gs_key_file),
      error = function(e) {
        glue("key file is not valid JSON, error: ", e$message) %>% 
          stop(call. = FALSE)
      }
    )
    
    # check gs access with key file
    if (check_gs_access) {
      tbl_check_gs_access(data_gs_title = data_gs_title, gs_key_file = gs_key_file)
    }
    
    # copy key file
    glue("Info: copying service account key file to {gs_key_file_save_path}") %>% message()
    file.copy(gs_key_file, to = gs_key_file_save_path) 
  } else if (is.null(gs_key_file) && check_gs_access) {
    glue("cannot check google spreadsheet access without a 'gs_key_file'. ", no_key_file_msg) %>% 
      stop(call. = FALSE)
  }
  
  # check for teams files
  if(!file.exists(template_teams_file))
    glue("teams file '{template_teams_file}' does not exist") %>% stop(call. = FALSE)
  
  # try to read teams file
  tryCatch(teams <- read_excel(template_teams_file), error = function(e) {
    glue("could not read teams Excel file '{template_teams_file}': {e$message}") %>% 
      stop(call. = FALSE)
  })
  
  # copy teams file
  if (!file.exists(file.path(folder, "teams.xlsx")) || overwrite) {
    glue("Info: copying '{basename(template_teams_file)}' to {folder}/teams.xlsx") %>% message()
    file.copy(template_teams_file, to = file.path(folder, "teams.xlsx"), overwrite = TRUE)
  }
  
  # check teams file
  check_teams(teams)
  
  # check for questions file
  if(!file.exists(template_questions_file))
    glue("questions file '{template_questions_file}' does not exist") %>% stop(call. = FALSE)
  
  # try to read questions file
  tryCatch(questions <- suppressMessages(read_excel(template_questions_file)), error = function(e) {
    glue("could not read questions Excel file '{template_questions_file}': {e$message}") %>% 
      stop(call. = FALSE)
  })
  
  # copy questions file
  if (!file.exists(file.path(folder, "questions.xlsx")) || overwrite) {
    glue("Info: copying '{basename(template_questions_file)}' to {folder}/questions.xlsx") %>% message()
    file.copy(template_questions_file, to = file.path(folder, "questions.xlsx"), overwrite = TRUE)
  }
  
  # check questions file
  check_questions(questions)
  
  # generate function call
  glue("Info: creating {folder}/app.R application file and application content") %>% message()
  
  # md template files
  system.file(package = "tbltools", "extdata", "team_rat_template_files") %>% 
    list.files(pattern = "*\\.md$", full.names = TRUE, include.dirs = FALSE, recursive = TRUE) %>% 
    file.copy(to = folder, overwrite = TRUE)
  
  if (!dir.exists(file.path(folder, "www"))) dir.create(file.path(folder, "www"))
  system.file(package = "tbltools", "extdata", "team_rat_template_files", "www", "app.css") %>% 
    file.copy(to = file.path(folder, "www"), overwrite = TRUE)
  
  # generate function call parameters ======
  parameters <- 
    list(
      data_gs_title = data_gs_title,
      app_title = "tRAT"
    ) %>% {
      map2_chr(names(.), ., function(var, val) {
        val <- 
          if(is_quosure(val)) quo_text(val)
        else if (is.null(val)) "NULL"
        else if(is.numeric(val) || is.logical(val)) val
        else if (is.character(val)) str_c("\"", val, "\"")
        else stop("don't know how to process ", class(val))
        str_c(var, " = ", val)
      })
    } %>% 
    collapse(sep = ",\n\t")
  
  glue(
    "library(tbltools)",
    "tbl_run_team_rat(",
    "  {parameters}",
    ")", .sep = "\n") %>% 
    cat(file = file.path(folder, "app.R"))
  
  # copy evaluation template
  glue("Info: creating {folder}/evaluation.Rmd evaluation file") %>% message()
  
  system.file(package = "tbltools", "extdata", "tRAT_evaluation_template.Rmd") %>% 
    read_lines() %>% 
    collapse(sep = "\n") %>% 
    str_interp(list(data_gs_title = data_gs_title)) %>% 
    cat(file = file.path(folder, "evaluation.Rmd"))
  
  # finishd
  glue("Info: set up of tbltools' tRAT app in directory '{folder}' is complete.",
       "Please modify the files in '{folder}' as appropriate and ",
       "run tbl_test_team_rat('{folder}') before before uploading to shiny server.") %>% 
    message()
  
  return(invisible(folder))
}

#' Duplicate a tRAT app
#' 
#' This functions makes it easy to duplicate an existing tRAT by copying all relevant files into a new folder. You should change the google spreadsheet the new tRAT is linked to by specifying the \code{data_gs_title} parameter (if you don't it will warn you but allow it). To create a new tRAT from scratch, use \link{tbl_setup_team_rat} instead. 
#' 
#' @param template folder where the existing tRAT that is to be duplicated is located (path must be either relative to the current working directory or an absolute file path on the operating system).
#' @param folder target folder where to setup the duplicated tRAT (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @inheritParams team_rat_server
#' @inheritParams tbl_setup_team_rat
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family tRAT functions
#' @export
tbl_duplicate_team_rat <- function(template = "tRAT", folder = "tRAT2", data_gs_title = NULL, overwrite = FALSE, check_gs_access = TRUE) {
  
  # check old folder
  check_team_rat_folder(template)
  
  # check new folder
  if(!dir.exists(folder)) {
    glue("Info: creating app directory '{folder}'") %>% message()
    dir.create(folder, recursive = TRUE)
  } else if (file.exists(file.path(folder, "app.R"))) {
    glue("Info: an app already exists in folder '{folder}' ",
         "{if(overwrite) 'but will be overwritten' else '(use \"overwrite = TRUE\" to overwrite)'}") %>% 
      message()
    if (!overwrite) return(invisible(folder))
  }
  
  # copy files
  files <- list.files(template, all.files = TRUE, recursive = TRUE, full.names = TRUE)
  glue("Info: copying {length(files)} files from template '{template}' into '{folder}'...") %>% 
    message()
  purrr::map(files, file.copy, to = folder, overwrite = TRUE)
  
  # find data gs title
  app_lines <- readr::read_lines(file = file.path(folder, "app.R"))
  old_data_gs_title_line <- stringr::str_detect(app_lines, "data_gs_title")
  if (sum(old_data_gs_title_line) != 1) {
    stop("could not identify where in the app.R file the 'data_gs_title' parameter is located", call. = FALSE)
  }
  
  if (!is.null(data_gs_title)) {
    # switching to new title
    glue("Info: changing spreadsheet title to '{data_gs_title}'.") %>% message()
    app_lines[old_data_gs_title_line] <- sprintf("\tdata_gs_title = \"%s\",", data_gs_title)
    cat(app_lines, file = file.path(folder, "app.R"), sep = "\n")
    
    # update evaluation.Rmd
    if (file.exists(file.path(folder, "evaluation.Rmd"))) {
      readr::read_lines(file = file.path(folder, "evaluation.Rmd")) %>% 
        stringr::str_replace_all(
          "(data_gs_title ?= ?)\\\"([^\"]*)\\\"",
          sprintf("data_gs_title = \"%s\"", data_gs_title)) %>% 
        cat(file = file.path(folder, "evaluation.Rmd"), sep = "\n")
    }
    
  } else {
    # keeping old title
    data_gs_title <-
      app_lines[old_data_gs_title_line] %>% 
      stringr::str_match("data_gs_title ?= ?\\\"([^\"]*)\\\"") %>% 
      { .[,2] }
    sprintf("no new google spreadsheet provided, the old spreadsheet ('%s') will be reused - this can lead to unexpected behaviour and is NOT recommend, please make sure to change the google spreadsheet name manually in %s", data_gs_title, file.path(folder, "app.R")) %>%
      warning(immediate. = TRUE, call. = FALSE)
  }
  
  # check gs_access
  if (check_gs_access) {
    tbl_check_gs_access(folder = folder, data_gs_title = data_gs_title)
  }
  
  # finalize
  glue("Info: tRAT app from '{template}' is now fully duplicated in directory '{folder}'.",
       "Please modify the files in '{folder}' as appropriate and ",
       "run tbl_test_team_rat('{folder}') before before uploading to shiny server.") %>% 
    message()
  
  return(invisible(folder))
}

#' Test run tRAT app locally
#' 
#' This function starts a tRAT app previously set up using \link{tbl_setup_team_rat}. This provides a local version of the app before deploying it to a shiny apps server (using \link{tbl_deploy_team_rat}). Keep in mind that the test application writes data to the google spreadsheet and make sure to delete any test records from the google spreadsheet that may interfere with any teams' tRAT prior to providing the students with the link to the deployed tRAT app. 
#' 
#' @param folder folder where the tRAT app is located (see \link{tbl_setup_team_rat} for details)
#' @param ... optional parameters passed to \link[shiny]{runApp}
#' @family tRAT functions
#' @export
tbl_test_team_rat <- function(folder = "tRAT", ...) {
  check_team_rat_folder(folder)
  runApp(folder, ...)
}

# convenience function for app existence check
check_team_rat_folder <- function(folder) {
  if(!dir.exists(folder)) {
    glue("tRAT app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain a tRAT app ('app.R' is missing)") %>% 
      stop(call. = FALSE)
  }
}

#' Run a tRAT app
#'
#' This function starts the tRAT user interface. Note that this function is typically NOT called directly but indirectly by setting up the tRAT app using \link{tbl_setup_team_rat}, adjusting the files in the tRAT app folder, and test running the app using \link{tbl_test_team_rat}.
#'
#' @inheritParams tbl_setup_team_rat
#' @inheritParams team_rat_server
#' @inheritParams team_rat_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @family tRAT functions
#' @export
tbl_run_team_rat <- function(
  data_gs_title, 
  teams = read_excel("teams.xlsx"), 
  questions = suppressMessages(read_excel("questions.xlsx")),
  app_title = "tRAT", 
  auto_login_access_code = NULL, ..., 
  launch = FALSE) {
  
  # require specific key file location for simplicity
  gs_key_file <- "gs_key_file.json"
  
  # safety checks
  if (!is.data.frame(teams)) stop("teams data frame required", call. = FALSE)
  if (!is.data.frame(questions)) stop("questions frame required", call. = FALSE)
  if (!file.exists(gs_key_file)) glue("no key file found ('{gs_key_file}' is missing)") %>% stop(call. = FALSE)
  
  # start-up message
  glue(
    "\n***************************************************************",
    "\nInfo: launching Team Readiness Assessment Test GUI (version {packageVersion('tbltools')})...",
    "\nInfo: app title: '{app_title}'",
    "\nInfo: teams: {nrow(teams)} teams",
    "\nInfo: questions: {nrow(questions)} "
  ) %>% message()
  
  # generate app
  app <- shinyApp(
    ui = team_rat_ui(app_title = app_title),
    server = team_rat_server(
      teams = teams,
      questions = questions,
      data_gs_title = data_gs_title,
      gs_key_file = gs_key_file,
      auto_login_access_code = auto_login_access_code
    )
  )
  
  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}

#' Deploy tRAT app
#' 
#' Upload the tRAT app to a shiny server via \link[rsconnect]{rsconnect}. This uses the rsconnect function \link[rsconnect]{deployApp} to upload or update a peer evaluation and thus requires rsconnect credentials to be already set. See \link[rsconnect]{setAccountInfo} or the \href{https://shiny.rstudio.com/articles/shinyapps.html#configure-rsconnect}{configuration help} for details on how to set your credentials for the \href{https://www.shinyapps.io}{shiny app platform}.
#' @param folder folder where the tRAT app is located (see \link{tbl_setup_team_rat} for details)
#' @param appName name of the application for the web address on shiny.io. Can be provided manually but must be unique within an account. By default is guessed from the folder simply by removing special characters, making everything lower case, and replacing spaces with \code{_}.
#' @param ... additional optional parameters passed on to \link[rsconnect]{deployApp}
#' @family tRAT functions
#' @export
tbl_deploy_team_rat <- function(folder = "tRAT", appName = guess_from(folder), ...) {
  
  if(!dir.exists(folder)) {
    glue("tRAT app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain an app ('app.R' is missing)") %>% 
      stop(call. = FALSE)
  }
  
  # simplify app name
  guess_from <- function(x) {
    x %>% 
      stringr::str_to_lower() %>% 
      stringr::str_replace_all(fixed(" "), "_") %>% 
      stringr::str_remove_all("[$@#%!'\"&*(){}\\[\\]]")
  }
  
  tryCatch(
    deployApp(folder, appName = appName, ...),
    error = function(e) {
      glue("something went wrong trying to upload your tRAT app. ",
           "Please see the function help (?tbl_deploy_team_rat) ", 
           "and make sure that your shiny server credentials are set properly. ",
           "The shiny apps server returned the following error: ", e$message) %>% 
        stop(call. = FALSE)
    }
  )
}


# utility functions ====

# get data gs title from an app folder (works for both tRAT and peer evals)
get_app_data_gs_title <- function(folder) {
  # find data gs title
  app_lines <- readr::read_lines(file = file.path(folder, "app.R"))
  old_data_gs_title_line <- stringr::str_detect(app_lines, "data_gs_title")
  if (sum(old_data_gs_title_line) != 1) {
    stop("could not identify where in the app.R file the 'data_gs_title' parameter is located", call. = FALSE)
  }
  data_gs_title <-
    app_lines[old_data_gs_title_line] %>% 
    stringr::str_match("data_gs_title ?= ?\\\"([^\"]*)\\\"") %>% 
    { .[,2] }
  return(data_gs_title)
}

# data check & prep functions =====

# safety checks for teams data frame
check_teams <- function(teams) {
  
  # global vars:
  access_code <- n <- NULL
  
  # check for data frame
  if(!is.data.frame(teams))
    stop("teams must be a data frame", call. = FALSE)
  
  # check for required columns
  req_columns <- c("team", "access_code")
  if (length(missing <- setdiff(req_columns, names(teams))) > 0)
    glue("missing column(s) in teams: {collapse(missing, sep=', ')}") %>% 
    stop(call. = FALSE)
  
  # check for unique access codes
  not_unique <- teams %>% group_by(.data$access_code) %>% tally() %>% filter(.data$n > 1)
  if (nrow(not_unique) > 0) {
    glue("all team access codes must be unique, found not unique access code(s): ",
         "{collapse(not_unique$access_code, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  
  return(mutate(teams, access_code = as.character(access_code)))
}

# safety checks for questions data frame and pivot longer
prepare_team_rat_questions <- function(questions) {
  if (!all(c("question", "correct") %in% names(questions))) {
    stop("missing required 'question' or 'correct' columns", call. = FALSE)
  }
  
  if (any(duplicated(questions$question))) {
    stop("duplicate question IDs", call. = FALSE)
  }
  
  # format questions
  questions_all <- questions %>%
    select(.data$question:.data$correct) %>%
    # preserve order
    mutate(question = forcats::as_factor(.data$question)) %>%
    tidyr::pivot_longer(
      cols = -c(.data$question, .data$correct), 
      values_to = "option",
      values_drop_na = TRUE
    ) %>%
    # make question id column for joins
    mutate(
      question_id = paste0("q-", .data$question)
    ) %>% 
    # number ids
    group_by(question_id) %>%
    mutate(
      option_idx = 1:dplyr::n(),
      option = as.character(.data$option),
      correct = as.character(.data$correct) == .data$option
    ) %>%
    ungroup() %>%
    # cleanup
    select(-name)
  
  # make sure correct answer is included
  correct_check <- questions_all %>%
    group_by(question_id) %>%
    summarize(has_correct = any(.data$correct), .groups = "drop")
  
  if (!all(correct_check$has_correct)) {
    stop("correct answer is not one of the options in ", 
         paste(with(correct_check, question_id[!has_correct]), collapse = ", "), 
         call. = FALSE)
  }
  
  return(questions_all)
}

# combine team questions and answers
combine_team_rat_questions_and_answers <- function(questions, answers) {
  
  if (!all(c("team", "question_id", "option") %in% names(questions))) {
    stop("missing required 'team', 'question_id' or 'option' columns in questions", call. = FALSE)
  }
  
  if (!all(c("team", "question_id", "guess") %in% names(answers))) {
    stop("missing required 'team', 'question_id' or 'guess' columns in answers", call. = FALSE)
  }
  
  if (("team" %in% names(questions)) != ("team" %in% names(answers))) {
    stop("team column in one but not both questions and answers data frame, must be either in neither or both", call. = FALSE)
  }
  
  # add guess number to answers
  answers <- answers %>%
    # add guess nr
    group_by(team, question_id) %>%
    mutate(guess_nr = seq_along(guess)) %>%
    ungroup()
  
  # combine
  questions %>%
    left_join(answers, by = c("team", "question_id", "option" = "guess")) %>%
    group_by(question_id) %>%
    mutate(
      guessed = !is.na(guess_nr),
      complete = any(guessed & correct)
    ) %>%
    ungroup()
}

# data loading/saving functions ==========

# load rRat answers
# @param ss - spreadsheet
read_team_rat <- function(ss, access_code) {
  
  # global vars
  timestamp <- score <- NULL
  
  is_gs <- is(ss, "sheets_id")
  
  if (is_gs) {
    # refresh sheet
    gs <- googlesheets4::as_sheets_id(ss)
    worksheets <- googlesheets4::sheet_names(gs)
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
    data <- 
      tibble(
        question_id = character(0), 
        guess = character(0)
      )
  } else if (is_gs) {
    # does exist and is a google spreadsheet
    data <- gs %>% 
      # retrieve data
      googlesheets4::read_sheet(
        sheet = access_code, 
        col_types = "c"
      ) 
  } else {
    # does exist and is a local file
    data <- read_excel(ss, sheet = access_code)
  }
  
  # return
  return(unique(data))
}

# save tRAT data
save_team_rat <- function(gs, access_code, question_id, guess) {
  
  # global vars
  timestamp <- NULL
  
  # refresh sheet
  gs <- googlesheets4::as_sheets_id(gs)
  
  # add timestamp and submitted info
  glue("Info: adding guess '{guess}' for question {question_id} to tab '{access_code}'") %>% message()
  data <- data.frame(question_id = as.character(question_id), guess = as.character(guess))
  
  # check for spreadsheet
  if (!access_code %in% googlesheets4::sheet_names(gs)) {
    # first guess
    googlesheets4::sheet_write(data = data, ss = gs, sheet = access_code)
  } else {
    # add guess
    googlesheets4::sheet_append(ss = gs, data = data, sheet = access_code)
  }
 
  return(invisible(data)) 
}



# data retrieval functions =====

#' Fetch the tRAT data
#' 
#' Fetches the tRAT data from the google spreadsheet and reads it (using \link{tbl_read_team_rat_data}). For standard installations of the tRAT app, all defaults should be sufficient.
#' @inheritParams tbl_setup_team_rat
#' @inheritParams tbl_run_team_rat
#' @param data_gs_id optional alternative to the \code{data_gs_title}, a google spread sheet ID text or object (see \link[googlesheets4]{sheets_id}). If provided, it takes precedence over the \code{data_gs_title} parameter as the google spreadsheet doesn't need to be searched by name anymore (i.e. can be loaded faster).
#' @param folder folder where the tRAT app is located (relative to the location of the RMarkdown file if used in the latter context)
#' @param download_to location where the whole tRAT data sheet will be downloaded to for more efficient read access
#' @export
tbl_fetch_team_rat_data <- function(
  folder = ".", data_gs_title = get_app_data_gs_title(folder), data_gs_id = NULL,
  teams = read_excel(file.path(folder, "teams.xlsx")), 
  questions = suppressMessages(read_excel(file.path(folder, "questions.xlsx"))),
  gs_key_file = file.path(folder, "gs_key_file.json"),
  download_to = file.path(folder, "tRAT_data_downloaded.xlsx")) {
  
  # safety checks
  if (!is.data.frame(teams)) stop("teams data frame required", call. = FALSE)
  if (!is.data.frame(questions)) stop("questions data frame required", call. = FALSE)
  prepare_team_rat_questions(questions) # safety check before going through the trouble of downloading
  
  # google sheet
  if (is.null(data_gs_id) && is.null(data_gs_title))
    stop("a google spreadsheet must be identified either via data_gs_title or data_gs_id parameter, neither is provided", call. = FALSE)
  
  if (!is.null(data_gs_id) && is(data_gs_id, "sheets_id")) {
    message("Info: proceeding with provided google spreadsheet")
    gs <- data_gs_id
  } else {
    # authenticate and get gs via title
    gs <- tbl_check_gs_access(data_gs_title = data_gs_title, data_gs_id = data_gs_id, gs_key_file = gs_key_file)
  }
  
  # downloading data
  glue("Info: downloading data to '{download_to}'... ") %>% message(appendLF = FALSE)
  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  if(file.exists(download_to)) file.remove(download_to)
  result <- googledrive::drive_download(googledrive::as_id(gs), path = download_to)
  if(file.exists(download_to)) message("complete.") else message("failed.")
  
  # read data
  tbl_read_team_rat_data(folder = folder, teams = teams, questions = questions, download_file = download_to)
}


#' Read downloaded tRAT data
#' 
#' Read tRAT data that's already downloaded. Usually called indirectly via \link{tbl_fetch_team_rat_data} unless there is no reason to fetch the latest data from the google spreadsheet.
#' @inheritParams tbl_fetch_team_rat_data
#' @param download_file downloaded tRAT data file, created by \link{tbl_fetch_team_rat_data}
#' @export
tbl_read_team_rat_data <- function(
  folder = ".", 
  teams = read_excel(file.path(folder, "teams.xlsx")), 
  questions = suppressMessages(read_excel(file.path(folder, "questions.xlsx"))),
  download_file = file.path(folder, "tRAT_data_downloaded.xlsx")) {
  
  # safety checks
  if (!is.data.frame(teams)) stop("teams data frame required", call. = FALSE)
  if (!is.data.frame(questions)) stop("questions data frame required", call. = FALSE)
  access_code_prefix <- "team_"
  teams <- check_teams(teams) %>% 
    mutate(
      # make sure access code is textual
      access_code = paste0(access_code_prefix, access_code)
    )
  
  # path
  if (!file.exists(download_file))
    glue("tRAT data file '{download_file}' does not exist, ",
         "make sure to run tbl_fetch_team_rat_data() first ",
         "or adjust the 'download_file' parameter to point to a valid ",
         "tRAT data file") %>% 
    stop(call. = FALSE)
  
  # fetch data
  answers <- teams %>% 
    mutate(answers = purrr::map(access_code, ~read_team_rat(download_file, .x))) %>%
    select(team, answers) %>%
    unnest(answers)
  
  # combine
  questions %>%
    prepare_team_rat_questions() %>%
    tidyr::crossing(team = teams$team) %>%
    combine_team_rat_questions_and_answers(answers) %>%
    arrange(team, question, option_idx) %>%
    return()
}

#' Summarize tRAT data
#' 
#' Summarizes the tRAT data.
#' 
#' @param data the tRAT data frame retrieved by \link{tbl_fetch_team_rat_data}
#' @param guess_points points for each successive guess. The default is 1 point for getting the right answer on the first guess, 0.5 points on the second guess, 0.25 point on the third and 0 points thereafter.
#' @export
tbl_summarize_team_rat_data <- function(data, guess_points = c(1, 0.5, 0.25)) {
  data %>%
    left_join(
      tibble(guess_nr = seq_along(guess_points), points = guess_points),
      by = "guess_nr"
    ) %>%
    mutate(points = ifelse(!is.na(points), points, 0)) %>%
    group_by(team) %>%
    summarize(
      n_questions = length(unique(question_id)),
      n_answered = sum(correct & guessed),
      n_max_points = n_questions * !!guess_points[1],
      n_points = sum((correct & guessed) * points),
      .groups = "drop" 
    )
}

# visualization functions ====

#' Generate tRATs
#' 
#' This function generates visual tRATs based on the data retrieved by \link{tbl_fetch_team_rat_data} (the same function is used internally by the tRAT apps).
#' 
#' @param data the tRAT data frame retrieved by \link{tbl_fetch_team_rat_data}
#' @param correct the color for correct answers (green by default)
#' @param incorrect the color for incorrect answers (red by default)
#' @param unknown the color for yet unknown answers (gray by default)
#' @param width the relative width of the option boxes (from 0 to 1)
#' @param height the relative height of the optin boxes (from 0 to 1)
#' @export
tbl_generate_team_rat <- function(team_rat_data, correct = "#4DAF4A", incorrect = "#E41A1C", unknown = "#999999", width = 0.9, height = 0.9) {
  
  # summary
  team_sum <- tbl_summarize_team_rat_data(team_rat_data)
  
  # plot
  team_rat_data %>% 
    left_join(team_sum, by = "team") %>%
    mutate(
      color = dplyr::case_when(
        guessed & correct ~ !!correct,
        guessed ~ !!incorrect,
        TRUE ~ !!unknown
      ),
      label = dplyr::if_else(
        guessed, sprintf("%s (#%d)", option, guess_nr), option
      ),
      question = forcats::fct_rev(question),
      panel = sprintf("%s\nAnswered: %.0f / %.0f\nPoints: %.2f / %.0f",
                      team, n_answered, n_questions, n_points, n_max_points)
    ) %>%
    ggplot2::ggplot() + 
    ggplot2::aes(x = option_idx, y = question) + 
    ggplot2::geom_tile(
      map = ggplot2::aes(fill = color),
      width = width, height = height, alpha = 1
    ) + 
    ggplot2::geom_text(
      map = ggplot2::aes(label = label),
      size = 6
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 24)
    ) +
    ggplot2::facet_wrap(~panel)
}
