# peer evaluation app functions =====

#' Set up a peer evaluation app
#' 
#' This functions makes it easy to setup a peer evaluation app folder. It creates the app script \code{app.R} and copies the \code{roster_file} (an Excel spreadsheet) as well as the necessary google drive credentials for the \code{data_gs_title} to the same directory. Unless \code{check_gs_access = FALSE}, it will call the \code{\link{tbl_check_gs_access}} function and ask for google drive credentials using \link[googlesheets]{gs_auth}. The entered credentials (or provided \code{gs_token}) must have access to the google spreadsheet (\code{data_gs_title}). Both \code{app.R} and the \code{roster_file} can be edited and tested in the app directory before uploading the whole app to a shiny server. On the shiny server, the \code{roster_file} will be read-only and all actual data will be stored in the google drive data file.
#' 
#' Use \link{tbl_test_peer_evaluation} with the same \code{folder} as parameter to test run the peer evaluation application locally on your computer. It is recommended to always test the peer evaluation application locally before uploading it to the shiny application server.
#' 
#' Use \link{tbl_duplicate_peer_evaluation} to create a copy of an existing peer evaluation rather than setting one up from scratch.
#' 
#' @inheritParams peer_evaluation_server
#' @param folder target folder where to setup the peer evaluation (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @param template_roster_file path to an excel (.xslx) file that contains the student roster information to use as template for the peer evaluation app (can be edited later)  - will use the package template by default
#' @param gs_token path to a google spreadsheet oauth 2.0 authentication token file (see \link[httr]{Token-class}). If none is provided or the file does not exist yet, will ask for google drive credentials interactively to generate a token for the peer evaluation app. The token is safe to use on a secure shiny app server but be careful to never post this token file anywhere publicly as it could be used to gain access to your google drive.
#' @param overwrite whether to overwrite the app in the target directory if it already exists
#' @param check_gs_access whether to confirm google spreadsheet access (using the \code{\link{tbl_check_gs_access}} function). Note that if this is set to \code{FALSE} (e.g. for avoiding a re-check when overwriting an existing peer evaluation folder), this function will NOT ask for google spreadsheet credenticals and NOT check that the provided \code{data_gs_title} is a valid spreadsheet the user has access to. Make sure to provide the necessary access credentials manually (e.g. a \code{gs_token.rds} file in the application folder), otherwise the peer evaluation application will not be able to run (locally or on the shiny apps server).
#' @inheritParams tbl_run_peer_evaluation
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family peer evaluation functions
#' @export
tbl_setup_peer_evaluation <- function(
  folder = "peer_evaluation", data_gs_title = "Peer Evaluation",
  template_roster_file = system.file(package = "tbltools", "extdata", "roster_template.xlsx"),
  gs_token = file.path(folder, "gs_token.rds"), overwrite = FALSE, check_gs_access = TRUE) {
  
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

  # generate function call
  glue("Info: creating {folder}/app.R application file and application content") %>% message()
  
  # md template files
  system.file(package = "tbltools", "extdata", "app_template_files") %>% 
    list.files(pattern = "*\\.md$", full.names = TRUE, include.dirs = FALSE, recursive = TRUE) %>% 
    file.copy(to = folder, overwrite = TRUE)
  
  if (!dir.exists(file.path(folder, "www"))) dir.create(file.path(folder, "www"))
  system.file(package = "tbltools", "extdata", "app_template_files", "www", "app.css") %>% 
    file.copy(to = file.path(folder, "www"), overwrite = TRUE)
  
  # generate function call parameters ======
  parameters <- 
    list(
      data_gs_title = data_gs_title,
      app_title = "Peer Evaluation",
      points_per_teammate = 10,
      min_points = 0,
      max_points = 15,
      min_point_difference = 2
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
  
  # token save path
  gs_token_save_path <- file.path(folder, "gs_token.rds")
  
  # copy token
  if (!is.null(gs_token) && file.exists(gs_token) && (!file.exists(gs_token_save_path) || overwrite)) {
    if (check_gs_access)
      glue("Info: copying authentication token to {folder}/gs_token.rds") %>% message()
    else
      glue("Info: copying authentication token (but without checking gs access) to {folder}/gs_token.rds") %>% message()
    file.copy(gs_token, to = gs_token_save_path) 
  }
  
  # check gs_access
  if (check_gs_access) {
    tbl_check_gs_access(folder = folder, data_gs_title = data_gs_title, gs_token = gs_token_save_path)
  }
  
  glue("Info: set up of tbltools' Peer Evaluation app in directory '{folder}' is complete.\n",
       "Please modify the files in '{folder}' as appropriate before uploading to shiny server.") %>% 
    message()
  
  return(invisible(folder))
}

#' Duplicate a peer evaluation app
#' 
#' This functions makes it easy to duplicate an existing peer evaluation by copying all relevant files into a new folder. You can optionally change the google spreadsheet the new peer evaluation is linked to by specifying the \code{data_gs_title} parameter. To create a new peer evaluation from scratch, use \link{tbl_setup_peer_evaluation} instead. 
#' 
#' @param template folder where the existing peer evaluation that is to be duplicated is located (path must be either relative to the current working directory or an absolute file path on the operating system).
#' @param folder target folder where to setup the duplicated peer evaluation (path must be either relative to the current working directory or an absolute file path on the operating system). If the folder does not exist yet, it will be created automatically.
#' @inheritParams peer_evaluation_server
#' @inheritParams tbl_setup_peer_evaluation
#' @return returns the \code{folder} invisibly for ease of use in pipelines
#' @family peer evaluation functions
#' @export
tbl_duplicate_peer_evaluation <- function(template = "peer_evaluation", folder = "peer_evaluation2", data_gs_title = NULL, overwrite = FALSE, check_gs_access = TRUE) {
  
  # check old folder
  check_peer_evaluation_folder(template)
  
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
  }
  
  # check gs_access
  if (check_gs_access) {
    tbl_check_gs_access(folder = folder, data_gs_title = data_gs_title)
  }
  
  # finalize
  glue("Info: peer evaluation app from '{template}' is now fully duplicated in directory '{folder}'.\n",
       "Please modify the files in '{folder}' as appropriate before uploading to shiny server.") %>% 
    message()
  
  return(invisible(folder))
}

#' Run a peer evaluation app
#'
#' This function starts the peer evaluation user interface. Note that this function is typically NOT called directly but indirectly by setting up the peer evalution app using \link{tbl_setup_peer_evaluation}, adjusting the files in the peer evaluation app folder, and test running the app using \link{tbl_test_peer_evaluation}.
#'
#' @inheritParams peer_evaluation_server
#' @inheritParams peer_evaluation_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @family peer evaluation functions
#' @export
tbl_run_peer_evaluation <- function(
  data_gs_title, 
  roster = read_excel("roster.xlsx"), 
  app_title = "Peer Evaluation", 
  welcome_md_file = "app_welcome.md", 
  self_eval_plus_md_file = "app_self_evaluation_plus.md", 
  self_eval_minus_md_file = "app_self_evaluation_minus.md", 
  teammate_eval_plus_md_file = "app_teammate_evaluation_plus.md", 
  teammate_eval_minus_md_file = "app_teammate_evaluation_minus.md", 
  quant_scores_md_file = "app_quantitative_scores.md",
  points_per_teammate = 10, max_points = 15, min_points = 0, min_point_difference = 2,
  require_self_eval = TRUE,
  require_peer_evals = TRUE,
  auto_login_access_code = NULL, ..., 
  launch = FALSE) {
  
  # require specific gs token location for simplicity
  gs_token <- "gs_token.rds"
  
  # safety checks
  if (!is.data.frame(roster)) stop("roster data frame required", call. = FALSE)
  content_files <- c(welcome_md_file, self_eval_plus_md_file, self_eval_minus_md_file, teammate_eval_plus_md_file, teammate_eval_minus_md_file, quant_scores_md_file)
  if (any(missing <- !file.exists(content_files)))
    glue("content files do not exist: '{collapse(content_files[missing], sep = \"', '\")}'") %>% 
    stop(call. = FALSE)
  if (!file.exists(gs_token)) glue("no authentication token file found ({gs_token} is missing)") %>% stop(call. = FALSE)
  
  # start-up message
  glue(
    "\n***************************************************************",
    "\nInfo: launching Peer Evaluation GUI (version {packageVersion('tbltools')})...",
    "\nInfo: app title: '{app_title}'",
    "\nInfo: roster: {nrow(roster)} students in {length(unique(roster$team))} teams",
    "\nInfo: points per teammate: {points_per_teammate} ", 
    "(min: {min_points}, max: {max_points}, min diff: {min_point_difference})"
  ) %>% message()

  # generate app
  app <- shinyApp(
    ui = peer_evaluation_ui(app_title = app_title),
    server = peer_evaluation_server(
      roster = roster,
      data_gs_title = data_gs_title,
      gs_token = "gs_token.rds",
      welcome_md_file = welcome_md_file, 
      self_eval_plus_md_file = self_eval_plus_md_file, 
      self_eval_minus_md_file = self_eval_minus_md_file, 
      teammate_eval_plus_md_file = teammate_eval_plus_md_file, 
      teammate_eval_minus_md_file = teammate_eval_minus_md_file, 
      quant_scores_md_file = quant_scores_md_file,
      points_per_teammate = points_per_teammate,
      max_points = max_points,
      min_points = min_points,
      min_point_difference = min_point_difference,
      require_self_eval = require_self_eval,
      require_peer_evals = require_peer_evals,
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
#' This function starts a peer evaluation app previously set up using \link{tbl_setup_peer_evaluation}. This provides a local version of the app before deploying it to a shiny apps server (using \link{tbl_deploy_peer_evaluation}). Keep in mind that the test application writes data to the google spreadsheet and make sure to delete any test records from the google spreadsheet that may interfere with any students' peer evaluations prior to providing the students with the link to the deployed peer evaluation app. 
#' 
#' @param folder folder where the peer evaluation app is located (see \link{tbl_setup_peer_evaluation} for details)
#' @param ... optional parameters passed to \link[shiny]{runApp}
#' @family peer evaluation functions
#' @export
tbl_test_peer_evaluation <- function(folder = "peer_evaluation", ...) {
  check_peer_evaluation_folder(folder)
  runApp(folder, ...)
}

# convenience function for app existence check
check_peer_evaluation_folder <- function(folder) {
  if(!dir.exists(folder)) {
    glue("peer evaluation app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain a peer evaluation app ('app.R' is missing)") %>% 
      stop(call. = FALSE)
  }
}

#' Deploy peer evaluation app
#' 
#' Upload the peer evaluation app to a shiny server via \link[rsconnect]{rsconnect}. This uses the rsconnect function \link[rsconnect]{deployApp} to upload or update a peer evaluation and thus requires rsconnect credentials to be already set. See \link[rsconnect]{setAccountInfo} or the \href{https://shiny.rstudio.com/articles/shinyapps.html#configure-rsconnect}{configuration help} for details on how to set your credentials for the \href{https://www.shinyapps.io}{shiny app platform}.
#' @param folder folder where the peer evaluation app is located (see \link{tbl_setup_peer_evaluation} for details)
#' @param appName name of the application for the web address on shiny.io. Can be provided manually but must be unique within an account. By default is guessed from the folder simply by removing special characters, making everything lower case, and replacing spaces with \code{_}.
#' @param ... additional optional parameters passed on to \link[rsconnect]{deployApp}
#' @family peer evaluation functionsde 
#' @export
tbl_deploy_peer_evaluation <- function(folder = "peer_evaluation", appName = guess_from(folder), ...) {
  
  if(!dir.exists(folder)) {
    glue("peer evaluation app folder '{folder}' does not exist") %>% 
      stop(call. = FALSE)
  }
  
  if (!file.exists(file.path(folder, "app.R"))) {
    glue("the folder '{folder}' does not seem to contain a peer evaluation app ('app.R' is missing)") %>% 
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
      glue("something went wrong trying to upload your peer evaluation app. ",
           "Please see the function help (?tbl_deploy_peer_evaluation) ", 
           "and make sure that your shiny server credentials are set properly. ",
           "The shiny apps server returned the following error: ", e$message) %>% 
        stop(call. = FALSE)
    }
  )
}

# peer evals data functions -----

# internal wrapper for public gs keys
get_example_gs_key <- function(id) {
  # public example spreadsheet (consider creating permanent ID via https://w3id.org/)
  tryCatch(
    key <- gs_key(id, lookup = FALSE, verbose = FALSE),
    error = function(e) {
      glue("could not retrieve example google spreadsheet with id {id} - ",
           "encountered the following error: {e$message}") %>% 
        stop(call. = FALSE)
    })
  return(key)
}

#' Example files
#' @rdname tbl_example
#' @export
#' @details \code{tbl_example_peer_evaluation} returns the google spreadsheet key (\link{gs_key}) for an example peer evaluation dataset.
tbl_example_peer_evaluation <- function() {
  get_example_gs_key("1u9p0erH13N-KFGsAEEY5Eo2cyXJhR62SA485GaRP3O8")
}

#' @rdname tbl_example
#' @export
#' @details \code{tbl_example_empty_peer_evaluation} returns the google spreadsheet key (\link{gs_key}) for an example empty/newly set-up peer evaluation.
tbl_example_empty_peer_evaluation <- function() {
  get_example_gs_key("1WcxbU3NOIrOzhf-PAyNGlgpxiEPLMFthe-3lldoCc2M")
}

#' @rdname tbl_example
#' @export
#' @details \code{tbl_example_roster} returns the example roster data frame
tbl_example_roster <- function() {
  read_excel(system.file(package = "tbltools", "extdata", "roster_template.xlsx"))
}

#' Fetch the peer evaluation data
#' 
#' Fetches the peer evaluation data from the google spreadsheet and reads it (using \link{tbl_read_peer_evaluation_data}). For standard installations of the peer evaluation app, all defaults should be sufficient with only parameter \code{data_gs_title} requiring a specification.
#' 
#' @inheritParams tbl_setup_peer_evaluation
#' @inheritParams tbl_run_peer_evaluation
#' @param folder folder where the peer evaluation app is located (relative to the location of the RMarkdown file if used in the latter context)
#' @param data_gs_key alternatively to the \code{data_gs_title} and \code{gs_token}, a google spread sheet key object generated via \code{\link[googlesheets]{gs_key}} can be provided. This allows the use of externally registered spreadsheets such as public example sheets. If provided, it takes precedence over the data_gs_title parameter. Note that this is NOT an option for running peer evaluations which need write access to the google spreadsheet.
#' @param download_to location where the whole peer evaluation data sheet will be downloaded to for more efficient read access
#' @export
tbl_fetch_peer_evaluation_data <- function(
  folder = ".", data_gs_title = NULL, data_gs_key = NULL,
  roster = read_excel(file.path(folder, "roster.xlsx")), 
  gs_token = file.path(folder, "gs_token.rds"),
  download_to = file.path(folder, "pe_data_downloaded.xlsx")) {
  
  # safety checks
  if (!is.data.frame(roster)) stop("roster data frame required", call. = FALSE)
  
  # google sheet
  if (is.null(data_gs_title) && is.null(data_gs_key))
    stop("a google spreadsheet must be identified either via data_gs_title or data_gs_key parameter, neither is provided", call. = FALSE)
  
  if (!is.null(data_gs_key)) {
    # get data via gs key, make sure it's a valid key
    gs <- gs_gs(data_gs_key, verbose = FALSE)
  } else {
    # authenticate and get gs via title
    gs <- tbl_check_gs_access(gs_token = gs_token, data_gs_title = data_gs_title)
  }
  
  # downloading data
  message("Info: downloading data... ", appendLF = FALSE)
  if(file.exists(download_to)) file.remove(download_to)
  result <- quietly(gs_download)(gs, to = download_to, overwrite = TRUE)
  result$messages %>% 
    str_replace(fixed(getwd()), "") %>% 
    str_replace("\\n", " ") %>% 
    message(appendLF = FALSE)
  
  # read data
  tbl_read_peer_evaluation_data(folder = folder, roster = roster, download_file = download_to)
}

#' Read downloaded peer evaluation data
#' 
#' Read peer evaluation data that's already downloaded. Usually called indirectly via \link{tbl_fetch_peer_evaluation_data} unless there is no reason to fetch the latest data from the google spreadsheet.
#' @inheritParams tbl_fetch_peer_evaluation_data
#' @param download_file downloaded peer evaluation data file, created by \link{tbl_fetch_peer_evaluation_data}
#' @export
tbl_read_peer_evaluation_data <- function(
  folder = ".", 
  roster = read_excel(file.path(folder, "roster.xlsx")),
  download_file = file.path(folder, "pe_data_downloaded.xlsx")) {
    
  # global vars
  team <- access_code <- timestamp <- submitted <- evaluatee_access_code <- submitted2 <- self_evaluation <- plus <- minus <- score <- NULL
  
  # safety checks
  if (!is.data.frame(roster)) stop("roster data frame required", call. = FALSE)
  students <- check_student_roster(roster) %>% 
    # get team member
    group_by(team) %>% 
    mutate(n_team_mates = dplyr::n() - 1L) %>% 
    ungroup() 
  
  # path
  if (!file.exists(download_file))
    glue("peer evaluation data file '{download_file}' does not exist, ",
         "make sure to run tbl_fetch_peer_evaluation_data() first ",
         "or adjust the 'download_file' parameter to point to a valid ",
         "peer evaluation data file") %>% 
    stop(call. = FALSE)
  
  # fetch student info
  access_code_prefix <- "id_"
  students <- students %>% mutate(access_code = str_c(access_code_prefix, access_code)) 
  pe_data <- students %>% 
    mutate(
      peer_eval = purrr::map(access_code, ~read_peer_eval(download_file, .x) %>% 
                               rename(evaluatee_access_code = access_code)),
      # safety check on timestamp column
      time_stamp_col = purrr::map_chr(peer_eval, ~class(.x$timestamp)[1])
    )
  
  if (any(problem <- pe_data$time_stamp_col != "POSIXct")) {
    glue::glue(
      "encountered problems with the timestamp column. Please check the ",
      "spreadsheet and make sure the 'timestamp' column on the following tabs ",
      "always has the same date & time format in each row. Problematic tabs:\n",
      "{paste(pe_data$access_code[problem], collapse = ', ')}"
    ) %>% stop(call. = FALSE)
  }
  
  # unnest the data (make sure those without records remain listed, hence the left join)
  pe_data <- 
    students %>% 
    dplyr::left_join(
      pe_data %>% select(access_code, peer_eval) %>% tidyr::unnest(peer_eval),
      by = "access_code"
    )
    
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
    nest(evaluations = c(evaluatee_access_code, self_evaluation, plus, minus, score))
  
  # update timestamp (can't do inside mutate, problems with the NA)
  pe_data <- within(pe_data, submitted_timestamp[!submitted] <- NA)
  
  return(pe_data)
}

#' Summarize peer evaluation data
#' 
#' Summarizes the peer evaluation data. Preserves all roster information (last name, first name, team, + any custom fields).
#' 
#' @param data the peer evaluation data frame retrieved by \link{tbl_fetch_peer_evaluation_data}
#' @param submitted_only only include evaluations that were actually submitted (rather than just saved)
#' @export
tbl_summarize_peer_evaluation_data <- function(data, submitted_only = FALSE) {
  
  # global vars
  started <- submitted <- submitted_timestamp <- evaluations <- access_code <- evaluatee_access_code <- self_evaluation <- n_evaluations <- score <- plus <- minus <- NULL
  
  # safety
  if (missing(data) || !is.data.frame(data))
    stop("no data frame supplied")
  
  # check for required columns
  req_columns <- c("access_code", "evaluations")
  if (length(missing <- setdiff(req_columns, names(data))) > 0)
    glue("missing column(s) in data frame: {collapse(missing, sep=', ')}") %>% 
    stop(call. = FALSE)
  
  summarize_evals <- . %>% sample() %>% stats::na.omit() %>% collapse(sep = "\n\n") %>% { ifelse(is.null(.), NA_character_, .) }
  
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
  
  # global vars
  evaluations <- NULL
  
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
  
  # global vars:
  access_code <- n <- NULL
  
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

# authentication =======

#' Check google spreadsheet access
#' 
#' This function checks whether google spreadsheet credentials are established and there is access to the \code{data_gs_title} google sheet. If the \code{gs_token} file does not exist yet or \code{new_credentials = TRUE}, it will establish new google drive credentials using \link[googlesheets]{gs_auth}.
#' 
#' @inheritParams tbl_setup_peer_evaluation
#' @param new_credentials if set to TRUE, will always prompt the user to establish new google drive credentials. If FALSE (the default), will only do so if the \code{gs_token} file does not exist yet.
#' @return returns the retrieved google spreadsheet key invisibly
#' @family peer evaluation functions
#' @export
tbl_check_gs_access <- function(folder = "peer_evaluation", data_gs_title = "Peer Evaluation",
                                gs_token = file.path(folder, "gs_token.rds"), new_credentials = FALSE) {
  
  # error msg 
  err_msg <- glue(
    "\n\nNote: there may be a problem with your authentication credentials (e.g. expired token, wrong account, etc.), ",
    "please run the following function for the peer evaluation folder and google spreadsheet to establish new credentials and confirm access to the google sheet:\n\n",
    "tbl_check_gs_access(folder = \"{if(!missing(folder)) {folder} else {'ENTER FOLDER'}}\", ",
    "data_gs_title = \"{data_gs_title}\", new_credentials = TRUE)"
  )
  
  # google sheets authentication
  if (file.exists(gs_token) && !new_credentials) {
    token <- try_to_authenticate(gs_token, err_msg = err_msg)
  } else {
    token <- try_to_authenticate(err_msg = err_msg)
    glue("Info: storing new authentication token in {gs_token}") %>% message()
    write_rds(x = token, path = gs_token)
  }
  
  # check google sheet presence
  # gs <- try_to_fetch_google_spreadsheet(data_gs_title, err_msg=err_msg)
  gs <- try_to_fetch_google_spreadsheet(data_gs_title, err_msg = err_msg)
  
  return(invisible(gs))
}

# authenticate with google server
try_to_authenticate <- function(gs_token = NULL, err_msg = "") {
  # google sheets authentication
  automatic <- is.null(getOption('httr_oob_default')) || !getOption('httr_oob_default')
  tryCatch({
    if (!is.null(gs_token)) {
      # authenticate quietly if token is provided
      message("Info: authenticating with google server via token... ", appendLF = FALSE)
      token <- quietly(gs4_auth)(token = gs_token, cache=FALSE)
    } else {
      # allow authentication info messages if token not provided
      glue("Info: authenticating with google server {if(automatic) 'automatically' else 'manually'}... ") %>% 
        message(appendLF = FALSE)
      token = gs4_token()
      # token <- gs_auth(token = NULL, new_user = TRUE, cache=FALSE)
    }
    message("complete.")
  },
  error = function(e) {
    if (automatic && str_detect(e$message, fixed("Failed to create server"))) {
      glue("could not authenticate automatically, please run 'options(httr_oob_default = TRUE)' and try again to authenticate manually: {e$message}") %>% 
        stop(call. = FALSE)
    } else {
      glue("google spreadsheet authentication failed: {e$message}{err_msg}") %>% 
        stop(call. = FALSE)
    }
  })
  return(token)
}

# find google spreadsheet
try_to_fetch_google_spreadsheet <- function(data_gs_title, err_msg= ""){
  message("Info: looking for spreadsheet... ", appendLF = FALSE)
  sheets = gs4_find()
  id = sheets[which(grepl(paste0('^', data_gs_title,'$'), sheets$name)),][['id']]
  tryCatch(gs <- gs4_get(id), error = function(e) {
    glue("google spreadsheet with title '{data_gs_title}' could not be retrieved: {e$message}{err_msg}") %>% 
      stop(call. = FALSE)
  })
  return(gs)
}

# data loading/saving functions ==========

# load peer evaluation
# @param ss - spreadsheet
read_peer_eval <- function(ss, access_code) {
  
  # global vars
  timestamp <- score <- NULL
  
  is_gs <- is(ss, "googlesheets4_spreadsheet")
  
  if (is_gs) {
    # refresh sheet
    gs <- gs4_get(ss)
    # worksheets <- gs_ws_ls(gs)
    worksheets = gs$sheets[['name']]
    
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
    # # does exist and is a google spreadsheet
    # data <- gs %>% 
    #   # retrieve data
    #   gs_read_csv(
    #     ws = access_code, 
    #     col_types = cols(
    #       timestamp = col_character(),
    #       submitted = col_logical(),
    #       access_code = col_character(),
    #       plus = col_character(),
    #       minus = col_character(),
    #       score = col_integer()
    #     )) %>% 
    #   # convert timestamp
    #   mutate(timestamp = ymd_hms(timestamp))
    data = gs %>%
      range_read(sheet = access_code, 
                 col_names = c('timestamp',
                               'submitted',
                               'access_code',
                               'plus',
                               'minus',
                               'score'),
                 col_types = 'clccci',
                 .name_repair='minimal') %>%
      mutate(timestamp = ymd_hms(timestamp))
    if (nrow(data) == 0) {
      data = tibble(timestamp=as.character(),
                    submitted=as.logical(),
                    access_code=as.character(),
                    plus=as.character(),
                    minus=as.character(),
                    score=as.integer()) %>%
        mutate(timestamp=ymd_hms(timestamp))
    } 
  } else {
    # does exist and is a local file
    data <- read_excel(ss, sheet = access_code) %>% 
      mutate(
        plus = as.character(plus),
        minus = as.character(minus)
      )
  }
  
  print(data)

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
  
  # global vars
  timestamp <- NULL
  
  # refresh sheet
  # gs <- gs_gs(gs)
  gs = gs4_get(gs)
  
  # add timestamp and submitted info
  data <- data %>% 
    mutate(
      timestamp = now("UTC"),
      submitted = submitted
    ) %>% 
    select(timestamp, submitted, everything()) %>% 
    as.data.frame()
  
  # check for spreadsheet
  worksheets = gs$sheets[['name']]
  # if (!access_code %in% gs_ws_ls(gs)) {
  if (!access_code %in% worksheets) {
    glue("Info: creating {nrow(data)} rows in new gs tab '{access_code}'") %>% message()
    # gs_ws_new(gs, ws_title = access_code, input = data, trim = TRUE)
    write_sheet(data=data, ss=gs, sheet=access_code)
  } else {
    # add new rows
    glue("Info: adding {nrow(data)} rows to tab '{access_code}'") %>% message()
    # gs_add_row(gs, ws = access_code, data)
    sheet_append(ss=gs, data=data, sheet=access_code)
  }
  
}
