#' Start the peer evaluation user interface
#'
#' @inheritParams app_server
#' @inheritParams app_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @inheritParams peer_evaluation_app_server
#' @export
tbl_run_peer_evaluation <- function(roster, ..., launch = TRUE) {
  
  # safety checks
  if (missing(roster)) stop("roster data frame required", call. = FALSE)
 
  # start-up message
  glue(
    "\n***************************************************************",
    "\nINFO: Launching Peer Evaluation GUI (version {packageVersion('tbltools')})...",
    "\nINFO: Roster: {nrow(roster)} students in {length(unique(roster$team))} teams"
  ) %>% message()
  
  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()
  
  # generate app
  app <- shinyApp(
    ui = peer_evaluation_ui(title = "bla"),
    server = peer_evaluation_server(roster = roster)
  )
  
  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}


tbl_setup_peer_evaluation <- function() {
  
}

tbl_test_peer_evaluation <- function() {
  
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