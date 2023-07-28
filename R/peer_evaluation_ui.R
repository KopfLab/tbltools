#' Peer Evaluation App UI
#'
#' Generates the user interface part of the peer evaluation app.
#' 
#' @param app_title the title of the Peer Evaluation App, e.g. "Class name - Peer Evaluation #1"
#' @keywords internal
peer_evaluation_ui <- function(app_title) {

  # Define UI ----
  ui <- fluidPage(
    
    # Set up shinyjs
    useShinyjs(),  
    
    # Stylesheet
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
    
    titlePanel(app_title),
    
    fluidRow(
      
      div(id = "loading-panel", class = "message-panel", 
          h2("Loading..."),
          p("Please wait, this may take a few seconds.")) %>% hidden(),
      div(id = "saving-panel", class = "message-panel", 
          h2("Saving..."), 
          p("Please wait, this may take a few seconds.")) %>% hidden(),
      div(id = "submit-panel", class = "message-panel", 
          h2("Submitting..."), 
          p("Please wait, this may take a few seconds.")) %>% hidden(),
      
      div(id = "access-panel",
          column(width = 12,
                 textInput("access_code", NULL, placeholder = "Enter your access code (case sensitive)"),
                 selectInput("auto_login_trigger", NULL, choices = "1", selected = "1") %>% hidden(),
                 actionButton("access", "Start")
          )),
      
      div(id = "main-panel", 
          column(width = 12, uiOutput("main"))
      ) %>% hidden()
    )
    
  )

}
