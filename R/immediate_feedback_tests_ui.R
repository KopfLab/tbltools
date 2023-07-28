#' Immediate Feedback Test App UI
#'
#' Generates the user interface part of the immediate feedback test app.
#' 
#' @param app_title the title of the app, e.g. "Class name - tRAT #1"
#' @keywords internal
immediate_feedback_test_ui <- function(app_title) {
  
  # set spinner color
  options(spinner.color = "black")
  
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
      div(id = "error-panel", class = "error-panel", 
          h2("Error. Please try again."),
          p("Encountered a data connection error. This can happen occasionally.")) %>% hidden(),
      div(id = "saving-panel", class = "message-panel", 
          h2("Saving..."), 
          p("Please wait, this may take a few seconds.")) %>% hidden(),
      
      div(id = "access-panel",
          column(width = 12,
                 textInput("access_code", NULL, placeholder = "Enter your access code"),
                 selectInput("auto_login_trigger", NULL, choices = "1", selected = "1") %>% hidden(),
                 actionButton("access", "Start")
          )),
      
      div(id = "main-panel", 
          column(width = 12, uiOutput("main"))
      ) %>% hidden()
    )
    
  )
  
}
