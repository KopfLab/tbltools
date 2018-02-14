#' Peer Evaluation App UI
#'
#' Generates the user interface part of the peer evaluation app.
#' 
#' @param app_title the title of the Peer Evaluation App, e.g. "Class name - Peer Evaluation #1"
peer_evaluation_ui <- function(app_title) {

  # Define UI ----
  ui <- fluidPage(
    
    useShinyjs(),  # Set up shinyjs
    titlePanel(app_title),
    
    fluidRow(
      
      div(id = "access_panel",
          column(width = 12,
                 textInput("access_code", NULL, placeholder = "Enter your access code"),
                 #selectInput("debug_trigger", NULL, choices = "1", selected = "1") %>% hidden(),
                 actionButton("access", "Start")
          )),
      
      div(id = "main_panel", 
          column(width = 12, uiOutput("main"))
      ) %>% hidden()
    )
    
  )

}
