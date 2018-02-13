#' Peer Evaluation App UI
#'
#' Generates the user interface part of the peer evaluation app.
#' 
#' @param title the title of the Peer Evaluation, e.g. "Class name - Peer Evaluation #1"
peer_evaluation_ui <- function(title = "Peer Evaluation") {

  # Define UI ----
  ui <- fluidPage(
    
    useShinyjs(),  # Set up shinyjs
    titlePanel(title),
    
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
