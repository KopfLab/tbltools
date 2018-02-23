#' Peer Evaluation App UI
#'
#' Generates the user interface part of the peer evaluation app.
#' 
#' @param app_title the title of the Peer Evaluation App, e.g. "Class name - Peer Evaluation #1"
peer_evaluation_ui <- function(app_title) {

  appCSS <- "
.message-panel {
  position: absolute;
  background: #ffffff;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}
"
  
  # Define UI ----
  ui <- fluidPage(
    
    useShinyjs(),  # Set up shinyjs
    inlineCSS(appCSS), # loading
    titlePanel(app_title),
    
    fluidRow(
      
      div(id = "loading-panel", class = "message-panel", 
          br(), br(), br(), h2("Loading...")) %>% hidden(),
      div(id = "saving-panel", class = "message-panel", 
          br(), br(), br(), h2("Saving..."), 
          p("Please wait, this may take a few seconds.")) %>% hidden(),
      div(id = "submit-panel", class = "message-panel", 
          br(), br(), br(), h2("Submitting..."), 
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
