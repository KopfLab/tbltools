
#' Immediate Feedback Test App Server
#'
#' Generates the server part of the tRAT app
#' @param roster data frame with the roster and access codes
#' @param questions data frame with the questions
#' @param auto_login_access_code set an automatic login access code for testing and debugging purposes
#' @keywords internal
immediate_feedback_test_server <- function(roster, questions, data_gs_title, gs_key_file, auto_login_access_code = NULL) {
  
  # plotting constants (could become parameters if that would be useful)
  answer_width <- 0.9
  answer_height <- 0.9
  
  # access code prefix
  access_code_prefix <- "id_"
  
  # safety check for the roster
  roster <- check_immediate_feedback_test_roster(roster) %>% 
    mutate(
      # make sure access code is textual
      access_code = str_c(access_code_prefix, access_code)
    )
  
  # safety check for questions
  plot_height <- nrow(questions) * 80
  plot_height_inches <- nrow(questions) * 0.8
  questions <-prepare_immediate_feedback_test_questions(questions)
  
  
  # spreadsheet
  gs <- tbl_check_gs_access(gs_key_file = gs_key_file, data_gs_title = data_gs_title)
  
  shinyServer(function(input, output, session) {
    
    message("\n\nINFO: Loading GUI instance ...")
    
    values <- reactiveValues(
      auto_login = FALSE,
      access_code = NULL,
      active = NULL, # active entry from the roster
      answers = NULL,
      state = NULL,
      gui_ready = FALSE,
      guess = NULL
    )
    
    # error loading modal dialog ====
    error_modal <-
      modalDialog(
        h2("Error: please try again", style = "color: red;"),
        h4("Sorry, the app encountered a data connection error. This happens occasionally. Please just try again. If this issue persists, please contact your instructor.", style = "color: red;"),
        footer = modalButton("Ok"),
        easyClose = TRUE, fade = FALSE
      )
    
    # data ====
    get_answers <- function(access_code) {
      tryCatch(
        read_immediate_feedback_test(gs, access_code),
        error = function(e) {
          message("Error: encountered a data read error.")
          message(e$message)
          showModal(error_modal)
          return(NULL)
        }
      )
    }
    
    # update state when answers get updated
    observeEvent(values$answers, {
      req(values$answers)
      req(values$id)
      values$state <- combine_immediate_feedback_test_questions_and_answers(
        mutate(questions, name = values$active$name), 
        mutate(values$answers, name = values$active$name)
      )
      message("Info: new state")
      print(filter(values$state, guessed))
    })
    
    # load access code ====
    load_access <- function(entered_access_code) {
      pure_access_code <- entered_access_code
      message("Info: Checking access code: ", pure_access_code)
      entered_access_code <- paste0(access_code_prefix, entered_access_code)
      hide("access-panel")
      show("loading-panel")
      # enforce case sensitive access code
      active <- filter(roster, stringr::str_to_lower(access_code) == stringr::str_to_lower(entered_access_code))
      load_access_code <- FALSE
      if (nrow(active) == 0) {
        showModal(modalDialog(h2(paste0("Unknown access code: ", pure_access_code)), easyClose = TRUE, fade = FALSE))
      } else if (is.null(values$access_code) || stringr::str_to_lower(values$access_code) != stringr::str_to_lower(entered_access_code)) {
        
        # active roster entry info
        active <- as.list(active[1,])
        
        # retrieve data from google spreadsheet
        message("Info: Retrieving previous answers for '", active$name, "'")
        answers <- get_answers(active$access_code)
        if (!is.null(answers)) load_access_code <- TRUE
      }
      
      # load access code GUI
      if (load_access_code) {
        message("Info: loading GUI for access code: ", pure_access_code)
        
        values$active <- active
        values$access_code <- active$access_code
        values$answers <- answers
        
        hide("loading-panel", anim = TRUE, animType = "fade")   
        show("main-panel")
      } else {
        hide("loading-panel", anim = TRUE, animType = "fade")   
        show("access-panel")
      }
    }
    
    observeEvent(input$access, load_access(input$access_code))
    
    # render main GUI ===
    output$main <- renderUI({
      req(values$access_code)
      values$gui_ready <- !is.null(values$access_code)
      if (!values$gui_ready)  return(NULL)
      
      isolate({
        # tabs
        message("Info: generating main GUI")
       
        # full_name tag list
        tagList(
          column(12, align="left",
             plotOutput("immediate_feedback_test", height = plot_height, click = clickOpts(id="tRAT_click")) %>%
               withSpinner(type = 5, proxy.height = paste0(plot_height - 50, "px"))
          ),
          column(12, align="center", downloadButton('downloadPlot', 'Download'))
        )
      })
    })
    
    # load main UI ====
    observeEvent(values$gui_ready, {
      req(values$active)
      req(values$access_code)
      message("Info: showing GUI for '", values$active$name, "'")
      if (values$auto_login == FALSE) {
        showModal(modalDialog(
          h2(str_c("Welcome ", values$active$name)),
          h4("Please decide which answers are correct and make your choices by clicking on the right option for each question."),
          footer = modalButton("Ok"),
          easyClose = TRUE, fade = FALSE
        ))
      }
      values$auto_login <- FALSE
    })
    
    # render plot =======
    output$tRAT <- renderPlot({
      req(values$state)
      req(nrow(values$state) > 0)
      message("Info: (re-)generating tRAT")
      tbl_generate_immediate_feedback_test(values$state, width = answer_width, height = answer_height)
    })
    
    # click on tRAT option ====
    observeEvent(input$tRAT_click, {
      req(values$state)
      location <- values$state %>%
        mutate(
          question_nr = as.integer(forcats::fct_rev(question)),
        ) %>%
        filter(
          question_nr - answer_height/2 < input$tRAT_click$y,
          question_nr + answer_height/2 > input$tRAT_click$y,
          option_idx - answer_width/2 < input$tRAT_click$x,
          option_idx + answer_width/2 > input$tRAT_click$x
        )
      if (nrow(location) == 1) {
        if (!location$guessed[1] && !location$complete[1]) {
          # not guessed yet and question is not yet complete
          values$guess <- location
          showModal(modalDialog(
            h2(paste0("Question ", location$question)),
            h4(paste0("Are you sure you want to pick option ", location$option, "?")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("make_guess", "Yes")
            ),
            easyClose = TRUE, fade = FALSE
          ))
        } else if (!location$guessed[1] && location$complete[1]) {
          # already complete
          showModal(modalDialog(
            h2(paste0("Question ", location$question)),
            h4("This question was already answered correctly."),
            footer = modalButton("Ok"),
            easyClose = TRUE, fade = FALSE
          ))
        }
      }
    })
    
    # save tRAT guess ====
    observeEvent(input$make_guess, {
      req(values$guess)
      removeModal()
      hide("main-panel")
      show("saving-panel")
      
      # try to save
      guess <- tryCatch(
        save_immediate_feedback_test(gs, values$access_code, question_id = values$guess$question_id, guess = values$guess$option),
        error = function(e) {
          message("Error: encountered a data read error.")
          message(e$message)
          showModal(error_modal)
          return(NULL)
        }
      )
  
      if (!is.null(guess)) {
        values$answers <- get_answers(values$access_code)
      }
      
      hide("saving-panel", anim = TRUE, animType = "fade")   
      show("main-panel")
    })
    
    # log out ==== FIXME: do we even need this? probably not
    
    logout_user <- function() {
      message("Info: logging out ", values$active$name)
      value$active <- NULL
      values$access_code <- NULL
      values$state <- NULL
      updateTextInput(session, "access_code", value = "")
      hide("submit-panel")
      show("access-panel")
      hide("main-panel")
    }
    observeEvent(input$logout, logout_user())
    
    # debug / auto-login ====
    observeEvent(input$auto_login_trigger, {
      if (!is.null(auto_login_access_code)) {
        message("Info: executing auto-login for access code ", auto_login_access_code)
        values$auto_login <- TRUE
        load_access(auto_login_access_code)
      }
    })
    
    # download plot =====
    output$downloadPlot <- downloadHandler(
      filename = function() { isolate("immediate_feedback_test.pdf") },
      content = function(filename) {
        req(values$state)
        req(nrow(values$state) > 0)
        message("Info: dowloading tRAT")
        plot <- tbl_generate_immediate_feedback_test(values$state, width = answer_width, height = answer_height)
        ggplot2::ggsave(file = filename, plot = plot, width = 5, height = plot_height_inches, device = "pdf")
      })
    
  })
  
}
