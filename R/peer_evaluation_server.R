
#' Peer Evaluation App Server
#'
#' Generates the server part of the peer evaluation app
#' @param roster data frame with the student roster
#' @param data_gs_title name of the google spreadsheet that should be used for storing the peer evaluation data. This spreadsheet must already exist and the credentials used when asked by this function must have write access to the spreadsheet.
#' @param gs_token path to a google spreadsheet oauth 2.0 authentication token file (see \link[httr]{Token-class}). If none is provided or the file does not exist yet, will ask for google drive credentials interactively to generate a token for the peer evaluation app. The token is safe to use on a secure shiny app server but be careful to never post this token file anywhere publicly as it could be used to gain access to your google drive. 
#' @param points_per_teammate points per teammate
peer_evaluation_server <- function(roster, data_gs_title, gs_token, points_per_teammate = 10) {
  
  # access code prefix (must stay the same as in tbl_fetch_peer_evaluation_data!)
  access_code_prefix <- "id_"
  
  # safety check for the student roster
  students <- check_student_roster(roster) %>% 
    mutate(
      # make sure access code is textual
      access_code = str_c(access_code_prefix, access_code),
      # construct full_name name
      full_name = str_trim(str_c(str_replace_na(first, ""), " ", str_replace_na(last, "")))
    )

  # spreadsheet
  try_to_authenticate(gs_token)
  gs <- try_to_fetch_google_spreadsheet(data_gs_title)
  
  shinyServer(function(input, output, session) {
    
    message("\n\nINFO: Loading GUI instance ...")
    
    values <- reactiveValues(
      debug = FALSE,
      access_code = NULL,
      student = NULL,
      data = list(), 
      team_mates = c(), # named list of the team mates (key=access_code, value=full name)
      total_score = 0 # the total score available
    )
    
    # data ====
    get_data_as_df <- reactive({
      # update data from fields and turn into data frame
      fields <- c("plus", "minus", "score")
      for (team_mate in names(values$data)) {
        for (field in fields) {
          value <- input[[str_c(field, "_", team_mate)]]
          if (!is.null(value))
            values$data[[team_mate]][[field]] <- value
          else
            values$data[[team_mate]][[field]] <- NA
        }
      }
      isolate(bind_rows(values$data))
    })
    
    get_student_data <- function(student_access_code, field, default = "") {
      value <- values$data[[student_access_code]][[field]]
      if (is.null(value) || is.na(value)) value <- default
      return(value)
    }
    
    calculate_scores <- reactive({
      get_data_as_df()$score %>% na.omit()
    })
    
    calculate_diff <- reactive({
      diff(range(calculate_scores()))
    })
    
    # load access code ====
    load_access <- function(entered_access_code) {
      entered_access_code <- str_c(access_code_prefix, entered_access_code)
      message("Checking access code: ", entered_access_code)
      hide("access-panel")
      show("loading-panel")
      student <- filter(students, tolower(access_code) == tolower(entered_access_code))
      load_access_code <- FALSE
      if (nrow(student) == 0) {
        showModal(modalDialog(h2(str_c("Unknown access code: ", entered_access_code)), easyClose = TRUE, fade = FALSE))
      } else if (is.null(values$access_code) || values$access_code != entered_access_code) {
        
        # save student info
        values$student <- as.list(student[1,])
        
        # retrieve data from google spreadsheet
        pe_data <- read_peer_eval(gs, entered_access_code)

        # figure out what to do with the data
        if (is.null(pe_data) || nrow(pe_data) == 0) {
          # completely new data set (i.e. no tab yet)
          message("Into: first time session - creating new data frame")
          pe_data <- students %>% 
            filter(team == values$student$team) %>% 
            arrange(access_code) %>% 
            select(access_code) %>% 
            mutate(plus = "", minus = "", score = NA_integer_)
          load_access_code <- TRUE
        } else if (pe_data$submitted[1]) {
          # already submitted
          message("Info: already submitted: ", entered_access_code)
          showModal(modalDialog(h2("Peer evaluation already submitted."), easyClose = TRUE, fade = FALSE))
        } else {
          # resuming
          message("Info: resuming previous session")
          pe_data <- select(pe_data, access_code, plus, minus, score)
          load_access_code <- TRUE
        }
      }
      
      # load access code
      if (load_access_code) {
        message("Info: loading GUI for access code: ", entered_access_code)
        values$access_code <- entered_access_code 
        values$data <- list()
        for (i in 1:nrow(pe_data)) {
          values$data[[pe_data[["access_code"]][i]]] <- as.list(pe_data[i,])
        }
        
        # find team mates
        values$team_mates <- 
          pe_data %>% 
          left_join(students, by = "access_code") %>% 
          filter(access_code != values$student$access_code) %>% 
          select(access_code, full_name) %>% deframe()
        # total score determined by number of teammates
        values$total_score <- length(values$team_mates)*points_per_teammate
        hide("loading-panel", anim = TRUE, animType = "fade")   
        show("main-panel")
      } else {
        hide("loading-panel", anim = TRUE, animType = "fade")   
        show("access-panel")
      }
    }
    
    observeEvent(input$access, load_access(input$access_code))
    
    # load main UI ====
    observeEvent(values$access_code, {
      req(values$student)
      message("Info: showing GUI for student ", values$student$full_name)
      if (values$debug == FALSE) {
        showModal(modalDialog(
          h2(str_c("Welcome ", values$student$first)),
          h4(str_c("Team: ", values$student$team)),
          p("Please reflect on your own efforts and provide qualitative and quantitative feedback on your teammates' contributions to your team's performance in the class. The quantitative feedback will be used as part of the overall course grade. For the qualitative feedback, focus on specific behaviors and their impacts on team performance. Write about something you or your teammate does well and at least one area for improvement for their future teamwork."),
          p("You can save your answers at any point by clicking the ", strong("Save"), " button and resume the peer evaluation at a later point simply by returning to this page and entering your access code again. To submitted your peer evaluation, please click the ", strong("submitted"), " button. Once you submitted, you can no longer change your answers."),
          footer = modalButton("Ok"),
          easyClose = TRUE, fade = FALSE
        ))
      }
    })
    
    # evaluation panels ===
    output$main <- renderUI({
      values$access_code
      if (is.null(values$access_code)) return(NULL)
      
      isolate({
        req(values$student)
        req(values$data)
        req(length(values$data) > 0)
        
        # tabs
        message("Info: generating tabs for teammates: ", str_c(values$team_mates, collapse = ", "))
        tabs <- c(
          list(get_qual_ui_self(values$student$access_code)),
          map2(names(values$team_mates), values$team_mates, get_qual_ui_team_mate),
          list(get_quant_scores_ui(values$team_mates))
        )
        
        # full_name tag list
        tagList(
          column(12, actionButton("save", "Save"), actionButton("submitted", "Submit"), align = "right"),
          column(12, 
                 do.call(tabsetPanel, args = c(tabs, list(selected = "Self evaluation")))
          )
        )
      })
    })
    
    # evaluation gui
    get_qual_ui_self <- function(team_mate_access_code) {
      tabPanel(
        "Self evaluation",
        column(6, 
               h5("What have you done well to positively impact your team's performance?"),
               textAreaInput(
                 str_c("plus_", team_mate_access_code), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate_access_code, "plus"), label = NULL)),
        column(6, 
               h5("What could you do differently to increase your contribution to your team?"),
               textAreaInput(
                 str_c("minus_", team_mate_access_code), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate_access_code, "minus"), label = NULL))
      )
    }
    
    get_qual_ui_team_mate <- function(team_mate_access_code, team_mate_name) {
      tabPanel(
        team_mate_name,
        column(6, 
               h5(str_c("What did ", str_extract(team_mate_name, "^(\\w+)"), 
                        " do well to positively impact your team's performance?")),
               textAreaInput(
                 str_c("plus_", team_mate_access_code), width = "100%", height = "200px", resize = "vertical",
                 value = get_student_data(team_mate_access_code, "plus"), label = NULL)),
        column(6, 
               h5(str_c("What could ", str_extract(team_mate_name, "^(\\w+)"), 
                        " do differently to increase his/her/their contribution to your team?")),
               textAreaInput(
                 str_c("minus_", team_mate_access_code), width = "100%", height = "200px", resize = "vertical",
                 value = get_student_data(team_mate_access_code, "minus"), label = NULL))
      )
    }
    
    # qunatitative scores
    get_quant_scores_ui <- function(team_mates) {
      
      team_mate_tags <- function(team_mate_access_code, team_mate_name) {
        tagList(column(3, align="left", h5(team_mate_name)), 
                column(9, numericInput(
                  str_c("score_", team_mate_access_code), NULL, 
                  value = get_student_data(team_mate_access_code, "score", default = points_per_teammate), 
                  min=0, max=15, step=1, width = "80px")))
      }
      
      tabPanel(
        "Quantitative Scores",
        br(),
        p("Please assign scores that reflect how you really feel about the extent to which the other members of your team contributed to your learning and/or your team’s performance. This is an opportunity to reward the members of your team who worked hard on your behalf. (Note: If you give everyone pretty much the same score you will be hurting those who did the most and helping those who did the least)."),
        
        h4("Some things to consider:"),
        
        tags$ul(
          tags$li("Preparation – Were they prepared when they came to class? "),
          tags$li("Contribution – Did they contribute productively to team discussion and work?"),
          tags$li("Respect for others’ ideas – Did they encourage others to contribute their ideas? "),
          tags$li("Flexibility – Were they flexible when disagreements occurred?")
        ),
        
        h4("Instructions:"),
        p("In the space below please rate each of the other members of your team. Each member's peer evaluation score will be the average of the points they receive from the other members of the team. To complete the evaluation you must:"),
        
        tags$ol(
          tags$li(str_c(
            "Assign an average of ten points to the other members of your team. Thus, you will assign a total of ", 
            values$total_score, " points.")),
          tags$li("Assign points from 0 to 15 (inclusive), whole numbers only."),
          tags$li("Differentiate some in your ratings. Your score range (max-min) must be at least 2. For example, you must give at least one score of 11 or higher and one score of 9 or lower.")
        ),
        
        h4("Scores:"),
        do.call(tagList, map2(names(team_mates), team_mates, team_mate_tags)),
        column(3, h5("Total:")),
        column(9, h5(textOutput("total_score"))),
        column(3, h5("Difference:")),
        column(9, h5(textOutput("score_diff")))
      )
    }
    
    # update quant score info =====
    output$total_score <- renderText({
      scores <- sum(calculate_scores())
      if (scores != values$total_score)
        str_c(scores, " (must add up to ", values$total_score, ")")
      else
        scores
    })
    output$score_diff <- renderText({
      diffs <- calculate_diff()
      if (diffs < 2)
        str_c(diffs, " (must be at least 2)")
      else
        diffs
    })
    
    # save =====
    observeEvent(input$save, {
      message("Saving for student ", values$student$full_name, "...")
      hide("main-panel")
      show("saving-panel")
      save_peer_eval(gs, values$access_code, get_data_as_df())
      showModal(modalDialog(
        h2("Saved"),
        p("Your draft evaluation has been saved. You can continue working on it now or resume at a later point. Make sure to return to submitted it before the deadline."),
        easyClose = TRUE, fade = FALSE
      ))
      hide("saving-panel", anim = TRUE, animType = "fade")   
      show("main-panel")
    })
    
    
    # submitted =====
    observeEvent(input$submitted, {
      # safety checks
      req(values$data)
      req(length(values$data) > 0)
      errors <- c()
      missing_data <- get_data_as_df() %>% 
        filter(is.na(plus) | is.na(minus) | plus == "" | minus == "") %>% 
        left_join(students, by = "access_code")
      if (nrow(missing_data) > 0) {
        if (values$student$access_code %in% missing_data$access_code)
          errors <- c(errors, str_c("self evaluation is incomplete"))
        missing_team <- missing_data %>% filter(access_code != values$student$access_code)
        if (nrow(missing_team) > 0)
          errors <- c(errors, str_c("evaluation for ", missing_team$full_name, " is incomplete"))
      }
      if (sum(calculate_scores()) != values$total_score)
        errors <- c(errors, str_c("quantitative scores must add up to ", values$total_score))
      if (calculate_diff() < 2)
        errors <- c(errors, str_c("quantitative scores must have a minimum difference of 2"))
      
      if (length(errors) > 0) {
        # show errors
        showModal(modalDialog(
          h2("Cannot be submitted yet"),
          p("All fields must be filled out. Please correct the following problems before submittedting your peer evaluation."),
          do.call(tags$ul, args = lapply(errors, tags$li)),
          easyClose = TRUE, fade = FALSE
        ))
      } else {
        # ask if they really want to submitted 
        showModal(modalDialog(
          h2("submitted"),
          p("Are you sure you want to submitted? You cannot go back."),
          footer = tagList(actionButton("confirm", "Yes"), modalButton("Not yet")),
          easyClose = TRUE, fade = FALSE
        ))
      }
    })
    
    observeEvent(input$confirm, {
      message("Submission for student ", values$student$full_name)
      hide("main-panel")
      removeModal()
      show("submit-panel")
      save_peer_eval(gs, values$access_code, get_data_as_df(), submitted = TRUE)
      
      showModal(modalDialog(
        h2("submitted"),
        p("Thank you, your evaluation has been submitted."),
        easyClose = TRUE, fade = FALSE
      ))
      values$access_code <- NULL
      hide("submit-panel")
      show("access-panel")
      hide("main-panel")
    })
    
    # debug ====
    observeEvent(input$debug_trigger, {
      values$debug <- TRUE
      load_access(input$debug_trigger)
    })
    
  })

}
