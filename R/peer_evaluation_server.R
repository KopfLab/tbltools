# safety checks for student roster data frame
check_student_roster <- function(roster) {
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
  
  return(roster)
}

#' Peer Evaluation App Server
#'
#' Generates the server part of the peer evaluation app
#' @param roster data frame with the student roster
peer_evaluation_server <- function(roster) {
  
  # safety check for the student roster
  students <- check_student_roster(roster) %>% 
    # construct full and short student names
    mutate(full = str_c(first, " ", last), short = full)

  # FIXME: continue here - maybe have a spreadsheet object passed in
  # use gs %>% gs_ws_ls() to get the list of titles
  # us gs_add_row to add rows (just add additional rows for each safe, easier than deleting previous ones)
  
  shinyServer(function(input, output, session) {
    
    message("\n\nINFO: Loading GUI instance ...")
    
    values <- reactiveValues(
      debug = FALSE,
      access_code = NULL,
      student = NULL,
      data = list(), 
      team_mates = c(), # the list of team mate names
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
    
    get_student_data <- function(student, field, default = "") {
      value <- values$data[[student]][[field]]
      if (is.null(value) || is.na(value)) value <- default
      return(value)
    }
    
    calculate_scores <- reactive({
      get_data_as_df()$score %>% na.omit()
    })
    
    calculate_diff <- reactive({
      diff(range(calculate_scores()))
    })
    
    # paths ====
    get_submission_path <- reactive({ file.path("data", str_c(values$student$full, " answers.submitted.csv")) })
    get_answers_path <- reactive({ file.path("data", str_c(values$student$full, " answers.csv")) })
    
    # load access code ====
    load_access <- function(entered_access_code) {
      message("Checking access code: ", entered_access_code)
      student <- filter(students, tolower(access_code) == tolower(entered_access_code))
      if (nrow(student) == 0) {
        showModal(modalDialog(h2(str_c("Unknown access code: ", entered_access_code)), easyClose = TRUE, fade = FALSE))
      } else if (is.null(values$access_code) || values$access_code != entered_access_code) {
        values$student <- as.list(student[1,])
        if (file.exists(get_submission_path())) {
          # already submitted
          message("Already submitted: ", entered_access_code)
          showModal(modalDialog(h2("Peer evaluation already submitted."), easyClose = TRUE, fade = FALSE))
        } else {
          # not yet submitted
          message("Setting access code: ", entered_access_code)
          values$access_code <- entered_access_code 
          if (file.exists(get_answers_path())) {
            message("Resuming previous session")
            data <- read_csv(get_answers_path(), col_types = "ccci")
          } else {
            message("First time session")
            data <- students %>% 
              filter(team == values$student$team) %>% 
              arrange(short, access_code) %>% 
              select(short) %>% 
              mutate(plus = "", minus = "", score = NA_integer_)
          }
          if (nrow(data) == 0) stop("no data provided", call. = FALSE)
          values$data <- list()
          for (i in 1:nrow(data)) {
            values$data[[data[["short"]][i]]] <- as.list(data[i,])
          }
          values$team_mates <- data %>% filter(short != values$student$short) %>% {.$short}
          values$total_score <- length(values$team_mates)*10
        }
      }
    }
    
    observeEvent(input$access, load_access(input$access_code))
    
    # load main UI ====
    observeEvent(values$access_code, {
      req(values$student)
      message("Showing UI for student ", values$student$short)
      hide("access_panel")
      if (values$debug == FALSE) {
        showModal(modalDialog(
          h2(str_c("Welcome ", values$student$first)),
          h4(str_c("Team: ", values$student$team)),
          p("Please reflect on your own efforts and provide qualitative and quantitative feedback on your teammates' contributions to your team's performance in the class. The quantitative feedback will be used as part of the overall course grade. For the qualitative feedback, focus on specific behaviors and their impacts on team performance. Write about something you or your teammate does well and at least one area for improvement for their future teamwork."),
          p("You can save your answers at any point by clicking the ", strong("Save"), " button and resume the peer evaluation at a later point simply by returning to this page and entering your access code again. To submit your peer evaluation, please click the ", strong("Submit"), " button. Once you submit, you can no longer change your answers."),
          footer = modalButton("Ok"),
          easyClose = TRUE, fade = FALSE
        ))
      }
      show("main_panel")
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
        message("Generating tabs for teammates: ", str_c(values$team_mates, collapse = ", "))
        tabs <- c(
          list(get_qual_ui_self(values$student$short)),
          lapply(values$team_mates, get_qual_ui_team_mate),
          list(get_quant_scores_ui(values$team_mates))
        )
        
        # full tag list
        tagList(
          column(12, actionButton("save", "Save"), actionButton("submit", "Submit"), align = "right"),
          column(12, 
                 do.call(tabsetPanel, args = c(tabs, list(selected = "Self evaluation")))
          )
        )
      })
    })
    
    # evaluation gui
    get_qual_ui_self <- function(team_mate) {
      tabPanel(
        "Self evaluation",
        column(6, 
               h5("What have you done well to positively impact your team's performance?"),
               textAreaInput(
                 str_c("plus_", team_mate), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate, "plus"), label = NULL)),
        column(6, 
               h5("What could you do differently to increase your contribution to your team?"),
               textAreaInput(
                 str_c("minus_", team_mate), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate, "minus"), label = NULL))
      )
    }
    
    get_qual_ui_team_mate <- function(team_mate) {
      tabPanel(
        team_mate,
        column(6, 
               h5(str_c("What did ", str_extract(team_mate, "^(\\w+)"), 
                        " do well to positively impact your team's performance?")),
               textAreaInput(
                 str_c("plus_", team_mate), width = "100%", height = "200px", resize = "vertical",
                 value = get_student_data(team_mate, "plus"), label = NULL)),
        column(6, 
               h5(str_c("What could ", str_extract(team_mate, "^(\\w+)"), 
                        " do differently to increase his/her/their contribution to your team?")),
               textAreaInput(
                 str_c("minus_", team_mate), width = "100%", height = "200px", resize = "vertical",
                 value = get_student_data(team_mate, "minus"), label = NULL))
      )
    }
    
    # qunatitative scores
    get_quant_scores_ui <- function(team_mates) {
      
      team_mate_tags <- function(name) {
        tagList(column(3, align="left", h5(name)), 
                column(9, numericInput(
                  str_c("score_", name), NULL, 
                  value = get_student_data(name, "score", default = 10), 
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
        do.call(tagList, lapply(team_mates, team_mate_tags)),
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
      message("Saving for student ", values$student$short, " to ", get_answers_path())
      write.csv(get_data_as_df(), file = get_answers_path(), row.names = FALSE)
      showModal(modalDialog(
        h2("Saved"),
        p("Your draft evaluation has been saved. You can continue working on it now or resume at a later point. Make sure to return to submit it before the deadline."),
        easyClose = TRUE, fade = FALSE
      ))
    })
    
    
    # submit =====
    observeEvent(input$submit, {
      # safety checks
      req(values$data)
      req(length(values$data) > 0)
      errors <- c()
      missing_data <- get_data_as_df() %>% 
        filter(is.na(plus) | is.na(minus) | plus == "" | minus == "")
      if (nrow(missing_data) > 0) {
        if (values$student$short %in% missing_data$short)
          errors <- c(errors, str_c("self evaluation is incomplete"))
        errors <- c(errors, str_c("evaluation for ", missing_data$short %>% { .[.!=values$student$short]}, " is incomplete"))
      }
      if (sum(calculate_scores()) != values$total_score)
        errors <- c(errors, str_c("quantitative scores must add up to ", values$total_score))
      if (calculate_diff() < 2)
        errors <- c(errors, str_c("quantitative scores must have a minimum difference of 2"))
      
      if (length(errors) > 0) {
        # show errors
        showModal(modalDialog(
          h2("Cannot submit yet"),
          p("All fields must be filled out. Please correct the following problems before submitting your peer evaluation."),
          do.call(tags$ul, args = lapply(errors, tags$li)),
          easyClose = TRUE, fade = FALSE
        ))
      } else {
        # ask if they really want to submit 
        showModal(modalDialog(
          h2("Submit"),
          p("Are you sure you want to submit? You cannot go back."),
          footer = tagList(actionButton("confirm", "Yes"), modalButton("Not yet")),
          easyClose = TRUE, fade = FALSE
        ))
      }
    })
    
    observeEvent(input$confirm, {
      message("Submission for student ", values$student$short, " to ", get_submission_path())
      write.csv(get_data_as_df(), file = get_submission_path(), row.names = FALSE)
      showModal(modalDialog(
        h2("Submit"),
        p("Thank you, your evaluation has been submitted."),
        easyClose = TRUE, fade = FALSE
      ))
      values$access_code <- NULL
      show("access_panel")
      hide("main_panel")
    })
    
    # debug ====
    observeEvent(input$debug_trigger, {
      values$debug <- TRUE
      load_access("VUI7")
    })
    
  })

}
