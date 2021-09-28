
#' Peer Evaluation App Server
#'
#' Generates the server part of the peer evaluation app
#' @param roster data frame with the student roster
#' @inheritParams tbl_setup_peer_evaluation
#' @param welcome_md_file path to a markdown (.md) file for the login welcome message
#' @param self_eval_plus_md_file markdown file for the "plus" self evaluation message
#' @param self_eval_minus_md_file markdown file for the "minus" self evaluation message
#' @param teammate_eval_plus_md_file markdown file for the "plus" teammate evaluation message
#' @param teammate_eval_minus_md_file markdown file for the "minus" teammate evaluation message
#' @param quant_scores_md_file markdown file for the quantiative scores message
#' @param points_per_teammate points per teammate
#' @param max_points the maximum number of points allowed per team member
#' @param min_points the smallest number of points allowed per team member
#' @param min_point_difference the minimum point difference required for the scores (set to 0 to allow all scores to be identical)
#' @param require_self_eval whether the qualitative self evaluation is required
#' @param require_peer_evals whether the qualitative self evaluation is required
#' @param auto_login_access_code set an automatic login access code for testing and debugging purposes
peer_evaluation_server <- function(roster, data_gs_title, gs_key_file, 
                                   welcome_md_file, self_eval_plus_md_file, self_eval_minus_md_file, 
                                   teammate_eval_plus_md_file, teammate_eval_minus_md_file, quant_scores_md_file,
                                   points_per_teammate = 10, max_points = 15, min_points = 0, min_point_difference = 2,
                                   require_self_eval = TRUE,
                                   require_peer_evals = TRUE,
                                   auto_login_access_code = NULL) {
  
  # gloval vars
  access_code <- first <- last <- plus <- minus <- team <- full_name <- score <- NULL
  
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
  gs <- tbl_check_gs_access(gs_key_file = gs_key_file, data_gs_title = data_gs_title)
  
  shinyServer(function(input, output, session) {
    
    message("\n\nINFO: Loading GUI instance ...")
    
    values <- reactiveValues(
      auto_login = FALSE,
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
          # never update score when it's the self-evaluation
          if (field == "score" && team_mate == values$student$access_code) next
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
      data <- get_data_as_df()
      if (nrow(data) == 0) return(c(0))
      else data$score %>% stats::na.omit()
    })
    
    calculate_diff <- reactive({
      diff(range(calculate_scores()))
    })
    
    # load access code ====
    load_access <- function(entered_access_code) {
      pure_access_code <- entered_access_code
      entered_access_code <- str_c(access_code_prefix, entered_access_code)
      message("Checking access code: ", entered_access_code)
      hide("access-panel")
      show("loading-panel")
      # enforce case sensitive access code
      student <- filter(students, access_code == entered_access_code)
      load_access_code <- FALSE
      if (nrow(student) == 0) {
        showModal(modalDialog(h2(str_c("Unknown access code: ", pure_access_code)), easyClose = TRUE, fade = FALSE))
      } else if (is.null(values$access_code) || values$access_code != entered_access_code) {
        
        # save student info
        values$student <- as.list(student[1,])
        
        # retrieve data from google spreadsheet
        pe_data <- read_peer_eval(gs, entered_access_code)

        # figure out what to do with the data
        if (nrow(pe_data) == 0) {
          # completely new data set (i.e. no tab yet)
          message("Into: first time session - creating new data frame")
          load_access_code <- TRUE
        } else if (pe_data$submitted[1]) {
          # already submitted
          message("Info: already submitted: ", entered_access_code)
          showModal(modalDialog(h2("Peer evaluation already submitted."), easyClose = TRUE, fade = FALSE))
        } else {
          # resuming
          message("Info: resuming previous session")
          load_access_code <- TRUE
        }
    
      }
      
      # load access code
      if (load_access_code) {
        message("Info: loading GUI for access code: ", entered_access_code)
        
        # merge in student info in case any are missing from the data
        pe_data <- select(pe_data, access_code, plus, minus, score)
        pe_data <- students %>% 
          filter(team == values$student$team) %>% 
          arrange(access_code) %>% 
          select(access_code) %>% 
          left_join(pe_data, by = "access_code") %>% 
          mutate(
            plus = ifelse(is.na(plus), "", plus), 
            minus = ifelse(is.na(minus), "", minus)
          )
        
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
      req(values$access_code)
      message("Info: showing GUI for student ", values$student$full_name)
      if (values$auto_login == FALSE) {
        showModal(modalDialog(
          h2(str_c("Welcome ", values$student$first)),
          h4(str_c("Team: ", values$student$team)),
          includeMarkdown(welcome_md_file),
          footer = modalButton("Ok"),
          easyClose = TRUE, fade = FALSE
        ))
      }
      values$auto_login <- FALSE
    })
    
    # evaluation panels ===
    output$main <- renderUI({
      values$access_code
      if (is.null(values$access_code))  return(NULL)
      
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
          column(12, actionButton("save", "Save"), actionButton("submitted", "Submit"), actionButton("logout", "Logout"), align = "right"),
          column(12, 
                 do.call(tabsetPanel, args = c(tabs, list(
                   selected = if (require_self_eval) "Self evaluation" else "Quantitative Scores"
                 )))
          )
        )
      })
    })
    
    # evaluation gui
    get_qual_ui_self <- function(team_mate_access_code) {
      tabPanel(
        "Self evaluation",
        column(6, 
               includeMarkdown(self_eval_plus_md_file),
               textAreaInput(
                 str_c("plus_", team_mate_access_code), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate_access_code, "plus"), label = NULL)),
        column(6, 
               includeMarkdown(self_eval_minus_md_file),
               textAreaInput(
                 str_c("minus_", team_mate_access_code), width = "100%", height = "200px", resize = "both",
                 value = get_student_data(team_mate_access_code, "minus"), label = NULL))
      )
    }
    
    get_qual_ui_team_mate <- function(team_mate_access_code, team_mate_name) {
      tabPanel(
        team_mate_name,
        column(6, 
               includeMarkdown(teammate_eval_plus_md_file),
               textAreaInput(
                 str_c("plus_", team_mate_access_code), width = "100%", height = "200px", resize = "vertical",
                 value = get_student_data(team_mate_access_code, "plus"), label = NULL)),
        column(6, 
               includeMarkdown(teammate_eval_minus_md_file),
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
                  min=min_points, max=max_points, step=1, width = "80px")))
      }
      
      tabPanel(
        "Quantitative Scores",
        br(),
        includeMarkdown(quant_scores_md_file),
        
        tags$ol(
          tags$li(
            glue("Assign an average of {points_per_teammate} points to the other members of your team. Thus, you will assign a total of {values$total_score} points.")),
          tags$li(
            glue("Assign points from {min_points} to {max_points} (inclusive), whole numbers only.")),
          if (min_point_difference > 0)
            tags$li(
              glue("Differentiate some in your ratings. Your score range (max-min) must be at least {min_point_difference}. For example, you must give at least one score of {points_per_teammate + min_point_difference/2} or higher and one score of {points_per_teammate - min_point_difference/2} or lower."))
        ),
        
        do.call(tagList, map2(names(team_mates), team_mates, team_mate_tags)),
        column(3, h5("Total:")),
        column(9, h5(textOutput("total_score"))),
        if (min_point_difference > 0) column(3, h5("Difference:")),
        if (min_point_difference > 0) column(9, h5(textOutput("score_diff")))
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
      if (diffs < min_point_difference)
        str_c(diffs, " (must be at least ", min_point_difference, ")")
      else
        diffs
    })
    
    # save =====
    observeEvent(input$save, {
      req(values$access_code)
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
      req(values$access_code)
      req(values$data)
      req(length(values$data) > 0)
      errors <- c()
      missing_data <- get_data_as_df() %>% 
        filter(is.na(plus) | is.na(minus) | plus == "" | minus == "") %>% 
        left_join(students, by = "access_code")
      if (nrow(missing_data) > 0) {
        if (require_self_eval && values$student$access_code %in% missing_data$access_code)
          errors <- c(errors, str_c("self evaluation is incomplete"))
        missing_team <- missing_data %>% filter(access_code != values$student$access_code)
        if (require_peer_evals && nrow(missing_team) > 0)
          errors <- c(errors, str_c("evaluation for ", missing_team$full_name, " is incomplete"))
      }
      if (sum(calculate_scores()) != values$total_score)
        errors <- c(errors, str_c("quantitative scores must add up to ", values$total_score))
      if (calculate_diff() < min_point_difference)
        errors <- c(errors, str_c("quantitative scores must have a minimum difference of ", min_point_difference))
      
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
          p("Are you sure you want to submit? You cannot go back."),
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
      logout_user()
    })
    
    # log out ====
    
    logout_user <- function() {
      message("Info: logging out ", values$student$full_name)
      values$access_code <- NULL
      values$data <- list()
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
    
  })

}
