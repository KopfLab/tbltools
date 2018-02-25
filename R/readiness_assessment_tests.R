# test setup =========

#' Setup a new RAT from scratch
#' 
#' @export
tbl_setup_RAT_from_scratch <- function() {
  
}

#' Setup a demo RAT
#'
#' @export
tbl_setup_RAT_from_demo <- function() {
  
}

# test generation ==========

#' Create RAT from Excel
#' 
#' Load RAT questions and answer key from Excel. Note that logical columns ('correct' and 'include') are evaluated to TRUE if they are =1, =TRUE, ='TRUE', ='yes' or ='x', everything else is FALSE. Uses \link{tbl_create_RAT_from_data_frame} internally. 
#' 
#' @param filepath the path to the excel file
#' @param questions_tab the name of the questions tab (requires at minimum columns 'question', 'answer', and logical TRUE/FALSE 'correct', plus logical 'include' if \code{filter_include=TRUE})
#' @param keys_tab the name of th keys tab (requires at mimimum columns 'number', 'option')
#' @param filter_include if set, only keeps questions that have the 'include' column set
#' @param fill_down_questions whether to fill down the questions column (i.e. if question an their parameters are only written in first row)
#' @export
tbl_create_RAT_from_excel <- function(filepath, questions_tab = "questions", key_tab = "key", filter_include = TRUE, fill_down_questions = TRUE) {
  questions <- read_excel(filepath, sheet = questions_tab) 
  answer_key <- read_excel(filepath, sheet = key_tab)
  
  # check for required columns
  if (length(missing <- setdiff(c("question", "answer", "correct", if(filter_include) "include"), names(questions))) > 0) {
    sprintf("Missing required column(s) on %s tab: %s", questions_tab, str_c(missing, collapse = ", ")) %>% 
      stop(call. = FALSE)
  }
  if (length(missing <- setdiff(c("number", "option"), names(answer_key))) > 0) {
    sprintf("Missing required column(s) on %s tab: %s", key_tab, str_c(missing, collapse = ", ")) %>% 
      stop(call. = FALSE)
  }
  
  # fill down questions
  if (fill_down_questions) {
    questions <- 
      mutate(questions, .group = cumsum(!is.na(question))) %>% 
      group_by(.group) %>% 
      mutate(question = question[1]) %>% 
      ungroup() %>% 
      select(-.group)
  }
  
  # filter include
  if (filter_include) {
    questions <- 
      group_by(questions, question) %>% 
      mutate(keep = is_true(include[1])) %>% 
      ungroup() %>% 
      filter(keep) %>% 
      select(-keep)
  }
  
  # return
  questions <- filter(questions, !is.na(question), !is.na(answer))
  answer_key <- filter(answer_key, !is.na(option))
  tbl_create_RAT_from_data_frame(questions, answer_key)
}

#' Create RAT from data frame
#' 
#' Note that the logical column 'correct' is evaluated to TRUE if values are =1, =TRUE, ='TRUE', ='yes' or ='x', everything else is FALSE.
#' 
#' @param questions data frame with questions (requires at minimum columns 'question', 'answer', and logical TRUE/FALSE 'correct')
#' @param answer_key data frame with answer key (requires at mimimum columns 'number', 'option')
#' @export
tbl_create_RAT_from_data_frame <- function(questions, answer_key) {
  # check for required columns
  if (length(missing <- setdiff(c("question", "answer", "correct"), names(questions))) > 0) {
    glue("Missing required column(s): {collapse(missing, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  if (length(missing <- setdiff(c("number", "option"), names(answer_key))) > 0) {
    glue("Missing required column(s): {collapse(missing, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  
  # check for reserved columns
  if (length(reserved <- intersect(c("iRAT_n", "tRAT_n"), names(questions)))) {
    glue("Encountered reserved column(s), please don't use these names: {collapse(reserved, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  
  # ensure proper data types
  questions <- questions %>% 
    mutate(correct = is_true(correct))
  answer_key <- answer_key %>% 
    mutate(number = suppressWarnings(as.integer(number)))
  
  # make sure there is exactly one correct answer per question
  find_trouble <- questions %>% group_by(question) %>% summarize(n_correct = sum(correct))
  if (any(trouble <- find_trouble$n_correct != 1)) {
    stop("The following questions have multiple or no answer:\n  - ", str_c(find_trouble$question[trouble], collapse = "\n  - "), call. = FALSE)
  }
  
  # make sure answer key is all valid numbers
  if (any(is.na(answer_key$number))) {
    stop("answer key has invalid number(s) in number column.", call. = FALSE)
  }
  
  # make sure answer key is all upercase letters
  if (any(trouble <- !answer_key$option %in% LETTERS)) {
    stop("The following answer key options are invalid uppercase letters: ", str_c(answer_key$option[trouble], collapse = ", "), call. = FALSE)
  }
  
  # numbering (in order encountered)
  questions <- questions %>% nest(-question) %>% mutate(tRAT_n = row_number(), iRAT_n = tRAT_n) %>% unnest()
  
  # return
  structure(
    list(
      questions = questions,
      answer_key = answer_key
    ),
    class = "RAT")
}

#' Arrange RAT questions
#' 
#' Function to make it easy to arrange an RAT either by fixed numbers (\code{fixed_number_column}) or semi-randomly or completely randomly. Can also arrange questions within specific groups using the \code{group_by_column}, and can use specific starting numbers for iRAT and tRAT numbering (which need not be the same in case tRATs need to match a specific IF-AT number sequence and iRATs should match a scantron). In case iRAT numbers are different from tRAT numbers, the tRAT numbering is the one matched to the answer key since this needs to match the IF-AT scratch-off.
#' 
#' @param rat Readiness Assessment Test object
#' @param by what to arrange by, options: \code{by="original"} leaves the original order as encountered, \code{by="random"} generates a random order, \code{by="semi-random"} generates a random order except for questions that have the \code{fixed_number_column} set, and \code{by="fixed"} uses the \code{fixed_number_column} (which has to be set for all questions in the latter case!). 
#' @param tRAT_n_start the start number for the tRAT question numbering (if the rRAT should fit a different part of an IF-AT), only matters if \code{by} is NOT \code{"fixed"}. Default is 1.
#' @param iRAT_n_start the start number for the iRAT question numbering. By default the same as \cde{tRAT_n_start}. Specify a different value from \code{tRAT_n_start} e.g. if tRAT starts at a higher IF-AT number but iRAT numbers should start at 1. 
#' @param iRAT_sel_per_q number of selections per question for the iRAT portion of this test, e.g. when using scantrons and giving the students the option to hedge their bets on the iRAT portion of the test. 
#' @param fixed_number_column name of a column in the RAT questions data frame that indicates the fixed question number. Only relevant if \code{by="semi-random"} or \code{by="fixed"}
#' @param group_by_column name of a column in the RAT questions data frame that indicates which questions to group together (groups will be arranged alphabetically, those with undefined group come last). Only relevant if \code{by="random"} or \code{by="semi-random"}. Throws an error if grouping and any fixed number questions are incompatible (e.g. a question has fixed number 5 but is part of the first group of only 3 questions).
#' @param random_seed can overwrite with a fixed value (e.g. \code{random_seed=42}) to get a reproducible "random" order in \code{by="random"} or \code{by="semi-random"} mode.
#' @return returns the RAT object with the location in the test specified
#' @export
tbl_arrange_RAT_questions <- function(rat, by = "original", 
                                      tRAT_n_start = 1, iRAT_n_start = tRAT_n_start, iRAT_sel_per_q = 1, 
                                      fixed_number_column = NULL, group_by_column = NULL, random_seed = random()) {
  if (!is(rat, "RAT")) 
    stop("can only arrange Readiness Assessment Test objects, found: ", class(rat)[1], call. = FALSE)
  
  # iRAT n_start
  iRAT_n_start_quo <- enquo(iRAT_n_start)
  iRAT_eq_tRAT_start <- quo_text(iRAT_n_start_quo) == "tRAT_n_start"
  iRAT_n_start <- eval_tidy(iRAT_n_start_quo)
  
  # random seed
  random <- function() {
    rn <- sample(.Random.seed, 1)
    if (by %in% c("random", "semi-random"))
      glue("Info: generating RAT arrangement with 'random_seed = {rn}' ", 
           "(include this in the function call to generate the exact same arrangement again).") %>%
      message()
    return(rn)
  }
  set.seed(random_seed)
  
  # always check if provided, even if not always relevant (by="original")
  if (by %in% c("semi-random", "fixed") && is.null(fixed_number_column) )
    stop("no 'fixed_number_column' provided for arrangement mode '", by, "'", call. = FALSE)
  if (!is.null(fixed_number_column) && !fixed_number_column %in% names(rat$questions))
    stop("the provided 'fixed_number_column' ", fixed_number_column, " does not exist for these RAT questions", call. = FALSE)
  if (!is.null(group_by_column) && !group_by_column %in% names(rat$questions)) 
    stop("the provided 'group_by_column' ", group_by_column, " does not exist for these RAT questions", call. = FALSE)
    
  # warnings for meaningless specifications
  if (!is.null(fixed_number_column) && !by %in% c("semi-random", "fixed"))
    glue("specifying fixed_number_column='{fixed_number_column}' has no effect in arrangement mode '{by}'") %>% 
    warning(immediate. = TRUE, call. = FALSE)
  
  if (!is.null(group_by_column) && !by %in% c("semi-random", "random"))
    glue("specifying group_by_column='{group_by_column}' has no effect in arrangement mode '{by}'") %>% 
    warning(immediate. = TRUE, call. = FALSE)
  
  if (tRAT_n_start != 1 && by == "fixed")
    glue("specifying tRAT_n_start={tRAT_n_start} has no effect in arrangement mode 'fixed'") %>% 
    warning(immediate. = TRUE, call. = FALSE)
  
  # reset questions iRAT/tRAT
  rat$questions <- rat$questions %>% 
    select(-tRAT_n, -iRAT_n)
  
  if (by == "original") {
    # us original order (i.e. as encountered) ====
    rat$questions <- rat$questions %>% 
      nest(-question) %>% 
      mutate(tRAT_n = row_number() + tRAT_n_start - 1L, 
             iRAT_n = iRAT_sel_per_q * (row_number() - 1L) + iRAT_n_start) %>% 
      unnest()
  } else if (by == "fixed") {
    # use fixed order =====
    rat$questions <- rat$questions %>% 
      group_by(question) %>% 
      mutate(tRAT_n = as.integer((!!sym(fixed_number_column))[1])) %>% 
      ungroup()
    if (nrow(missing <- filter(rat$questions, is.na(tRAT_n))) > 0) {
      stop("The following questions do not have a fixed number:\n  - ", str_c(unique(missing$question), collapse = "\n  - "), call. = FALSE)
    }
    # adjust iRAT numbering too
    if (iRAT_eq_tRAT_start) iRAT_n_start <- min(rat$questions$tRAT_n)
    rat$questions <- rat$questions %>% 
      mutate(iRAT_n = iRAT_sel_per_q * (tRAT_n - min(tRAT_n)) + iRAT_n_start)
  } else if (by %in% c("random", "semi-random")) {
    # random and semi-random =====
    numbers <- rat$questions %>% 
      # set fixed numbers and group column and get values from first entry for the question
      { .$.fixed_number <- if(by == "semi-random") .[[fixed_number_column]] else NA_integer_; . } %>% 
      { .$.group <- if (!is.null(group_by_column)) .[[group_by_column]] else "one group"; . } %>% 
      group_by(question) %>% 
      summarize(.fixed_number = as.integer(.fixed_number[1]), .group = .group[1]) %>% 
      # initial numbering (in groups)
      ungroup() %>% 
      arrange(.group) %>% 
      mutate(.init_n = row_number() + tRAT_n_start - 1L) %>% 
      # get group ranges
      group_by(.group) %>% 
      mutate(.group_n_min = min(.init_n), .group_n_max = max(.init_n))
    
    # safety check about semi-random numbers in fixed position
    if (nrow(trouble <- filter(numbers, !is.na(.fixed_number), .fixed_number < .group_n_min | .fixed_number > .group_n_max)) > 0) {
      stop("The following question(s) have a fixed number that is impossible with the provided group arrangements:\n  - ", 
           str_c(with(trouble, sprintf("#%.0f (group range #%.0f-#%.0f): %s", .fixed_number, .group_n_min, .group_n_max, question)), collapse = "\n  - "), call. = FALSE)
    }
    if (nrow(trouble <- filter(numbers, !is.na(.fixed_number), duplicated(.fixed_number) | duplicated(.fixed_number, fromLast = TRUE))) > 0) {
      stop("The following question(s) have duplicate fixed numbers:\n  - ", 
           str_c(with(trouble, sprintf("#%.0f (group range #%.0f-#%.0f): %s", .fixed_number, .group_n_min, .group_n_max, question)), collapse = "\n  - "), call. = FALSE)
    }
    
    # generate random numbers within group (or semi-random if any fixed numbers are set)
    generate_random_group_numbers <- function(fixed_number, group_n_min, group_n_max) {
      fixed_idx <- !is.na(fixed_number)
      number <- rep(NA, length(fixed_number))
      
      # find choices
      choices <- (group_n_min[1]:group_n_max[1])
      if (sum(fixed_idx) > 0)
        choices <- choices[!choices %in% fixed_number[fixed_idx]]
      
      # assigning numbers
      if (sum(fixed_idx) > 0) number[fixed_idx] <- fixed_number[fixed_idx]
      if (sum(!fixed_idx) > 0) number[!fixed_idx] <- sample(choices, size = sum(!fixed_idx))
    
      return(number)
    }
    
    # generate tRAT and iRAT numbers
    numbers <- numbers %>% 
      group_by(.group) %>% 
      mutate(tRAT_n = generate_random_group_numbers(.fixed_number, .group_n_min, .group_n_max)) %>% 
      ungroup()
    if (iRAT_eq_tRAT_start) iRAT_n_start <- min(numbers$tRAT_n)
    numbers <- numbers %>% 
      mutate(iRAT_n = iRAT_sel_per_q * (tRAT_n - min(tRAT_n)) + iRAT_n_start) %>% 
      select(question, tRAT_n, iRAT_n)

    # join in numbers
    rat$questions <- rat$questions %>% 
      left_join(numbers, by = "question") 
    
  } else {
    stop("unrecognized 'by' parameter: ", by, call. = FALSE)
  }
  
  # make sure numbering is integers
  rat$questions <- rat$questions %>% 
    mutate(tRAT_n = as.integer(tRAT_n), iRAT_n = as.integer(iRAT_n)) 
  
  # check that answer key is consistents
  get_RAT_options(rat)
  
  return(rat)
}

#' Generate RAT choices
#' 
#' Generates the actual multiple choice questions in an RAT
#' 
#' @inheritParams tbl_arrange_RAT_questions
#' @param answer_layout how to arrange the answers, layouts supported by default are \code{"vertical"} and \code{"horizontal"} (recommended for image answers). The layout can be overwritten for individual questions by setting the \code{answer_layout_column} parameter. Custom answer layouts can be provided using the \code{answer_layout_funcs} parameter. 
#' @param answer_layout_column set this parameter to a column name in the questions data frame that has a different layout name for questions that are indended to deviate from the default layout (\code{answer_layout}). All layouts must be defined in the \code{answer_layout_funs} (\code{"vertical"} and \code{"horizontal"} by default).
#' @param answer_layout_funcs Specify custom answer layouts by providing layout functions that differ from the default. See \code{tbl_default_RAT_layouts} for details on how these work.
#' @param random_seed can overwrite with a fixed value (e.g. \code{random_seed=42}) to get a reproducible "random" order of the answer options
#' @export
tbl_generate_RAT_choices <- function(rat, answer_layout = "vertical", 
                                     answer_layout_column = NULL, answer_layout_funs = tbl_default_RAT_layouts(), 
                                     random_seed = random()) {
  if (!is(rat, "RAT")) 
    stop("can only use Readiness Assessment Test objects, found: ", class(rat)[1], call. = FALSE)
  
  # safety checks
  layout_options <- names(answer_layout_funs)
  if (!answer_layout %in% layout_options)
    stop("Unsupported answer layout: ", answer_layout, ". Only know: ", str_c(layout_options, collapse = ", "), call. = FALSE)
  if (!is.null(answer_layout_column) && !answer_layout_column %in% names(rat$questions)) 
    stop("the provided 'answer_layout_column' ", answer_layout_column, " does not exist for these RAT questions", call. = FALSE)
  
  # random seed
  random <- function() {
    rn <- sample(.Random.seed, 1)
    glue("Info: generating order of answer choices with 'random_seed = {rn}' ",
         "(include this in the function call to generate the exact same order again).") %>% 
      message()
    return(rn)
  }
  set.seed(random_seed)
  
  # get RAT options
  rat_options <- get_RAT_options(rat)
  
  # layout
  if (!is.null(answer_layout_column)) {
    rat_options <- rat_options %>% 
      { .$.layout <- .[[answer_layout_column]]; . } %>% 
      group_by(question) %>% 
      mutate(.layout = ifelse(!is.na(.layout[1]), .layout[1], answer_layout)) %>% 
      ungroup()
  } else {
    rat_options$.layout <- answer_layout
  } 
  
  # check for incorrect layout information
  trouble <- filter(rat_options, !.layout %in% layout_options) %>% select(tRAT_n, .layout, question) %>% unique()
  if (nrow(trouble) > 0) {
    stop("The following question(s) have unrecognized layout settings:\n  - ", 
         str_c(with(trouble, sprintf("#%.0f (unknown layout '%s'): %s", 
                                     tRAT_n, .layout, question)), collapse = "\n  - "), call. = FALSE)
  }
  
  # assign options
  assign_options <- function(correct, option, n_options) {
    answers <- sample(LETTERS[1:n_options[1]])
    correct_idx <- which(correct)
    random_correct_idx <- which(answers == option[1])
    answers[random_correct_idx] <- answers[correct_idx]
    answers[correct_idx] <- option[1]
    return(answers)
  } 
  rat_options <- rat_options %>% 
    group_by(question) %>% 
    mutate(answer_option = assign_options(correct, option, n_options)) %>%
    # sort by question # and options (A-X)
    arrange(tRAT_n, answer_option) 

  # assemble question in markdown
  group_by(rat_options, tRAT_n, question) %>% 
    do({
      with(., {
        data_frame(
          label = sprintf(
            "### %s: %s\n%s", 
            get_question_number(iRAT_n[1], tRAT_n[1]), question[1], answer_layout_funs[[.layout[1]]](answer_option, answer))
        )
      })
    }) %>% 
    # print to document
    { cat(str_c(.$label, collapse = "\n\n")) }
  
  invisible(rat)
}


# utilities functions -----

#' Retrieve default RAT layouts
#' 
#' This function provides the default layout options for TBL questions (horizontal and vertical arrangement) but can easily be expanded with custom layouts. To introduce additional/alternative question layouts, simply overwrite the \code{answer_layout_funs} parameter in \link{tbl_generate_RAT_choices} with a list of functions that have the layout names as keys and functions that take two parameters (a vector of \code{answer_option} letters A, B, C, D, etc. and an \code{answer} vector of the same length with the actual answers) as values. The functions must return valid markdown. 
#' 
#' @export
tbl_default_RAT_layouts <- function() {
  list(
    horizontal = function(answer_option, answer) { 
      str_c(str_c(answer_option, ": ", answer), collapse = ", ")
    },
    vertical = function(answer_option, answer) { 
      str_c(" - ", str_c(str_c(answer_option, ": ", answer), collapse = "\n - ")) 
    }
  )
}

# get iRAT / tRAT number
get_question_number <- function(iRAT_n, tRAT_n) {
  
  iRAT_sel_n <- iRAT_n %>% sort() %>% diff()
  if (is_empty(iRAT_sel_n)) iRAT_sel_n <- 1
  if (!all(iRAT_sel_n == iRAT_sel_n[1])) {
    glue("inconsistent number of selection options for iRAT questions: {collapse(iRAT_sel_n, sep = ', ')}") %>% 
      stop(call. = FALSE)
  }
  iRAT_sel_n <- iRAT_sel_n[1]
  
  iRAT_n_text <- 
    if (iRAT_sel_n > 1) sprintf("%.0f-%.0f", iRAT_n, iRAT_n + iRAT_sel_n - 1)
    else sprintf("%.0f", iRAT_n)
  tRAT_n_text <- sprintf("%.0f", tRAT_n)
  
  ifelse(iRAT_n_text != tRAT_n_text, sprintf("iRAT %s / tRAT %s", iRAT_n_text, tRAT_n_text), iRAT_n_text)
}

# get RAT options and check answer key consistency
# helper function to check that each question has a sufficient number of answer options for the designated answer key
# returns RAT options
get_RAT_options <- function(rat) {
  
  # join in answer key
  rat_options <- left_join(rat$questions, rat$answer_key, by = c("tRAT_n" = "number")) 
  if (nrow(missing <- filter(rat_options, is.na(option))) > 0) {
    stop("Missing answer key for question numbers\n ", str_c(missing$number %>% unique(), collapse = ", "), call. = FALSE)
  }
  
  # check for troubles where not as many answers as options
  rat_options <- rat_options %>% 
    group_by(question) %>%
    mutate(
      n_options = n(),
      n_correct = which(LETTERS == option[1])
    ) %>% 
    ungroup()
  trouble <- filter(rat_options, n_correct > n_options) %>% select(tRAT_n, question, n_options, option) %>% unique()
  if(nrow(trouble) > 0) {
    trouble <- trouble %>% 
      mutate(label = as.character(glue("#{tRAT_n} (only {n_options} answers but '{option}' correct): {question}")))
    glue("The following question(s) do NOT have enough possible answers to fit the correct option, ",
         "consider using by='fixed' or by='semi-random' question arrangement ",
         "to make sure these are located at an appropriate position in the answer key:\n",
         "  - {collapse(trouble$label, sep = '\n  - ')}") %>% 
      stop(call. = FALSE)
  }
  
  return(rat_options)
}

#' @export
print.RAT <- function(x, ...) {
  # get number in there
  x_sum <- x$questions %>% 
    group_by(question, iRAT_n, tRAT_n) %>% 
    summarize(answers = n()) %>% 
    ungroup() %>% 
    mutate(label = str_c("#", get_question_number(iRAT_n, tRAT_n), ": ", question, " (", answers, " answers)")) %>% 
    arrange(iRAT_n)
  cat("Readiness Assessment Test (RAT) with", nrow(x_sum), "questions:\n  -",
      str_c(x_sum$label, collapse = "\n  - "))
  cat("\nAnswer Key:",
      str_c(with(x$answer_key, str_c(number, option)), collapse = ", "))
}

# check for values that all count as true
is_true <- function(x) {
  ifelse(is.na(x), FALSE, ifelse(x == TRUE | x == "TRUE" | x == "true" | x == "True" | x == "yes" | x == "x", TRUE, FALSE))
}