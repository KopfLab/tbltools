#' Create Readiness Assessment Test from Excel
#' Load RAT questions and answer key from Excel. Note that logical columns are evaluated to TRUE if they are =1, =TRUE, ='TRUE', ='yes' or ='x', everything else is FALSE.
#' @param filepath the path to the excel file
#' @param questions_tab the name of the questions tab (requires at minimum columns 'question', 'answer', and logical TRUE/FALSE 'correct', plus logical 'include' if \code{filter_inclue=TRUE})
#' @param keys_tab the name of th keys tab (requires at mimimum columns 'number', 'option')
#' @param filter_include if set, only keeps questions that have the 'include' column set
#' @param fill_down_questions whether to fill down the questions column (i.e. if question only written in first row)
#' @export
create_RAT_from_excel <- function(filepath, questions_tab = "questions", key_tab = "key", filter_include = TRUE, fill_down_questions = TRUE) {
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
  questions <- filter(questions, !is.na(question))
  create_RAT(questions, answer_key)
}

#' Create RAT
#' @param questions data frame with questions (requires at minimum columns 'question', 'answer', and logical TRUE/FALSE 'correct')
#' @param answer_key data frame with answer key (requires at mimimum columns 'number', 'option')
#' @export
create_RAT <- function(questions, answer_key) {
  # check for required columns
  if (length(missing <- setdiff(c("question", "answer", "correct"), names(questions))) > 0) {
    sprintf("Missing required column(s) on %s tab: %s", questions_tab, str_c(missing, collapse = ", ")) %>% 
      stop(call. = FALSE)
  }
  if (length(missing <- setdiff(c("number", "option"), names(answer_key))) > 0) {
    sprintf("Missing required column(s) on %s tab: %s", key_tab, str_c(missing, collapse = ", ")) %>% 
      stop(call. = FALSE)
  }
  
  # ensure proper data types
  questions <- questions %>% 
    mutate(correct = is_true(correct))
  
  # make sure there is exactly one correct answer per question
  find_trouble <- questions %>% group_by(question) %>% summarize(n_correct = sum(correct))
  if (any(trouble <- find_trouble$n_correct != 1)) {
    stop("The following questions have multiple or no answer:\n  - ", str_c(find_trouble$question[trouble], collapse = "\n  - "), call. = FALSE)
  }
  
  # return
  structure(
    list(
      questions = questions,
      answer_key = answer_key
    ),
    class = "RAT")
}

#' Arrange RAT questions
#' @param rat Readiness Assessment Test object
#' @param by what to arrange by, options: \code{by="original"} leaves the original order as encountered, \code{by="random"} generates a random order, \code{by="semi-random"} generates a random order except for questions that have the \code{fixed_number_column} set, and \code{by="fixed"} used the \code{fixed_number_column} (which has to be set for all questions!). 
#' @param fixed_number_column name of a column in the RAT questions data frame that indicates the fixed question number. Only relevant if \code{by="semi-random"} or \code{by="fixed"}
#' @param group_by_column name of a column in the RAT questions data frame that indicates which questions to group together (groups will be arranged alphabetically, those with undefined group come last). Only relevant if \code{by="random"} or \code{by="semi-random"}. Throws an error if grouping and any fixed number questions are incompatible (e.g. a question has fixed number 5 but is part of the first group of only 3 questions).
#' @param random_seed can overwrite with a fixed value (e.g. \code{random_seed=42}) to get a reproducible "random" order in \code{by="random"} or \code{by="semi-random"} mode.
#' @return returns the RAT with the questions having a new 'number' column
#' @export
arrange_RAT_questions <- function(rat, by = "original", fixed_number_column = NULL, group_by_column = NULL, random_seed = random()) {
  if (!is(rat, "RAT")) 
    stop("can only arrange Readiness Assessment Test classes, found: ", class(rat)[1], call. = FALSE)
  
  # random seed
  random <- function() sample(.Random.seed, 1)
  set.seed(random_seed)
  
  # always check if provided, even if not always relevant (by="original")
  if (!is.null(fixed_number_column) && !fixed_number_column %in% names(rat$questions))
    stop("the provided 'fixed_number_column' ", fixed_number_column, " does not exist for these RAT questions", call. = FALSE)
  if (!is.null(group_by_column) && !group_by_column %in% names(rat$questions)) 
    stop("the provided 'group_by_column' ", group_by_column, " does not exist for these RAT questions", call. = FALSE)
    
  rat$questions$number <- NULL
  if (by == "original") {
    # keep original order (i.e. as encountered)
    numbers_df <- data_frame(question = unique(rat$questions$question), number= 1:length(unique(rat$questions$question)))
  } else if (by == "fixed") {
    # use fixed order
    numbers_df <- rat$questions %>% 
      { .$.fixed_number <- .[[fixed_number_column]]; . } %>% 
      group_by(question) %>% summarize(number = as.integer(.fixed_number[1]))
    if (nrow(missing <- filter(numbers_df, is.na(number))) > 0) {
      stop("The following questions do not have a fixed number:\n  - ", str_c(missing$question, collapse = "\n  - "), call. = FALSE)
    }
  } else if (by %in% c("random", "semi-random")) {
    numbers_df <- rat$questions %>% 
      # set fixed numbers and group column and get values from first entry for the question
      { .$.fixed_number <- if(by == "semi-random") .[[fixed_number_column]] else NA_integer_; . } %>% 
      { .$.group <- if (!is.null(group_by_column)) .[[group_by_column]] else "one group"; . } %>% 
      group_by(question) %>% 
      summarize(.fixed_number = as.integer(.fixed_number[1]), .group = .group[1]) %>% 
      # initial numbering (in groups)
      ungroup() %>% 
      arrange(.group) %>% 
      mutate(.init_n = 1:n()) %>% 
      # get group ranges
      group_by(.group) %>% 
      mutate(.group_n_min = min(.init_n), .group_n_max = max(.init_n))
    
    # safety check about semi-random numbers in fixed position
    if (nrow(trouble <- filter(numbers_df, !is.na(.fixed_number), .fixed_number < .group_n_min | .fixed_number > .group_n_max)) > 0) {
      stop("The following question(s) have a fixed number that is impossible with the provided group arrangements:\n  - ", 
           str_c(with(trouble, sprintf("#%.0f (group range #%.0f-#%.0f): %s", .fixed_number, .group_n_min, .group_n_max, question)), collapse = "\n  - "), call. = FALSE)
    }
    
    # generate random numbers within group (or semi-random if any fixed numbers are set)
    generate_random_group_numbers <- function(fixed_number, group_n_min, group_n_max) {
      fixed_idx <- !is.na(fixed_number)
      choices <- (group_n_min[1]:group_n_max[1]) %>% { .[!. %in% fixed_number[fixed_idx]] }
      number <- rep(NA, length(fixed_number))
      number[fixed_idx] <- fixed_number[fixed_idx]
      number[!fixed_idx] <- sample(choices)
      return(number)
    }
    
    # generate numbers
    numbers_df <- numbers_df %>% 
      group_by(.group) %>% 
      mutate(number = generate_random_group_numbers(.fixed_number, .group_n_min, .group_n_max))
  } else {
    stop("unrecognized 'by' parameter: ", by, call. = FALSE)
  }
  
  # join in numbers
  rat$questions <- rat$questions %>% 
    left_join(select(ungroup(numbers_df), question, number), by = "question") %>%
    select(number, everything()) %>% 
    arrange(number, question)
  return(rat)
}

#' Generate RAT multiple choice questions
#' @inheritParams arrange_RAT_questions
#' @export
generate_RAT_choices <- function(rat) {
  if (!is(rat, "RAT")) 
    stop("can only arrange Readiness Assessment Test classes, found: ", class(rat)[1], call. = FALSE)
  
  # default sort
  if (!"number" %in% names(rat$questions)) {
    rat <- arrange_RAT_questions(rat, by = "original")
  }
  
  invisible(rat)
}


# utilities functions -----

#' @export
print.RAT <- function(x, ...) {
  # default sort
  if (!"number" %in% names(x$questions)) {
    x <- arrange_RAT_questions(x, by = "original")
  }
  
  # get number in there
  x_sum <- x$questions %>% 
    group_by(question, number) %>% 
    summarize(n = n()) %>% 
    mutate(label = str_c("#", number, ": ", question, " (", n, " answers)")) %>% 
    arrange(number)
  cat("Readiness Assessment Test (RAT) with", nrow(x_sum), "questions:\n  -",
      str_c(x_sum$label, collapse = "\n  - "))
  cat("\nAnswer Key:",
      str_c(with(x$answer_key, str_c(number, option)), collapse = ", "))
}

is_true <- function(x) {
  ifelse(is.na(x), FALSE, ifelse(x == TRUE | x == "TRUE" | x == "yes" | x == "x", TRUE, FALSE))
}