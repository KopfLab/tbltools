context("Readiness Assessment Tests")

# test data frame
questions <-
  bind_rows(
    tibble(
      question = "1+1",
      number = 5,
      group = "A",
      layout = "horizontal",
      answer = as.character(1:5),
      correct = c(F,T,F,F,F)
    ),
    tibble(
      question = "what says the cow", 
      number = 3,
      group = "B",
      layout = "vertical",
      answer = c("baah", "miau", "wuff", "mooo"),
      correct = c(F,F,F,T)
    ),
    tibble(
      question = "is 42 the answer to everything?", 
      number = 4,
      group = "B",
      layout = "vertical",
      answer = c("yes", "no", "maybe"),
      correct = c(F,F,T)
    )
  )

answer_key <-
  tibble(
    number = 1:50,
    option = sample(c("A", "B", "C"), length(number), replace = TRUE)
  )

# is_true ====

test_that("test that is_true works properly", {
  
  expect_true(tbltools:::is_true(TRUE))
  expect_true(tbltools:::is_true("TRUE"))
  expect_true(tbltools:::is_true("true"))
  expect_true(tbltools:::is_true("True"))
  expect_true(tbltools:::is_true("x"))
  expect_true(tbltools:::is_true("yes"))
  expect_false(tbltools:::is_true("FALSE"))
  expect_false(tbltools:::is_true("no"))
  expect_false(tbltools:::is_true(""))
  expect_false(tbltools:::is_true(NA_character_))
  
})

# question numbering ====

test_that("question numbering works properly", {
  expect_error(get_question_number(c(1,3,6,8), 1:4), "inconsistent")
  expect_equal(get_question_number(1:2, 1:2), c("1", "2"))
  expect_equal(get_question_number(1:2, 2:3), c("iRAT 1 / tRAT 2", "iRAT 2 / tRAT 3"))
  expect_equal(get_question_number(1:2*2, 2:3), c("iRAT 2-3 / tRAT 2", "iRAT 4-5 / tRAT 3"))
})

# RAT generation ====

test_that("test that RAT generation works properly", {
  
  # RAT creation ====
  expect_error(tbl_create_RAT_from_data_frame(), "argument.*missing")
  expect_error(tbl_create_RAT_from_data_frame(questions), "argument.*missing")
  expect_error(tbl_create_RAT_from_data_frame(select(questions, -question), answer_key), "Missing required column")
  expect_error(tbl_create_RAT_from_data_frame(select(questions, -answer), answer_key), "Missing required column")
  expect_error(tbl_create_RAT_from_data_frame(select(questions, -correct), answer_key), "Missing required column")
  expect_error(tbl_create_RAT_from_data_frame(questions, select(answer_key, -number)), "Missing required column")
  expect_error(tbl_create_RAT_from_data_frame(questions, select(answer_key, -option)), "Missing required column")
  expect_error(tbl_create_RAT_from_data_frame(mutate(questions, iRAT_n = 0), answer_key), "reserved column")
  expect_error(tbl_create_RAT_from_data_frame(mutate(questions, tRAT_n = 0), answer_key), "reserved column")
  expect_error(tbl_create_RAT_from_data_frame(mutate(questions, correct = FALSE), answer_key), "multiple or no answer")
  expect_error(tbl_create_RAT_from_data_frame(mutate(questions, correct = TRUE), answer_key), "multiple or no answer")
  expect_error(tbl_create_RAT_from_data_frame(questions, mutate(answer_key, option = "a")), "invalid uppercase letters")
  expect_error(tbl_create_RAT_from_data_frame(questions, mutate(answer_key, number = "a")), "invalid number")
  expect_output(print(rat <- tbl_create_RAT_from_data_frame(questions, answer_key)), "Readiness Assessment Test.*Answer Key")
  expect_true(is(rat, "RAT"))
  expect_equal(names(rat), c("questions", "answer_key"))
  expect_true(all(c("question", "iRAT_n", "tRAT_n", "answer", "correct") %in% names(rat$questions)))
  expect_equal(nrow(rat$questions), nrow(questions))
  expect_true(all(c("number", "option") %in% names(rat$answer_key)))
  expect_equal(nrow(rat$answer_key), nrow(answer_key))
  
  ## RAT from excel
  expect_error(tbl_create_RAT_from_excel("DNE"), "file.*does not exist")
  
  # RAT arranging ====
  expect_error(tbl_arrange_RAT_questions(), "argument.*missing")
  expect_error(tbl_arrange_RAT_questions(5), "can only arrange.*objects")
  expect_error(tbl_arrange_RAT_questions(rat, by = "fixed"), "no.*fixed_number_column.*provided")
  expect_error(tbl_arrange_RAT_questions(rat, by = "semi-random"), "no.*fixed_number_column.*provided")
  expect_error(tbl_arrange_RAT_questions(rat, fixed_number_column = "DNE"), "does not exist")
  expect_error(tbl_arrange_RAT_questions(rat, group_by_column = "DNE"), "does not exist")
  expect_error(tbl_arrange_RAT_questions(rat, by = "DNE"), "unrecognized.*parameter")
  
  ## test arrangements
  rat2 <- rat
  rat2$questions <- mutate(rat2$questions, number = ifelse(number != 3, NA, number))
  expect_arrangement <- function(rat, tRAT_ns, iRAT_ns, ...) {
    arr_rat <- tbl_arrange_RAT_questions(rat, ...)
    expect_equal(c(unique(arr_rat$questions$tRAT_n), unique(arr_rat$questions$iRAT_n)),
                 as.integer(c(tRAT_ns, iRAT_ns)))
  }
  
  ## original
  expect_arrangement(rat, c(1, 2, 3), c(1, 2, 3), by = "original")
  expect_arrangement(rat, c(3, 4, 5), c(3, 4, 5), by = "original", tRAT_n_start = 3)
  expect_arrangement(rat, c(1, 2, 3), c(3, 4, 5), by = "original", iRAT_n_start = 3)
  expect_arrangement(rat, c(3, 4, 5), c(1, 2, 3), by = "original", tRAT_n_start = 3, iRAT_n_start = 1)
  expect_arrangement(rat, c(1, 2, 3), c(1, 4, 7), by = "original", iRAT_sel_per_q = 3)
  expect_arrangement(rat, c(3, 4, 5), c(3, 6, 9), by = "original", tRAT_n_start = 3, iRAT_sel_per_q = 3)
  expect_silent(tbl_arrange_RAT_questions(rat, by = "original"))
  expect_warning(tbl_arrange_RAT_questions(rat, by = "original", fixed_number_column = "number"), "no effect")
  expect_warning(tbl_arrange_RAT_questions(rat, by = "original", group_by_column = "group"), "no effect")
  
  ## fixed
  expect_arrangement(rat, c(5, 3, 4), c(5, 3, 4), by = "fixed", fixed_number_column = "number")
  expect_arrangement(rat, c(5, 3, 4), c(3, 1, 2), by = "fixed", fixed_number_column = "number", iRAT_n_start = 1)
  expect_arrangement(rat, c(5, 3, 4), c(7, 3, 5), by = "fixed", fixed_number_column = "number", iRAT_sel_per_q = 2)
  expect_arrangement(rat, c(5, 3, 4), c(11, 5, 8), by = "fixed", fixed_number_column = "number", iRAT_n_start = 5, iRAT_sel_per_q = 3)
  expect_silent(tbl_arrange_RAT_questions(rat, by = "fixed", fixed_number_column = "number"))
  expect_error(tbl_arrange_RAT_questions(rat2, by = "fixed", fixed_number_column = "number"), "questions do not have a fixed number")
  expect_warning(tbl_arrange_RAT_questions(rat, by = "fixed", fixed_number_column = "number", tRAT_n_start = 5), "no effect")
  expect_warning(tbl_arrange_RAT_questions(rat, by = "fixed", fixed_number_column = "number", group_by_column = "group"), "no effect")
  
  # semi - random numbers
  if (as.numeric(version$major) >= 3 && as.numeric(version$minor) >= 6) {
    suppressWarnings(RNGkind(sample.kind = "Rounding")) # to account for random number generation changes in R version >=3.6.0
  }
  expect_arrangement(rat2, c(2, 3, 1), c(2, 3, 1), by = "semi-random", fixed_number_column = "number", random_seed = 42)
  expect_arrangement(rat2, c(1, 3, 2), c(1, 3, 2), by = "semi-random", fixed_number_column = "number", random_seed = 42, group_by_column = "group")
  expect_arrangement(rat2, c(5, 3, 4), c(5, 3, 4), by = "semi-random", fixed_number_column = "number", random_seed = 42, tRAT_n_start = 3)
  expect_arrangement(rat2, c(2, 3, 1), c(4, 5, 3), by = "semi-random", fixed_number_column = "number", random_seed = 42, iRAT_n_start = 3)
  expect_arrangement(rat2, c(1, 3, 2), c(3, 5, 4), by = "semi-random", fixed_number_column = "number", random_seed = 42, iRAT_n_start = 3, group_by_column = "group")
  expect_arrangement(rat2, c(2, 3, 4), c(2, 3, 4), by = "semi-random", fixed_number_column = "number", random_seed = 42, tRAT_n_start = 2, group_by_column = "group")
  expect_arrangement(rat2, c(2, 3, 1), c(5, 9, 1), by = "semi-random", fixed_number_column = "number", random_seed = 42, iRAT_sel_per_q = 4)
  expect_arrangement(rat2, c(2, 3, 1), c(7, 11, 3), by = "semi-random", fixed_number_column = "number", random_seed = 42, iRAT_n_start = 3, iRAT_sel_per_q = 4)
  expect_arrangement(rat2, c(1, 3, 2), c(1, 3, 2), by = "semi-random", fixed_number_column = "number", random_seed = 123)
  expect_message(tbl_arrange_RAT_questions(rat2, by = "semi-random", fixed_number_column = "number"), "generating.*with.*random_seed")
  expect_silent(tbl_arrange_RAT_questions(rat2, by = "semi-random", fixed_number_column = "number", random_seed = 42))
  expect_error(tbl_arrange_RAT_questions(rat2, by = "semi-random", fixed_number_column = "number", tRAT_n_start = 4), "number.*impossible")
  expect_error(tbl_arrange_RAT_questions(rat2, by = "semi-random", fixed_number_column = "number", group_by_column = "group", tRAT_n_start = 3), "number.*impossible")
  
  ## random
  expect_arrangement(rat, c(3, 1, 2), c(3, 1, 2), by = "random", random_seed = 42)
  expect_arrangement(rat, c(7, 5, 6), c(7, 5, 6), by = "random", random_seed = 42, tRAT_n_start = 5)
  expect_arrangement(rat, c(3, 1, 2), c(14, 12, 13), by = "random", random_seed = 42, iRAT_n_start = 12)
  expect_arrangement(rat, c(7, 5, 6), c(3, 1, 2), by = "random", random_seed = 42, tRAT_n_start = 5, iRAT_n_start = 1)
  expect_arrangement(rat, c(7, 5, 6), c(7, 1, 4), by = "random", random_seed = 42, tRAT_n_start = 5, iRAT_n_start = 1, iRAT_sel_per_q = 3)
  expect_arrangement(rat, c(3, 2, 1), c(3, 2, 1), by = "random", random_seed = 7)
  expect_arrangement(rat, c(1, 2, 3), c(1, 2, 3), by = "random", random_seed = 42, group_by_column = "group")
  expect_arrangement(rat, c(1, 3, 2), c(1, 3, 2), by = "random", random_seed = 7, group_by_column = "group")
  expect_arrangement(rat, c(1, 3, 2), c(3, 5, 4), by = "random", random_seed = 7, group_by_column = "group", iRAT_n_start = 3)
  expect_arrangement(rat, c(1, 3, 2), c(3, 13, 8), by = "random", random_seed = 7, group_by_column = "group", iRAT_n_start = 3, iRAT_sel_per_q = 5)
  expect_arrangement(rat, c(1, 3, 2), c(1, 11, 6), by = "random", random_seed = 7, group_by_column = "group", iRAT_sel_per_q = 5)
  expect_message(tbl_arrange_RAT_questions(rat, by = "random"), "generating.*with.*random_seed")
  expect_silent(tbl_arrange_RAT_questions(rat, by = "random", random_seed = 42))
  expect_warning(tbl_arrange_RAT_questions(rat, by = "random", fixed_number_column = "number"), "no effect")
  
  # RAT answer choices =====
  expect_error(tbl_generate_RAT_choices(), "argument.*missing")
  expect_error(tbl_generate_RAT_choices(5), "can only use.*objects")
  
  ## options
  rat3 <- rat
  rat3$answer_key <- filter(rat$answer_key, number < 3)
  expect_error(get_RAT_options(rat3), "Missing answer key")
  rat3$answer_key <- mutate(rat$answer_key, option = ifelse(number != 3, "F", option))
  expect_error(get_RAT_options(rat3), "NOT have enough possible answers")
  expect_true(is(random_rat <- tbl_arrange_RAT_questions(rat, by = "random"), "RAT"))
  expect_true(is(rat_options <- get_RAT_options(random_rat), "data.frame"))
  expect_equal(
    nest(rat_options, data = c(-question, -option, -n_options, -n_correct))$option, 
    left_join(random_rat$questions %>% nest(data = c(-question, -tRAT_n)), answer_key, by = c("tRAT_n" = "number"))$option)
  expect_equal(
    nest(rat_options, data = c(-question, -option, -n_options, -n_correct))$n_correct, 
    match(left_join(random_rat$questions %>% nest(data = c(-question, -tRAT_n)), answer_key, by = c("tRAT_n" = "number"))$option, LETTERS))
  
  ## layout
  expect_error(tbl_generate_RAT_choices(rat, answer_layout = "DNE"), "Unsupported answer layout")
  expect_error(tbl_generate_RAT_choices(rat, answer_layout_column = "DNE"), "does not exist")
  rat4 <- rat
  rat4$questions <- mutate(rat$questions, layout = ifelse(number == 3, "DNE", layout))
  expect_error(tbl_generate_RAT_choices(rat4, answer_layout_column = "layout"), "unrecognized layout setting")
  
  ## actual choices
  expect_message(capture.output(tbl_generate_RAT_choices(rat)), "generating.*with.*random_seed")
  expect_output(tbl_generate_RAT_choices(rat, random_seed = 42), "### 2.*what says the cow")
  
})
