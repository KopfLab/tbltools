context("Peer Evaluation")

test_that("test that peer evaluation functions throw the proper errors", {

  # student roster
  expect_error(check_student_roster(42), "must be a data frame")
  roster <- data_frame(last = "last", first = "first", access_code = "42", team = "test")
  expect_error(check_student_roster(roster %>% select(-last)), "missing column.*last")
  expect_error(check_student_roster(roster %>% select(-first)), "missing column.*first")
  expect_error(check_student_roster(roster %>% select(-access_code)), "missing column.*access_code")
  expect_error(check_student_roster(roster %>% select(-team)), "missing column.*team")
  expect_true(is.data.frame(check_student_roster(roster)))
  roster <- bind_rows(roster, roster)
  expect_error(check_student_roster(roster), "not unique access code")
  
  # start evalution
  expect_error(tbl_run_peer_evaluation(), "roster.*required")

})
