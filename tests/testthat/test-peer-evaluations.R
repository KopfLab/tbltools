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
    
  # read data
  expect_error(tbltools:::read_peer_eval("DNE"), "neither a google spreadsheet nor a valid path")
  
  # app setup
  tmp <- tempdir()
  expect_error(tbl_setup_peer_evaluation(folder = tmp, template_roster_file = "DNE"), "roster file.*does not exist")
  tmp_file <- tempfile()
  cat(file = tmp_file, "fake_token")
  if (file.exists(tmp_file)) # safety measure to avoid test getting hung up on interactive authentication
    expect_error(tbl_setup_peer_evaluation(folder = tmp, gs_token = tmp_file), "authentication failed")
  unlink(tmp_file)
  expect_true(file.exists(file.path(tmp, "roster.xlsx")))
  
  # app start
  expect_error(tbl_run_peer_evaluation(roster = 5), "roster.*required")
  expect_error(tbl_run_peer_evaluation(roster = data_frame()), "files do not exist")

  # app testing
  expect_error(tbl_test_peer_evaluation(folder = "DNE"), "does not exist")
  expect_error(tbl_test_peer_evaluation(folder = "."), "not.*contain a peer evaluation app")
  
  # app deployment
  expect_error(tbl_deploy_peer_evaluation(folder = "DNE"), "does not exist")
  expect_error(tbl_deploy_peer_evaluation(folder = "."), "not.*contain a peer evaluation app")
  
  # app fetch data
  expect_error(tbl_fetch_peer_evaluation_data(), "roster.*cannot be opened")
  expect_error(tbl_fetch_peer_evaluation_data(roster = 5), "roster.*required")
  
  # summaarize
  expect_error(tbl_summarize_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_summarize_peer_evaluation_data(5), "no data frame supplied")
  
  # export
  expect_error(tbl_export_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_export_peer_evaluation_data(5), "no data frame supplied")
  
})
