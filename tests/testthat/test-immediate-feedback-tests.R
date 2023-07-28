context("immediate feedback test")

# errors throws -----

test_that("test that immediate feedback test functions throw the proper errors", {

  # student roster
  expect_error(check_immediate_feedback_test_roster(42), "must be a data frame")
  roster <- tibble(name = "team", access_code = "42")
  expect_error(check_immediate_feedback_test_roster(roster %>% select(-name)), "missing column")
  expect_error(check_immediate_feedback_test_roster(roster %>% select(-access_code)), "missing column.*access_code")
  expect_true(is.data.frame(check_immediate_feedback_test_roster(roster)))
  roster <- bind_rows(roster, roster)
  expect_error(check_immediate_feedback_test_roster(roster), "not unique access code")
    
  # read data
  expect_error(read_immediate_feedback_test("DNE"), "neither a google spreadsheet nor a valid path")
  
  # app setup
  tmp <- file.path(tempdir(), "tRat1")
  expect_error(tbl_setup_immediate_feedback_test(folder = tmp, template_roster_file = "DNE", check_gs_access = FALSE), "roster file.*does not exist")
  expect_error(tbl_setup_immediate_feedback_test(folder = tmp, template_questions_file = "DNE", check_gs_access = FALSE), "questions file.*does not exist")
  expect_error(tbl_setup_immediate_feedback_test(folder = tmp, gs_key_file = system.file("extdata", "broken_json_file.json", package="tbltools")), "not valid JSON")
  expect_error(tbl_setup_immediate_feedback_test(folder = tmp, gs_key_file = system.file("extdata", "broken_key_file.json", package="tbltools")), "authentication failed")
  expect_error(tbl_setup_immediate_feedback_test(folder = tmp, gs_key_file = system.file("extdata", "gs_key_file_example.json", package="tbltools")), "don't have access")
  expect_message(tbl_setup_immediate_feedback_test(folder = tmp, check_gs_access = FALSE), "complete")
  expect_message(tbl_setup_immediate_feedback_test(folder = tmp, check_gs_access = FALSE), "app already exists.*to overwrite")
  expect_message(tbl_setup_immediate_feedback_test(folder = tmp, overwrite = TRUE, check_gs_access = FALSE), "will be overwritten")
  expect_message(tbl_setup_immediate_feedback_test(folder = tmp, overwrite = TRUE, check_gs_access = FALSE), "complete")
  expect_false(file.exists(file.path(tmp, "gs_key_file.json")))
  expect_true(file.exists(file.path(tmp, "roster.xlsx")))
  expect_true(file.exists(file.path(tmp, "questions.xlsx")))
  
  # duplicate app
  tmp2 <- file.path(tempdir(), "tRat2")
  expect_error(tbl_duplicate_immediate_feedback_test(tmp, tmp2, overwrite = TRUE, data_gs_title = "new"), "key file does not exist")
  expect_message(tbl_duplicate_immediate_feedback_test(tmp, tmp2, data_gs_title = "new"), "app already exists.*to overwrite")
  expect_message(tbl_duplicate_immediate_feedback_test(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE, data_gs_title = "new"), "will be overwritten")
  expect_message(tbl_duplicate_immediate_feedback_test(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE, data_gs_title = "new"), "changing spreadsheet title")
  expect_warning(tbl_duplicate_immediate_feedback_test(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE), "no new.*spreadsheet provided")
  expect_message(tbl_duplicate_immediate_feedback_test(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE, data_gs_title = "new"), "fully duplicated")
  file.copy(system.file("extdata", "gs_key_file_example.json", package="tbltools"), file.path(tmp2, "gs_key_file.json"))
  expect_true(file.exists(file.path(tmp2, "gs_key_file.json")))
  
  # app start
  expect_error(tbl_run_immediate_feedback_test(roster = 5), "data frame required")
  expect_error(tbl_run_immediate_feedback_test(roster = tibble(), questions = 5), "questions.*required")
  expect_error(tbl_run_immediate_feedback_test(roster = tibble(), questions = tibble()), "no key file")

  # app testing
  expect_error(tbl_test_immediate_feedback_test(folder = "DNE"), "does not exist")
  expect_error(tbl_test_immediate_feedback_test(folder = "."), "not.*contain.* app")
  expect_error(tbl_test_immediate_feedback_test(tmp), "no key file")
  expect_error(tbl_test_immediate_feedback_test(tmp2), "don't have access")
  
  # app deployment
  expect_error(tbl_deploy_immediate_feedback_test(folder = "DNE"), "does not exist")
  expect_error(tbl_deploy_immediate_feedback_test(folder = "."), "not.*contain an app")
  
  # cleanup
  unlink(tmp, recursive = TRUE)
  unlink(tmp2, recursive = TRUE)
  
  # FIXME implement
  # # app fetch data
  # expect_error(tbl_fetch_peer_evaluation_data(), "does not exist")
  # expect_error(tbl_fetch_peer_evaluation_data(roster = 5), "roster.*required")
  # expect_error(tbl_fetch_peer_evaluation_data(
  #   roster = tibble(last = "bond", first = "james", access_code = "1234", team = "MI6")),
  #   "spreadsheet must be identified")
  # 
  # # read data
  # expect_error(tbl_read_peer_evaluation_data(), "does not exist")
  # expect_error(tbl_read_peer_evaluation_data(roster = 5), "roster.*required")
  # expect_error(tbl_read_peer_evaluation_data(
  #   roster = tibble(last = "bond", first = "james", access_code = "1234", team = "MI6"), download_file = "DNE"),
  #   "peer evaluation data file.*does not exist")
  # 
  # # summaarize
  # expect_error(tbl_summarize_peer_evaluation_data(), "no data frame supplied")
  # expect_error(tbl_summarize_peer_evaluation_data(5), "no data frame supplied")
  # 
  # # export
  # expect_error(tbl_export_peer_evaluation_data(), "no data frame supplied")
  # expect_error(tbl_export_peer_evaluation_data(5), "no data frame supplied")
  # 
  # # example data
  # expect_error(try_to_fetch_google_spreadsheet("DNE"), "could not retrieve")

})

# example data ---- FIXME implement, see peer evals for examples

