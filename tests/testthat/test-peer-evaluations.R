context("Peer Evaluation")

# errors throws -----

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
  expect_error(read_peer_eval("DNE"), "neither a google spreadsheet nor a valid path")
  
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
  expect_error(tbl_fetch_peer_evaluation_data(), "does not exist")
  expect_error(tbl_fetch_peer_evaluation_data(roster = 5), "roster.*required")
  expect_error(tbl_fetch_peer_evaluation_data(
    roster = data_frame(last = "bond", first = "james", access_code = "1234", team = "MI6")),
    "spreadsheet must be identified")
  
  # read data
  expect_error(tbl_read_peer_evaluation_data(), "does not exist")
  expect_error(tbl_read_peer_evaluation_data(roster = 5), "roster.*required")
  expect_error(tbl_read_peer_evaluation_data(
    roster = data_frame(last = "bond", first = "james", access_code = "1234", team = "MI6"), download_file = "DNE"),
    "peer evaluation data file.*does not exist")
  
  # summaarize
  expect_error(tbl_summarize_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_summarize_peer_evaluation_data(5), "no data frame supplied")
  
  # export
  expect_error(tbl_export_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_export_peer_evaluation_data(5), "no data frame supplied")
  
  # example data
  expect_error(get_example_gs_key("DNE"), "could not retrieve")
})

# example data ----

test_that("test that example peer evaluations can be accessed the processed", {
  
  # example sheets
  expect_equal(tbl_example_peer_evaluation()$sheet_title, "Peer Evaluations Example")
  expect_equal(tbl_example_empty_peer_evaluation()$sheet_title, "Peer Evaluations Example Empty")
  
  # example roster
  expect_true(file.exists(system.file(package = "tbltools", "extdata", "roster_template.xlsx")))
  expect_true(is.data.frame(roster <- readxl::read_excel(system.file(package = "tbltools", "extdata", "roster_template.xlsx"))))
  
  # example empty data from sheets
  expect_message(
    pe_data <- tbl_fetch_peer_evaluation_data(
      roster = roster,
      data_gs_key = tbl_example_empty_peer_evaluation(),
      download_to = "empty_example.xlsx"
    ),
    "successfully downloaded")
  expect_true(is.data.frame(pe_data))
  expect_equal(pe_data %>% select(-evaluations, -submitted_timestamp),
               roster %>% group_by(team) %>% 
                 mutate(
                   access_code = str_c("id_", access_code),
                   n_team_mates = n() - 1L, 
                   started = FALSE, submitted = FALSE) %>% 
                 ungroup())
  
  # make sure direct fetch and data read work the same way
  expect_true(is.data.frame(pe_data2 <- 
                              tbl_read_peer_evaluation_data(
                                roster = roster,
                                download_file = "empty_example.xlsx")))
  expect_equal(pe_data %>% tidyr::unnest(evaluations), pe_data2 %>% tidyr::unnest(evaluations))
  
  # example full data
  expect_message(
    pe_data_full <- tbl_fetch_peer_evaluation_data(
      roster = roster,
      data_gs_key = tbl_example_peer_evaluation(),
      download_to = "full_example.xlsx"
    ),
    "successfully downloaded")
  
  # NOTE: example sheet via title testing not possible without token
  
  # make sure downloaded files are deleted if found
  on.exit({
    if(file.exists("empty_example.xlsx"))
      file.remove("empty_example.xlsx")
    if(file.exists("full_example.xlsx"))
      file.remove("full_example.xlsx")
  })
})

# peer evaluation app -----

test_that("test that settin up peer evaluation works", {
  
  
  
})