context("Peer Evaluation")

# errors throws -----

test_that("test that peer evaluation functions throw the proper errors", {

  # student roster
  expect_error(check_student_roster(42), "must be a data frame")
  roster <- tibble(last = "last", first = "first", access_code = "42", team = "test")
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
  tmp <- file.path(tempdir(), "pe1")
  expect_error(tbl_setup_peer_evaluation(folder = tmp, template_roster_file = "DNE", check_gs_access = FALSE), "roster file.*does not exist")
  expect_error(tbl_setup_peer_evaluation(folder = tmp, gs_key_file = system.file("extdata", "broken_json_file.json", package="tbltools")), "not valid JSON")
  expect_error(tbl_setup_peer_evaluation(folder = tmp, gs_key_file = system.file("extdata", "broken_key_file.json", package="tbltools")), "authentication failed")
  expect_error(tbl_setup_peer_evaluation(folder = tmp, gs_key_file = system.file("extdata", "gs_key_file_example.json", package="tbltools")), "authentication failed")
  expect_message(tbl_setup_peer_evaluation(folder = tmp, check_gs_access = FALSE), "complete")
  expect_message(tbl_setup_peer_evaluation(folder = tmp, check_gs_access = FALSE), "app already exists.*to overwrite")
  expect_message(tbl_setup_peer_evaluation(folder = tmp, overwrite = TRUE, check_gs_access = FALSE), "will be overwritten")
  expect_message(tbl_setup_peer_evaluation(folder = tmp, overwrite = TRUE, check_gs_access = FALSE), "complete")
  expect_false(file.exists(file.path(tmp, "gs_key_file.json")))
  expect_true(file.exists(file.path(tmp, "roster.xlsx")))
  
  # duplicate app
  tmp2 <- file.path(tempdir(), "pe2")
  expect_error(tbl_duplicate_peer_evaluation(tmp, tmp2, overwrite = TRUE), "key file does not exist")
  expect_message(tbl_duplicate_peer_evaluation(tmp, tmp2), "app already exists.*to overwrite")
  expect_message(tbl_duplicate_peer_evaluation(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE), "will be overwritten")
  expect_message(tbl_duplicate_peer_evaluation(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE, data_gs_title = "NEW"), "changing spreadsheet title")
  expect_message(tbl_duplicate_peer_evaluation(tmp, tmp2, overwrite = TRUE, check_gs_access = FALSE), "fully duplicated")
  file.copy(system.file("extdata", "gs_key_file_example.json", package="tbltools"), file.path(tmp2, "gs_key_file.json"))
  expect_true(file.exists(file.path(tmp2, "gs_key_file.json")))
    
  # check access
  expect_error(tbl_check_gs_access(folder = tmp), "key file does not exist")
  expect_error(tbl_check_gs_access(gs_key_file = system.file("extdata", "gs_key_file_example.json", package="tbltools")), "authentication failed")
  expect_error(tbl_check_gs_access(folder = tmp2), "authentication failed")
  
  # app start
  expect_error(tbl_run_peer_evaluation(roster = 5), "roster.*required")
  expect_error(tbl_run_peer_evaluation(roster = tibble()), "files do not exist")

  # app testing
  expect_error(tbl_test_peer_evaluation(folder = "DNE"), "does not exist")
  expect_error(tbl_test_peer_evaluation(folder = "."), "not.*contain a peer evaluation app")
  expect_error(tbl_test_peer_evaluation(tmp), "no key file")
  expect_error(tbl_test_peer_evaluation(tmp2), "authentication failed")
  
  # app deployment
  expect_error(tbl_deploy_peer_evaluation(folder = "DNE"), "does not exist")
  expect_error(tbl_deploy_peer_evaluation(folder = "."), "not.*contain a peer evaluation app")
  
  # app fetch data
  expect_error(tbl_fetch_peer_evaluation_data(), "does not exist")
  expect_error(tbl_fetch_peer_evaluation_data(roster = 5), "roster.*required")
  expect_error(tbl_fetch_peer_evaluation_data(
    roster = tibble(last = "bond", first = "james", access_code = "1234", team = "MI6")),
    "spreadsheet must be identified")
  
  # read data
  expect_error(tbl_read_peer_evaluation_data(), "does not exist")
  expect_error(tbl_read_peer_evaluation_data(roster = 5), "roster.*required")
  expect_error(tbl_read_peer_evaluation_data(
    roster = tibble(last = "bond", first = "james", access_code = "1234", team = "MI6"), download_file = "DNE"),
    "peer evaluation data file.*does not exist")
  
  # summaarize
  expect_error(tbl_summarize_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_summarize_peer_evaluation_data(5), "no data frame supplied")
  
  # export
  expect_error(tbl_export_peer_evaluation_data(), "no data frame supplied")
  expect_error(tbl_export_peer_evaluation_data(5), "no data frame supplied")
  
  # example data
  expect_error(try_to_fetch_google_spreadsheet("DNE"), "could not retrieve")
  
  # cleanup
  unlink(tmp, recursive = TRUE)
  unlink(tmp2, recursive = TRUE)
})

# example data ----

test_that("test that example peer evaluations can be accessed the processed", {
  
  # example sheets
  expect_true(is(tbl_example_peer_evaluation(), "sheets_id"))
  expect_equal(tbl_example_peer_evaluation() %>% as.character(), "1u9p0erH13N-KFGsAEEY5Eo2cyXJhR62SA485GaRP3O8")
  expect_true(is(tbl_example_empty_peer_evaluation(), "sheets_id"))
  expect_equal(tbl_example_empty_peer_evaluation() %>% as.character(), "1WcxbU3NOIrOzhf-PAyNGlgpxiEPLMFthe-3lldoCc2M")
  
  # example roster
  expect_true(file.exists(system.file(package = "tbltools", "extdata", "roster_template.xlsx")))
  expect_true(is.data.frame(roster <- tbl_example_roster()))
  expect_equal(names(roster), c("last", "first", "access_code", "team"))
  
  # example empty data from sheets
  expect_message(
    pe_data <- tbl_fetch_peer_evaluation_data(
      roster = roster,
      data_gs_id = tbl_example_empty_peer_evaluation(),
      download_to = "empty_example.xlsx"
    ),
    "complete")
  expect_true(is.data.frame(pe_data))
  expect_equal(pe_data %>% select(-evaluations, -submitted_timestamp),
               roster %>% group_by(team) %>% 
                 mutate(
                   access_code = str_c("id_", access_code),
                   n_team_mates = dplyr::n() - 1L, 
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
      data_gs_id = tbl_example_peer_evaluation(),
      download_to = "full_example.xlsx"
    ),
    "complete")
  
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