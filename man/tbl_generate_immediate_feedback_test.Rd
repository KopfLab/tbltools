% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/immediate_feedback_tests.R
\name{tbl_generate_immediate_feedback_test}
\alias{tbl_generate_immediate_feedback_test}
\title{Generate immediate feedback tests}
\usage{
tbl_generate_immediate_feedback_test(
  immediate_feedback_test_data,
  correct = "#4DAF4A",
  incorrect = "#E41A1C",
  unknown = "#999999",
  width = 0.9,
  height = 0.9
)
}
\arguments{
\item{immediate_feedback_test_data}{the immediate feedback test data frame retrieved by \link{tbl_fetch_immediate_feedback_test_data}}

\item{correct}{the color for correct answers (green by default)}

\item{incorrect}{the color for incorrect answers (red by default)}

\item{unknown}{the color for yet unknown answers (gray by default)}

\item{width}{the relative width of the option boxes (from 0 to 1)}

\item{height}{the relative height of the optin boxes (from 0 to 1)}
}
\description{
This function generates visual immediate feedback tests based on the data retrieved by \link{tbl_fetch_immediate_feedback_test_data} (the same function is used internally by the immediate feedback test apps).
}
