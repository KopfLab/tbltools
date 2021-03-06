% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readiness_assessment_tests.R
\name{tbl_create_RAT_from_data_frame}
\alias{tbl_create_RAT_from_data_frame}
\title{Create RAT from data frame}
\usage{
tbl_create_RAT_from_data_frame(questions, answer_key)
}
\arguments{
\item{questions}{data frame with questions (requires at minimum columns 'question', 'answer', and logical TRUE/FALSE 'correct')}

\item{answer_key}{data frame with answer key (requires at mimimum columns 'number', 'option')}
}
\description{
Note that the logical column 'correct' is evaluated to TRUE if values are =1, =TRUE, ='TRUE', ='yes' or ='x', everything else is FALSE.
}
