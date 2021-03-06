% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peer_evaluation.R
\name{tbl_summarize_peer_evaluation_data}
\alias{tbl_summarize_peer_evaluation_data}
\title{Summarize peer evaluation data}
\usage{
tbl_summarize_peer_evaluation_data(data, submitted_only = FALSE)
}
\arguments{
\item{data}{the peer evaluation data frame retrieved by \link{tbl_fetch_peer_evaluation_data}}

\item{submitted_only}{only include evaluations that were actually submitted (rather than just saved)}
}
\description{
Summarizes the peer evaluation data. Preserves all roster information (last name, first name, team, + any custom fields).
}
