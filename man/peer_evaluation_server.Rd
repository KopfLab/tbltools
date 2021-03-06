% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peer_evaluation_server.R
\name{peer_evaluation_server}
\alias{peer_evaluation_server}
\title{Peer Evaluation App Server}
\usage{
peer_evaluation_server(roster, data_gs_title, gs_token, welcome_md_file,
  self_eval_plus_md_file, self_eval_minus_md_file,
  teammate_eval_plus_md_file, teammate_eval_minus_md_file,
  quant_scores_md_file, points_per_teammate = 10, max_points = 15,
  min_points = 0, min_point_difference = 2,
  auto_login_access_code = NULL)
}
\arguments{
\item{roster}{data frame with the student roster}

\item{data_gs_title}{name of the google spreadsheet that should be used for storing the peer evaluation data. This spreadsheet must already exist and the credentials used when asked by this function must have write access to the spreadsheet.}

\item{gs_token}{path to a google spreadsheet oauth 2.0 authentication token file (see \link[httr]{Token-class}). The token is safe to use on a secure shiny app server but be careful to never post this token file anywhere publicly as it could be used to gain access to your google drive.}

\item{welcome_md_file}{path to a markdown (.md) file for the login welcome message}

\item{self_eval_plus_md_file}{markdown file for the "plus" self evaluation message}

\item{self_eval_minus_md_file}{markdown file for the "minus" self evaluation message}

\item{teammate_eval_plus_md_file}{markdown file for the "plus" teammate evaluation message}

\item{teammate_eval_minus_md_file}{markdown file for the "minus" teammate evaluation message}

\item{quant_scores_md_file}{markdown file for the quantiative scores message}

\item{points_per_teammate}{points per teammate}

\item{max_points}{the maximum number of points allowed per team member}

\item{min_points}{the smallest number of points allowed per team member}

\item{min_point_difference}{the minimum point difference required for the scores (set to 0 to allow all scores to be identical)}

\item{auto_login_access_code}{set an automatic login access code for testing and debugging purposes}
}
\description{
Generates the server part of the peer evaluation app
}
