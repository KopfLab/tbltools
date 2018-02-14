% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peer_evaluation_server.R
\name{peer_evaluation_server}
\alias{peer_evaluation_server}
\title{Peer Evaluation App Server}
\usage{
peer_evaluation_server(roster, data_gs_title, gs_token,
  points_per_teammate = 10)
}
\arguments{
\item{roster}{data frame with the student roster}

\item{data_gs_title}{name of the google spreadsheet that should be used for storing the peer evaluation data. This spreadsheet must already exist and the credentials used when asked by this function must have write access to the spreadsheet.}

\item{gs_token}{a google spreadsheet oauth 2.0 authentication token (see \link[httr]{Token-class}). If none is provided (\code{g_token = NULL}, the default), will ask for google drive credentials interactively to generate a token for the peer evaluation app. The token is safe to use on a secure shiny app server but be careful to never post this token file anywhere publicly as it could be used to gain access to your google drive.}

\item{points_per_teammate}{points per teammate}
}
\description{
Generates the server part of the peer evaluation app
}