% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callTimeGPT.R
\name{callTimeGPT}
\alias{callTimeGPT}
\title{Interact with the TimeGPT API (raw text)}
\usage{
callTimeGPT(text, token = NULL, verbose = FALSE, historic = FALSE)
}
\arguments{
\item{text}{A string to send as the body of the request. For more details, see
\url{https://docs.nixtla.io/reference/timegpt_timegpt_post}.}

\item{token}{The authorization token to interact with the \code{TimeGPT API}.}

\item{verbose}{If \code{TRUE}, additional console information about the API request and
response are provided.}

\item{historic}{If \code{TRUE}, the fitted time series data for the in-sample period are provided.}
}
\value{
A list object with the TimeGPT server response.
}
\description{
This function is designed to interact with the \code{TimeGPT API}.
It sends a request to the \code{TimeGPT API} and returns the server response.
}
\author{
Daniele Girolimetto
}
