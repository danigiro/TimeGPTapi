#' TimeGPTapi: Interact with the TimeGPT API
#'
#' \code{TimeGPT} is a generative pre-trained transformer model for time series
#' analysis (\url{https://docs.nixtla.io/docs}) developed by  \bold{Nixtla}
#' (\url{https://nixtla.io}). It was trained on the largest collection of public
#' time series and can predict the future values of a single time series based
#' on the provided. This R package provides
#' an R interface to interact with the \code{TimeGPT API}.
#'
#' @section Copyright:
#' This R package is released under the \emph{GNU General Public License v3.0}.
#' The \code{TimeGPT} model and \code{API} are proprietary technologies developed
#' by \bold{Nixtla}, and all rights are reserved. The use of the \code{TimeGPT API}
#' is subject to Nixtla's terms and conditions, which can be found at \url{https://nixtla.io/}.
#'
#' @author Daniele Girolimetto
#'
#' @docType package
#' @name TimeGPTapi-package
#' @keywords package
#'
"_PACKAGE"

#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames time ts tsp
#' @importFrom httr2 resp_body_string req_body_raw request req_headers oauth_token req_perform
#' @importFrom forecast forecast
NULL
