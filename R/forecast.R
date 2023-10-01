#' Forecasting using TimeGPT model
#'
#' Returns forecasts and other information for the univariate \code{TimeGPT} model.
#'
#' @usage forecast(object, h = 10, level=c(80, 95), xreg,
#'                 token = NULL, verbose = FALSE, ...)
#'
#' @param object An object of class "\code{ets}". Usually the result of a call
#' to \code{\link{ets}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals.
#' @param xreg Optionally, a numerical vector or matrix of external regressors, which must have
#' the same number of rows as y.
#' @inheritParams callTimeGPT
#' @param ... Other arguments.
#'
#' @return An object of class "\code{forecast}".
#'
#' @author Daniele Girolimetto
#'
#' @rdname forecast.TimeGPT
#' @export
forecast.TimeGPT <- function(object, h = 10, level=c(80, 95), xreg,
                             token = NULL, verbose = FALSE, ...){
  if(!is.list(object) ){
    cli::cli_abort("{.arg object} is not a list (TimeGPT class).")
  }

  if(!missing(xreg)){
    cli::cli_abort("External regressors are supported yet.")
  }

  body <- object$api
  body$fh <- h
  body$level <- level

  body <- convert2text(body)

  rsp_api <- callTimeGPT(text = body, token = token, verbose = verbose, historic = FALSE)

  out <- list()

  lower_names <- names(rsp_api$data)[grepl("lo-", names(rsp_api$data), fixed = TRUE)]
  upper_names <- names(rsp_api$data)[grepl("hi-", names(rsp_api$data), fixed = TRUE)]

  out$mean <- rsp_api$data$value
  x <- object$x

  if(length(lower_names)!=0){
    out$lower <- do.call(cbind, rsp_api$data[lower_names])
    colnames(out$lower) <- paste0(gsub("\\D", "", lower_names), "%")
  }

  if(length(upper_names)!=0){
    out$upper <- do.call(cbind, rsp_api$data[upper_names])
    colnames(out$upper) <- paste0(gsub("\\D", "", upper_names), "%")
  }

  if (!is.null(x)) {
    tspx <- tsp(x)
    nx <- max(which(!is.na(x)))
    if (nx != length(x) | is.null(tsp(out$mean))) {
      tspx[2] <- time(x)[nx]
      start.f <- tspx[2] + 1 / tspx[3]
      out$mean <- ts(out$mean, frequency = tspx[3], start = start.f)

      if(length(lower_names)!=0){
        out$lower <- ts(out$lower, frequency = tspx[3], start = start.f)
      }

      if(length(upper_names)!=0){
        out$upper <- ts(out$upper, frequency = tspx[3], start = start.f)
      }
    }
  }

  out$method <- object$method
  out$model <- object
  out$rsp_api <- object$rsp_api
  out$fitted <- object$fitted
  out$residuals <- object$residuals
  out$series <- object$series
  out$x <- x
  out$level <- level
  return(structure(out, class = "forecast"))
}
