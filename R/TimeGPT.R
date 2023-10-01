#' Fit TimeGPT model to univariate time series
#'
#' Returns an \code{TimeGPT} model applied to y.
#'
#' @aliases print.TimeGPT
#'
#' @usage TimeGPT(y, time, freq, xreg, token = NULL, clean_ex_first = TRUE,
#'                finetune_steps = 0, verbose = FALSE, historic = FALSE)
#'
#' @param y A numeric vector or time series of class \code{ts}.
#' @param time A \code{date} vector which must have the same number of rows as \code{y}.
#' @param xreg Optionally, a numerical vector or matrix of external regressors, which must have
#' the same number of rows as \code{y}.
#' @param freq A chracter identifying the frequency: \code{D} for daily, \code{M} for monthly,
#' \code{H} for hourly, and \code{W} for weekly.
#' @param clean_ex_first If \code{TRUE}, the exogenous signal is cleaned, otherwise the exogenous
#' variables are applied after the large time model.
#' @param finetune_steps The number of tuning steps used to train the large time model on the data.
#' @inheritParams callTimeGPT
#'
#' @author Daniele Girolimetto
#'
#' @return An object of class "\code{TimeGPT}".
#' @rdname TimeGPT
#' @export
TimeGPT <- function(y, time, freq, xreg, token = NULL, clean_ex_first = TRUE,
                    finetune_steps = 0, verbose = FALSE, historic = FALSE){
  if(NCOL(y)!=1 | !is.numeric(y)){
    cli::cli_abort("Only univariate time series are supported.")
  }

  if(!missing(xreg)){
    cli::cli_abort("External regressors are supported yet.")
  }

  if(clean_ex_first){
    clean_ex_first <- "true"
  }else{
    clean_ex_first <- "false"
  }

  if(missing(freq)){
    cli::cli_abort("Provide a valid freq")
  }else{
    freq <- match.arg(freq, c("D", "W", "M", "H"))
  }

  api <- list(y = setNames(as.numeric(y), time),
              freq = freq,
              clean_ex_first = clean_ex_first,
              finetune_steps = finetune_steps)

  rsp_api <- NULL
  fitted <- residuals <- rep(NA, length(y))

  if(historic){
    rsp_api <- callTimeGPT(convert2text(api), token = token,
                            verbose = verbose, historic = historic)
    if(rsp_api$message=="success"){
      start_na <- length(y)-length(rsp_api$data$y)
      fitted <- c(rep(NA, start_na), rsp_api$data$value)
      residuals <- c(rep(NA, start_na), rsp_api$data$y-rsp_api$data$value)

      tspx <- tsp(y)
      if (!is.null(tspx)) {
        fitted <- ts(fitted, frequency = tspx[3], start = tspx[1])
        residuals <- ts(residuals, frequency = tspx[3], start = tspx[1])
      }
    }
  }

  obj <- structure(list(y = y,
                        method = "TimeGPT",
                        call = match.call(),
                        series = deparse(substitute(y)),
                        fitted = fitted,
                        residuals = y-fitted,
                        rsp_api = rsp_api,
                        api = api),
                   class = "TimeGPT")
  return(obj)
}

#' @export
print.TimeGPT <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(paste(x$method, "\n\n"))
  if(!is.null(x$call)) {
    cat(paste("Call:\n", deparse1(x$call), "\n\n"))
  }

  if(!is.null(x$x)) {
    cat(paste0("Time series (", x$series,"):\n"))
    print(summary(x$x))
    cat("\n")
  }

  if(!all(is.na(x$residuals))) {
    cat(paste0("Residuals:\n"))
    print(summary(x$residuals))
    stats <- c(mean(abs(x$residuals), na.rm = TRUE), mean((x$residuals)^2, na.rm = TRUE))
    names(stats) <- c("MSE", "MAE")
    cat("\n")
    cat(paste("In-sample accuracy:\n MSE =", round(stats[1], digits),
              "\n MAE =", round(stats[2], digits)))
    cat("\n")
  }
}

