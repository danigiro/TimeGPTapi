#' Title
#'
#' @param x
#' @param freq
#' @param clean_ex_first
#' @param finetune_steps
#'
#' @return
#' @export
#'
#' @examples
#' obj <- TimeGPT(AirPassengers)
TimeGPT <- function(x, time, freq, token, clean_ex_first = TRUE, finetune_steps = 0,
                    verbose = FALSE, historic = FALSE){
  if(NCOL(x)!=1 | !is.numeric(x)){
    stop("Only univariate time series are supported.", call. = FALSE)
  }

  if(clean_ex_first){
    clean_ex_first <- "true"
  }else{
    clean_ex_first <- "false"
  }

  if(missing(freq)){
    stop("Provide a valid freq", call. = FALSE)
  }
  api <- list(y = setNames(as.numeric(x), time),
              freq = freq,
              clean_ex_first = clean_ex_first,
              finetune_steps = finetune_steps)

  resp_api <- NULL
  fitted <- residuals <- rep(NA, length(x))

  if(historic){
    resp_api <- callTimeGPT(convert2text(api), token = token,
                            verbose = verbose, historic = historic)
    if(resp_api$message=="success"){
      start_na <- length(x)-length(resp_api$data$y)
      fitted <- c(rep(NA, start_na), resp_api$data$value)
      residuals <- c(rep(NA, start_na), resp_api$data$y-resp_api$data$value)

      tspx <- tsp(x)
      if (!is.null(tspx)) {
        fitted <- ts(fitted, frequency = tspx[3], start = tspx[1])
        residuals <- ts(residuals, frequency = tspx[3], start = tspx[1])
      }
    }
  }

  obj <- structure(list(x = x,
                        method = "TimeGPT",
                        call = match.call(),
                        series = deparse(substitute(x)),
                        fitted = fitted,
                        residuals = x-fitted,
                        resp_api = resp_api,
                        api = api),
                   class = "TimeGPT")
  return(obj)
}

#' @export
print.TimeGPT <- function(x, ...) {
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

