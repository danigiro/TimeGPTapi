#' Title
#'
#' @param object
#' @param h
#' @param token
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
forecast.TimeGPT <- function(object, h = 10, level=c(80, 95), token, verbose = FALSE){
  if(!is.list(object) ){
    stop("'object' is not a list (TimeGPT class)", call. = FALSE)
  }

  body <- object$api
  body$fh <- h
  body$level <- level

  if(verbose){
    message("Preparing the data...")
  }

  body <- convert2text(body)

  rsp_api <- callTimeGPT(text = body, token = token, verbose = verbose, historic = FALSE)

  pred <- rsp_api$data$value
  x <- object$x

  if (!is.null(x)) {
    tspx <- tsp(x)
    nx <- max(which(!is.na(x)))
    if (nx != length(x) | is.null(tsp(pred))) {
      tspx[2] <- time(x)[nx]
      start.f <- tspx[2] + 1 / tspx[3]
      pred <- ts(pred, frequency = tspx[3], start = start.f)
    }
  }

  return(structure(
    list(method = object$method,
         model = object,
         resp_api = resp_api,
         fitted = object$fitted,
         residuals = object$residuals,
         mean = pred, x = x, series = object$series),
    class = "forecast"))
}
