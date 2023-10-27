#' Interact with the TimeGPT API (raw text)
#'
#' This function is designed to interact with the \code{TimeGPT API}.
#' It sends a request to the \code{TimeGPT API} and returns the server response.
#'
#' @param text A string to send as the body of the request. For more details, see
#' \url{https://docs.nixtla.io/reference/timegpt_timegpt_post}.
#' @param token The authorization token to interact with the \code{TimeGPT API}.
#' @param verbose If \code{TRUE}, additional console information about the API request and
#' response are provided.
#' @param historic If \code{TRUE}, the fitted time series data for the in-sample period are provided.
#'
#' @author Daniele Girolimetto
#'
#' @return A list object with the TimeGPT server response.
#' @export
callTimeGPT <- function(text, token = NULL, verbose = FALSE, historic = FALSE){

  if(!is.character(text)){
    cli::cli_abort("{.arg text} is not a character.")
  }

  if(historic){
    url <- "https://dashboard.nixtla.io/api/timegpt_historic"
  }else{
    url <- "https://dashboard.nixtla.io/api/timegpt"
  }

  encode <- "json"

  if(is.null(token)){
    token <- get_timeGPT_token()
  }else{
    set_TimeGPT_token(token)
    token <- get_timeGPT_token()
  }

  rsp_api <- NULL
  req <- req_body_raw(req_headers(request(url),
                                  Accept = "application/json",
                                  Authorization = paste0("Bearer ", as.character(token)[2])),
                      text)
  if(verbose){
    cli::cli_progress_step("Making a request to TimeGPT (NIXTLA).",
                           msg_done = "API request completed. Satus: {rsp_api$message}.")
  }
  rsp_api <- fromJSON(resp_body_string(req_perform(req)))

  if(verbose){
    cli::cli_alert_info(paste0("If you have questions or need support about TimeGPT, ",
                               "please email {.email ops@nixtla.io}. For any other ",
                               "questions related to the R package, visit ",
                               "{.url http://github.com/danigiro/TimeGPTapi}"))
  }

  return(rsp_api)
}

#' Set and get the TimeGPT API token in the R environment
#'
#' Set the \code{TimeGPT} token as an environment variable. You should add
#' \code{TIMEGPT_TOKEN = <token>} to your \code{.Renviron} otherwise you will
#' have to call this function every session.
#'
#' @param token The authorization token to interact with the \code{TimeGPT API}
#' \url{https://dashboard.nixtla.io}.
#'
#' @author Daniele Girolimetto
#'
#' @return \code{get_timeGPT_token} returns the token.
#' @rdname timeGPT_token
#'
#' @order 2
#' @export
get_timeGPT_token <- function(){
  token <- Sys.getenv("TIMEGPT_TOKEN")
  if(identical(token, "")){
    cli::cli_abort(paste0("No API token found, please supply with {.arg token} argument or with ",
                          "{.help [{.fun set_TimeGPT_token}](TimeGPTapi::set_TimeGPT_token)}. ",
                          "If you don't have a token, visit {.url https://dashboard.nixtla.io}"))
  }else{
    return(oauth_token(token))
  }
}

#' @rdname timeGPT_token
#' @order 1
#' @export
set_TimeGPT_token <- function(token){
  if (is.null(token)) {
    token <- askpass::askpass("Please enter your API token (https://dashboard.nixtla.io)")
  }
  Sys.setenv("TIMEGPT_TOKEN" = token)
}

convert2text <- function(object){
  if(!is.list(object)){
    stop("'object' is not a list (TimeGPT class)", call. = FALSE)
  }
  out <- paste0('{', paste0(lapply(names(object), function(x){
    obj <- object[[x]]
    if(length(obj)==1 && (is.numeric(obj) | obj %in% c("true", "false"))){
      paste0('"', x, '":', obj)
    }else if(length(obj)==1){
      paste0('"', x, '":"', obj, '"')
    }else if(is.null(names(obj))){
      paste0('"', x, '":[', paste(obj, collapse = ","), ']')
    }else{
      paste0('"', x, '":{', paste0('"', names(obj), '":', obj, collapse = ','), '}')
    }
  }), collapse = ','), '}')
  return(out)
}
