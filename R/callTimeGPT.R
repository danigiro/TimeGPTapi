#' Title
#'
#' @param text
#' @param token
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
callTimeGPT <- function(text, token, verbose = FALSE, historic = FALSE){

  if(!is.character(text)){
    stop("'text' is not a character", call. = FALSE)
  }

  if(!is.character(token)){
    stop("'token' is not a character", call. = FALSE)
  }

  if(historic){
    url <- "https://dashboard.nixtla.io/api/timegpt_historic"
  }else{
    url <- "https://dashboard.nixtla.io/api/timegpt"
  }

  encode <- "json"

  if(verbose){
    message("Making a request to TimeGPT (NIXTLA)...")
  }

  rsp_api <- VERB("POST", url, body = text,
                  add_headers('authorization' = paste0('Bearer ', token)),
                  content_type("application/json"),
                  accept("application/json"),
                  encode = encode)
  rsp_api <- fromJSON(content(rsp_api, "text"))

  if(verbose){
    message("API request completed. Satus: ", rsp_api$message, "\n",
            "If you have questions or need support about TimeGPT, please email ops@nixtla.io\n",
            "For any other questions related to the R package, visit")

  }

  return(rsp_api)
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
