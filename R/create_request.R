# Helper function to create a request
#' Title
#'
#' @param url string. The URL for the request.
#' @param api_key string. The API key for authentication.
#' @param body string. The body of the request.
#' @param method string. The method of the request, GET,DELETES
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 

create_request <- function(url, api_key, body = NULL, method = "GET") {
  request_object <- tryCatch({
    if (is.null(body)) {
      httr2::request(url) |>
        httr2::req_method(method) |>
        httr2::req_auth_bearer_token(api_key)
    } else {
      httr2::request(url) |>
        httr2::req_method(method) |>
        httr2::req_auth_bearer_token(api_key) |>
        httr2::req_body_json(body)
    }
    
  }, error = function(e) {
    stop("Error creating the request: Possible reasons could be an invalid URL, invalid API key, or network issues. Details: ", conditionMessage(e))
    # return(NULL)
  })
  return(request_object)
}


