# Helper function to create a request
#' Title
#'
#' @param url string. The URL for the request.
#' @param api_key string. The API key for authentication.
#' @param body string. The body of the request.
#'
#' @return
#' @export
#'
#' @examples
create_request <- function(url, api_key, body = NULL) {
  if (is.null(body)) {
    tryCatch({
      httr2::request(url) |> 
        httr2::req_auth_bearer_token(api_key)
    }, error = function(e) {
      warning("Error: Failed to create request without body. ", conditionMessage(e))
      return(NULL)
    })
  } else {
    tryCatch({
      httr2::request(url) |> 
        httr2::req_auth_bearer_token(api_key) |> 
        httr2::req_body_json(body)
    }, error = function(e) {
      warning("Error: Failed to create request with body. ", conditionMessage(e))
      return(NULL)
    })
  }
}
