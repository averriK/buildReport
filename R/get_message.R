# Helper function to get the JSON message from the response
#' Title
#'
#' @param response string
#'
#' @return
#' @export
#'
#' @examples
get_message <- function(response) {
  tryCatch({
    httr2::resp_body_json(response)
  }, error = function(e) {
    status_code <- tryCatch({
      response$status_code
    }, error = function(inner_e) {
      "Unknown status code"
    })
    warning("Error parsing the response: Possible reasons could be an invalid response structure or server error. Status code: ", status_code, ". Details: ", conditionMessage(e))
    
    return(NULL)
  })
}



