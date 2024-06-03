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
    
    stop("Error parsing the response: Possible reasons could be an invalid response structure or server error.  Details: ", conditionMessage(e))
    
    # return(NULL)
  })
}



