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
    if (is.null(response)) stop("Received null response")
    httr2::resp_body_json(response)
  }, error = function(e) {
    warning("Error: Failed to parse response. ", conditionMessage(e),
            " Status code: ", response$status_code,
            " Response content: ", response)
    return(NULL)
  })
}
