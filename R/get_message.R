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
  if (is.null(response)) {
    warning("Received null response")
    return(NULL)
  }
  tryCatch({
    httr2::resp_body_json(response)
  }, error = function(e) {
    status_code <- tryCatch({
      response$status_code
    }, error = function(inner_e) {
      "Unknown status code"
    })
    warning("Error: Failed to parse response. ", conditionMessage(e),
            " Status code: ", status_code)
    return(NULL)
  })
}

# get_message <- function(response) {
#   tryCatch({
#     if (is.null(response)) stop("Received null response")
#     httr2::resp_body_json(response)
#   }, error = function(e) {
#     warning("Error: Failed to parse response. ", conditionMessage(e),
#             " Status code: ", response$status_code)
#     return(NULL)
#   })
# }

