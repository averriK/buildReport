# Helper function to perform the request and get response
#' Title
#'
#' @param req string
#'
#' @return
#' @export
#'
#' @examples
get_response <- function(req) {
  tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    warning("Error: Failed to perform the request. ", conditionMessage(e))
    return(NULL)
  })
}
