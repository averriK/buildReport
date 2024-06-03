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
    warning("Error performing the request: Possible reasons could be network issues or server unavailability. Details: ", conditionMessage(e))
    return(NULL)
  })
}
