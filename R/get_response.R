# Helper function to perform the request and get response
#' Title
#'
#' @param request_object string
#'
#' @return
#' @export
#'
#' @examples


get_response <- function(request_object) {
  tryCatch({
    httr2::req_perform(request_object)
  }, error = function(e) {
    warning("Error performing the request: Possible reasons could be network issues or server unavailability. Details: ", conditionMessage(e))
    return(NULL)
  })
}
