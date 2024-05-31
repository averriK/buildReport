# Helper function to get status from message
#' Title
#'
#' @param message string
#'
#' @return
#' @export
#'
#' @examples
get_status <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null body")
    message$status
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}
