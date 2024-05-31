# Helper function to get ID from message
#' Title
#'
#' @param message string
#'
#' @return
#' @export
#'
#' @examples


get_id <- function(message) {
  tryCatch({
    if (is.null(message) || is.null(message$id)) stop("Received null message or missing ID")
    message$id
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}
