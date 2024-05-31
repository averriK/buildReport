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
    if (is.null(message)) stop("Received null body")
    message$id
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}
