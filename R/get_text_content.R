# Helper function to get the text content from the model response
#' Title
#'
#' @param message string
#'
#' @return
#' @export
#'
#' @examples
get_text_content <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null content")
    message$choices[[1]]$message$content
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}
