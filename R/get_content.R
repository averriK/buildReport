#' Title
#'
#' @param message string. The message to extract the content from.
#'
#' @return
#' @export
#'
#' @examples
get_content <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null content")
    message$choices[[1]]$message$content
  }, error = function(e) {
    stop("Error: Unexpected response structure. Error extracting content from response. The response format might be different than expected.", conditionMessage(e))
    # return(NULL)
  })
}

