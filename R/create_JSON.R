#' Title
#'
#' @param data data.frame. The data to be converted to JSON
#'
#' @return
#' @export
#' 
#' @importFrom jsonlite toJSON
#'
#' @examples
create_JSON <- function(data) {
  tryCatch({
    data$conversation <- apply(data, 1, function(row) {
      # Define your format_chat logic here
      # For example, converting a row to the desired chat format
      list(role = row["role"], content = row["content"])
    })
    JSON <- sapply(data$conversation, jsonlite::toJSON, auto_unbox = TRUE)
    return(JSON)
  }, error = function(e) {
    warning("Error during data formatting: Possible reasons could be invalid data structure, missing required columns, or issues in the format_chat logic. Details: %s", conditionMessage(e))
    
    return(NULL)
  })
}
