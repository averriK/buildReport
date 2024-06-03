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
#' 

create_JSON <- function(data) {
  JSON <- tryCatch({
    if (!all(c("prompt", "response") %in% colnames(data))) {
      stop("Error: Data must contain 'prompt' and 'response' columns.")
    }
    conversation <- apply(data, 1, function(row) {
      list(
        messages = list(
          list(role = "user", content = row[["prompt"]]),
          list(role = "assistant", content = row[["response"]])
        )
      )
    })
    sapply(conversation, jsonlite::toJSON, auto_unbox = TRUE)
  }, error = function(e) {
    stop(sprintf("Error during data formatting: %s. This might be caused by invalid data structure or types.", conditionMessage(e)))
  })
  return(JSON)
}

# create_JSON <- function(data) {
#   JSON <- tryCatch({
#     if(!all(c("prompt", "response") %in% colnames(data))) {
#       stop("Data must contain 'prompt' and 'response' columns")
#     }
#     conversation <- apply(data, 1, function(row) {
#       list(
#         messages = list(
#           list(role = "user", content = row["prompt"]),
#           list(role = "assistant", content = row["response"])
#         )
#       )
#     })
#     sapply(conversation, jsonlite::toJSON, auto_unbox = TRUE)
#    
#   }, error = function(e) {
#     warning("Error during data formatting: Possible reasons could be invalid data structure, missing required columns, or issues in the format_chat logic. Details: %s", conditionMessage(e))
#     
#     return(NULL)
#   })
#   return(JSON)
# }
