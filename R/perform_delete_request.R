#' Perform Delete Request
#' 
#' @param url The URL for the DELETE request.
#' @param api_key The API key for authentication.
#' @param model_id The ID of the model to be deleted.
#' @export

perform_delete_request <- function(url, api_key, model_id) {
  req <- create_request(url, api_key)
  
  if (is.null(req)) {
    warning("Failed to create request for model ", model_id)
    return()
  }
  
  response <- get_response(req)
  
  if (is.null(response)) {
    warning("Failed to delete model ", model_id, ": no response received.")
    return()
  }
  
  status_code <- tryCatch({
    response$status_code
  }, error = function(e) {
    warning("Failed to retrieve status code for model ", model_id, ": ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(status_code) || status_code != 200) {
    warning("Failed to delete model ", model_id, ". Status code: ", status_code)
  } else {
    message("Model ", model_id, " deleted successfully.")
  }
}

