#' Perform Delete Request
#' 
#' @param url The URL for the DELETE request.
#' @param api_key The API key for authentication.
#' @param model_id The ID of the model to be deleted.
#' @export
perform_delete_request <- function(url, api_key, model_id) {
  req <- create_request(url, api_key, method = "DELETE")
  
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
  
  if (is.null(status_code)) {
    warning("Failed to delete model ", model_id, ". Status code: ", status_code)
    response_content <- tryCatch({
      httr2::resp_body_string(response)
    }, error = function(e) {
      "Failed to retrieve response content"
    })
    print(paste("Debug: Response content for failed deletion of model ", model_id, ":", response_content))
    return()
  }
  
  if (status_code == 404) {
    warning("Model not found: ", model_id, ". Status code: 404")
    return()
  }
  
  if (!(status_code %in% c(200, 204))) {
    warning("Failed to delete model ", model_id, ". Status code: ", status_code)
    response_content <- tryCatch({
      httr2::resp_body_string(response)
    }, error = function(e) {
      "Failed to retrieve response content"
    })
    print(paste("Debug: Response content for failed deletion of model ", model_id, ":", response_content))
  } else {
    response_content <- tryCatch({
      httr2::resp_body_json(response)
    }, error = function(e) {
      warning("Failed to parse response content for model ", model_id, ": ", conditionMessage(e))
      return(NULL)
    })
    
    if (!is.null(response_content) && response_content$deleted) {
      message("Model ", model_id, " deleted successfully.")
    } else {
      warning("Model ", model_id, " was not marked as deleted. Status code: ", status_code)
    }
  }
}

