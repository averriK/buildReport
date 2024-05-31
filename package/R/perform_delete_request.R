# Helper function to perform a DELETE request with error handling and log messages
perform_delete_request <- function(url, api_key, model_id) {
  req <- create_request(url, api_key)

  response <- get_response(req)

  if (is.null(response)) {
    warning("Failed to delete model ", model_id, ": no response received.")
    return()
  }

  message <- get_message(response)

  if (is.null(message)) {
    warning("Failed to delete model ", model_id, ": could not parse response.")
    return()
  }

  status <- get_status(message)

  if (status != "succeeded") {
    warning("Failed to delete model ", model_id, ". Status: ", status)
  } else {
    message("Model ", model_id, " deleted successfully.")
  }
}
