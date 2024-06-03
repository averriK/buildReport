#' Create Fine-Tuning Job
#' 
#' @param file_id string. The ID of the training file.
#' @param debug boolean. Logical indicating whether to print debug information.
#' @param api_key string. The API key for authentication.
#'
#' @return The ID of the created fine-tuning job.
#' @export

create_fine_tuning_job <- function(file_id, api_key, debug = TRUE) {
  URL <- "https://api.openai.com/v1/fine_tuning/jobs"
  
  # Create the request body
  BODY <- list(
    model = "gpt-3.5-turbo",
    training_file = file_id,
    suffix = "custom_model"
  )
  
  if (debug) {
    # Debugging: Print the request body
    cat("Debug: Request body:\n")
    print(BODY)
  }
  
  REQUEST <- create_request(url = URL, api_key = api_key, body = BODY, method = "POST")
  RESPONSE <- get_response(REQUEST)
  
  if (debug) {
    # Debugging: Print the raw response
    cat("Debug: Raw response:\n")
    print(RESPONSE)
  }
  
  MESSAGE <- get_message(RESPONSE)
  
  if (debug) {
    # Debugging: Print the parsed message
    cat("Debug: Parsed message:\n")
    print(MESSAGE)
  }
  
  ID <- get_id(MESSAGE)
  
  if (is.null(ID)) {
    warning("Failed to retrieve the fine-tuning job ID.")
    if (!debug) {
      # Print the raw response and parsed message in case of error, even if debug is FALSE
      cat("Debug: Raw response:\n")
      print(RESPONSE)
      cat("Debug: Parsed message:\n")
      print(MESSAGE)
    }
  }
  
  return(ID)
}


