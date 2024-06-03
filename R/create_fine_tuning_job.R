#' Create Fine-Tuning Job
#' 
#' @param file_id string. The ID of the training file.
#' @param debug boolean. Logical indicating whether to print debug information.
#' @param api_key string. The API key for authentication.
#'
#' @return The ID of the created fine-tuning job.
#' @export

create_fine_tuning_job <- function(file_id, api_key, debug = FALSE) {
  URL <- "https://api.openai.com/v1/fine_tuning/jobs"
  
  # Create the request body
  BODY <- list(
    model = "gpt-3.5-turbo",
    training_file = file_id,
    suffix = "custom_model"
  )
  tryCatch({
    REQ <- create_request(url = URL, api_key = api_key, body = BODY, method = "POST")
    RESP <- get_response(REQ)
    MESSAGE <- get_message(RESP)
    ID <- MESSAGE$id
    return(ID)
  }, error = function(e) {
    warning("Error occurred during fine-tuning job creation:", conditionMessage(e))
    return(NULL)
  })
  
  
}


