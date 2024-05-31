# Function to monitor the fine-tuning job
monitor_fine_tuning <- function(fine_tune_id, api_key) {
  URL <- paste0("https://api.openai.com/v1/fine_tuning/jobs/", fine_tune_id)

  REQUEST <- create_request(URL, api_key)
  if (is.null(REQUEST)) return(NULL)

  RESPONSE <- get_response(REQUEST)
  if (is.null(RESPONSE)) return(NULL)

  MESSAGE <- get_message(RESPONSE)
  if (is.null(MESSAGE)) return(NULL)

  STATUS <- get_status(MESSAGE)
  return(STATUS)
}
