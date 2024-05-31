create_fine_tuning_job <- function(file_id, api_key) {
  URL <- "https://api.openai.com/v1/fine_tuning/jobs"

  # Create the request
  BODY <- list(
    model = "gpt-3.5-turbo",
    training_file = file_id,
    suffix = "custom_model"
  )

  REQUEST <- create_request(url=URL, api_key, body=BODY)
  RESPONSE <- get_response(REQUEST)
  MESSAGE <- get_message(RESPONSE)
  ID <- get_id(MESSAGE)

  return(ID)
}
