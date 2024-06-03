#' Monitor Fine-Tuning Job
#' 
#' @param fine_tune_id The ID of the fine-tuning job.
#' @param api_key The API key for authentication.
#' @return The status of the fine-tuning job.
#' @export
monitor_fine_tuning <- function(fine_tune_id, api_key) {
  URL <- paste0("https://api.openai.com/v1/fine_tuning/jobs/", fine_tune_id)

  tryCatch({
    REQ <- create_request(url = URL, api_key = api_key, method = "GET")
    RESP <- get_response(REQ)
    MESSAGE <- get_message(RESP)
    STATUS <- MESSAGE$status
    return(STATUS)
  }, error = function(e) {
    warning("Error occurred during fine-tuning job monitoring:", conditionMessage(e))
    return(NULL)
  })
}


