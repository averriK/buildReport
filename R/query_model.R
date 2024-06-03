# Generalized function to query a model
#' Title
#'
#' @param prompt string. The prompt to send to the model
#' @param api_key string. The API key for authentication
#' @param model_name string. The name of the model to query
#' @param temperature integer. The temperature for sampling
#'
#' @return
#' @export
#'
#' @examples
query_model <- function(prompt, api_key, model_name = "gpt-4o",  temperature = 0) {
  CONTENT <- tryCatch({
    URL <- "https://api.openai.com/v1/chat/completions"
    
    BODY <- list(
      model = model_name,
      messages = list(
        list(role = "user", content = prompt)
      ),
      max_tokens = NULL,
      temperature = temperature
    )
    
    REQUEST <- create_request(url = URL, api_key = api_key, body = BODY, method = "POST")
    RESPONSE <- get_response(REQUEST)
    MESSAGE <- get_message(RESPONSE)
    
    CONTENT <- MESSAGE$choices[[1]]$message$content
    
    
    
    
    return(CONTENT)
  }, error = function(e) {
    warning("Error occurred during query: ", conditionMessage(e))
    return(NULL)
  })
  
  return(CONTENT)
}



