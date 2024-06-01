# Generalized function to query a model
#' Title
#'
#' @param prompt string. The prompt to send to the model
#' @param api_key string. The API key for authentication
#' @param model_name string. The name of the model to query
#' @param max_tokens integer. he maximum number of tokens to generate
#' @param temperature integer. The temperature for sampling
#' @param debug boolean. Whether to print debug information
#'
#' @return
#' @export
#'
#' @examples
query_model <- function(prompt, api_key, model_name = "gpt-4", max_tokens = NULL, temperature = 0, debug = FALSE) {
  URL <- "https://api.openai.com/v1/chat/completions"
  
  BODY <- list(
    model = model_name,
    messages = list(
      list(role = "user", content = prompt)
    ),
    max_tokens = max_tokens,
    temperature = temperature
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
  
  OUTPUT <- tryCatch({
    if (is.null(MESSAGE) || is.null(MESSAGE$choices) || length(MESSAGE$choices) == 0) {
      stop("Received null or empty message")
    }
    MESSAGE$choices[[1]]$message$content
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
  
  return(OUTPUT)
}


