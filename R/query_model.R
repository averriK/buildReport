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

library(httr2)

query_model <- function(prompt, api_key, model_name = "gpt-4o", temperature = 0) {
  URL <- "https://api.openai.com/v1/chat/completions"
  BODY <- list(
    model = model_name,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = temperature
  )
  tryCatch({
    # Create the HTTP request
    REQ <- get_request(url = URL, api_key = api_key, body = BODY, method = "POST")
    # Perform the HTTP request
    RESP <- get_response(REQ)
    # Parse the response body JSON
    MESSAGE <- get_message(RESP)
    
    # Extract the content from the response message
    CONTENT <- get_content(MESSAGE)
    
    return(CONTENT)
  }, error = function(e) {
    warning("Error occurred during query:", conditionMessage(e))
    return(NULL)}
  )
  
}


# query_model <- function(prompt, api_key, model_name = "gpt-4o",  temperature = 0) {
#   
#   URL <- "https://api.openai.com/v1/chat/completions"
#   BODY <- list(
#     model = model_name,
#     messages = list(
#       list(role = "user", content = prompt)
#     ),
#     max_tokens = NULL,
#     temperature = temperature
#   )
#   
#   tryCatch({
#     # REQ <- get_request(url = URL, api_key = api_key, body = BODY, method = "POST")
#     
#     REQ <- tryCatch({
#       httr2::request(URL) |>
#         httr2::req_method("POST") |>
#         httr2::req_auth_bearer_token(api_key) |>
#         httr2::req_body_json(BODY)
#     }, error = function(e) {
#       warning("Error occurred during query: ", conditionMessage(e))
#       return(NULL)
#     })
#     
#     
#     # RESP <- get_response(REQ)
#     RESP <- tryCatch({
#       httr2::req_perform(REQ)
#     }, error = function(e) {
#       warning("Error occurred during query: ", conditionMessage(e))
#       return(NULL)
#     })
#     # MESSAGE <- get_message(RESPONSE)
#     MESSAGE <- tryCatch({
#       httr2::resp_body_json(RESP)
#     }, error = function(e) {
#       warning("Error occurred during query: ", conditionMessage(e))
#       return(NULL)
#     })
#     CONTENT <- tryCatch(
#       {MESSAGE$choices[[1]]$message$content},
#       error = function(e) {
#         warning("Error occurred during query: ", conditionMessage(e))
#         return(NULL)
#       }      )
#     return(CONTENT)
#   }, error = function(e) {
#     warning("Error occurred during query: ", conditionMessage(e))
#     return(NULL)
#   })
# }



