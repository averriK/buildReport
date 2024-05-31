# Generalized function to query a model
query_model <- function(prompt, api_key, model_name = "gpt-4", max_tokens = NULL, temperature = 0) {
  BODY <- list(
    model = model_name,
    messages = list(
      list(role = "user", content = prompt)
    ),
    max_tokens = max_tokens,
    temperature = temperature
  )

  URL <- "https://api.openai.com/v1/chat/completions"

  REQUEST <- create_request(URL, api_key, BODY)
  if (is.null(REQUEST)) return(NULL)

  RESPONSE <- get_response(REQUEST)
  if (is.null(RESPONSE)) return(NULL)

  MESSAGE <- get_message(RESPONSE)
  if (is.null(MESSAGE)) return(NULL)

  OUTPUT <- get_text_content(MESSAGE)
  return(OUTPUT)
}
