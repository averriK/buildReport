library(httr2)
library(jsonlite)
library(purrr)


prepareGlossary <- function() {
  glossary <- data.frame(
    spanish = c("inteligencia artificial", "aprendizaje supervisado"),
    english = c("artificial intelligence", "supervised learning")
  )
  return(glossary)
}


applyGlossary <- function(text, glossary) {
  for (i in 1:nrow(glossary)) {
    text <- gsub(glossary$spanish[i], glossary$english[i], text)
  }
  return(text)
}


generalChatCompletion <- function(system, user, history, glossary) {
  validateAPI(api_key)

  # Apply the glossary to the user text
  user <- applyGlossary(user, glossary)

  token_count <- checkTokens(user)
  max_tokens <- 1000

  model <- selectModel(token_count, max_tokens)

  result <- getResponse(api_key, model, system, user, history, max_tokens)

  history <- updateHistory(history, user, result)

  return(list(result = result, history = history))
}

validateAPI <- function(api_key) {
  test_url <- "https://api.openai.com/v1/models"
  response <- request(test_url) %>%
    req_auth_bearer_token(api_key) %>%
    req_perform()

  if (response$status_code != 200) {
    stop("Invalid API key. Please check your API key and try again.")
  }
}

checkTokens <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]
  token_count <- length(words)
  return(token_count)
}


selectModel <- function(token_count, max_tokens) {
  model_token_limits <- c("gpt-3.5-turbo" = 4096, "gpt-3.5-turbo-128k" = 128000, "gpt-4" = 8192, "gpt-4-turbo" = 8192, "gpt-4o" = 8192)
  default_model <- "gpt-4o"
  extended_model <- "gpt-3.5-turbo-128k"

  if (token_count + max_tokens > model_token_limits[[default_model]]) {
    return(extended_model)
  } else {
    return(default_model)
  }
}

getResponse <- function(api_key, model, system, user, history, max_tokens) {
  endpoint <- "https://api.openai.com/v1/chat/completions"
  messages <- list(
    list(role = "system", content = system)
  )

  if (length(history) > 0) {
    messages <- c(messages, history)
  }

  messages <- append(messages, list(list(role = "user", content = user)))

  payload <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = 0.5
  )

  response <- request(endpoint) %>%
    req_auth_bearer_token(api_key) %>%
    req_body_json(payload) %>%
    req_perform()

  if (response$status_code != 200) {
    content <- response %>% resp_body_string()
    stop("API request failed with status code ", response$status_code, ". Response content: ", content)
  }

  content <- response |> httr2::resp_body_json(simplifyVector = TRUE)
  return(content$choices$message$content)
}


updateHistory <- function(history, user, response) {
  c(history,
    list(
      list(role = "user", content = user),
      list(role = "assistant", content = response)
    )
  ) |> purrr::compact()
}
