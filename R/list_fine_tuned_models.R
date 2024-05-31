# Function to list models not owned by OpenAI ("openai", "system")
list_fine_tuned_models <- function(api_key) {
  response <- request("https://api.openai.com/v1/models") %>%
    req_auth_bearer_token(api_key) %>%
    req_perform()

  content <- resp_body_json(response)

  # Convert the list of models to a data.table
  DT <- data.table::rbindlist(content$data, fill = TRUE)

  # Filter out models owned by OpenAI ("openai", "system")
  user_owned_models <- DT[!(owned_by %in% c("openai", "system"))]

  return(user_owned_models)
}
