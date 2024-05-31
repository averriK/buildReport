# Function to delete multiple models owned by you
delete_fine_tune_model <- function(model_ids, api_key) {
  for (model_id in model_ids) {
    url <- paste0("https://api.openai.com/v1/models/", model_id)
    response <- request(url) %>%
      req_method("DELETE") %>%
      req_auth_bearer_token(api_key) %>%
      req_perform()

    if (response$status_code == 200) {
      message(paste("Model", model_id, "deleted successfully."))
    } else {
      content <- resp_body_string(response)
      warning(paste("Failed to delete model", model_id, ". Status code:", response$status_code, ". Response:", content))
    }
  }
}
