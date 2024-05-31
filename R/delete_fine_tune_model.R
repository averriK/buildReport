#' Delete Fine-Tuned Models
#' 
#' @param model_ids A vector of model IDs to delete. If NULL, deletes all models except the latest.
#' @param api_key The API key for authentication.
#' @export
delete_fine_tune_model <- function(model_ids = NULL, api_key) {
  if (is.null(model_ids)) {
    user_owned_models <- list_fine_tuned_models(api_key)
    if (nrow(user_owned_models) > 1) {
      latest_model_id <- get_latest_model(user_owned_models)
      model_ids <- user_owned_models$id[user_owned_models$id != latest_model_id]
    } else {
      message("No models available for deletion.")
      return()
    }
  }

  for (model_id in model_ids) {
    url <- paste0("https://api.openai.com/v1/models/", model_id)
    perform_delete_request(url, api_key, model_id)
  }
}

# Assuming these helper functions exist from the previous code
# list_fine_tuned_models(api_key) and get_latest_model(DT)
