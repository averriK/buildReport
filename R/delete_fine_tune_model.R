#' Delete Fine-Tuned Models
#' 
#' @param model_ids A vector of model IDs to delete. If NULL, deletes all models except the latest.
#' @param api_key The API key for authentication.
#' @export
delete_fine_tune_model <- function(models = NULL, api_key) {
  DT <- list_fine_tuned_models(api_key)
  
  model_ids <- switch(tolower(models),
                      "all" = DT$id,
                      "previous" = {
                        if (nrow(DT) > 1) {
                          latest_model_id <- get_latest_model(DT)
                          DT$id[DT$id != latest_model_id]
                        } else {
                          character(0)
                        }
                      },
                      NULL = {
                        message("No models specified for deletion.")
                        return()
                      },
                      models  # Assumes models is a character vector of model IDs
  )
  
  if (length(model_ids) == 0) {
    message("No models available for deletion.")
    return()
  }
  
  for (model_id in model_ids) {
    url <- paste0("https://api.openai.com/v1/models/", model_id)
    perform_delete_request(url, api_key, model_id)
  }
}

