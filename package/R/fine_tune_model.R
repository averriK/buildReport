# Function to handle the entire fine-tuning workflow
fine_tune_model <- function(file_id, api_key, interval = 10) {
  # List all fine-tuned models
  user_owned_models <- list_fine_tuned_models(api_key)

  # Check if a model associated with the given file_id already exists
  if (nrow(user_owned_models) > 0 && any(grepl(file_id, user_owned_models$id))) {
    existing_model <- user_owned_models[grepl(file_id, id)]
    latest_model_id <- get_latest_model(existing_model)
    print(paste("Model associated with the file_id already exists. Model ID:", latest_model_id))
    return(latest_model_id)
  }

  fine_tune_id <- tryCatch({
    # Create a fine-tuning job if the model doesn't already exist
    fine_tune_id <- create_fine_tuning_job(file_id, api_key)

    if (is.null(fine_tune_id)) {
      stop("Failed to create fine-tuning job.")
    }

    print(paste("Fine-tuning job ID:", fine_tune_id))
    fine_tune_id

  }, error = function(e) {
    warning("An error occurred while creating the fine-tuning job: ", conditionMessage(e))
    return(NULL)
  })

  # Initialize status before entering the loop
  status <- tryCatch({
    monitor_fine_tuning(fine_tune_id, api_key)
  }, error = function(e) {
    warning("An error occurred while monitoring the fine-tuning job: ", conditionMessage(e))
    NULL
  })

  # Monitor the fine-tuning job until it succeeds or fails
  while (!is.null(status) && !(status %in% c("succeeded", "failed", "cancelled"))) {
    print(paste("Fine-tuning job status:", status))

    Sys.sleep(interval)  # Wait for the specified interval before checking again

    status <- tryCatch({
      monitor_fine_tuning(fine_tune_id, api_key)
    }, error = function(e) {
      warning("An error occurred while monitoring the fine-tuning job: ", conditionMessage(e))
      NULL
    })
  }

  return()
}
