library(httr2)

# Helper function to create a request
create_request <- function(url, api_key, body = NULL) {
  if (is.null(body)) {
    tryCatch({
      httr2::request(url) %>%
        httr2::req_auth_bearer_token(api_key)
    }, error = function(e) {
      warning("Error: Failed to create request without body. ", conditionMessage(e))
      return(NULL)
    })
  } else {
    tryCatch({
      httr2::request(url) %>%
        httr2::req_auth_bearer_token(api_key) %>%
        httr2::req_body_json(body)
    }, error = function(e) {
      warning("Error: Failed to create request with body. ", conditionMessage(e))
      return(NULL)
    })
  }
}

# Helper function to get the JSON message from the response
get_message <- function(response) {
  tryCatch({
    if (is.null(response)) stop("Received null response")
    httr2::resp_body_json(response)
  }, error = function(e) {
    warning("Error: Failed to parse response. ", conditionMessage(e),
            " Status code: ", response$status_code,
            " Response content: ", response)
    return(NULL)
  })
}

# Helper function to perform the request and get response
get_response <- function(req) {
  tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    warning("Error: Failed to perform the request. ", conditionMessage(e))
    return(NULL)
  })
}

# Helper function to get status from message
get_status <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null body")
    message$status
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}

# Helper function to get ID from message
get_id <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null body")
    message$id
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}

# Helper function to get the text content from the model response
get_text_content <- function(message) {
  tryCatch({
    if (is.null(message)) stop("Received null content")
    message$choices[[1]]$message$content
  }, error = function(e) {
    warning("Error: Unexpected response structure. ", conditionMessage(e))
    return(NULL)
  })
}

# Function to format chat data
format_chat <- function(row) {
  list(
    messages = list(
      list(role = "user", content = row["prompt"]),
      list(role = "assistant", content = row["response"])
    )
  )
}

# Convert the dataset to JSONL format and upload directly
upload_training_data <- function(df, api_key) {
  tryCatch({
    # Convert the data frame to JSONL format
    df$conversation <- apply(df, 1, format_chat)
    jsonl_data <- sapply(df$conversation, toJSON, auto_unbox = TRUE)

    # Create a temporary file to store JSONL data
    tmp <- tempfile(fileext = ".jsonl")
    con <- file(tmp, "w")
    writeLines(jsonl_data, con)
    close(con)

    # Upload the training data
    response <- request("https://api.openai.com/v1/files") %>%
      req_auth_bearer_token(api_key) %>%
      req_body_multipart(file = upload_file(tmp), purpose = "fine-tune") %>%
      req_perform()

    # Remove the temporary file
    unlink(tmp)

    # Process the response
    if (response$status_code != 200) {
      stop("Failed to upload training data. Status code: ", response$status_code)
    }

    content <- resp_body_json(response)
    return(content$id)
  }, error = function(e) {
    warning("Error occurred during file upload: ", conditionMessage(e))
    return(NULL)
  })
}

# Updated predefined lookup table for max tokens for known models
max_tokens_lookup <- list(
  "gpt-4" = 8192,
  "gpt-4-turbo" = 128000,
  "gpt-4o" = 128000,
  "gpt-3.5-turbo" = 4096,
  "gpt-3.5-turbo-16k" = 16384,
  "text-davinci-003" = 4000
)



# Function to get the max tokens for a model using the lookup table
get_max_tokens <- function(model_name) {
  if (model_name %in% names(max_tokens_lookup)) {
    return(max_tokens_lookup[[model_name]])
  } else {
    warning("Model name not found in the lookup table.")
    return(NULL)
  }
}

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

# Function to identify the latest model
get_latest_model <- function(DT) {
  # Separate models with and without ckpt-step
  DT[, has_ckpt_step := grepl(":ckpt-step-", id)]

  # If there are models without ckpt-step, prioritize the latest one of those
  if (any(DT$has_ckpt_step == FALSE)) {
    latest_model <- DT[has_ckpt_step == FALSE][order(-created)][1]
  } else {
    # Otherwise, use the model with the highest ckpt-step
    DT[, ckpt_step := as.numeric(gsub(".*:ckpt-step-(\\d+).*", "\\1", id))]
    DT <- DT[!is.na(ckpt_step)]
    latest_model <- DT[order(-ckpt_step)][1]
  }

  return(latest_model$id)
}
