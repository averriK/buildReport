#' Title
#'
#' @param DT data.table
#' @param api_key The API key for authentication.
#'
#' @return
#' @export
#' 
#' @importFrom jsonlite toJSON
#' @importFrom httr2 request req_auth_bearer_token req_body_multipart req_perform
#' @importFrom httr upload_file
#'
#' @examples
#' 
# Convert the dataset to JSONL format and upload directly
upload_training_data <- function(DT, api_key) {
  tryCatch({
    # Convert the data frame to JSONL format
    DT$conversation <- apply(DT, 1, format_chat)
    jsonl_data <- sapply(DT$conversation, jsonlite::toJSON, auto_unbox = TRUE)

    # Create a temporary file to store JSONL data
    tmp <- tempfile(fileext = ".jsonl")
    con <- file(tmp, "w")
    writeLines(jsonl_data, con)
    close(con)

    # Upload the training data
    response <- httr2::request("https://api.openai.com/v1/files") |> 
      httr2::req_auth_bearer_token(api_key) |> 
      httr2::req_body_multipart(file = httr::upload_file(tmp), purpose = "fine-tune") |> 
      httr2::req_perform()

    # Remove the temporary file
    unlink(tmp)

    # Process the response
    if (response$status_code != 200) {
      stop("Failed to upload training data. Status code: ", response$status_code)
    }

    content <- httr2::resp_body_json(response)
    return(content$id)
  }, error = function(e) {
    warning("Error occurred during file upload: ", conditionMessage(e))
    return(NULL)
  })
}

format_chat <- function(row) {
  list(
    messages = list(
      list(role = "user", content = row["prompt"]),
      list(role = "assistant", content = row["response"])
    )
  )
}
