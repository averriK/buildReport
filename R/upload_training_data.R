#' Title
#'
#' @param data data.table. The data.table containing the user and assistant data.
#' @param api_key string. The API key for authentication.
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

upload_training_data <- function(data, api_key) {
  tryCatch({
    JSON <- create_JSON(data)
    BODY <- create_body_upload(JSON)
    URL <- "https://api.openai.com/v1/files"
    REQUEST <- create_request(url = URL, api_key = api_key, body = BODY, method = "POST")
    RESPONSE <- get_response(REQUEST)
    MESSAGE <- get_message(RESPONSE)
    
    if (is.null(MESSAGE)) {
      stop("Failed to upload training data.")
    }
    
    return(MESSAGE$id)
  }, error = function(e) {
    warning("Error occurred during file upload: ", conditionMessage(e))
    return(NULL)
  })
}

