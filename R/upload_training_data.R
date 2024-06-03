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
#' @importFrom brio writeLines
#' @examples
#'

upload_training_data <- function(data, api_key) {
  URL <- "https://api.openai.com/v1/files"
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)
  
  tryCatch({
    # Step 1: Create JSON data
    JSON <- create_JSON(data)
    
    # Step 2: Write JSON data to a temporary file
    tryCatch({
      brio::writeLines(JSON, tmp)
    }, error = function(e) {
      stop("Failed to write JSON data to file. Check file permissions, available disk space, and the file path. Details", conditionMessage(e))
    })
    
    # Step 3: Create the HTTP request

    REQ <- tryCatch({
      httr2::request(URL) |>
        httr2::req_auth_bearer_token(api_key) |>
        httr2::req_body_multipart(file = httr::upload_file(tmp), purpose = "fine-tune")
    }, error = function(e) {
      stop("Error creating HTTP request: Possible causes include issues with network connectivity, the API key, or constructing the request body. Details", conditionMessage(e))
    })
    
    # Step 4: Perform the request
    RESP <- get_response(REQ)
    CONTENT <- httr2::resp_body_json(RESP)
    return(CONTENT$id)

  }, error = function(e) {
    warning("An error occurred during the data upload process:", conditionMessage(e))
    return(NULL)
  })
}



