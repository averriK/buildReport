# Function to list models not owned by OpenAI ("openai", "system")
#' Title
#'
#' @param api_key The API key for authentication.
#'
#' @return
#' @export
#'
#' @examples
list_fine_tuned_models <- function(api_key) {
  response <- httr2::request("https://api.openai.com/v1/models") |> 
    httr2::req_auth_bearer_token(api_key) |> 
    httr2::req_perform()

  content <- httr2::resp_body_json(response)

  # Convert the list of models to a data.table
  DT <- data.table::rbindlist(content$data, fill = TRUE)

  # Filter out models owned by OpenAI ("openai", "system")
  user_owned_models <- DT[!(owned_by %in% c("openai", "system"))]

  return(user_owned_models)
}
