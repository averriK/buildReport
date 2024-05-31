# Function to get the max tokens for a model using the lookup table
#' Title
#'
#' @param model_name string. Model name
#'
#' @return
#' @export
#'
#' @examples
get_max_tokens <- function(model_name) {

  max_tokens_lookup <- list(
    "gpt-4" = 8192,
    "gpt-4-turbo" = 128000,
    "gpt-4o" = 128000,
    "gpt-3.5-turbo" = 4096,
    "gpt-3.5-turbo-16k" = 16384,
    "text-davinci-003" = 4000
  )

  if (model_name %in% names(max_tokens_lookup)) {
    return(max_tokens_lookup[[model_name]])
  } else {
    warning("Model name not found in the lookup table.")
    return(NULL)
  }
}
