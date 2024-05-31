# Helper function to perform the request and get response
get_response <- function(req) {
  tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    warning("Error: Failed to perform the request. ", conditionMessage(e))
    return(NULL)
  })
}
