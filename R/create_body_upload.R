#' Title
#'
#' @param JSON string. JSON data to be uploaded
#'
#' @return
#' @export
#' 
#' @importFrom httr upload_file
#'
#' @examples
create_body_upload <- function(JSON) {
  body <- tryCatch({
    # Create a temporary file to store JSONL data
    tmp <- tempfile(fileext = ".jsonl")
    con <- file(tmp, "w")
    writeLines(JSON, con)
    close(con)
    
    # Create the request body
    aux <- list(file = httr::upload_file(tmp), purpose = "fine-tune")
    
    # Remove the temporary file immediately after creating the body
    unlink(tmp)
    
    return(aux)
  }, error = function(e) {
    warning("Error during body creation: Possible reasons could be issues in file creation, writing JSON data to the file, or constructing the request body. Details: ", conditionMessage(e))
    
    return(NULL)
  })
}
