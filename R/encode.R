#' Title
#'
#' @param text string
#' @param language string
#'
#' @return
#' @export
#' 
#' @importFrom data.table rbindlist as.data.table
#' @importFrom utils tail
#'
#' @examples
encode <- function(text, language = "ES") {
  LANG <- language
  
  # Ensure the text ends with a newline
  if (length(text) == 0 || nzchar(utils::tail(text, 1))) {
    text <- c(text, "")
  }
  
  # Process the text with different stages
  stages <- c("captions", "includes", "code", "math", "inline_r", "references", "citations", "figures", "equations")
  
  INDEX <- NULL
  for (TYPE in stages) {
    sprintf("> %s", TYPE) |> cat(fill = TRUE)
    TAG <- markup(text, type = TYPE)
    INDEX <- data.table::rbindlist(list(INDEX, TAG$index))
    text <- TAG$text
  }
  
  return(list(text = text, index = INDEX))
}
