#' Title
#'
#' @param text string
#' @param index data.table
#' @param language string
#'
#' @return
#' @export
#' 
#' @importFrom data.table rbindlist as.data.table
#'
#' @examples
decode <- function(text, index, language = "ES") {
  LANG <- language
  
  # Fix Paths in INDEX for "includes" type
  index[TYPE == "includes", VALUE := gsub(x = VALUE, pattern = "\\{\\{< include article/qmd/([^/]+\\.qmd) >\\}\\}", replacement = paste0("{{< include ", language, "/\\1 >}}"))]
  
  # Process the text using the INDEX
  process_text <- function(SOURCE, INDEX) {
    AUX <- SOURCE
    for (i in 1:nrow(INDEX)) {
      AUX <- gsub(INDEX$ID[i], INDEX$VALUE[i], AUX, fixed = TRUE)
    }
    return(AUX)
  }
  
  processed_text <- process_text(text, index)
  
  return(processed_text)
}
