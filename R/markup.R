

#' Title
#'
#' @param text string
#' @param type string
#'
#' @return
#' @export
#' 
#' @importFrom data.table rbindlist as.data.table
#'
#' @examples
markup <- function(text, type = "markdown") {
  REGEX <- list(
    captions = "#\\| (fig|tbl)-cap:\\s+\"(.*?)\"",
    math = "\\$\\$\\s*([^$]+?)\\s*\\$\\$|\\$\\s*([^$]+?)\\s*\\$|\\$\\s*([^$]+?)\\s*\\$",
    references = "(?:(?:@fig-|@tbl-|@eq-)\\w+|\\[@(?:fig-|tbl-|eq-)\\w+\\])",
    citations = "\\\\?\\[\\-?@([\\w,]+)\\\\?\\]",
    includes = "\\{\\{.*?\\}\\}",
    figures = "```\\{[^}]*\\}(?:.|\\n)*?```",
    equations = "```\\{[^}]*\\}(?:.|\\n)*?```",
    code = "```\\{r\\}(?:.|\\n)*?```",
    inline_r = "`r .*?`"
  )
  
  stopifnot(type %in% names(REGEX))
  PATTERN <- REGEX[[type]]
  SOURCE <- text
  INDEX <- NULL
  TEXT <- paste(SOURCE, collapse = "\n")
  IDX <- stringr::str_extract_all(TEXT, pattern = PATTERN) |> data.table::as.data.table()
  
  if (nrow(IDX) > 0) {
    IDX <- IDX[, list(TYPE = type, VALUE = V1, ID = sapply(V1, function(x) {
      paste0("<ID.", toupper(digest::digest(object = x, algo = "crc32")), ">")
    }))]
    
    AUX <- TEXT
    for (i in 1:nrow(IDX)) {
      AUX <- gsub(IDX$VALUE[i], IDX$ID[i], AUX, fixed = TRUE)
    }
    INDEX <- data.table::rbindlist(list(INDEX, IDX), use.names = TRUE) |> unique()
    TEXT <- AUX
  }
  
  TAG <- list(source = SOURCE, text = TEXT, index = INDEX)
  return(TAG)
}
