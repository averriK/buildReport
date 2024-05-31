# Function to format chat data
#' Title
#'
#' @param row string. Row ID
#'
#' @return
#' @export
#'
#' @examples
format_chat <- function(row) {
  list(
    messages = list(
      list(role = "user", content = row["prompt"]),
      list(role = "assistant", content = row["response"])
    )
  )
}
