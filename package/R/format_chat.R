# Function to format chat data
format_chat <- function(row) {
  list(
    messages = list(
      list(role = "user", content = row["prompt"]),
      list(role = "assistant", content = row["response"])
    )
  )
}
