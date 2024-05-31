# Function to identify the latest model
get_latest_model <- function(DT) {
  # Separate models with and without ckpt-step
  DT[, has_ckpt_step := grepl(":ckpt-step-", id)]

  # If there are models without ckpt-step, prioritize the latest one of those
  if (any(DT$has_ckpt_step == FALSE)) {
    latest_model <- DT[has_ckpt_step == FALSE][order(-created)][1]
  } else {
    # Otherwise, use the model with the highest ckpt-step
    DT[, ckpt_step := as.numeric(gsub(".*:ckpt-step-(\\d+).*", "\\1", id))]
    DT <- DT[!is.na(ckpt_step)]
    latest_model <- DT[order(-ckpt_step)][1]
  }

  return(latest_model$id)
}
