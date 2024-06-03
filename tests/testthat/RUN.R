devtools::load_all()

training_data <- data.frame(
  prompt = c(
    "La AI está transformando el mundo.",
    "El aprendizaje automático es una rama de la inteligencia artificial.",
    "El aprendizaje profundo es una técnica avanzada de IA.",
    "La visión por computadora permite a las máquinas interpretar imágenes.",
    "Los modelos de lenguaje pueden generar texto coherente.",
    "La IA se utiliza en la detección de fraudes.",
    "Los chatbots pueden ayudar en el servicio al cliente.",
    "El procesamiento del lenguaje natural es crucial para la IA.",
    "Las redes neuronales están inspiradas en el cerebro humano.",
    "La IA está revolucionando la industria de la salud."
  ),
  response = c(
    "AI is transforming the world.",
    "Machine learning is a branch of artificial intelligence.",
    "Deep learning is an advanced AI technique.",
    "Computer vision enables machines to interpret images.",
    "Language models can generate coherent text.",
    "AI is used in fraud detection.",
    "Chatbots can assist in customer service.",
    "Natural language processing is crucial for AI.",
    "Neural networks are inspired by the human brain.",
    "AI is revolutionizing the healthcare industry."
  )
)

# Load the package
# Define API key and file ID for testing
api_key <- Sys.getenv("OPENAI_API_KEY2")

# Upload training data directly from the data frame
devtools::load_all()
file_id <- upload_training_data(training_data, api_key)

# 1. Create and fine-tune the model
cat("Creating fine-tuning job...\n")
fine_tune_id <- create_fine_tuning_job(file_id, api_key)
cat("Fine-tuning job ID:", fine_tune_id, "\n")

# 2. Monitor the fine-tuning job
cat("Monitoring fine-tuning job...\n")
status <- monitor_fine_tuning(fine_tune_id, api_key)
while (!is.null(status) && !(status %in% c("succeeded", "failed", "cancelled"))) {
  cat("Current status:", status, "\n")
  Sys.sleep(10)  # Wait for 10 seconds before checking again
  status <- monitor_fine_tuning(fine_tune_id, api_key)
}
cat("Final status:", status, "\n")

# 3. Check if the fine-tuned model already exists
cat("Checking for existing fine-tuned models...\n")
user_owned_models <- list_fine_tuned_models(api_key)
print(user_owned_models)

# 7. Assuming we have recreated some models for further testing
# Test deleting all models except the latest one
cat("Deleting all models except the latest one...\n")
delete_fine_tune_model("previous", api_key)

# 8. Verify the deletion by listing models again
cat("Verifying deletion of models...\n")
list_fine_tuned_models(api_key)

# 10. Query response
devtools::load_all()
prompt <- "Translate to English: Забота об окружающей среде порой сложна и требует внимания."
query_response <- query_model(prompt, api_key)
# 10. Query the fine-tuned model if it succeeded
if (status == "succeeded") {
  latest_model_id <- get_latest_model(user_owned_models)
  cat("Querying the fine-tuned model...\n")
  prompt <- "Translate to English: Забота об окружающей среде порой сложна и требует внимания."
  query_response <- query_model(model_name=latest_model_id, prompt, api_key)
  cat("Query response:", query_response, "\n")
} else {
  cat("Fine-tuning job did not succeed. Skipping query.\n")
}


# 9 Encode
text <- brio::read_lines("tests/testthat/model.qmd") |> paste(collapse = "\n")
result <- encode(text, language = "EN")
text_encoded <- result$text
index <- result$index


# 11 Decode
text_decoded <- decode(text=text_encoded,index=index)
text_decoded |> cat()

