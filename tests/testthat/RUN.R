data <- data.frame(
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
# library(FineTuneAPI) # Uncomment this line if you have installed the package

# Define API key and file ID for testing
api_key <- Sys.getenv("OPENAI_API_KEY2")
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

latest_model_id <- get_latest_model(user_owned_models)

# 4. Query the fine-tuned model if it succeeded
if (status == "succeeded") {
  cat("Querying the fine-tuned model...\n")
  prompt <- "Translate to English: Забота об окружающей среде порой сложна и требует внимания."
  model_name <- latest_model_id
  query_response <- query_model(model_name, prompt, api_key)
  cat("Query response:", query_response, "\n")
} else {
  cat("Fine-tuning job did not succeed. Skipping query.\n")
}

# 5. Delete fine-tuned models (except the latest one if no specific model IDs are provided)
cat("Deleting fine-tuned models...\n")
delete_fine_tune_model(model_ids = NULL, api_key)

# 6. Verify the deletion by listing models again
cat("Verifying deletion of models...\n")
user_owned_models_after_deletion <- list_fine_tuned_models(api_key)
print(user_owned_models_after_deletion)
