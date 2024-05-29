# Load environment variables from .Renviron file
readRenviron("~/.Renviron")

# Get the API key from the environment
api_key <- Sys.getenv("OPENAI_API_KEY2")

if (api_key == "") {
  stop("API_KEY is not set in the .Renviron file.")
}

glossary <- prepareGlossary()

# Example usage
system <- "You are a professional translator. Translate the following text from Spanish to English, maintaining a technical and formal style."

# Simulate translating multiple chapters
chapters <- list(
  "Capítulo 1: Introducción a la inteligencia artificial. La inteligencia artificial es una rama de la informática...",
  "Capítulo 2: Algoritmos de aprendizaje supervisado. Los algoritmos de aprendizaje supervisado incluyen...",
  "Capítulo 3: Redes neuronales profundas. Las redes neuronales profundas son un tipo de algoritmo de aprendizaje automático..."
)

# Initialize history
history <- list()

# Translate each chapter and update the history
for (chapter in chapters) {
  response <- generalChatCompletion(system, chapter, history, glossary)
  print(response$result)

  # Append the new history to the existing history
  history <- response$history
}
