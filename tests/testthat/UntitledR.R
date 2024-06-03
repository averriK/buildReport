library(data.table)
library(httr2)
library(epoxy)

# Sample data
DATA <- data.table(
  ID = c("Chapter 1", "Chapter 2", "Chapter 3", "Chapter 4", "Chapter 5"),
  VALUE = c(
    "Texto en español del capitulo 1", 
    "Texto arbitrario capitulo 2", 
    "Texto arbitrario capitulo 3",
    "Texto arbitrario capitulo 4",
    "Texto arbitrario capitulo 5"
  )
)

SYSTEM <- data.table(
  ID = c("translate", "review"),
  VALUE = c(
    "Translate the following text to {LANGUAGE} and ensure that only the translated text is returned, without comments:\n\n",
    "Review and fix the style and grammar of the following text in {LANGUAGE} according to the given detailed instructions, and ensure that only the reviewed text is returned, without comments:\n\n"
  )
)

API_KEY <- "your_api_key_here"  # Replace with your actual API key

# Function to send request to GPT model
query_model <- function(prompt, api_key, model_name = "gpt-4", max_tokens = NULL, temperature = 0, debug = FALSE) {
  url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = model_name,
    messages = list(list(role = "user", content = prompt)),
    max_tokens = max_tokens,
    temperature = temperature
  )
  
  response <- httr2::request(url) |> 
    httr2::req_method("POST") |> 
    httr2::req_auth_bearer_token(api_key) |> 
    httr2::req_body_json(body) |> 
    httr2::req_perform()
  
  content <- httr2::resp_body_json(response)
  return(content$choices[[1]]$message$content)
}

# Workflow function to process each chapter
process_chapter <- function(chapter_id, language, instructions_id, api_key) {
  # Extract chapter text
  chapter_text <- DATA[ID == chapter_id]$VALUE
  
  # Define the detailed translation prompt
  detailed_prompt <- "
  You are a highly proficient translator specialized in technical and formal documents. Your task is to translate the following text into {LANGUAGE}. Please adhere to the following detailed instructions:

  1. Translation Task:
     - Translate the provided text into {LANGUAGE}, ensuring that the translation is accurate, retains the original meaning, and uses formal and appropriate language.
     - If the text is already in {LANGUAGE}, return the text unchanged as the response.

  2. Formatting:
     - Headings: Maintain the heading structure as indicated by hash symbols (e.g., #, ##, ###) and translate the text within the headings.
     - Font Style: Preserve the font style markers for italic, bold, and bold italic text (e.g., asterisks or underscores for italic, double asterisks for bold) and translate the text within these markers.
     - Blockquotes: Retain the blockquote format as indicated by the greater-than symbol and translate the text inside.
     - Bulleted Lists: Maintain the bullet points as indicated by dash, asterisk, or plus symbols and translate the text within each bullet.
     - Numbered Lists: Keep the numbering intact and translate the text in the list items.
     - Code Blocks: Preserve the code formatting indicated by backticks for inline code and triple backticks for code blocks. Do not translate the code content. Only translate comments or text around the code if necessary.
     - Links: Translate the link text as indicated by square brackets but keep the URL unchanged.
     - Images: Translate the alt text as indicated by exclamation mark and square brackets but keep the URL unchanged.
     - Horizontal Rules: Retain the horizontal rule markers as indicated by three or more dashes, asterisks, or underscores as they are.
     - Tables: Translate the table headers and cell contents while preserving the table structure as indicated by pipes and dashes.
     - Footnotes: Translate the footnote text while keeping the reference format indicated by square brackets and caret unchanged.
     - Inline HTML: Translate the text within HTML tags but keep the HTML structure unchanged.
     - Strikethrough: Preserve the strikethrough formatting indicated by double tilde and translate the text within.

  3. Special Instructions:
     - Verify if the provided text is already in {LANGUAGE}. If so, return the text as it is without any changes.
     - Ensure the translation is clear, concise, and free of grammatical errors.

  4. Response Format:
     - Your response should only contain the translated text or the unchanged text if it is already in {LANGUAGE}. Do not include any comments or explanations in your response.
     - Ensure that the response is formatted neatly and is easy to read.

  The text to be translated is provided below:

  {TEXT}

  Please proceed with the translation, ensuring all the above instructions are followed.
  "
  
  # Use epoxy to glue the values into the prompt
  prompt <- epoxy::epoxy(detailed_prompt, LANGUAGE = language, TEXT = chapter_text)
  
  # Send prompt to GPT
  response_text <- query_model(prompt, api_key)
  
  return(response_text)
}

# Example usage
chapter_id <- "Chapter 1"
language <- "French"
instructions_id <- "translate"
translated_text <- process_chapter(chapter_id, language, instructions_id, API_KEY)

print(translated_text)

# If you want to review the translated text
instructions_id <- "review"
reviewed_text <- process_chapter(chapter_id, language, instructions_id, API_KEY)

print(reviewed_text)
