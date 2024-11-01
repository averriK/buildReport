# nolint start
buildReport <- function(
    projectFolder = ".",
    buildFolder = "_build",
    markupFolder = "_markup",
    publishFolder = "_publish",
    exportFolder = "_export",
    renderFolder = "_render",
    markup_filename = "markup.Rds",
    index_filename = "index.qmd",
    quarto_filename = "_quarto.yml",
    language = "EN",
    output_format = c("html", "docx"),
    deploy = FALSE
) {
    # Check if project is initialized
    if (!dir.exists(file.path(projectFolder, "_yml")) || 
        !dir.exists(file.path(projectFolder, "_extensions"))) {
        stop("Project structure not initialized. Please run init_project() first.")
    }

    # First include .validAuthor helper
    .validAuthor <- function(author) {
        fields_to_check <- c("name", "orcid", "email")
        for (field in fields_to_check) {
            if (!is.null(author[[field]])) {return(TRUE)}
        }
    }

    .buildYAML <- function(language="ES", output_format=c("html","docx","els-pdf"), output_dir=exportFolder) {
        # Get the yml path - this is now the standard location
        yml_path <- file.path(projectFolder, "_yml")
        
        if(!dir.exists(output_dir)) dir.create(output_dir)
        LANG <- language
        
        # Language-independent stage
        DATA <- list()
        
        FIELD <- list(project=list(
            type="default",
            'output-dir'=output_dir
        ), engine="knitr", jupyter="python3")
        DATA <- c(DATA, FIELD)
        
        # Handle references.bib from project folder
        FILE <- list.files(projectFolder, pattern = "references\\.bib$", recursive = TRUE, full.names = TRUE)
        if(length(FILE)==1) {
            FIELD <- list(bibliography=FILE)
            DATA <- c(DATA, FIELD)
        }

        # Handle _authors.yml from _yml folder
        FILE <- file.path(yml_path, "_authors.yml")
        if(!file.exists(FILE)) {
            stop("Required file '_authors.yml' not found in _yml folder")
        }
        FIELD <- read_yaml(FILE, readLines.warn=FALSE)
        FIELD$author <- Filter(.validAuthor, FIELD$author)
        DATA <- c(DATA, FIELD)
        
        # Handle _params.yml
        FILE <- file.path(yml_path, "_params.yml")
        if(file.exists(FILE)) {
            FIELD <- read_yaml(FILE, readLines.warn=FALSE)
            DATA <- c(DATA, FIELD)
        }
        
        # Handle styles.docx
        FILE <- file.path(yml_path, "styles.docx")
        if(file.exists(FILE)) {
            FIELD <- list("reference-doc"=FILE)
            DATA <- c(DATA, FIELD)
        }

        # Handle _format.yml
        FILE <- file.path(yml_path, "_format.yml")
        if(!file.exists(FILE)) {
            stop("Required file '_format.yml' not found in _yml folder")
        }
        FIELD <- read_yaml(FILE, readLines.warn=FALSE)
        FIELD$format <- FIELD$format[names(FIELD$format) %in% output_format]
        if("els-pdf" %in% names(FIELD$format) && is.null(FIELD$format$`els-pdf`$journal$name)) {
            FIELD$format$`els-pdf`$journal$name <- gsub(DATA$subtitle, pattern="\n", replacement="")
        }
        DATA <- c(DATA, FIELD)
        
        # Language-dependent stage
        if(dir.exists(file.path(renderFolder, LANG))) {
            PATH <- file.path(renderFolder, LANG)
        } else {
            PATH <- projectFolder
        }
        LDATA <- list()
        
        # Handle _crossref.yml from _yml folder
        FILE <- file.path(yml_path, "_crossref.yml")
        if(!file.exists(FILE)) {
            stop("Required file '_crossref.yml' not found in _yml folder")
        }
        FIELD <- read_yaml(FILE, readLines.warn=FALSE)
        LDATA <- c(LDATA, FIELD[[LANG]])
        
        # Handle title, subtitle, and abstract
        for(doc_type in c("TITLE", "SUBTITLE", "ABSTRACT")) {
            FILE <- list.files(PATH, pattern = paste0("_", doc_type, "\\.qmd$"), recursive = TRUE, full.names = TRUE)
            if(length(FILE)==1) {
                VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
                FIELD <- list()
                FIELD[[tolower(doc_type)]] <- VAR
                LDATA <- c(LDATA, FIELD)
            }
        }
        
        # Add params
        FIELD <- list(params=list(
            background= "white",
            render="none",
            ext="png",
            lang=LANG
        ))
        LDATA <- c(LDATA, FIELD)
        
        # Write YAML
        YAML <- as.yaml(c(DATA, LDATA))
        TEXT <- YAML |> 
            gsub(pattern=":\\s*yes($|\\n)", replacement=": true\\1") |> 
            gsub(pattern=":\\s*no($|\\n)", replacement=": false\\1")
        FILE <- file.path(projectFolder, quarto_filename)
        brio::write_lines(text=TEXT, path=FILE)
    }

    # Also need to include .chatCompletion for completeness
    .chatCompletion <- function(
        model,
        isAzure=FALSE,
        messages = NULL,
        temperature = 1,
        n = 1,
        stream = FALSE,
        stop = NULL,
        max_tokens = NULL,
        presence_penalty = 0,
        frequency_penalty = 0,
        logit_bias = NULL,
        user = NULL,
        openai_organization = Sys.getenv("OPENAI_ORGANIZATION")
    ) {
        if(isAzure==FALSE) {
            API_KEY <- Sys.getenv("OPENAI_API_KEY")
            BASE_URL <- "https://api.openai.com/v1/chat/completions"
        } else {
            API_KEY <- Sys.getenv("AZURE_API_KEY")
            API_VERSION <- "2023-08-01-preview"
            RESOURCE_ID <- "na-oai-canadaeast"
            BASE_URL <- epoxy::epoxy("https://{RESOURCE_ID}.openai.azure.com/openai/deployments/{model}/chat/completions?api-version={API_VERSION}")
        }
        
        HEADERS <- "application/json"
        if (!is.null(openai_organization)) {
            HEADERS["OpenAI-Organization"] <- openai_organization
        }
        
        body <- list(
            model = model,
            messages = messages,
            temperature = temperature,
            n = n,
            stream = stream,
            stop = stop,
            max_tokens = max_tokens,
            presence_penalty = presence_penalty,
            frequency_penalty = frequency_penalty,
            logit_bias = logit_bias,
            user = user
        )
        
        if(isAzure==TRUE) {
            response <- httr2::request(BASE_URL) |>
                httr2::req_headers("Content-Type" = HEADERS, "api-key" = API_KEY) |>
                httr2::req_user_agent("@averriK") |>
                httr2::req_body_json(body) |> 
                httr2::req_retry(max_tries = 4) |>
                httr2::req_throttle(rate = 15) |>
                httr2::req_perform() 
        } else {
            response <- httr2::request(BASE_URL) |>
                httr2::req_auth_bearer_token(token = API_KEY) |>
                httr2::req_headers("Content-Type" = "application/json") |> 
                httr2::req_user_agent("@averriK") |>
                httr2::req_body_json(body) |> 
                httr2::req_retry(max_tries = 4) |>
                httr2::req_throttle(rate = 40) |>
                httr2::req_perform() 
        }
        
        parsed <- response |> httr2::resp_body_json(simplifyVector = TRUE)
        
        if(resp_is_error(response)) {
            stop(paste0(
                "OpenAI API request failed [",
                httr2::resp_status(response),"] - [",
                httr2::resp_status_desc(response),"] - [",
                httr2::resp_check_status(response),
                "]:\n\n",
                parsed$error$message
            ), call. = FALSE)
        }
        
        return(parsed)
    }

    # Main execution flow
    tryCatch({
        if(exists(".preRender")) .preRender()
        .buildYAML(
            language = language,
            output_format = output_format,
            output_dir = publishFolder
        )
        quarto::quarto_render(input = file.path(projectFolder, index_filename))
        if(exists(".postRender")) .postRender()
        if (deploy) {
            system2("netlify", args = c("deploy --prod"))
        }
    }, error = function(e) {
        message("Error during rendering: ", e$message)
        if(exists(".postRender")) .postRender()
        stop(e)
    })
}
# nolint end
