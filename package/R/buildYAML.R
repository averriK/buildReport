buildYAML <- function(language="ES",projectFolder=file.path("."),format=c("html","docx","els-pdf"),output_dir=file.path("_publish")){

  if(!dir.exists(output_dir)) dir.create(output_dir)

  LANG <- language

  # ---------------------------------------------------------------------------
  # Language-independent stage
  DATA <- list()

  FIELD <- list(project=list(
    type="default",
    'output-dir'=output_dir
  ),engine="knitr",jupyter="python3")
  DATA <- c(DATA,FIELD)

  FILE <- list.files(file.path(buildFolder),pattern = "_params\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) ## |> paste(collapse = "\n")
    DATA <- c(DATA,FIELD)
  }
  FILE <- list.files(file.path(projectFolder) ,pattern = "references\\.bib$",recursive = TRUE,full.names = FALSE)
  if(length(FILE)==1){
    VAR <- FILE
    FIELD <- list("bibliography"=VAR)
    DATA <- c(DATA,FIELD)
  }

  FILE <- list.files(file.path(buildFolder) ,pattern = "styles\\.docx$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- FILE
    FIELD <- list("reference-doc"=VAR)
    DATA <- c(DATA,FIELD)
  }

  FILE <- list.files(file.path(projectFolder) ,pattern = "_authors\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) # #|> paste(collapse = "\n")
    FIELD$author <- Filter(.validAuthor, FIELD$author)
    DATA <- c(DATA,FIELD)
  }

  FILE <- list.files(file.path(buildFolder),pattern = "_format\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE)
    FIELD$format <- FIELD$format[names(FIELD$format) %in% format]
    if("els-pdf" %in% names(FIELD$format) && is.null(FIELD$format$`els-pdf`$journal$name)){
      FIELD$format$`els-pdf`$journal$name <- gsub(DATA$subtitle, pattern="\n",replacement="")
    }
    DATA <- c(DATA,FIELD)
  }


  # ------------------------------------------------------
  # Language-dependent stage

  stopifnot(dir.exists(file.path(renderFolder,LANG) ))
  LDATA <- list()

  FILE <- list.files(file.path(buildFolder) ,pattern = "_crossref\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) ## |> paste(collapse = "\n")
    LDATA <- c(LDATA,FIELD[[LANG]])
  }

  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_TITLE\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(title=VAR)
    LDATA <- c(LDATA,FIELD)
  }

  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_SUBTITLE\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(subtitle=VAR)
    LDATA <- c(LDATA,FIELD)
  }

  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_ABSTRACT\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(abstract=VAR)
    LDATA <- c(LDATA,FIELD)
  }

  # --------------------------------------------------------------------------
  FIELD <- list(params=list(
    background= "white",
    render="none",
    ext="png",
    lang=LANG
  ))
  LDATA <- c(LDATA,FIELD)

  # ---------------------------------------------------------------------------
  YAML <-as.yaml(c(DATA,LDATA))
  TEXT <- YAML |>  gsub(pattern=":\\s*yes($|\\n)", replacement=": true\\1") |> gsub(pattern=":\\s*no($|\\n)", replacement=": false\\1")
  FILE <- file.path(".",quarto_filename)
  brio::write_lines(text=TEXT, path=FILE)

}
