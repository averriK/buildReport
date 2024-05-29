encodeQMD <- function(language="ES",projectFolder=file.path("."),markupFolder=file.path(".markup"),markup_filename="markup.Rds"){
  # Codifica los archivos de sourceFolder y los guarda en markupFolder/LANG
  LANG <- language
  # ------------------------------------------------------

  IPATH <- file.path(projectFolder)
  OPATH <- file.path(markupFolder,LANG )
  if(!dir.exists(OPATH)) dir.create(OPATH,recursive = TRUE)

  DIRS <- list.dirs(IPATH, full.names = TRUE, recursive = TRUE)
  DIRS <- grep("^_", basename(DIRS), value = TRUE, invert = TRUE)

  FILES <- sapply(DIRS, function(dir) {
    list.files(dir, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)
  }, USE.NAMES = FALSE) |> unlist()

  INDEX <- NULL
  for(FILE in FILES){
    SOURCE <- brio::read_lines(FILE) |> paste(collapse = "\n")

    if (length(SOURCE) == 0 || nzchar(tail(SOURCE, 1))) {
      # Agrega una nueva línea vacía al contenido
      SOURCE <- c(SOURCE, "")}
    sprintf("Fix headings in %s...",FILE) |> cat(fill=TRUE)
    SOURCE <- gsub(pattern =  "^(#+)([^\\s])",replacement =  "\\1 \\2", SOURCE)

    sprintf("Markup source: %s...",FILE) |> cat(fill=TRUE)
    # ------------------------------------------------------
    TYPE <- "captions"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( SOURCE,type=TYPE)
    INDEX <-  rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "includes"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "code"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "math"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))

    # ------------------------------------------------------
    TYPE <- "inline_r"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "references"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "citations"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "figures"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "equations"

    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TEXT <- TAG$text
    brio::write_lines(TEXT,path=file.path(OPATH,basename(FILE)))

  }
  saveRDS(INDEX,file.path(OPATH,markup_filename))
  return(TEXT)
}
