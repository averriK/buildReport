qmdTranslate <- function(model="gpt4o",source_language="ES",target_language="EN"){}

.qmdTranslate.deepl <- function(source_language,target_language,auth_key=Sys.getenv("DEEPL_AUTH_KEY")){
  # Traduce los archivos de markupFolder/SOURCE_LANG y los guarda en markupFolder/TARGET_LANG
  SOURCE_LANG <- source_language
  TARGET_LANG <- target_language
  AUTH_KEY <- auth_key
  # ------------------------------------------------------
  IPATH <- file.path(markupFolder,SOURCE_LANG)
  stopifnot(dir.exists(IPATH))

  # if(!dir.exists(IPATH)){
  #   .encode(language =source_language)
  # }

  OPATH <- file.path(markupFolder,TARGET_LANG )
  if(!dir.exists(OPATH)) {
    dir.create(OPATH,recursive = TRUE)
  }

  CONTEXT <- "Multivariate linear regression models, earthquake engineering, seismic ground motions, dynamic site response, seismic design, mining infrastructure, ground-motion intensity measures, peak-ground accelerations(PGA), peak-ground velocities (PGV), Arias Intensity (AI)."

  FILES <- list.files(IPATH,pattern = "\\.qmd$",full.names = FALSE)
  # ----------------------------------------------------------------------------
  # Build CRC for INDEX


  FILE <- file.path(IPATH,markup_filename)
  INDEX <- readRDS(FILE)

  FILE.CRC <- file.path(OPATH,paste0(".",markup_filename,".crc"))
  CRC <- digest::digest(INDEX,algo="sha512")
  OK <- !file.exists(FILE.CRC) || (file.exists(FILE.CRC) && brio::read_lines(FILE.CRC) != CRC)


  if(OK){
    sprintf("Translating captions index to %s...",TARGET_LANG) |> cat(fill=TRUE)
    INDEX[TYPE=="captions",VALUE:=deeplr::translate(
      text = VALUE,
      source_lang = SOURCE_LANG,
      target_lang = TARGET_LANG,
      auth_key = AUTH_KEY,
      split_sentences = TRUE,
      # endpoint="https://api.deepl.com/v2/translate",
      # formality="prefer_more",
      preserve_formatting=TRUE,      #context= CONTEXT
    )]
    saveRDS(INDEX,file.path(OPATH,markup_filename))
    # FILE.CRC <- file.path(OPATH,paste0(".",markup_filename,".crc"))
    # CRC <- digest::digest(INDEX,algo="sha512")
    brio::write_lines(text=CRC,path=FILE.CRC)
  }


  for(FILE in FILES){

    SOURCE <- brio::read_lines(path=file.path(IPATH,FILE))
    EOL <- brio::file_line_endings(file.path(IPATH,FILE))

    CRC <- digest::digest(SOURCE,algo="sha512")
    FILE.CRC <- file.path(OPATH,paste0(".",FILE,".crc"))

    OK <- !file.exists(FILE.CRC) || (file.exists(FILE.CRC) && brio::read_lines(FILE.CRC) != CRC)

    if(OK){
      sprintf("Translating source %s to %s...",FILE,TARGET_LANG) |> cat(fill=TRUE)

      TEXT <- deeplr::translate(
        text = paste(SOURCE,collapse = "\n"),
        source_lang = SOURCE_LANG,
        target_lang = TARGET_LANG,
        auth_key = Sys.getenv("DEEPL_AUTH_KEY"),
        # endpoint="https://api.deepl.com/v2/translate",
        # formality="prefer_more",
        split_sentences = TRUE,
        preserve_formatting=TRUE,        #context= CONTEXT
      )
      brio::write_lines(TEXT,path=file.path(OPATH,FILE), eol=EOL)
      # CRC <- digest::digest(paste(TEXT,collapse = "\n"),algo="sha512")
      # FILE.CRC <- file.path(OPATH,paste0(".",FILE,".crc"))
      brio::write_lines(text=CRC,path=FILE.CRC)

    }



  }
}

.qmdTranslate.openAI_API <- function(source_language,target_language,auth_key=Sys.getenv("OPENAI_AUTH_KEY")){}

.qmdTranslate.azureAI_API <- function(source_language,target_language,auth_key=Sys.getenv("AZUREAI_AUTH_KEY")){}
