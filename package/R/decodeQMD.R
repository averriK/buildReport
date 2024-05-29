decodeQMD <- function(language="ES",markupFolder=",.markup",renderFolder=".render",markup_filename="markup.Rds"){
  LANG <- language
  # ------------------------------------------------------
  IPATH <- file.path(markupFolder,LANG)
  stopifnot(dir.exists(IPATH))
  OPATH <- file.path(renderFolder,LANG,"qmd")
  if(!dir.exists(OPATH)) dir.create(OPATH,recursive = TRUE)

  FILES <- list.files(IPATH,pattern = "\\.qmd$",full.names = FALSE)

  INDEX <- readRDS(file.path(IPATH,markup_filename)) |> as.data.table()

  # Fix Paths
  INDEX[TYPE=="includes",VALUE:=gsub(x=VALUE,pattern =  "\\{\\{< include article/qmd/([^/]+\\.qmd) >\\}\\}",replacement =  paste0("{{< include ", OPATH, "/\\1 >}}"))]

  for(FILE in FILES){
    sprintf("Rebuilding source %s to %s...",FILE,LANG) |> cat(fill=TRUE)
    SOURCE <- brio::read_lines(file.path(IPATH,FILE))
    EOL <- brio::file_line_endings(file.path(IPATH,FILE))
    AUX <- SOURCE
    for (i in 1:nrow(INDEX)) {
      AUX <- gsub(INDEX$ID[i], INDEX$VALUE[i], AUX, fixed = TRUE)
    }

    TEXT <- AUX
    sprintf("Rebuilding captions in %s...",LANG) |> cat(fill=TRUE)
    AUX <- TEXT
    for (i in 1:nrow(INDEX)) {
      AUX <- gsub(INDEX$ID[i], INDEX$VALUE[i], AUX, fixed = TRUE)
    }

    TEXT <- AUX
    # Write File

    brio::write_lines(TEXT,path=file.path(OPATH,FILE), eol=EOL)

  }
}
