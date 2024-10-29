#' Initialize Project Structure
#' 
#' Creates the necessary folder structure and copies required files
#' for a new project.
#' 
#' @param projectFolder Path to the project folder
#' @param overwrite Logical. Should existing files be overwritten?
#' @return Invisible TRUE if successful
#' @export
init_project <- function(projectFolder = ".", overwrite = FALSE) {
    # Get package resources path
    extdata_path <- system.file("extdata", package = "yourpackage")
    
    # Create and copy _yml folder
    yml_source <- file.path(extdata_path, "_yml")
    yml_destination <- file.path(projectFolder, "_yml")
    if (!dir.exists(yml_destination)) {
        dir.create(yml_destination, recursive = TRUE)
    }
    file.copy(
        from = list.files(yml_source, full.names = TRUE),
        to = yml_destination,
        recursive = TRUE,
        overwrite = overwrite
    )
    
    # Create and copy _extensions folder
    extensions_source <- file.path(extdata_path, "_extensions")
    extensions_destination <- file.path(projectFolder, "_extensions")
    if (!dir.exists(extensions_destination)) {
        dir.create(extensions_destination, recursive = TRUE)
    }
    file.copy(
        from = list.files(extensions_source, full.names = TRUE),
        to = extensions_destination,
        recursive = TRUE,
        overwrite = overwrite
    )
    
    # Create _build folder
    build_path <- file.path(projectFolder, "_build")
    if (!dir.exists(build_path)) {
        dir.create(build_path, recursive = TRUE)
    }
    
    message("Project structure initialized successfully!")
    invisible(TRUE)
} 