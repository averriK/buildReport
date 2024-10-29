# Helper functions and data for tests
create_test_qmd <- function(dir) {
    writeLines(
        c("---",
          "title: Test Document",
          "---",
          "",
          "## Test Content",
          "",
          "This is a test document."),
        file.path(dir, "index.qmd")
    )
}

create_test_yaml <- function(dir) {
    # Create _yml directory
    yml_dir <- file.path(dir, "_yml")
    dir.create(yml_dir, recursive = TRUE)
    
    # Create _authors.yml
    writeLines(
        c("author:",
          "  - name: Test Author",
          "    email: test@example.com",
          "    orcid: 0000-0000-0000-0000"),
        file.path(yml_dir, "_authors.yml")
    )
    
    # Create _crossref.yml
    writeLines(
        c("EN:",
          "  fig-prefix: Figure",
          "  tbl-prefix: Table",
          "ES:",
          "  fig-prefix: Figura",
          "  tbl-prefix: Tabla"),
        file.path(yml_dir, "_crossref.yml")
    )
    
    # Create _format.yml
    writeLines(
        c("format:",
          "  html:",
          "    toc: true",
          "  docx:",
          "    toc: true",
          "  els-pdf:",
          "    toc: true"),
        file.path(yml_dir, "_format.yml")
    )
    
    # Create _params.yml
    writeLines(
        c("params:",
          "  background: white",
          "  render: none",
          "  ext: png"),
        file.path(yml_dir, "_params.yml")
    )
} 