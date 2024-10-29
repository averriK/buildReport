test_that("buildReport works with initialized project", {
    # Create a temporary test directory
    test_dir <- file.path(tempdir(), "test_project")
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir)
    
    # Initialize project
    init_project(test_dir)
    
    # Create test files
    create_test_qmd(test_dir)
    create_test_yaml(test_dir)
    
    # Run buildReport
    expect_no_error(
        buildReport(
            projectFolder = test_dir,
            language = "EN",
            output_format = c("html", "docx")
        )
    )
    
    # Check output files exist
    expect_true(file.exists(file.path(test_dir, "_quarto.yml")))
    
    # Cleanup
    unlink(test_dir, recursive = TRUE)
})

test_that("buildReport fails without initialization", {
    # Create a temporary test directory
    test_dir <- file.path(tempdir(), "test_project")
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir)
    
    # Create test qmd without initialization
    create_test_qmd(test_dir)
    
    # Run buildReport without initialization
    expect_error(
        buildReport(
            projectFolder = test_dir,
            language = "EN",
            output_format = c("html", "docx")
        ),
        "Project structure not initialized"
    )
    
    # Cleanup
    unlink(test_dir, recursive = TRUE)
})

test_that("buildReport handles different languages", {
    # Create a temporary test directory
    test_dir <- file.path(tempdir(), "test_project")
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir)
    
    # Initialize project
    init_project(test_dir)
    
    # Create test files
    create_test_qmd(test_dir)
    create_test_yaml(test_dir)
    
    # Test Spanish
    expect_no_error(
        buildReport(
            projectFolder = test_dir,
            language = "ES",
            output_format = c("html")
        )
    )
    
    # Test English
    expect_no_error(
        buildReport(
            projectFolder = test_dir,
            language = "EN",
            output_format = c("html")
        )
    )
    
    # Cleanup
    unlink(test_dir, recursive = TRUE)
})