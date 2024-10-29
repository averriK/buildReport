test_that("init_project creates correct structure", {
    # Create a temporary test directory
    test_dir <- file.path(tempdir(), "test_project")
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir)
    
    # Run init_project
    expect_no_error(init_project(test_dir))
    
    # Check required folders exist
    expect_true(dir.exists(file.path(test_dir, "_yml")))
    expect_true(dir.exists(file.path(test_dir, "_extensions")))
    expect_true(dir.exists(file.path(test_dir, "_build")))
    
    # Check required files exist
    expect_true(file.exists(file.path(test_dir, "_yml", "_authors.yml")))
    expect_true(file.exists(file.path(test_dir, "_yml", "_crossref.yml")))
    expect_true(file.exists(file.path(test_dir, "_yml", "_format.yml")))
    expect_true(file.exists(file.path(test_dir, "_yml", "_params.yml")))
    
    # Cleanup
    unlink(test_dir, recursive = TRUE)
})

test_that("init_project handles existing directories", {
    # Create a temporary test directory
    test_dir <- file.path(tempdir(), "test_project")
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir)
    
    # Create existing structure
    dir.create(file.path(test_dir, "_yml"))
    
    # Run init_project with overwrite = FALSE
    expect_no_error(init_project(test_dir, overwrite = FALSE))
    
    # Run init_project with overwrite = TRUE
    expect_no_error(init_project(test_dir, overwrite = TRUE))
    
    # Cleanup
    unlink(test_dir, recursive = TRUE)
}) 