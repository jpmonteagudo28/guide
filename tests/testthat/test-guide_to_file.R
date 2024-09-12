test_that("file-related functions work with mock directory", {

  # Define the directory structure
  mock_structure <- list(
    "data" = list(
      "myfile.txt" = TRUE
    ),
    "backup_data" = list(
      "myfile_2.txt" = TRUE
    ),
    "scripts" = list(
      "func" = list(
        "foo.R" = TRUE,
        "bar.R" = TRUE
      ),
      "helper" = list(
        "foofy_bar.R" = TRUE,
        "foo_helper" = list(
          "foofy.R" = TRUE
        )
      )
    )
  )

  # Create the directory and get the mock_here function
  mock_env <- create_mock_dir(mock_structure)
  mock_here <- mock_env$here

  # Now you can use mock_here like this:
  expect_true(file.exists(mock_here("data", "myfile.txt")))
  expect_true(file.exists(mock_here("scripts", "func", "foo.R")))

  # Test your guide_to_file function
  expect_equal(normalizePath(guide_to_file("myfile.txt", folder = "data"), winslash = "/"),
               normalizePath(mock_here("data", "myfile.txt"), winslash = "/"))

  expect_error(guide_to_file("nonexistent.txt", folder = "data"), "File not found in the 'data' folder")
})
