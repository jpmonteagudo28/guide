test_that("guide_to_parent works as expected with mock directory", {

  # Define mock directory structure
  mock_structure <- list(
    "level1" = list(
      "level2" = list(
        "level3" = list(
          "file.txt" = TRUE
        )
      )
    )
  )

  # Create the mock directory
  mock_dir <- create_mock_dir(mock_structure)

  # Set working directory to "level3"
  setwd(file.path(mock_dir$path, "level1", "level2", "level3"))

  # Confirm we are in "level3"
  expect_equal(basename(getwd()), "level3")

  # Use guide_to_parent to go up 2 levels
  guide_to_parent(levels_up = 2)

  # Confirm we are now in "level1"
  expect_equal(basename(getwd()), "level1")

  # Go up one more level to the project root
  guide_to_parent(levels_up = 1)

  # Confirm we are now in the mock project root
  expect_equal(basename(getwd()), "mock_project")

  # Test for warning when trying to go up more levels than available
  expect_warning(guide_to_parent(levels_up = 10),
               "Reached the root directory. Cannot go up further.")

  # Test for error with invalid directory input
  expect_error(guide_to_parent("invalid_dir"),
               "The specified directory does not exist: invalid_dir")
})
