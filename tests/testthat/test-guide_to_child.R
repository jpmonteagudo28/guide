test_that("guide_to_child works as expected", {

  # Define the mock directory structure
  dir_structure <- list(
    "level1" = list(
      "level2" = list(
        "level3" = list(
          "file.txt" = TRUE
        )
      )
    )
  )

  # Create the mock directory
  mock_dir <- create_mock_dir(dir_structure)

  # Set the working directory to level1
  setwd(mock_dir$here("level1"))

  # Test that guide_to_child successfully changes to an existing directory
  guide_to_child("level2")
  expect_equal(basename(getwd()), "level2")

  # Go back to level1
  guide_to_parent(levels_up = 1)

  # Test that guide_to_child creates a directory when `create = TRUE`
  guide_to_child("new_child", create = TRUE)
  expect_equal(basename(getwd()), "new_child")
  expect_true(dir.exists(file.path(mock_dir$path, "level1", "new_child")))

  # Go back to level1
  guide_to_parent(levels_up = 1)

  # Test that guide_to_child throws an error when directory doesn't exist and `create = FALSE`
  expect_error(guide_to_child("nonexistent_child", create = FALSE),
               "The specified directory does not exist: .*")

  # Go back to mock project root
  guide_to_parent(levels_up = 1)
})
