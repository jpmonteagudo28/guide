test_that("guide_to_file finds file in the specified folder", {
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "data", "test_file.txt")

  # Create a temp 'data' folder and a file
  dir.create(file.path(temp_dir, "data"))
  writeLines("test content", temp_file)

  # Test if function correctly finds the file
  result <- guide_to_file("test_file.txt", folder = "data")
  expect_true(file.exists(result))

  unlink(temp_dir, recursive = TRUE)
})

test_that("guide_to_file stops if file is not found", {
  expect_error(guide_to_file("nonexistent_file.txt", folder = "data"),
               "File not found")
})

test_that("guide_to_file prompts for folder selection when multiple folders are found", {
  temp_dir <- tempdir()

  # Create multiple 'data' folders
  dir.create(file.path(temp_dir, "data_1"))
  dir.create(file.path(temp_dir, "data_2"))
  dir.create(file.path(temp_dir, "data_3"))

  expect_error(guide_to_file("test_file.txt", folder = "data"), "folder not found")

  unlink(temp_dir, recursive = TRUE)
})
