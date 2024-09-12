#-------------------------------------------#
# Author: JP Monteagudo
# Liberty University, Dept. of Public Health
#-------------------------------------------#
#' @title
#' Create a Mock Directory Structure
#'
#' @description
#' This function creates a temporary directory structure based on the provided
#' directory structure. It sets up directories and files as specified and
#' provides a function to mimic `here::here()` behavior for accessing files
#' within the mock directory. The temporary directory is automatically cleaned up
#' after use.
#'
#' @param dir_structure A named list defining the directory and file structure.
#'   Directories are represented by lists, while files are represented by `TRUE`.
#' @param .env The environment in which to schedule the cleanup. Default is
#'   `parent.frame()`.
#'
#' @return A list containing:
#'   \item{path}{The path to the created mock directory.}
#'   \item{here}{A function that mimics `here::here()` to access files within
#'     the mock directory.}
#'
#' @details
#' The function creates a temporary directory at a location determined by
#' `tempdir()`, sets up the specified directory and file structure, and returns
#' a list containing the path to the mock directory and a function to access
#' files within it. The working directory is temporarily changed to the mock
#' directory during the operation, and the original working directory is restored
#' after cleanup.
#'
#' @keywords internal
#'
create_mock_dir <- function(dir_structure, .env = parent.frame()) {
  testthat::skip_if_not_installed("withr")

  # Create a temporary directory
  temp_dir <- tempdir()
  project_path <- file.path(temp_dir, "mock_project")
  dir.create(project_path, recursive = TRUE, showWarnings = FALSE)

  # Recursively create directories and files as specified in `dir_structure`
  create_dir_structure <- function(base_path, content) {
    for(item in names(content)) {
      current_path <- file.path(base_path, item)
      if(is.list(content[[item]])) {
        # If it's a list, it's a directory
        dir.create(current_path, recursive = TRUE, showWarnings = FALSE)
        # Recursively create sub-directories/files
        create_dir_structure(current_path, content[[item]])
      } else {
        # Otherwise, create a file
        file.create(current_path)
        # Write some content to the file to ensure it's created
        writeLines("This is a mock file.", con = current_path)
      }
    }
  }

  tryCatch({
    create_dir_structure(project_path, dir_structure)
  }, error = function(e) {
    stop("Failed to create mock directory structure: ", e$message)
  })

  # Set the working directory to the project path
  old_wd <- getwd()
  setwd(project_path)

  # Create a function to mimic here::here() behavior
  mock_here <- function(...) {
    file.path(project_path, ...)
  }

  # Schedule cleanup
  withr::defer({
    setwd(old_wd)
    unlink(project_path, recursive = TRUE)
  }, envir = .env)

  # Print debug information
  cat("Created mock directory at:", project_path, "\n")
  cat("Contents of mock directory:\n")
  print(list.files(project_path, recursive = TRUE))

  # Return a list with the project path and the mock_here function
  list(
    path = project_path,
    here = mock_here
  )
}
