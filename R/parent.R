#'
#' @title
#' Guide to Move Working Directory to Parent Directory
#' @description
#' This function moves the working directory up a specified number of levels in the directory hierarchy.
#'
#'
#' @param current_dir A character string specifying the current directory name or path. Defaults to `getwd()`.
#' @param levels_up An integer specifying how many levels up to move in the directory tree. Defaults to 1.
#'
#' @details
#' The function allows for traversing up the directory tree by the specified number of levels.
#' It can handle both absolute and relative directory paths and throws an error if the current directory is invalid.
#' If the directory provided in `current_dir` is invalid or unreachable, the function throws an error.
#' If the user reaches the root directory,a warning is issued.
#'
#' @return The new working directory (invisibly) after moving up the directory tree.
#'
#' @examples
#' \dontrun{
#' # Move up one level in the directory tree
#' guide::guide_to_parent()
#'
#' # Move up two levels in the directory tree
#' guide::guide_to_parent(levels_up = 2)
#' }
#'
#' @export
guide_to_parent <- function(current_dir = getwd(), levels_up = 1) {

  # If current_dir is  folder name, check if it exists in current working directory
  if (!grepl("^/", current_dir) && !grepl("^[A-Za-z]:", current_dir)) {
    current_wd <- getwd()

    # Check if current_dir same as last part of the current working directory
    if (basename(current_wd) == current_dir) {
      current_dir <- current_wd
    } else {
      potential_dir <- file.path(current_wd, current_dir)

      # Check if this potential path exists, if so, use it
      if (dir.exists(potential_dir)) {
        current_dir <- potential_dir
      } else {
        stop("The specified directory does not exist: ", current_dir)
      }
    }
  }

  # Normalize the path
  cd <- try(normalizePath(current_dir, mustWork = TRUE), silent = TRUE)

  if (inherits(cd, "try-error")) {
    stop("Unable to find or access the directory: ", current_dir)
  }

  # Traverse up the directory tree by the specified number of levels
  for (i in seq_len(levels_up)) {
    parent_dir <- dirname(cd)
    if (parent_dir == cd) {
      warning("Reached the root directory. Cannot go up further.")
      break
    }
    cd <- parent_dir
  }

  # Set the new directory
  setwd(cd)
  message("Working directory set to: ", cd)

  # Return the new working directory
  invisible(cd)
}
