#' @title
#' Guide to Move Working Directory to Child Directory
#'
#' @description
#' This function moves the working directory to a specified child directory. If the directory does not exist,
#' the user has the option to create it.
#'
#' @param child_dir A character string specifying the name of the child directory to move into.
#' @param create A logical value indicating whether to create the child directory if it does not exist. Defaults to `FALSE`.
#'
#' @details
#' The function attempts to move the working directory to the specified child directory. If the directory does not exist
#' and `create` is set to `TRUE`, the directory will be created. If `create` is set to `FALSE`, an error will be thrown
#' if the directory does not exist.
#'
#' @return The new working directory (invisibly) after moving into the child directory.
#'
#' @examples
#' \dontrun{
#' # Move to a child directory named "project"
#' guides::guide_to_child("project")
#'
#' # Move to a child directory and create it if it doesn't exist
#' guides::guide_to_child("project", create = TRUE)
#' }
#'
#' @export
guide_to_child <- function(child_dir, create = FALSE){

  current_wd <- getwd()

  new_dir <- file.path(current_wd, child_dir)

  # Check if the directory exists
  if (dir.exists(new_dir)) {
    # If it exists, set it as the working directory
    setwd(new_dir)
    message("Working directory set to: ", new_dir)
  } else if (create) {
    # If it doesn't exist and create is TRUE, create the directory
    dir.create(new_dir, recursive = TRUE)
    setwd(new_dir)
    message("Created and set working directory to: ", new_dir)
  } else {
    # If it doesn't exist and create is FALSE, throw an error
    stop("The specified directory does not exist: ", new_dir,
         "\nUse create = TRUE to create the directory if it doesn't exist.")
  }

  # Return the new working directory invisibly
  invisible(new_dir)
}
