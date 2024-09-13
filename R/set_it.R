#' @title
#' Guide to File Location in Specific Folders
#'
#' @description
#' This function locates a file within a specified folder in the current project directory or its subdirectories.
#' It can search across multiple folders and prompt the user to select a folder if multiple folders with the specified name are found.
#'
#' @param file A character string specifying the name of the file to be located.
#' @param ... Additional arguments to specify options such as the folder name. The default folder is `"data"`.
#'
#' @details
#' This function searches for a specified file within a folder and its sub-directories. If multiple folders
#' matching the specified name exist, the user is prompted to select one. If no folder is found or if the file does not exist
#' in the chosen folder, an error is returned.
#'
#' @return A character string representing the file path if the file is found. An error is thrown if the file is not found.
#'
#' @examples
#' \dontrun{
#' # Locate a file named "data.csv" in the "data" folder
#' guides::guide_to_file("data.csv", folder = "data")
#'
#' # Locate a file named "results.txt" in the default "data" folder
#' guides::guide_to_file("results.txt")
#' }
#'
#' @export
guide_to_file <- function(file,...){

  stopifnot(is.character(file))

  args <- list(...)
  folder <- if ("folder" %in% names(args)) args$folder else "data"

  root_dir <- getwd()

  renv_exists <- dir.exists(file.path(root_dir,"renv"))
  dirs <- list.dirs(root_dir, full.names = TRUE, recursive = TRUE)

  if(renv_exists){
    dirs <- dirs[!grepl("/renv", dirs)]
  }

  target_dirs <- grep(paste0("/", folder, "$"), dirs, value = TRUE)

  if(length(target_dirs) == 0){
    stop(paste0("'", folder, "' folder not found in the current directory or subdirectories"))
  }

  if (length(target_dirs) > 1) {

    # Automatically choose the first folder and notify the user
    target_dir <- target_dirs[1]

    message("Multiple '", folder,
            "' folders found. Automatically selecting the first one: ",
            target_dir)
  } else {
    target_dir <- target_dirs
  }

  file_path <- file.path(target_dir,file)

  if(!file.exists(file_path)){
    stop(paste("File not found in the '", folder, "' folder", sep = ""))
  }

  return(file_path)
}
