guide_to_file <- function(file,...){

  stopifnot(is.character(file))

  args <- list(...)
  folder <- if ("folder" %in% names(args)) args$folder else "data"

  root_dir <- here::here()

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
    cat("Multiple '", folder, "' folders found:\n", sep = "")
    for (i in seq_along(target_dirs)) {
      cat(i, ": ", target_dirs[i], "\n", sep = "")
    }
    choice <- as.integer(readline(prompt = paste("Enter the number of the '", folder,
                                                 "' folder you want to use: ", sep = "")))

    if (is.na(choice) || choice < 1 || choice > length(target_dirs)) {
      stop("Invalid selection")
    }
    target_dir <- target_dirs[choice]
  } else {
    target_dir <- target_dirs[1]
  }

  file_path <- here::here(target_dir,file)

  if(!file.exists(file_path)){
    stop(paste("File not found in the '", folder, "' folder", sep = ""))
  }

  return(file_path)
}


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

#> Create function to set working directory to a child directory
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
