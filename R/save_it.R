#' @title
#' Save Object to File in Various Formats
#'
#' @description
#' This function saves an R object to a file in one of several supported formats.
#' It supports saving in `.txt`, `.csv`, `.rds`, `.xlsx`, `.json`, `.xml`, and `.zip` formats.
#'
#' @param object The R object to be saved. This can be a data frame, list, or other compatible data structures depending on the file format.
#' @param file A character string specifying the name of the file, including the file extension.
#' @param sep A character string specifying the field separator string. Defaults to `" "`. This applies to `.txt` and `.csv` formats.
#' @param na_strings A logical value indicating whether to treat missing values as `"NA"`. Defaults to `TRUE`.
#'
#' @details
#' The function automatically detects the file format based on the file extension and applies the appropriate saving method.
#' Supported formats include:
#'
#' - **.txt**: Saves as a text file.
#' - **.csv**: Saves as a comma-separated values file.
#' - **.rds**: Saves as an RDS file.
#' - **.xlsx**: Saves as an Excel file (requires the `openxlsx` package).
#' - **.json**: Saves as a JSON file (requires the `jsonlite` package).
#' - **.zip**: Compresses and saves as a zip archive, with the object saved as a temporary `.txt` file inside.
#' - **.sav**: Saves as a SPSS .sav file
#' - **.sas7bdat**: Saves as SAS file
#' - **.dta**: Saves as a Stata .dta file
#'
#' If the necessary packages for certain formats are not installed, the function will stop and prompt the user to install the required package.
#'
#' @return None. The function saves the file and prints a confirmation message.
#'
#' @examples
#' \dontrun{
#' # Save a data frame as a CSV file
#' guide::save_as(mtcars, "mtcars.csv")
#'
#' # Save an object as an RDS file
#' guide::save_as(mtcars, "mtcars.rds")
#'
#' # Save a data frame as an Excel file
#' guide::save_as(mtcars, "mtcars.xlsx")
#'
#' # Save a data frame as a JSON file
#' guide::save_as(mtcars, "mtcars.json")
#' }
#'
#' @export

save_as <- function(object, file , sep = " ", na_strings = TRUE){

  stopifnot(is.character(file))

  if(na_strings){
    na_string <- "NA"
  }

  file_ext <- tools::file_ext(file)

  switch(
    file_ext,
    "txt"  = {
      utils::write.table(object,file, sep = sep, na = na_string,row.names = FALSE,col.names = TRUE)
    },
    "csv" = {
      utils::write.table(object,file, sep = sep, na = na_string, row.names = FALSE)
    },
    "rds" = {
      saveRDS(object,file)
    },
    "xlsx" ={
      if(!requireNamespace("openxlsx", quietly = TRUE)){
        stop("'openxlsx' package required but not installed")
      }
      openxlsx::write.xlsx(object,file,na.string = na_string)
    },
    "json" = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("'jsonlite' package required but not installed")
      }
      jsonlite::write_json(object,path = file,na = "null", pretty = TRUE)
    },
    "zip" = {
      temp_file <- tempfile(fileext = paste0(".",tools::file_ext(sub("\\.zip$","",file))))
      utils::write.table(object,temp_file,sep = sep,na = na_string)
      zip::zipr(file,files = temp_file)

      unlink(temp_file)
    },
    "sav" = {
      if(!requireNamespace("haven", quietly = TRUE)) {
        stop("'haven' package required but not installed.")
      }
      haven::write_sav(object, file)
    },
    "sas7bdat" = {
      if(!requireNamespace("haven", quietly = TRUE)) {
        stop("'haven' package required but not installed.")
      }
      haven::write_xpt(object, file)
    },
    "dta" = {
      if(!requireNamespace("haven", quietly = TRUE)) {
        stop("'haven' package required but not installed")
      }
      haven::write_dta(object, file)
    },
    stop(paste("Unsupported file format:", file_ext))
  )
  cat("File saved as", file, "\n")
}
