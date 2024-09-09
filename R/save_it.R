save_as <- function(object, file , sep = " ", na_strings = TRUE){

  stopifnot(is.character(file))

  if(na_strings){
    na_string <- "NA"
  }

  file_ext <- tools::file_ext(file)

  switch(
    file_ext,
    "txt"  = {
      write.table(object,file, sep = sep, na = na_string,row.names = FALSE,col.names = TRUE)
    },
    "csv" = {
      write.csv(object,file, sep = sep, na = na_string, row.names = FALSE)
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
        stop("'jsonlite' package is required but not installed")
      }
      jsonlite::write_json(object,path = file,na = "null", pretty = TRUE)
    },
    "xml" = {
      if(!requireNamespace("xml2", quietly = TRUE)) {
        stop("'xml2' package is required but not installed")
      }
      if (!is.data.frame(object)) {
        stop("XML format requires a list input")
      }
      object <- as.list(object)
      xml <- xml2::as_xml_document(object)
      xml2::write_xml(xml,file)
    },
    "zip" = {
      temp_file <- tempfile(fileext = paste0(".",tools::file_ext(sub("\\.zip$","",file))))
      write.table(object,temp_file,sep = sep,na.strings = na_string)
      zip::zipr(file,files = temp_file)

      unlink(temp_file)
    },
    stop(paste("Unsupported file format:", file_ext))
  )
  cat("File saved as", file, "\n")
}
