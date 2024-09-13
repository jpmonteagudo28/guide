test_that("save_as works with different formats", {

  # Create a temporary directory for testing
  temp_dir <- withr::local_tempdir()

  # Mock object to save
  mock_df <- data.frame(
    ID = 1:3,
    Name = c("Alice", "Bob", NA),
    Value = c(4.5, 2.3, NA),
    stringsAsFactors = FALSE
  )

  # Test for .txt file
  txt_file <- file.path(temp_dir, "data.txt")
  save_as(mock_df, txt_file)
  expect_true(file.exists(txt_file))
  expect_true(grepl("Alice", readLines(txt_file)[2]))

  # Test for .csv file
  csv_file <- file.path(temp_dir, "data.csv")
  save_as(mock_df, csv_file, sep = ",")
  expect_true(file.exists(csv_file))
  expect_equal(read.csv(csv_file)$Name[1], "Alice")

  # Test for .rds file
  rds_file <- file.path(temp_dir, "data.rds")
  save_as(mock_df, rds_file)
  expect_true(file.exists(rds_file))
  expect_equal(readRDS(rds_file)$Name[2], "Bob")

  # Test for .xlsx file (skip if package not installed)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    xlsx_file <- file.path(temp_dir, "data.xlsx")
    save_as(mock_df, xlsx_file)
    expect_true(file.exists(xlsx_file))
    wb <- openxlsx::loadWorkbook(xlsx_file)
    expect_equal(openxlsx::read.xlsx(wb)$Name[2], "Bob")
  }

  # Test for .json file (skip if package not installed)
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json_file <- file.path(temp_dir, "data.json")
    save_as(mock_df, json_file)
    expect_true(file.exists(json_file))
    json_content <- jsonlite::fromJSON(json_file)
    expect_equal(json_content$Name[1], "Alice")
  }

  # Test for .zip file
  zip_file <- file.path(temp_dir,"data.zip")
  save_as(mock_df, zip_file)
  expect_true(file.exists(zip_file))

  # Test for .sav (SPSS) format
  sav_file <- file.path(temp_dir, "data.sav")
  save_as(mock_df, sav_file)
  expect_true(file.exists(sav_file))
  actual_df <- haven::read_sav(sav_file)

  # Check that the actual object is a data frame
  expect_s3_class(actual_df, "tbl_df")
  expect_s3_class(actual_df, "tbl")
  expect_s3_class(actual_df, "data.frame")


  # Test for .sas7bdat (SAS) format
  sas_file <- file.path(temp_dir, "data.sas7bdat")
  save_as(mock_df, sas_file)
  expect_true(file.exists(sas_file))
  actual_df <- haven::read_xpt(sas_file)

  # Check that the actual object is a data frame and has the expected classes
  expect_s3_class(actual_df, "tbl_df")
  expect_s3_class(actual_df, "tbl")
  expect_s3_class(actual_df, "data.frame")

  # Test for .dta (Stata) format
  dta_file <- file.path(temp_dir, "data.dta")
  save_as(mock_df, dta_file)
  expect_true(file.exists(dta_file))
  actual_df <- haven::read_dta(dta_file)

  # Check that the actual object is a data frame and has the expected classes
  expect_s3_class(actual_df, "tbl_df")
  expect_s3_class(actual_df, "tbl")
  expect_s3_class(actual_df, "data.frame")

  # Test for unsupported file format
  unsupported_file <- tempfile(fileext = ".unsupported")
  expect_error(save_as(mock_df, unsupported_file), "Unsupported file format")

  # Clean up
  unlink(c(txt_file, csv_file, rds_file,
           xlsx_file, json_file, zip_file,
           sav_file,sas_file,dta_file,
           unsupported_file), force = TRUE)
})
