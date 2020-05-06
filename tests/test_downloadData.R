source("../R/downloadData.R")
library(testthat)

context("Checking downloadData function.")

test_that("Invalid arguments", {
    expect_error(downloadData(fileType = "DOC"), 
        "Unknown fileType. Please provide either EXCEL, CSV, or TXT.")
    expect_error(downloadData(sheetIndex = 0),
        "Invalid index of the sheet. Please provide an index greater than 1.")
})

