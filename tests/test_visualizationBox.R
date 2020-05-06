source("../R/visualizationBox.R")
library(testthat)

context("Checking visualizationBox function.")

test_that("Invalid/exclusive arguments", {
    tcga <- downloadData()

    cancer_type = c("BRCA"); patients = c("TCGA-D3-A1Q5");
    phenotype = "age_at_initial_pathologic_diagnosis"
    metadata_col = "race"; metadata_levels = c("WHITE", "ASIAN")

    expect_error(visualizationBox(tcga, cancer_type = cancer_type, patients = patients,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Please provide either cancer_type or patients argument.")

    phenotype = "vital_status"
    expect_error(visualizationBox(tcga, cancer_type = cancer_type,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Invalid phenotype to compare. Please provide a column name with continuous variables.")

    cancer_type = c("error"); phenotype = "age_at_initial_pathologic_diagnosis"
    expect_error(visualizationBox(tcga, cancer_type = cancer_type,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Invalid cancer_type. Please provide among: *")

    patients = c("error")
    expect_error(visualizationBox(tcga, patients = patients,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Invalid patients barcode. Please check your patients argument: *")

    cancer_type = c("BRCA"); metadata_col = "error"
    expect_error(visualizationBox(tcga, cancer_type = cancer_type,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Invalid column name or too many column names.")

    metadata_col = "race"; metadata_levels = c("error1", "error2")
    expect_error(visualizationBox(tcga, cancer_type = cancer_type,
        phenotype = phenotype, metadata_col = metadata_col, metadata_levels = metadata_levels),
        "Invalid levels *")
})
