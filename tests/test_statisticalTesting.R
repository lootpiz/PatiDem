source("../R/statisticalTesting.R")
library(testthat)

context("Checking statisticalTesting function.")

test_that("Invalid arguments/input", {
    tcga = downloadData()

    df = data.frame(column_one = tcga$gender, column_two = tcga$vital_status)
    expect_error(statisticalTesting(df, categorical = FALSE),
        "Please provide continuous variables in column_one.")

    df = data.frame(column_one = tcga$age, column_two = tcga$Redaction)
    expect_error(statisticalTesting(df, categorical = FALSE),
        "Column_two must have at least two levels.")
    expect_error(statisticalTesting(df, categorical = TRUE),
        "Please provide discrete variables or at least two levels.")

    df = data.frame(column_one = tcga$age_at_initial_pathologic_diagnosis,
        column_two = tcga$race)
    zz = statisticalTesting(df, categorical = FALSE)
    expect_equal(attributes(zz)$class, "Continuous variables")

    df = data.frame(column_one = tcga$gender, column_two = tcga$vital_status)
    zz = statisticalTesting(df, categorical = TRUE)
    expect_equal(attributes(zz)$class, "Categorical variables")
    expect_equal(zz$fisher.p.value, 3.65098e-05)
})
