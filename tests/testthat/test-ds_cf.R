test_that("Solutions is 1", {
  expect_equal(load_test_ds_cf()$solutions, 3)
})

test_that("Result table are correct", {
  ds_cf_result_table <- readRDS(testthat::test_path("fixtures", "ds_cf_result_table.rds"))
  expect_identical(round(load_test_ds_cf()$out,2), ds_cf_result_table)
})

test_that("Residual tables are correct", {
  ds_cf <- load_test_ds_cf()
  ds_cf_residual_0 <- readRDS(testthat::test_path("fixtures", "ds_cf_residual_0.rds"))
  ds_cf_residual <- readRDS(testthat::test_path("fixtures", "ds_cf_residual.rds"))
  expect_identical(round(ds_cf$residual0,2), ds_cf_residual_0)
  expect_identical(round(ds_cf$residual[, , 1],2), ds_cf_residual)
})

test_that("Normed and projected weights are correct", {
  ds_cf <- load_test_ds_cf()
  ds_cf_norm_sub <- readRDS(testthat::test_path("fixtures", "ds_cf_norm_sub.rds"))
  ds_cf_proj_sub <- readRDS(testthat::test_path("fixtures", "ds_cf_proj_sub.rds"))
  ds_cf_norm_opt <- readRDS(testthat::test_path("fixtures", "ds_cf_norm_opt.rds"))
  ds_cf_proj_opt <- readRDS(testthat::test_path("fixtures", "ds_cf_proj_opt.rds"))
  expect_identical(round(ds_cf$norm_sub,2), ds_cf_norm_sub)
  expect_identical(round(ds_cf$proj_sub,2), ds_cf_proj_sub)
  expect_identical(round(ds_cf$norm_opt,2), ds_cf_norm_opt)
  expect_identical(round(ds_cf$proj_opt,2), ds_cf_proj_opt)
})
