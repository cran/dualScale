test_that("Transform data if NA values", {
  input <- readRDS(testthat::test_path("fixtures", "singaporean_na.rds"))
  check <- ds_mc_check(input)
  out <- ds_mc(input) %>%
    suppressWarnings() %>%
    suppressMessages()
  expect_equal(out$orig_data, check$t_data)
})

test_that("Solutions is 6", {
  expect_equal(load_test_ds_mc()$solutions, 6)
})

test_that("Result table is correct", {
  ds_mc_out <- readRDS(testthat::test_path("fixtures", "ds_mc_out.rds"))
  expect_identical(round(load_test_ds_mc()$out,2), ds_mc_out)
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
