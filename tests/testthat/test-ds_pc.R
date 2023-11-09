test_that("Solutions is 3", {
  ds_pc <- load_test_ds_pc()
  expect_equal(ds_pc$solutions, 3)
})

test_that("Matrix E is correct", {
  ds_pc <- load_test_ds_pc()
  ds_pc_mat_e <- readRDS(testthat::test_path("fixtures", "ds_pc_mat_e.rds"))
  expect_identical(round(ds_pc$mat_e,2), ds_pc_mat_e)
})

test_that("Marginals of Matrix E are zero", {
  expect_equal(rowSums(load_test_ds_pc()$mat_e), integer(10))
})

test_that("Result table is correct", {
  ds_pc <- load_test_ds_pc()
  ds_pc_out <- readRDS(testthat::test_path("fixtures", "ds_pc_out.rds"))
  expect_identical(round(ds_pc$out,2), ds_pc_out)
})

test_that("Normed and projected weights are correct", {
  ds_pc <- load_test_ds_pc()
  ds_pc_norm_sub <- readRDS(testthat::test_path("fixtures", "ds_pc_norm_sub.rds"))
  ds_pc_proj_sub <- readRDS(testthat::test_path("fixtures", "ds_pc_proj_sub.rds"))
  ds_pc_norm_opt <- readRDS(testthat::test_path("fixtures", "ds_pc_norm_opt.rds"))
  ds_pc_proj_opt <- readRDS(testthat::test_path("fixtures", "ds_pc_proj_opt.rds"))
  expect_identical(round(ds_pc$norm_sub,2), ds_pc_norm_sub)
  expect_identical(round(ds_pc$proj_sub,2), ds_pc_proj_sub)
  expect_identical(round(ds_pc$norm_opt,2), ds_pc_norm_opt)
  expect_identical(round(ds_pc$proj_opt,2), ds_pc_proj_opt)
})
