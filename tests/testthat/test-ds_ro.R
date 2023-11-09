test_that("Solutions is 3", {
  ds_ro <- load_test_ds_ro()
  expect_equal(ds_ro$solutions, 3)
})

test_that("Matrix E is correct", {
  ds_ro <- load_test_ds_ro()
  ds_ro_mat_e <- readRDS(testthat::test_path("fixtures", "ds_ro_mat_e.rds"))
  expect_identical(round(ds_ro$mat_e,2), ds_ro_mat_e)
})

test_that("Marginals of Matrix E are zero", {
  expect_equal(rowSums(load_test_ds_ro()$mat_e), integer(10))
})

test_that("Results tables are correct", {
  ds_ro <- load_test_ds_ro()
  ds_ro_out <- readRDS(testthat::test_path("fixtures", "ds_ro_out.rds"))
  ds_ro_out_rank <- readRDS(testthat::test_path("fixtures", "ds_ro_out_rank.rds"))
  expect_identical(round(ds_ro$out,2), ds_ro_out)
  expect_identical(round(ds_ro$out_rank,2), ds_ro_out_rank)
})

test_that("Normed and projected weights are correct", {
  ds_ro <- load_test_ds_ro()
  ds_ro_norm_sub <- readRDS(testthat::test_path("fixtures", "ds_ro_norm_sub.rds"))
  ds_ro_proj_sub <- readRDS(testthat::test_path("fixtures", "ds_ro_proj_sub.rds"))
  ds_ro_norm_opt <- readRDS(testthat::test_path("fixtures", "ds_ro_norm_opt.rds"))
  ds_ro_proj_opt <- readRDS(testthat::test_path("fixtures", "ds_ro_proj_opt.rds"))
  expect_identical(round(ds_ro$norm_sub,2), ds_ro_norm_sub)
  expect_identical(round(ds_ro$proj_sub,2), ds_ro_proj_sub)
  expect_identical(round(ds_ro$norm_opt,2), ds_ro_norm_opt)
  expect_identical(round(ds_ro$proj_opt,2), ds_ro_proj_opt)
})

test_that("Normed and projected ranks weights are correct", {
  ds_ro <- load_test_ds_ro()
  #ds_ro_norm_sub_rank <- readRDS(testthat::test_path("fixtures", "ds_ro_norm_sub_rank.rds"))
  #ds_ro_proj_sub_rank <- readRDS(testthat::test_path("fixtures", "ds_ro_proj_sub_rank.rds"))
  ds_ro_norm_opt_rank <- readRDS(testthat::test_path("fixtures", "ds_ro_norm_opt_rank.rds"))
  ds_ro_proj_opt_rank <- readRDS(testthat::test_path("fixtures", "ds_ro_proj_opt_rank.rds"))
  #expect_identical(ds_ro$norm_sub_rank, ds_ro_norm_sub_rank)
  #expect_identical(ds_ro$proj_sub_rank, ds_ro_proj_sub_rank)
  expect_identical(round(ds_ro$norm_opt_rank,2), ds_ro_norm_opt_rank)
  expect_identical(round(ds_ro$proj_opt_rank,2), ds_ro_proj_opt_rank)
})
