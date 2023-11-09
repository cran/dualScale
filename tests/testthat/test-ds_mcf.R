test_that("Transform data if NA values", {
  input <- readRDS(testthat::test_path("fixtures", "singaporean_na.rds"))
  check <- ds_mc_check(input, "rad")$t_data
  out <- ds_mcf(input, crit = 1) %>%
    suppressWarnings() %>%
    suppressMessages()
  expect_equal(out$orig_data, check)
})

test_that("Solutions is 2", {
  ds_mcf <- load_test_ds_mcf()
  expect_equal(ds_mcf$solutions_mcf, 2)
})

test_that("Results tables are correct", {
  ds_mcf <- load_test_ds_mcf()
  ds_mcf_out_a <- readRDS(testthat::test_path("fixtures", "ds_mcf_out_a.rds"))
  ds_mcf_out_b <- readRDS(testthat::test_path("fixtures", "ds_mcf_out_b.rds"))
  ds_mcf_out_c <- readRDS(testthat::test_path("fixtures", "ds_mcf_out_c.rds"))
  expect_identical(round(ds_mcf$out_a,2), ds_mcf_out_a)
  expect_identical(round(ds_mcf$out_b,2), ds_mcf_out_b)
  expect_identical(round(ds_mcf$out_c,2), ds_mcf_out_c)
})

test_that("Normed and projected weights are correct", {
  ds_mcf <- load_test_ds_mcf()
  ds_mcf_norm_sub_a <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_sub_a.rds"))
  ds_mcf_proj_sub_a <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_sub_a.rds"))
  ds_mcf_norm_opt_a <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_opt_a.rds"))
  ds_mcf_proj_opt_a <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_opt_a.rds"))
  ds_mcf_norm_sub_b <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_sub_b.rds"))
  ds_mcf_proj_sub_b <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_sub_b.rds"))
  ds_mcf_norm_opt_b <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_opt_b.rds"))
  ds_mcf_proj_opt_b <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_opt_b.rds"))
  ds_mcf_norm_sub_c <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_sub_c.rds"))
  ds_mcf_proj_sub_c <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_sub_c.rds"))
  ds_mcf_norm_opt_c <- readRDS(testthat::test_path("fixtures", "ds_mcf_norm_opt_c.rds"))
  ds_mcf_proj_opt_c <- readRDS(testthat::test_path("fixtures", "ds_mcf_proj_opt_c.rds"))
  expect_identical(round(ds_mcf$norm_sub_a,2), ds_mcf_norm_sub_a)
  expect_identical(round(ds_mcf$proj_sub_a,2), ds_mcf_proj_sub_a)
  expect_identical(round(ds_mcf$norm_opt_a,2), ds_mcf_norm_opt_a)
  expect_identical(round(ds_mcf$proj_opt_a,2), ds_mcf_proj_opt_a)
  expect_identical(round(ds_mcf$norm_sub_b,2), ds_mcf_norm_sub_b)
  expect_identical(round(ds_mcf$proj_sub_b,2), ds_mcf_proj_sub_b)
  expect_identical(round(ds_mcf$norm_opt_b,2), ds_mcf_norm_opt_b)
  expect_identical(round(ds_mcf$proj_opt_b,2), ds_mcf_proj_opt_b)
  expect_identical(round(ds_mcf$norm_sub_c,2), ds_mcf_norm_sub_c)
  expect_identical(round(ds_mcf$proj_sub_c,2), ds_mcf_proj_sub_c)
  expect_identical(round(ds_mcf$norm_opt_c,2), ds_mcf_norm_opt_c)
  expect_identical(round(ds_mcf$proj_opt_c,2), ds_mcf_proj_opt_c)
})
