test_that("The input data is the same as the initial_data result
          with act mode", {
  out <- load_act_bad_coded()
  expect_equal(out$initial_data, load_test_bad_coded())
})

test_that("The input data is the same as the initial_data result
          with rad mode", {
  out <- load_rad_bad_coded()
  expect_equal(out$initial_data, load_test_bad_coded())
})

test_that("The function show error when incorrect mode is choosen", {
  expect_error(
    ds_mc_check(
      input = load_test_bad_coded(),
      mode = "test"
    )
  )
})

test_that("The input data will converted with rad mode", {
  t_data_rad_bad_coded <- readRDS(testthat::test_path("fixtures", "t_data_rad_bad_coded.rds"))
  expect_identical(round(load_rad_bad_coded()$t_data,2), t_data_rad_bad_coded)
})


test_that("The input data will converted with act mode", {
  t_data_act_bad_coded <- readRDS(testthat::test_path("fixtures", "t_data_act_bad_coded.rds"))
  expect_identical(round(load_act_bad_coded()$t_data,2), t_data_act_bad_coded)
})
