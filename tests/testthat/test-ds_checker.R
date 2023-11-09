test_that("Values must be an integer", {
  input <- readRDS(testthat::test_path("fixtures", "checker_example.rds"))
  ds_input_check(input, 1) %>% expect_error()
})

test_that("Solutions must be an integer", {
  input <- readRDS(testthat::test_path("fixtures", "cf_example.rds"))
  ds_input_check(input, 1.5) %>% expect_error()
})

test_that("You have NA values", {
  input <- readRDS(testthat::test_path("fixtures", "checker_example_na.rds"))
  ds_mc_input(input, 1, "act") %>%
    expect_message(regexp = glue::glue("\nYou have NA values."))
})

test_that("A value must be assigned in crit", {
  input <- readRDS(testthat::test_path("fixtures", "checker_example_na.rds"))
  ds_mcf_input(input, 1, "act", NA) %>% expect_error()
})

test_that("Item Criterium is bigger than 2", {
  input <- readRDS(testthat::test_path("fixtures", "checker_example_na.rds"))
  ds_mcf_input(input, 1, "act", 3) %>% expect_error()
})
