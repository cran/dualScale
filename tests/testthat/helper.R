# Load data ---------------------------------------------------------------

load_cf_example <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "cf_example.rds")))
}

load_cf_example_na <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "cf_example_na.rds")))
}

load_test_bad_coded <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "mc_bad_code.rds")))
}

load_mc_example <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "mc_example.rds")))
}

load_pc_example <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "pc_example.rds")))
}

load_ro_example <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "ro_example.rds")))
}

load_sc_example <- function() {
  as.matrix(readRDS(testthat::test_path("fixtures", "sc_example.rds")))
}


# Load functions ----------------------------------------------------------

load_act_bad_coded <- function() {
  ds_mc_check(
    input = load_test_bad_coded(),
    mode = "act"
  )
}

load_rad_bad_coded <- function() {
  ds_mc_check(
    input = load_test_bad_coded(),
    mode = "rad"
  )
}

load_test_ds_cf <- function() {
  ds_cf(
    input = load_cf_example()
  )
}

load_test_ds_mc <- function() {
  ds_mc(
    input = load_mc_example()
  )
}

load_test_ds_mcf <- function() {
  ds_mcf(
    input = load_mc_example(),
    crit = 1
  )
}

load_test_ds_pc <- function() {
  ds_pc(input = load_pc_example())
}

load_test_ds_ro <- function() {
  ds_ro(input = load_ro_example())
}
