test_that("`max` is given when `sols` is NA", {
  expect_equal(ds_select_solutions(NULL, 1), 1)
})

test_that("`max` is given when `max` is less than `sols`", {
  suppressMessages(expect_equal(ds_select_solutions(2, 1), 1))
})

test_that("`sols` is given when `sols` is less than `max`", {
  expect_equal(ds_select_solutions(1, 2), 1)
})
