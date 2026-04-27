test_that("dtriang tests", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_error(dtriang(0.5, min = 0, max = 1, mode = -1))
})
