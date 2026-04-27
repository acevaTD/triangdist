test_that("dtriang tests", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_error(dtriang(0.5, min = 0, max = 1, mode = -1))
  expect_true(dtriang(0.8, 0, 1, 0.5) > 0)
})


test_that("ptriang tests", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_error(ptriang(0.5, 1, 0, 0.5))
  expect_error(ptriang(0.5, min = 0, max = 1, mode = 2))
  expect_equal(ptriang(2, 0, 1, 0.5), 1)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
})
