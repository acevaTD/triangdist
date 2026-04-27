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

test_that("qtriang tests", {
  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
  expect_error(qtriang(-0.1, 0, 1, 0.5))
  expect_error(qtriang(1.1, 0, 1, 0.5))
  expect_error(qtriang(1.5, min = 0, max = 1, mode = 0.5))
  expect_error(qtriang(-0.01, min = 0, max = 1, mode = 0.5))
  expect_error(qtriang(1.01, min = 0, max = 1, mode = 0.5))
  expect_error(qtriang(0.5, min = 2, max = 1, mode = 1.5))
  expect_true(qtriang(0.8, 0, 1, 0.5) > 0.5)
  expect_error(qtriang(0.5, min = 0, max = 1, mode = 2))
})

test_that("rtriang tests", {
  n <- 50
  samples <- rtriang(n, 0, 1, 0.5)
  expect_length(samples, n)
  expect_true(all(samples >= 0 & samples <= 1))
})
