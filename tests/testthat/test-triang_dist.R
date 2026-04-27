test_that("multiplication works", {
  expect_equal(2 * 2, 5)
})

test_that("Error handling correct", {
  expect_equal(dtriang(0,3,2,1), "min can't be greater than max")
})
