test_that("reexported functions work", {
  # Test that reexported functions are available
  expect_true(exists("data.table"))
  expect_true(is.function(data.table))

  # Test data.table creation
  dt = data.table(x = 1:3, y = letters[1:3])
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 3)
})
