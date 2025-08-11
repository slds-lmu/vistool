test_that("LossFunction creation works", {
  # Create a simple regression loss function
  loss_regr = LossFunction$new(
    id = "test_l2",
    label = "Test L2 Loss",
    task_type = "regr",
    fun = function(r) r^2
  )

  expect_s3_class(loss_regr, "LossFunction")
  expect_equal(loss_regr$id, "test_l2")
  expect_equal(loss_regr$label, "Test L2 Loss")
  expect_equal(loss_regr$task_type, "regr")
  expect_true(is.function(loss_regr$fun))

  # Create a classification loss function
  loss_classif = LossFunction$new(
    id = "test_hinge",
    label = "Test Hinge Loss",
    task_type = "classif",
    fun = function(r) pmax(0, 1 - r)
  )

  expect_equal(loss_classif$task_type, "classif")
})

test_that("LossFunction evaluation works", {
  loss = LossFunction$new(
    id = "l2",
    label = "L2 Loss",
    task_type = "regr",
    fun = function(r) r^2
  )

  # Test single value
  result = loss$fun(2)
  expect_equal(result, 4)

  # Test vector
  results = loss$fun(c(-1, 0, 1))
  expect_equal(results, c(1, 0, 1))
})

test_that("Built-in loss functions work", {
  # Test L2 loss
  loss_l2 = lss("l2_se")
  expect_s3_class(loss_l2, "LossFunction")
  expect_equal(loss_l2$task_type, "regr")

  # Test L1 loss
  loss_l1 = lss("l1_ae")
  expect_s3_class(loss_l1, "LossFunction")
  expect_equal(loss_l1$task_type, "regr")

  # Test evaluation
  result_l2 = loss_l2$fun(2)
  expect_equal(result_l2, 4) # 2^2

  result_l1 = loss_l1$fun(2)
  expect_equal(result_l1, 2) # abs(2)
})

test_that("Loss function dictionary works", {
  dict = dict_loss
  expect_true(length(dict) > 0)

  # Test getting available losses
  keys = dict$keys()
  expect_true("l2_se" %in% keys)
  expect_true("l1_ae" %in% keys)

  # Test as.data.table method
  dt = as.data.table(dict)
  expect_s3_class(dt, "data.table")
  expect_true("task_type" %in% colnames(dt))
})

test_that("LossFunction input validation works", {
  # Invalid task type
  expect_error(
    LossFunction$new(
      id = "test",
      label = "Test",
      task_type = "invalid",
      fun = function(r) r^2
    ),
    "Must be element of set"
  )
})

test_that("Huber loss works", {
  # Test huber loss if available
  if ("huber" %in% dict_loss$keys()) {
    loss_huber = lss("huber")
    expect_s3_class(loss_huber, "LossFunction")
    expect_equal(loss_huber$task_type, "regr")

    # Test evaluation (should be quadratic for small values, linear for large)
    small_val = loss_huber$fun(0.5) # Should be quadratic
    large_val = loss_huber$fun(2) # Should be linear
    expect_true(is.numeric(small_val))
    expect_true(is.numeric(large_val))
  }
})

test_that("Parameterized loss functions work", {
  # Test pinball loss with custom quantile
  loss_pinball_default = lss("pinball")
  loss_pinball_custom = lss("pinball", quantile = 0.3)

  expect_s3_class(loss_pinball_default, "LossFunction")
  expect_s3_class(loss_pinball_custom, "LossFunction")

  # Check that custom parameters are reflected in the label
  expect_equal(loss_pinball_default$label, "Pinball Loss")
  expect_equal(loss_pinball_custom$label, "Pinball Loss (quantile=0.3)")

  # Check that custom parameters affect the computation
  test_values = c(-1, 0, 1)
  default_results = loss_pinball_default$fun(test_values)
  custom_results = loss_pinball_custom$fun(test_values)

  expect_false(identical(default_results, custom_results))
  expect_true(all(is.numeric(default_results)))
  expect_true(all(is.numeric(custom_results)))

  # Test Huber loss with custom delta
  loss_huber_default = lss("huber")
  loss_huber_custom = lss("huber", delta = 2)

  expect_s3_class(loss_huber_default, "LossFunction")
  expect_s3_class(loss_huber_custom, "LossFunction")

  expect_equal(loss_huber_default$label, "Huber Loss")
  expect_equal(loss_huber_custom$label, "Huber Loss (delta=2)")

  # Check computation differences with larger values (outside delta threshold)
  large_test_values = c(-3, -1.5, 0, 1.5, 3)
  huber_default_results = loss_huber_default$fun(large_test_values)
  huber_custom_results = loss_huber_custom$fun(large_test_values)

  expect_false(identical(huber_default_results, huber_custom_results))
  expect_true(all(is.numeric(huber_default_results)))
  expect_true(all(is.numeric(huber_custom_results)))
})

test_that("lss function returns original when no parameters provided", {
  # Test that lss without parameters returns the original object
  loss_original = dict_loss$get("l2_se")
  loss_from_lss = lss("l2_se")

  expect_identical(loss_original, loss_from_lss)
})
