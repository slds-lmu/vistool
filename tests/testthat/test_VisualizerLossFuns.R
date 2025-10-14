test_that("VisualizerLossFuns creation works", {
  # Create loss functions
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis = VisualizerLossFuns$new(list(loss1, loss2))

  expect_s3_class(vis, "VisualizerLossFuns")
  expect_equal(length(vis$losses), 2)
  # The names should be the IDs of the loss functions
  expect_true(all(c(loss1$id, loss2$id) %in% names(vis$losses)))
})

test_that("VisualizerLossFuns plotting works", {
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis = VisualizerLossFuns$new(list(loss1, loss2))

  # Test plotting
  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # Should have both loss functions in the plot
  expect_true(length(p$layers) > 0)
})

test_that("VisualizerLossFuns with prediction data works", {
  loss = lss("l2_se")

  # With prediction and true values
  vis = VisualizerLossFuns$new(
    list(loss),
    y_pred = seq(-2, 2, by = 0.5),
    y_true = 0
  )

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerLossFuns input validation works", {
  loss1 = lss("l2_se") # regression

  # Basic test - single loss function should work
  vis1 = VisualizerLossFuns$new(list(loss1))
  expect_s3_class(vis1, "VisualizerLossFuns")
  expect_equal(vis1$task_type, "regr")

  # Try to create VisualizerLossFuns with mixed task types if possible
  classif_keys = dict_loss$keys()[sapply(dict_loss$keys(), function(k) {
    tryCatch(
      {
        loss = dict_loss$get(k)
        loss$task_type == "classif"
      },
      error = function(e) FALSE
    )
  })]

  if (length(classif_keys) > 0) {
    loss2 = lss(classif_keys[1]) # classification

    # Should fail with mixed task types
    expect_error(
      VisualizerLossFuns$new(list(loss1, loss2)),
      "task_type.*need to be the same"
    )
  }
})

test_that("VisualizerLossFuns customization works", {
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis = VisualizerLossFuns$new(list(loss1, loss2))

  # Test customization through plot parameters
  p = vis$plot(line_col = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerLossFuns single loss function works", {
  loss = lss("l2_se")

  vis = VisualizerLossFuns$new(list(loss))

  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # Test that the task_type and input_type are correctly set
  expect_equal(vis$task_type, "regr")
  expect_equal(vis$input_type, "score")
})

test_that("VisualizerLossFuns with classification task works", {
  # Try to find a classification loss function
  if (any(grepl("logistic|hinge", dict_loss$keys()))) {
    # Get classification losses with score input (not probability)
    classif_keys = dict_loss$keys()[sapply(dict_loss$keys(), function(k) {
      loss = dict_loss$get(k)
      loss$task_type == "classif" && loss$input_default == "score"
    })]

    if (length(classif_keys) > 0) {
      loss = lss(classif_keys[1])

      vis = VisualizerLossFuns$new(list(loss))

      p = vis$plot()
      expect_s3_class(p, "ggplot")

      # Test that the task_type and input_type are correctly set
      expect_equal(vis$task_type, "classif")
      expect_equal(vis$input_type, "score")
    }
  }
})

test_that("VisualizerLossFuns with classification probability-based task works", {
  # Get classification losses with probability input
  classif_prob_keys = dict_loss$keys()[sapply(dict_loss$keys(), function(k) {
    loss = dict_loss$get(k)
    loss$task_type == "classif" && loss$input_default == "probability"
  })]

  if (length(classif_prob_keys) > 0) {
    loss = lss(classif_prob_keys[1])

    vis = VisualizerLossFuns$new(list(loss))

    p = vis$plot()
    expect_s3_class(p, "ggplot")

    # Test that the task_type and input_type are correctly set
    expect_equal(vis$task_type, "classif")
    expect_equal(vis$input_type, "probability")
  }
})

test_that("VisualizerLossFuns range setting works", {
  loss = lss("l2_se")

  vis = VisualizerLossFuns$new(list(loss))

  # Test plotting with custom x-axis limits
  p1 = vis$plot(x_limits = c(-10, 10))
  expect_s3_class(p1, "ggplot")

  # Test plotting with default limits
  p2 = vis$plot()
  expect_s3_class(p2, "ggplot")
})

test_that("VisualizerLossFuns line styling works", {
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis = VisualizerLossFuns$new(list(loss1, loss2))

  # Test customization through plot parameters
  p = vis$plot(line_width = c(1, 2), line_type = c("solid", "dashed"))
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerLossFuns as_visualizer integration works", {
  loss = lss("l2_se")

  # Test through as_visualizer
  preds = seq(-2, 2)
  vis = as_visualizer(loss, y_pred = preds, y_true = rep(0, length(preds)))

  expect_s3_class(vis, "VisualizerLossFuns")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerLossFuns with custom y_pred and y_true works", {
  loss = lss("l2_se")

  # Test with regression data
  vis = VisualizerLossFuns$new(
    list(loss),
    y_pred = c(1, 2, 3),
    y_true = c(1.1, 2.2, 2.8)
  )

  # Should use task type from loss function
  expect_equal(vis$task_type, "regr")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})
