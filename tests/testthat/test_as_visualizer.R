test_that("task validation enforces compatibility and derives limits", {
  skip_if_not_installed("mlr3learners")

  task_1d = tsk("mtcars")
  task_1d$select("gear")
  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  hyp2d = hypothesis(
    fun = function(gear, cyl) gear + cyl,
    type = "regr",
    predictors = c("gear", "cyl"),
    domain = list(gear = c(2, 6), cyl = c(2, 8))
  )

  expect_snapshot(error = TRUE, as_visualizer(task_2d, learner = learner, hypothesis = hyp2d))
  expect_snapshot(error = TRUE, as_visualizer(task_2d, learner = learner, domain = list(gear = c(0, 1))))
  expect_snapshot(error = TRUE, as_visualizer(task_2d, learner = learner, y_pred = 1))
  expect_snapshot(error = TRUE, as_visualizer(task_1d, learner = learner, x2_limits = c(0, 1)))

  vis_1d = as_visualizer(task_1d, learner = learner, padding = 0.1)
  orig = range(task_1d$data()[["gear"]])
  expected = c(orig[1] - diff(orig) * 0.1, orig[2] + diff(orig) * 0.1)
  limits_1d = vis_1d$.__enclos_env__$private$.data_structure$limits$x1
  expect_equal(limits_1d, expected)
})

test_that("hypothesis validation requires domain coverage", {
  hyp1d = hypothesis(
    fun = function(x) x,
    type = "regr",
    predictors = "x",
    domain = list(x = c(-1, 1))
  )
  hyp2d = hypothesis(
    fun = function(x, y) x + y,
    type = "regr",
    predictors = c("x", "y"),
    domain = list(x = c(-1, 1), y = c(-2, 2))
  )
  hyp_no_domain = hypothesis(
    fun = function(x) x,
    type = "regr",
    predictors = "x",
    domain = NULL
  )

  expect_snapshot(error = TRUE, as_visualizer(hyp_no_domain))
  expect_snapshot(error = TRUE, as_visualizer(hyp1d, input_type = "score"))
  expect_snapshot(error = TRUE, as_visualizer(hyp1d, y_pred = 1))
  expect_snapshot(error = TRUE, as_visualizer(hyp1d, x2_limits = c(0, 1)))
  expect_warning(as_visualizer(hyp2d, retrain = FALSE), "retrain is ignored")
})

test_that("objective validation enforces dimensional constraints", {
  obj_1d = Objective$new(
    id = "obj1",
    fun = function(x) x^2,
    label = "f",
    xdim = 1,
    lower = -2,
    upper = 2
  )
  obj_2d = obj("TF_branin")
  obj_unknown = Objective$new(
    id = "obj_na",
    fun = function(x) sum(x^2),
    label = "f",
    xdim = NA,
    xtest = c(0, 0)
  )

  expect_snapshot(error = TRUE, as_visualizer(obj_1d, type = "2d"))
  expect_snapshot(error = TRUE, as_visualizer(obj_2d, type = "1d"))
  expect_snapshot(error = TRUE, as_visualizer(obj_unknown))

  expect_s3_class(as_visualizer(obj_1d), "VisualizerObj")
  expect_s3_class(as_visualizer(obj_2d, type = "surface"), "VisualizerSurfaceObj")
})

test_that("loss visualizer validation rejects incompatible inputs", {
  loss_reg = lss("l2_se")
  loss_ce = lss("cross-entropy")

  expect_snapshot(error = TRUE, as_visualizer(loss_reg, n_points = 5))
  expect_snapshot(error = TRUE, as_visualizer(loss_reg, y_curves = "y1"))
  expect_snapshot(error = TRUE, as_visualizer(loss_reg, y_pred = 1:2, y_true = 1:3))
  expect_snapshot(error = TRUE, as_visualizer(loss_ce, input_type = "probability", y_pred = c(-0.1, 0.5)))
  expect_snapshot(error = TRUE, as_visualizer(list(loss_reg, 1)))

  expect_s3_class(as_visualizer(loss_reg, y_pred = seq(-3, 3), y_true = 0), "VisualizerLossFuns")
  expect_s3_class(as_visualizer(loss_ce, input_type = "probability", y_pred = seq(0.1, 0.9, length.out = 5)), "VisualizerLossFuns")

  vis_loss_custom = as_visualizer(loss_reg, n_points = 512L)
  vis_loss_custom$plot()
  expect_equal(
    vis_loss_custom$.__enclos_env__$private$.loss_plot_settings$n_points,
    512L
  )
})

test_that("surface requests warn on large grids", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("plotly")

  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  expect_warning(as_visualizer(task_2d, learner = learner, type = "surface", n_points = 256L), "n_points")

  obj_2d = obj("TF_branin")
  expect_warning(as_visualizer(obj_2d, type = "surface", n_points = 256L), "n_points")
})

test_that("positive construction paths remain available", {
  skip_if_not_installed("mlr3learners")

  task_2d = tsk("mtcars")
  task_2d$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis_task = as_visualizer(task_2d, learner = learner)
  expect_s3_class(vis_task, "VisualizerModel")

  obj_2d = obj("TF_branin")
  vis_obj = as_visualizer(obj_2d)
  expect_s3_class(vis_obj, "VisualizerObj")

  loss_reg = lss("l2_se")
  vis_loss = as_visualizer(loss_reg, y_pred = rnorm(20), y_true = rnorm(20))
  expect_s3_class(vis_loss, "VisualizerLossFuns")
})
