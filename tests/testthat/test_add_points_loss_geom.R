test_that("add_points loss geometry (1D regression) produces residual squares and segments", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("rpart")
  # create target y = 2x + 1 with small noise
  dt = data.table::data.table(x = seq(-1, 1, length.out = 11))
  dt[, `:=`(y = 2 * x + 1)]
  task = mlr3::TaskRegr$new(id = "lin", backend = dt, target = "y")
  learner = mlr3::lrn("regr.rpart")
  vis = as_visualizer(task, learner = learner)
  # Points purposely off the line to create residuals
  pts = data.frame(x = c(-0.5, 0, 0.75), y = c(2.2, 1.1, 2.4))
  vis$add_points(pts, loss = "l2_se")
  p = vis$plot()
  # Basic structural checks: plot is ggplot and contains expected layers (line + points + rect/segment)
  expect_true(inherits(p, "ggplot"))
  geoms = vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("GeomRect", geoms)))
  expect_true(any(grepl("GeomSegment", geoms)))
})
