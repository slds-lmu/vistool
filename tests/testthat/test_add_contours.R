test_that("VisualizerObj add_contours works for 2D objectives", {
  skip_if_not_installed("ggplot2")

  o = obj("TF_branin")
  vis = as_visualizer(o, type = "2d", n_points = 50)

  expect_no_error(vis$add_contours(bins = 7))
  expect_no_error(vis$add_contours(binwidth = 1))

  p = NULL
  expect_no_error(p <- vis$plot())
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(p, "ggplot")
    geom_names = vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true(any(grepl("Contour|Raster|Tile|GeomContour|GeomRaster|GeomTile", geom_names, ignore.case = TRUE)))
  }
})

test_that("VisualizerModel add_contours works for 2D tasks", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ggplot2")

  task = mlr3::tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
  learner = mlr3::lrn("classif.rpart")

  vis = as_visualizer(task, learner = learner, type = "2d", n_points = 40)

  expect_no_error(vis$add_contours(bins = 8))
  expect_no_error(vis$add_contours(breaks = seq(4, 8, by = 0.5)))

  p = NULL
  expect_no_error(p <- vis$plot())
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    expect_s3_class(p, "ggplot")
    # There should be at least one contour geom or raster/filled geom in layers
    geom_names = vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_true(any(grepl("Contour|Raster|Tile|GeomContour|GeomRaster|GeomTile", geom_names, ignore.case = TRUE)))
  }
})