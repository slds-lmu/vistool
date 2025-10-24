test_that("ggplot latex controls produce expressions", {
  skip_if_not_installed("latex2exp")

  obj = Objective$new(id = "latex_1d", fun = function(x) x^2, xdim = 1, lower = -2, upper = 2)
  vis = as_visualizer(obj, type = "1d", n_points = 50)

  p = vis$plot(plot_title = "$\\alpha$", latex = TRUE)
  expect_true(inherits(p$labels$title, "expression"))
  expect_equal(attr(p$labels$title, "latex"), "$\\alpha$")

  expect_true(inherits(p$labels$x, "expression"))
  expect_equal(attr(p$labels$x, "latex"), "$x$")
})

test_that("point annotations respect annotations_latex flag", {
  skip_if_not_installed("latex2exp")

  obj = Objective$new(id = "latex_ann", fun = function(x) x^2, xdim = 1, lower = -2, upper = 2)
  vis = as_visualizer(obj, type = "1d", n_points = 50)
  vis$add_points(
    points = data.frame(x = c(-0.5, 0.5), y = c(0.25, 0.25)),
    annotations = c("$\\alpha$", "beta"),
    annotations_latex = c(TRUE, FALSE)
  )

  p = vis$plot()
  built = ggplot2::ggplot_build(p)
  label_layer = built$data[[length(built$data)]]
  expect_true(inherits(label_layer$label[[1]], "expression"))
  expect_equal(attr(label_layer$label[[1]], "latex"), "$\\alpha$")
  expect_identical(label_layer$label[[2]], "beta")
})

test_that("plotly surfaces format LaTeX titles and axes", {
  obj = Objective$new(
    id = "latex_surface",
    fun = function(x) x[1]^2 + x[2]^2,
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )
  vis = as_visualizer(obj, type = "surface", n_points = 15)

  plt = vis$plot(
    plot_title = "$\\gamma$",
    latex = list(title = TRUE, x = TRUE, y = TRUE, z = TRUE)
  )
  layout_attrs = plt$x$layoutAttrs[[length(plt$x$layoutAttrs)]]
  title_text = layout_attrs$title$text
  expect_true(inherits(title_text, "TeX"))
  expect_identical(as.character(title_text), "$\\gamma$")
  x_text = layout_attrs$scene$xaxis$title$text
  expect_true(inherits(x_text, "TeX"))
  expect_identical(as.character(x_text), "$x1$")
  y_text = layout_attrs$scene$yaxis$title$text
  expect_true(inherits(y_text, "TeX"))
  expect_identical(as.character(y_text), "$x2$")
  z_text = layout_attrs$scene$zaxis$title$text
  expect_true(inherits(z_text, "TeX"))
  expect_identical(as.character(z_text), "$y$")

  vis$add_points(points = matrix(c(0, 0), ncol = 2), annotations = "$\\beta$", annotations_latex = TRUE)
  flat = vis$plot(flatten = TRUE)
  flat_layout = flat$x$layoutAttrs[[length(flat$x$layoutAttrs)]]
  ann_text = flat_layout$annotations$text
  expect_true(inherits(ann_text, "TeX"))
  expect_identical(as.character(ann_text), "$\\beta$")
})
