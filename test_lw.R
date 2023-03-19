library(plotly)

n_points <- 1000L
set.seed(123)
x_1 <- rnorm(n_points, mean = 1, sd = 1)
x_2 <- rnorm(n_points, mean = 1, sd = 1)
set.seed(456)
y_univ <- 1 + 0.5 * x_1 + rnorm(n_points, sd = 0.5)
y_biv <- 1 + 0.5 * x_1 + 0.5 * x_2 + rnorm(n_points, sd = 0.5)
dt_univ <- data.table(x_1, y = y_univ)
dt_biv <- data.table(x_1, x_2, y = y_biv)

Xmat_univ = model.matrix(~ x_1, dt_univ)
Xmat_biv = model.matrix(~ x_1 + x_2, dt_biv)

obj = rloss_dict$get("RL_l2")
obj$reset_xdim(3L)
obj$reset_fargs(Xmat = Xmat_biv, y = y_biv)
obj$reset_xdim(3L)
opt = OptimizerGD$new(
  obj, x_start = c(2, 1.6, 1.6), print_trace = FALSE, lr = 0.0001
)
opt$optimize(steps = 100)
opt$x

pred = LMPredictor$new("l2", dt_biv, y ~ x_1 + x_2, opt$x)
head(pred$predict())

obj$reset_xdim(ncol(Xmat_biv) - 1)
viz = Visualizer$new(obj, x1limits = c(-10, 10), x2limits = c(-10, 10))
viz$initLayerSurface(colorscale = list(c(0, 1), c("white", "black")))
viz$plot()
