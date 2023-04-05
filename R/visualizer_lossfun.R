#' @title Loss Function Visualizer
#' 
#' @include visualizer.R
#'
#' Visualizer for loss functions with univariate input.
#'
#' This class is used to create visualizations/animations.
#' The used plotting backends are plotly (https://plotly.com/) and ggplot2
#' (https://ggplot2.tidyverse.org//).
#' @export
VisualizerLossFunction = R6::R6Class(
  "VisualizerLossFunction",
  inherit = Visualizer,
  public = list(
    
    loss_function = NULL,

    initialize = function(
      loss_function, x1limits = NULL, padding = 0, npoints = 100
    ) {
      checkmate::assertFunction(loss_function, args = "x")
      checkmate::assertNumber(padding, lower = 0)
      checkmate::assertNumeric(x1limits, len = 2, null.ok = TRUE)
      
      self$loss_function = loss_function
      if (is.null(x1limits)) x1limits = c(-3, 3)
      
      self$grid = list(
        x1 = unique(
          seq(
            min(x1limits) - x1pad, max(x1limits) + x1pad, length.out = npoints
          )
        )
      )
      self$zmat = loss_function(self$grid$x1)
      
      return(invisible(self))
      
    },
    
    addLayerLoss = function(id, loss_fun, col = "blue", ...) {
      private$p_layers <- append(private$p_layers, list(id))
      this_color <- list(col)
      names(this_color) <- id
      private$p_colors <- append(private$p_colors, this_color)
      self$data_table[[id]] <- loss_fun(self$data_table$x)
      private$p_plot <- private$p_plot +
        geom_function(
          data.frame(x = self$data_table$x, color = id), 
          mapping = aes(x = x, col = color), 
          fun = loss_fun,
          ...
        )
      return(invisible(self))
      
    },
    
    addLayerAnnotation = function(
      residual_highlight,
      type = "lines",
      line = list(size = 3, color = "blue"),
      col = "blue",
      fill = "blue",
      hjust = 1,
      ...
    ) {
      checkmate::assertNumeric(residual_highlight, min.len = 1)
      if (type == "lines") {
        plot_dt = data.table(
          x = residual_highlight, 
          xend = residual_highlight, 
          y = 0,
          yend = loss_function(residual_highlight)
        )
        private$p_plot = private$p_plot +
          geom_segment(
            plot_dt,
            mapping = aes(x = x, xend = xend, y = y, yend = yend),
            col = col, 
            ...
          )  
      } else if (type == "points") {
        private$p_plot = private$p_plot +
          geom_point(
            data.frame(
              x = residual_highlight, y = loss_function(residual_highlight)
            ),
            mapping = aes(x = x, y = y),
            col = col,
            fill = fill, 
            ...
          )  
      } else if (type == "text") {
        private$p_plot = private$p_plot +
          geom_text(
            data.frame(
              x = residual_highlight, y = loss_function(residual_highlight)
            ),
            mapping = aes(x = x, y = y, label = sprintf("%.2f", y)),
            col = col,
            hjust = hjust,
            ...
          )
      }
      else stop("Supported annotation types: `lines`, `points`, `text`")
  
      return(invisible(self))
    }
  )
)


library(data.table)
library(ggplot2)
library(plotly)
n_points <- 10L
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

loss_fun = rloss_dict$get("RL_huber")$fun

viz = VisualizerPrediction$new(preddy, padding = 0.1)
viz$initLayerUnivariate(col = "red", linetype = "dashed")
viz$addLayerScatter("x_1")
# viz$addLayerResiduals("x_1", idx_residuals = c(3, 5), quadratic = TRUE)
# viz$addLayerResiduals("x_1", idx_residuals = c(2, 4), col = "green")
# viz$addLayerHyperplane("foo", coeffs = c(1, 0.7))
viz$addLayerHyperplane("bar", coeffs = c(1, 0.3), col = "green")
viz$plot("henlo")


obj = rloss_dict$get("RL_l2")
obj$reset_xdim(3L)
obj$reset_fargs(Xmat = Xmat_biv, y = y_biv)

opt = OptimizerGD$new(
  obj, x_start = c(2, 2, 1.6), print_trace = FALSE, lr = 0.0001
)
opt$optimize(steps = 1000)

preddy = LMPredictor$new("test", dt_biv, y ~ x_1 + x_2, opt$x)
preddy$predict()

viz = VisualizerPrediction$new(preddy)
viz$initLayerBivariate(
  type = "surface", line = list(width = 0)
)
viz$addLayerScatter("x_1", "x_2")
viz$addLayerResiduals("x_1", "x_2")
viz$plot()

# foo = VisualizerOptim$new(obj, x1limits = c(-10, 10), x2limits = c(-10, 10))
# foo$initLayerBivariate()
# foo$addLayerOptimizationTrace(opt)
# foo$plot()

mtcars$id <- seq_len(nrow(mtcars))
ms <- replicate(2, mtcars, simplify = F)
ms[[2]]$mpg <- 0
m <- group2NA(dplyr::bind_rows(ms), "id")