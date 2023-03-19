#' @title Prediction Visualizer
#' 
#' @include visualizer.R
#'
#' Visualizer for prediction hypersurfaces.
#'
#' This class is used to create visualizations/animations.
#' The used plotting backends are plotly (https://plotly.com/) and ggplot2
#' (https://ggplot2.tidyverse.org//).
#' @export
VisualizerPrediction = R6::R6Class(
  "VisualizerPrediction",
  inherit = Visualizer,
  public = list(
    #' @field predictor (`LMPredictor`) The objective which was optimized. 
    #' This object is used to generate the surface/contour lines.
    predictor = NULL,
    
    initialize = function(
      predictor,
      x1limits = NULL, 
      x2limits = NULL, 
      padding = 0,
      npoints = 100
    ) {
      self$predictor = checkmate::assertR6(predictor, "LMPredictor")
      if (predictor$with_intercept) {
        xmat = predictor$xmat[, -1]
      } else xmat = predictor$xmat
      xdim = ifelse(is.matrix(xmat), ncol(xmat), 1)
      if (xdim > 2) {
        stop(
          sprintf("1- or 2-dimensional inputs required, but xdim = %s`", xdim)
        )
      }
      if (xdim == 1) xmat = as.matrix(xmat)
      checkmate::assertNumber(padding, lower = 0)
      checkmate::assertNumeric(x1limits, len = 2, null.ok = TRUE)
      checkmate::assertNumeric(x2limits, len = 2, null.ok = TRUE)
      
      if (is.null(x1limits)) {
        x1limits = c(min(xmat[, 1]), max(xmat[, 1]))
      }
      if (xdim > 1 & is.null(x2limits)) {
        x2limits = c(min(xmat[, 2]), max(xmat[, 2]))
      }
      x1pad = (max(x1limits) - min(x1limits)) * padding
      
      self$grid = list(
        x1 = unique(
          seq(
            min(x1limits) - x1pad, max(x1limits) + x1pad, length.out = npoints
          )
        )
      )
      
      if (xdim > 1) {
        x2pad = (max(x2limits) - min(x2limits)) * padding
        self$grid = append(
          self$grid,
          list(
            x2 = unique(
              seq(
                min(x2limits) - x2pad, max(x2limits) + x2pad, 
                length.out = npoints
              )
            )
          )
        )
        lm_surface = expand.grid(
          x1 = self$grid$x1, x2 = self$grid$x2, KEEP.OUT.ATTRS = F
        )
        lm_surface$y = rep(0, length(self$grid$x1))
        self$zmat = reshape2::acast(lm_surface, x2 ~ x1, value.var = "y")
        self$zmat = outer(
          self$grid$x1, 
          self$grid$x2, 
          function(x1, x2) {
            xin = cbind(x1, x2)
            if (self$predictor$with_intercept) xin = cbind(1, xin)
            apply(xin, 1, function(x) x %*% self$predictor$coeffs)
            }
          )
      } else {
        if (self$predictor$with_intercept) {
          self$zmat = cbind(1, self$grid$x1) %*% self$predictor$coeffs
        } else self$zmat = self$grid$x1 %*% self$predictor$coeffs
      }

      return(invisible(self))
      
    },
    
    addLayerScatter = function(
      covariate_1, 
      covariate_2 = NULL, 
      marker = list(size = 5, color = "black", symbol = "cross"),
      shape = "cross",
      col = "black",
      ...
    ) {
      if (private$p_layer_primary == "line") {
        private$p_plot = private$p_plot +
          geom_point(
            data.table(
              x = self$predictor$prediction[[covariate_1]], 
              y = self$predictor$prediction$target
            ),
            mapping = aes(x, y),
            shape = shape, 
            col = col, 
            ...
          )
      } else if (private$p_layer_primary == "contour") {
        private$p_plot = private$p_plot %>% 
          add_trace(
            x = self$predictor$prediction[[covariate_1]], 
            y = self$predictor$prediction[[covariate_2]],
            mode = "markers", 
            type = "scatter",
            marker = marker,
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      } else if (private$p_layer_primary == "surface") {
        private$p_plot = private$p_plot %>% 
          add_trace(
            x = self$predictor$prediction[[covariate_1]], 
            y = self$predictor$prediction[[covariate_2]],
            z = self$predictor$prediction$pred,
            mode = "markers", 
            type = "scatter3d",
            marker = marker,
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      }
      return(invisible(self))
    },
    
    addLayerResiduals = function(
      covariate_1, 
      covariate_2 = NULL,
      idx_residuals = NULL,
      quadratic = FALSE,
      line = list(size = 3, color = "blue"),
      marker = list(color = "blue", opacity = 0.1),
      col = "blue",
      fill = "blue",
      alpha = 0.1,
      ...
    ) {
      if (private$p_layer_primary == "line") {
        pred = self$predictor$prediction
        if (quadratic) {
          plot_dt = data.table(
            xmin = pred[[covariate_1]], 
            xmax = pred[[covariate_1]] + (pred$pred - pred$target), 
            ymin = pred$target,
            ymax = pred$pred
          )
        } else {
          plot_dt = data.table(
            x = pred[[covariate_1]], 
            xend = pred[[covariate_1]], 
            y = pred$target,
            yend = pred$pred
          )
        }
        if (!is.null(idx_residuals)) plot_dt = plot_dt[idx_residuals, ]
        if (quadratic) {
          private$p_plot = private$p_plot +
            geom_rect(
              plot_dt,
              mapping = aes(
                x = NULL,
                y = NULL,
                xmin = xmin, 
                xmax = xmax, 
                ymin = ymin, 
                ymax = ymax
                ),
              alpha = alpha,
              col = col,
              fill = fill,
              ...
            )
        } else {
          private$p_plot = private$p_plot +
            geom_segment(
              plot_dt,
              mapping = aes(x = x, xend = xend, y = y, yend = yend),
              col = col, 
              ...
            )
        }
      } else if (private$p_layer_primary == "contour") {
        warning(
          "Contour plots do not support residuals. Mapping to marker size"
        )
        if (quadratic) stop("Quadratic residuals only supported in 1D")
        plot_cols = c(covariate_1, covariate_2, "residual")
        plot_dt = copy(self$predictor$prediction)[, ..plot_cols]
        plot_dt$residual = abs(plot_dt$residual)
        setnames(plot_dt, c("x1", "x2", "res"))
        if (!is.null(idx_residuals)) plot_dt = plot_dt[idx_residuals, ]
        private$p_plot = private$p_plot %>% 
          add_trace(
            data = plot_dt,
            x = ~x1, 
            y = ~x2,
            mode = "markers", 
            type = "scatter",
            marker = append(marker, list(size = ~res * 50)),
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      } else if (private$p_layer_primary == "surface") {
        if (quadratic) stop("Quadratic residuals only supported in 1D")
        plot_cols = c(covariate_1, covariate_2, "target", "pred")
        plot_dt = copy(self$predictor$prediction)[
          , ..plot_cols
          ][, aux := as.double(NA)] # prevents residuals from being connected
        plot_dt = data.table::melt(
          plot_dt, id.vars = c(covariate_1, covariate_2)
        )
        if (!is.null(idx_residuals)) plot_dt = plot_dt[idx_residuals, ]
        data.table::setorderv(plot_dt, c(covariate_1))
        private$p_plot = private$p_plot %>% 
          add_paths(
            x = plot_dt[[covariate_1]], 
            y = plot_dt[[covariate_2]],
            z = plot_dt$value,
            line = line,
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      }
      return(invisible(self))
    },
    
    addLayerHyperplane = function(
      id, coeffs, col = "blue", linetype = "solid", ...
    ) {
      private$p_ids = append(private$p_ids, id)
      this_aes <- list(color = col, linetype = linetype)
      this_aes <- list(this_aes)
      names(this_aes) = id
      private$p_line_aes <- append(private$p_line_aes, this_aes)
      if (private$p_layer_primary == "line") {
        private$p_plot <- private$p_plot +
          geom_abline(
            data.frame(
              theta_0 = ifelse(self$predictor$with_intercept, coeffs[1], 0),
              theta_1 = coeffs[2],
              color = id,
              linetype = id
            ),
            mapping = aes(
              intercept = theta_0, 
              slope = theta_1, 
              col = color, 
              linetype = linetype
            ),
            show.legend = FALSE, # avoid overlap w initial geom_line() legend
            ...
          )
      }
    },
    
    addLegend = function() {
      # TODO implement proper legends
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

obj = rloss_dict$get("RL_l2")
obj$reset_xdim(2L)
obj$reset_fargs(Xmat = Xmat_univ, y = y_univ)

opt = OptimizerGD$new(
  obj, x_start = c(2, 1.6), print_trace = FALSE, lr = 0.0001
)
opt$optimize(steps = 1000)

preddy = LMPredictor$new("test", dt_univ, y ~ x_1, opt$x)
preddy$predict()

viz = VisualizerPrediction$new(preddy)
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
  type = "contour", line = list(width = 0)
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