#' LMPredictor class
#'
#' This class is used to make predictions for linear regression models.
#' @export
LMPredictor = R6::R6Class(
  "LMPredictor",
  public = list(
    #' @field id (`character(1)` The id of the prediction.
    id = NULL,
    #' @field data Data to predict on.
    data = NULL,
    #' @field prediction Result of LM prediction.
    prediction = NULL,
    #' @field id ID of regression computation.
    formula = NULL,
    #' @field coeffs Regression coefficients
    coeffs = NULL,

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (`character(1)` The id of the objective.
    #' @param data (`data.table()`) The input data.table.
    #' @param formula (`formula` The regression formula.
    #' @param coeffs (`numeric()` The regression coefficients.
    initialize = function(id, data, formula, coeffs = NULL) {
      self$id = checkmate::assertString(id)
      self$formula = checkmate::assertFormula(formula)
      self$data = checkmate::assertDataTable(data)
      # private$p_num_predictors = length(
      #   stringr::str_squish(
      #     stringr::str_split_1(paste0(formula)[length(formula)], "\\+")
      #   )
      # )
      if (!is.null(coeffs)) self$coeffs = checkmate::assertNumeric(coeffs)
      else {
        self$coeffs = coefficientss(lm(self$formla, self$data))
      }
      private$p_target = paste0(formula)[2]
      xmat = model.matrix(formula, data = data)
      dim_diff = ncol(xmat) - length(self$coeffs)
      if (dim_diff == 0) {
        private$p_with_intercept = TRUE
        private$p_xmat = xmat
      }
      else if (dim_diff == 1) {
        private$p_with_intercept = FALSE
        private$p_xmat = xmat[, -1]
      }
      else stop("Number of coefficients does not match number of predictors.")
    },
    #' @description Makes LM predictions according to formula and coefficients.
    #' @param data (`data.table()`) Optional new data.
    predict = function(data = NULL) {
      if (is.null(data)) dt = private$p_xmat
      else {
        dt_in = checkmate::assertDataTable(dt)
        dt = model.matrix(self$formula, dt_in)
        if (!private$p_with_intercept) dt = dt[, -1]
        if (ncol(dt) != length(self$coeffs)) {
          stop(
            sprintf(
              "Data has %i cols %s but there are (%i) coefficients",
              ncol(dt),
              ifelse(private$p_with_intercept, "with intercept", ""),
              length(self$coefficients)
            )
          )
        }
      }
      pred = dt %*% self$coeffs
      self$prediction = data.table::data.table(dt)[
        , `:=`(target = self$data[[private$p_target]], pred = pred)
        ][, residual := target - pred]
      return(self$prediction)
    }
  ),
  private = list(
    # @field p_target (`character(1)`) Regression target.
    p_target = NULL,
    # @field p_xmat (`matrix()`) Model matrix.
    p_xmat = NULL,
    # @field p_with_intercept (`logical()`) Whether to include intercept term.
    p_with_intercept = NULL
  )
)

