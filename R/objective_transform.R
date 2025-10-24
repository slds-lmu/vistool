#' @title Objective transformations
#'
#' @description
#' Construct reusable scalar transformations for objective functions. A transform
#' is described by its value function and first and second derivatives with
#' respect to the scalar objective value. The returned object can be supplied to
#' [`Objective`] instances to obtain transformed values, gradients, and Hessians
#' without re-deriving model specific logic.
#'
#' @param value (`function(v)`) Scalar transformation applied to the base
#'   objective value.
#' @param d1 (`function(v)`) First derivative of the transformation with respect
#'   to `v`.
#' @param d2 (`function(v)`) Second derivative of the transformation with respect
#'   to `v`.
#' @param id (`character(1)`) Identifier used for logging and labels.
#' @param domain (`function(v)`) Optional guard that should throw an informative
#'   error if the transform is undefined for `v`. Called before evaluating the
#'   transformation or its derivatives.
#'
#' @return A list describing the transform. The list carries class
#'   `"objective_transform"` and contains the entries `value`, `d1`, `d2`, `id`,
#'   and `domain`.
#'
#' @examples
#' identity_transform = objective_transform_identity()
#' log_transform = objective_transform_log()
#'
#' identity_transform$value(10)
#' log_transform$d1(10)
#'
#' @export
objective_transform = function(value, d1, d2, id = "custom", domain = NULL) {
  value = assertFunction(value, args = "v")
  d1 = assertFunction(d1, args = "v")
  d2 = assertFunction(d2, args = "v")
  id = assertString(id)
  if (!is.null(domain)) {
    domain = assertFunction(domain, args = "v")
  }

  structure(
    list(value = value, d1 = d1, d2 = d2, id = id, domain = domain),
    class = "objective_transform"
  )
}

#' @rdname objective_transform
#' @export
objective_transform_identity = function() {
  objective_transform(
    value = function(v) v,
    d1 = function(v) rep(1, length(v)),
    d2 = function(v) rep(0, length(v)),
    id = "identity",
    domain = NULL
  )
}

#' @rdname objective_transform
#' @param eps (`numeric(1)`) Non-negative smoothing constant added before taking
#'   the logarithm. Use a positive value to guard against zero crossings.
#' @export
objective_transform_log = function(eps = 0) {
  eps = assertNumber(eps, lower = 0)

  guard = function(v) {
    if (any(v + eps <= 0)) {
      stop(sprintf(
        "Log transform undefined for objective value %s (eps = %g).",
        paste0(signif(v[v + eps <= 0], 5), collapse = ", "),
        eps
      ), call. = FALSE)
    }
    invisible(TRUE)
  }

  objective_transform(
    value = function(v) {
      guard(v)
      log(v + eps)
    },
    d1 = function(v) {
      guard(v)
      1 / (v + eps)
    },
    d2 = function(v) {
      guard(v)
      -1 / (v + eps)^2
    },
    id = if (eps == 0) "log" else sprintf("log+%g", eps),
    domain = guard
  )
}
