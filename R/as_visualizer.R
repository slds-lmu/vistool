#' @title Convert to visualizer
#'
#' @description
#' `as_visualizer()` inspects its input and instantiates the matching visualizer class.
#' All common arguments are validated first so that incompatible combinations fail fast
#' with informative messages before any expensive rendering work is scheduled.
#'
#' @template param_x
#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_hypothesis
#' @template param_domain
#' @template param_n_points
#'
#' @param y_pred (`numeric()`)
#'   Predicted values (for loss function visualizations).
#' @param y_true (`numeric()`)
#'   True values (for loss function visualizations).
#' @param input_type (`character(1)`)
#'   One of `"auto"` (default), `"score"`, or `"probability"`. Passed through to
#'   the loss visualizer.
#' @param y_curves (`character(1)`)
#'   Which response curve(s) to draw when `input_type = "probability"`.
#'   One of `"both"`, `"y1"`, or `"y0"`.
#' @template param_learner
#' @param retrain (`logical(1)`)
#'   If a learner is supplied, whether it should be (re)trained on the task (`TRUE`, default)
#'   or reused as-is (`FALSE`). If `FALSE` but the learner is untrained, a warning is issued
#'   and training is performed.
#'
#' @return An object inheriting from a Visualizer class (e.g. `VisualizerModel`,
#' `VisualizerObj`, or `VisualizerLossFuns`) depending on the input and selected type.
#'
#' @details
#' By default (`type = "auto"`), `as_visualizer()` chooses a ggplot2 backend for 1D and 2D
#' inputs. For 2D models or objectives you can opt into interactive plotly surfaces via
#' `type = "surface"`. All inputs are checked by a shared validation helper that enforces
#' mutual exclusivity of `learner`/`hypothesis`, ensures limits are finite and ordered, and
#' rejects parameters that are irrelevant for the chosen input.
#'
#' @section Supported inputs:
#' \tabular{llll}{
#'   Input & Auto `type` & Allowed overrides & Notes \\
#'   `Task` & `"1d"` (1 feature) or `"2d"` (2 features) & `type = "surface"` (2D only) & Exactly one of `learner`/`hypothesis`; `domain` is ignored and therefore rejected. \\
#'   `Hypothesis` & matches `input_dim` & `type = "surface"` (2D only) & Requires complete `domain`; `learner` not supported. \\
#'   `Objective` & matches `xdim` when available & `type = "surface"` (2D only) & `learner`/`hypothesis` not supported. \\
#'   `LossFunction` or list of losses & always `"1d"` & `n_points` & `input_type`, `y_pred`, `y_true`, `y_curves`, and `n_points` are forwarded to the loss visualizer. \\
#' }
#'
#' @section Validation:
#' Common argument validation is centralized so that:
#' * unused parameters (e.g. 2D limits supplied for 1D losses) raise an error,
#' * limits supplied manually are finite, strictly ordered, and match the dimensionality,
#' * high grid resolutions for interactive surfaces trigger explicit warnings, and
#' * probability inputs for loss visualizations are checked for consistency of
#'   `input_type`, `y_pred`, `y_true`, and `y_curves`.
#'
#' @seealso
#' [VisualizerModel], [VisualizerSurfaceModel], [VisualizerObj], [VisualizerSurfaceObj],
#' [VisualizerLossFuns]
#'
#' @examples
#' obj = obj("TF_branin")
#' vis = as_visualizer(obj, type = "2d")
#'
#' # High resolutions on surfaces warn before plotting
#' tryCatch(
#'   as_visualizer(obj, type = "surface", n_points = 256L),
#'   warning = function(w) message("Warning: ", conditionMessage(w))
#' )
#'
#' @export
as_visualizer = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                         padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                         input_type = "auto", y_curves = "both", learner = NULL,
                         hypothesis = NULL, domain = NULL, retrain = TRUE) {
  UseMethod("as_visualizer")
}

#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                              padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                              input_type = "auto", y_curves = "both", learner = NULL,
                              hypothesis = NULL, domain = NULL, retrain = TRUE) {
  payload = validate_visualizer_args("task", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Hypothesis = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                    padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                                    input_type = "auto", y_curves = "both", learner = NULL,
                                    hypothesis = NULL, domain = NULL, retrain = TRUE) {
  payload = validate_visualizer_args("hypothesis", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Objective = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                   padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                                   input_type = "auto", y_curves = "both", learner = NULL,
                                   hypothesis = NULL, domain = NULL, retrain = TRUE) {
  payload = validate_visualizer_args("objective", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                      padding = 0, n_points = 1000L, y_pred = NULL, y_true = NULL,
                                      input_type = "auto", y_curves = "both", learner = NULL,
                                      hypothesis = NULL, domain = NULL, retrain = TRUE) {
  payload = validate_visualizer_args("loss", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.list = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                              padding = 0, n_points = 1000L, y_pred = NULL, y_true = NULL,
                              input_type = "auto", y_curves = "both", learner = NULL,
                              hypothesis = NULL, domain = NULL, retrain = TRUE) {
  payload = validate_visualizer_args("loss_list", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

# -------------------------------------------------------------------------
# Validation helpers

#' @keywords internal
validate_visualizer_args = function(kind, args) {
  kind = match.arg(kind, c("task", "hypothesis", "objective", "loss", "loss_list"))
  switch(kind,
    task = validate_visualizer_task(args),
    hypothesis = validate_visualizer_hypothesis(args),
    objective = validate_visualizer_objective(args),
    loss = validate_visualizer_loss(args, multiple = FALSE),
    loss_list = validate_visualizer_loss(args, multiple = TRUE)
  )
}

#' @keywords internal
construct_visualizer = function(plan) {
  if (length(plan$warnings)) {
    for (msg in plan$warnings) {
      warning(msg, call. = FALSE)
    }
  }

  switch(plan$visualizer,
    VisualizerModel = do.call(VisualizerModel$new, plan$args),
    VisualizerSurfaceModel = do.call(VisualizerSurfaceModel$new, plan$args),
    VisualizerObj = do.call(VisualizerObj$new, plan$args),
    VisualizerSurfaceObj = do.call(VisualizerSurfaceObj$new, plan$args),
    VisualizerLossFuns = do.call(VisualizerLossFuns$new, plan$args),
    stop(sprintf("Unknown visualizer '%s'.", plan$visualizer))
  )
}

validate_visualizer_task = function(args) {
  task = args$x
  learner = args$learner
  hypothesis = args$hypothesis
  call = args$call
  domain = args$domain

  ensure_unused_args_absent(call, c("y_pred", "y_true", "input_type", "y_curves"), "for task visualizations")

  if (!is.null(learner) && !is.null(hypothesis)) {
    stop("Provide exactly one of 'learner' or 'hypothesis', not both.")
  }
  if (is.null(learner) && is.null(hypothesis)) {
    stop("One of 'learner' or 'hypothesis' is required for Task visualizations.")
  }
  if (!is.null(domain) || arg_supplied(call, "domain")) {
    stop("Argument 'domain' is not supported when visualizing a Task; limits are derived from task data.")
  }

  padding = checkmate::assert_number(args$padding, lower = 0)
  retrain = checkmate::assert_flag(args$retrain)
  n_points = as.integer(checkmate::assert_integerish(args$n_points, lower = 10, len = 1))

  type = tolower(args$type)
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "surface"))

  feature_names = task$feature_names
  n_features = length(feature_names)
  if (n_features == 0) {
    stop("Task must contain at least one feature to be visualized.")
  }

  resolved_type = resolve_task_type(type, n_features)

  if (n_features == 1 && arg_supplied(call, "x2_limits")) {
    stop("Argument 'x2_limits' is not applicable to 1D tasks.")
  }

  task_data = task$data(cols = feature_names)

  x1_limits = normalize_limits(args$x1_limits, "x1_limits")
  if (is.null(x1_limits)) {
    x1_limits = range_from_vector(task_data[[feature_names[1]]], feature_names[1])
  }

  if (n_features == 1) {
    x2_limits = NULL
    x1_limits = apply_padding_if_needed(x1_limits, padding)
  } else {
    x2_limits = normalize_limits(args$x2_limits, "x2_limits")
    if (is.null(x2_limits)) {
      x2_limits = range_from_vector(task_data[[feature_names[2]]], feature_names[2])
    }
  }

  warnings = maybe_warn_surface(resolved_type, n_points)

  list(
    visualizer = if (resolved_type == "surface") "VisualizerSurfaceModel" else "VisualizerModel",
    args = list(
      task = task,
      learner = learner,
      x1_limits = x1_limits,
      x2_limits = x2_limits,
      padding = padding,
      n_points = n_points,
      hypothesis = hypothesis,
      domain = NULL,
      retrain = retrain
    ),
    warnings = warnings
  )
}

validate_visualizer_hypothesis = function(args) {
  hyp = args$x
  assert_hypothesis(hyp)

  learner = args$learner
  supplied_hypothesis_argument = args$hypothesis
  call = args$call

  ensure_unused_args_absent(call, c("y_pred", "y_true", "input_type", "y_curves"), "for hypothesis visualizations")

  if (!is.null(learner)) {
    stop("Argument 'learner' is not supported when visualizing a Hypothesis.")
  }
  if (!is.null(supplied_hypothesis_argument)) {
    stop("Argument 'hypothesis' must be NULL when x is already a Hypothesis.")
  }

  padding = checkmate::assert_number(args$padding, lower = 0)
  n_points = as.integer(checkmate::assert_integerish(args$n_points, lower = 10, len = 1))

  type = tolower(args$type)
  resolved_type = resolve_hypothesis_type(type, hyp$input_dim)

  if (hyp$input_dim == 1 && arg_supplied(call, "x2_limits")) {
    stop("Argument 'x2_limits' is not applicable to 1D hypotheses.")
  }

  domain = args$domain
  if (is.null(domain)) domain = hyp$domain
  if (is.null(domain)) {
    stop("'domain' is required when visualizing a hypothesis without a Task.")
  }
  domain = validate_domain(domain, hyp$predictors)

  x1_limits = normalize_limits(args$x1_limits, "x1_limits")
  x2_limits = normalize_limits(args$x2_limits, "x2_limits")

  if (hyp$input_dim == 1) {
    if (is.null(x1_limits)) {
      x1_limits = domain[[hyp$predictors[1]]]
    }
    x1_limits = apply_padding_if_needed(x1_limits, padding)
    x2_limits = NULL
  } else {
    if (is.null(x1_limits)) {
      x1_limits = domain[[hyp$predictors[1]]]
    }
    if (is.null(x2_limits)) {
      x2_limits = domain[[hyp$predictors[2]]]
    }
  }

  warnings = maybe_warn_surface(resolved_type, n_points)
  if (arg_supplied(call, "retrain") && !identical(args$retrain, TRUE)) {
    warnings = c(warnings, "Argument retrain is ignored for Hypothesis visualizations.")
  }

  list(
    visualizer = if (resolved_type == "surface") "VisualizerSurfaceModel" else "VisualizerModel",
    args = list(
      task = NULL,
      learner = NULL,
      x1_limits = x1_limits,
      x2_limits = x2_limits,
      padding = padding,
      n_points = n_points,
      hypothesis = hyp,
      domain = domain,
      retrain = TRUE
    ),
    warnings = warnings
  )
}

validate_visualizer_objective = function(args) {
  objective = args$x
  checkmate::assert_r6(objective, "Objective")

  learner = args$learner
  if (!is.null(learner)) {
    stop("Argument 'learner' is not supported for Objective visualizations.")
  }

  call = args$call
  ensure_unused_args_absent(call, c("y_pred", "y_true", "input_type", "y_curves", "domain", "hypothesis", "retrain"), "for objective visualizations")

  padding = checkmate::assert_number(args$padding, lower = 0)
  n_points = as.integer(checkmate::assert_integerish(args$n_points, lower = 10, len = 1))

  type = tolower(args$type)
  resolved_type = resolve_objective_type(type, objective$xdim)

  x1_limits = normalize_limits(args$x1_limits, "x1_limits")
  x2_limits = normalize_limits(args$x2_limits, "x2_limits")

  constructor = NULL
  constructor_args = NULL

  if (resolved_type == "1d") {
    if (is.null(x1_limits)) {
      bounds = c(get_objective_bound(objective$lower, 1L), get_objective_bound(objective$upper, 1L))
      if (any(is.na(bounds))) {
        stop("Objective bounds not available; please specify 'x1_limits'.")
      }
      x1_limits = bounds
    }
    x1_limits = apply_padding_if_needed(x1_limits, padding)
    if (arg_supplied(call, "x2_limits")) {
      stop("Argument 'x2_limits' is not applicable to 1D objectives.")
    }
    constructor = "VisualizerObj"
    constructor_args = list(
      objective = objective,
      x1_limits = x1_limits,
      x2_limits = NULL,
      padding = padding,
      n_points = n_points,
      type = "1d"
    )
  } else if (resolved_type == "2d") {
    if (is.null(x1_limits)) {
      bounds = c(get_objective_bound(objective$lower, 1L), get_objective_bound(objective$upper, 1L))
      if (any(is.na(bounds))) {
        stop("Objective bounds not available; please specify 'x1_limits'.")
      }
      x1_limits = bounds
    }
    if (is.null(x2_limits)) {
      bounds = c(get_objective_bound(objective$lower, 2L), get_objective_bound(objective$upper, 2L))
      if (any(is.na(bounds))) {
        stop("Objective bounds not available; please specify 'x2_limits'.")
      }
      x2_limits = bounds
    }
    constructor = "VisualizerObj"
    constructor_args = list(
      objective = objective,
      x1_limits = x1_limits,
      x2_limits = x2_limits,
      padding = padding,
      n_points = n_points,
      type = "2d"
    )
  } else {
    if (is.null(x1_limits)) {
      bounds = c(get_objective_bound(objective$lower, 1L), get_objective_bound(objective$upper, 1L))
      if (any(is.na(bounds))) {
        stop("Objective bounds not available; please specify 'x1_limits'.")
      }
      x1_limits = bounds
    }
    if (is.null(x2_limits)) {
      bounds = c(get_objective_bound(objective$lower, 2L), get_objective_bound(objective$upper, 2L))
      if (any(is.na(bounds))) {
        stop("Objective bounds not available; please specify 'x2_limits'.")
      }
      x2_limits = bounds
    }
    constructor = "VisualizerSurfaceObj"
    constructor_args = list(
      objective = objective,
      x1_limits = x1_limits,
      x2_limits = x2_limits,
      padding = padding,
      n_points = n_points
    )
  }

  warnings = maybe_warn_surface(resolved_type, n_points)

  list(
    visualizer = constructor,
    args = constructor_args,
    warnings = warnings
  )
}

validate_visualizer_loss = function(args, multiple) {
  if (!is.null(args$learner)) {
    stop("Argument 'learner' is not supported for loss function visualizations.")
  }

  disallowed = c("x1_limits", "x2_limits", "padding", "domain", "hypothesis", "retrain")
  for (nm in disallowed) {
    if (arg_supplied(args$call, nm)) {
      stop(sprintf("Argument '%s' is not supported for loss function visualizations.", nm))
    }
  }

  type = tolower(args$type)
  checkmate::assert_choice(type, choices = c("auto", "1d"))

  input_type = tolower(args$input_type)
  checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
  checkmate::assert_choice(args$y_curves, choices = c("both", "y1", "y0"))

  losses = if (multiple) args$x else list(args$x)
  is_loss = vapply(losses, function(obj) inherits(obj, "LossFunction"), logical(1))
  if (!all(is_loss)) {
    bad = which(!is_loss)
    stop(sprintf("The following elements of 'x' are not LossFunction objects: %s", paste(bad, collapse = ", ")))
  }

  loss_task_types = unique(vapply(losses, function(obj) obj$task_type, character(1)))

  defaults = vapply(losses, function(obj) obj$input_default, character(1))

  if (arg_supplied(args$call, "y_curves")) {
    resolved_scale = if (input_type == "auto") {
      unique(defaults)
    } else {
      input_type
    }
    if (length(resolved_scale) != 1L) {
      stop("Argument 'y_curves' can only be used when 'input_type' resolves to a single scale.")
    }
    if (resolved_scale != "probability") {
      stop("Argument 'y_curves' is only supported when 'input_type' is 'probability'.")
    }
  }

  n_points = as.integer(checkmate::assert_integerish(args$n_points, len = 1, lower = 10))

  y_pred = args$y_pred
  y_true = args$y_true
  if (!is.null(y_pred)) checkmate::assert_numeric(y_pred, any.missing = FALSE)
  if (!is.null(y_true)) checkmate::assert_numeric(y_true, any.missing = FALSE)
  if (!is.null(y_pred) && !is.null(y_true)) {
    len_pred = length(y_pred)
    len_true = length(y_true)
    if (len_pred != len_true) {
      if (len_pred == 1L) {
        y_pred = rep(y_pred, len_true)
      } else if (len_true == 1L) {
        y_true = rep(y_true, len_pred)
      } else {
        stop("'y_pred' and 'y_true' must have the same length when both are supplied.")
      }
    }
  }

  if (input_type == "probability" || (input_type == "auto" && length(unique(defaults)) == 1L && unique(defaults) == "probability")) {
    if (!all(loss_task_types == "classif")) {
      stop("Probability-scale inputs are only valid for classification losses.")
    }
    if (!is.null(y_pred) && any(y_pred < -1e-8 | y_pred > 1 + 1e-8)) {
      stop("'y_pred' must lie within [0, 1] when 'input_type' is 'probability'.")
    }
  }

  list(
    visualizer = "VisualizerLossFuns",
    args = list(
      losses = losses,
      y_pred = y_pred,
      y_true = y_true,
      input_type = input_type,
      n_points = n_points
    ),
    warnings = character(0)
  )
}

resolve_task_type = function(requested, n_features) {
  if (requested == "auto") {
    if (n_features == 1) {
      return("1d")
    }
    if (n_features == 2) {
      return("2d")
    }
    stop("Auto visualization only supports 1D and 2D tasks. For higher dimensions, please specify 'type' explicitly.")
  }
  if (requested == "1d" && n_features != 1) {
    stop("1D visualization requires a task with exactly 1 feature.")
  }
  if (requested %in% c("2d", "surface") && n_features != 2) {
    stop("2D and surface visualizations require a task with exactly 2 features.")
  }
  requested
}

resolve_hypothesis_type = function(requested, input_dim) {
  checkmate::assert_choice(requested, choices = c("auto", "1d", "2d", "surface"))
  if (requested == "auto") {
    if (input_dim == 1) {
      return("1d")
    }
    if (input_dim == 2) {
      return("2d")
    }
    stop("Hypothesis supports only 1D or 2D inputs.")
  }
  if (requested == "1d" && input_dim != 1) {
    stop("1D visualization requires a hypothesis with exactly 1 predictor.")
  }
  if (requested == "2d" && input_dim != 2) {
    stop("2D visualization requires a hypothesis with exactly 2 predictors.")
  }
  if (requested == "surface" && input_dim != 2) {
    stop("Surface visualization requires a 2D hypothesis.")
  }
  requested
}

resolve_objective_type = function(requested, n_dim) {
  checkmate::assert_choice(requested, choices = c("auto", "1d", "2d", "surface"))
  if (requested == "auto") {
    if (is.na(n_dim)) {
      stop("Auto visualization type detection is not supported for objectives with unknown dimensionality; please specify 'type' explicitly (1d, 2d, or surface).")
    }
    if (n_dim == 1) {
      return("1d")
    }
    if (n_dim == 2) {
      return("2d")
    }
    stop("Auto visualization only supports 1D and 2D objectives. For higher dimensions, please specify 'type' explicitly.")
  }
  if (!is.na(n_dim)) {
    if (requested == "1d" && n_dim != 1) {
      stop("1D visualization requires an objective with exactly 1 dimension.")
    }
    if (requested %in% c("2d", "surface") && n_dim != 2) {
      stop("2D and surface visualizations require an objective with exactly 2 dimensions.")
    }
  }
  requested
}

normalize_limits = function(limits, name) {
  if (is.null(limits)) {
    return(NULL)
  }
  checkmate::assert_numeric(limits, len = 2, any.missing = FALSE)
  if (!all(is.finite(limits))) {
    stop(sprintf("Argument '%s' must contain finite values.", name))
  }
  if (limits[1] >= limits[2]) {
    stop(sprintf("Argument '%s' must satisfy lower < upper.", name))
  }
  limits
}

range_from_vector = function(values, label) {
  if (is.null(values) || length(values) == 0) {
    stop(sprintf("Unable to derive limits for '%s'; no data available.", label))
  }
  if (is.factor(values)) {
    values = as.numeric(values)
  }
  rng = range(values, na.rm = TRUE)
  if (any(!is.finite(rng))) {
    stop(sprintf("Unable to derive finite limits for '%s'; please provide them explicitly.", label))
  }
  if (rng[1] == rng[2]) {
    base = if (rng[1] == 0) 1 else abs(rng[1])
    delta = base * 0.05
    rng = c(rng[1] - delta, rng[2] + delta)
  }
  rng
}

apply_padding_if_needed = function(limits, padding) {
  if (padding <= 0) {
    return(limits)
  }
  width = limits[2] - limits[1]
  if (width <= 0) {
    stop("Cannot apply padding to zero-width limits.")
  }
  pad = width * padding
  if (!is.finite(pad)) {
    stop("Padding produced non-finite limits; please choose a smaller padding value.")
  }
  c(limits[1] - pad, limits[2] + pad)
}

maybe_warn_surface = function(resolved_type, n_points, threshold = 200L) {
  if (resolved_type == "surface" && n_points > threshold) {
    return(sprintf("Requested surface grid with n_points = %d; values above %d can cause slow interactive rendering.", n_points, threshold))
  }
  character(0)
}

validate_domain = function(domain, predictors) {
  checkmate::assert_list(domain, names = "named", types = "numeric")
  missing = setdiff(predictors, names(domain))
  if (length(missing)) {
    stop(sprintf("Domain is missing limits for predictor(s): %s", paste(missing, collapse = ", ")))
  }
  for (nm in predictors) {
    vals = domain[[nm]]
    checkmate::assert_numeric(vals, len = 2, any.missing = FALSE)
    if (!all(is.finite(vals))) {
      stop(sprintf("Domain values for '%s' must be finite.", nm))
    }
    if (vals[1] >= vals[2]) {
      stop(sprintf("Domain limits for '%s' must satisfy lower < upper.", nm))
    }
  }
  domain
}

arg_supplied = function(call, name) {
  call_names = names(call)
  if (!is.null(call_names) && name %in% call_names) {
    return(TRUE)
  }
  dots = call[["..."]]
  if (!is.null(dots)) {
    dot_names = names(dots)
    if (!is.null(dot_names) && name %in% dot_names) {
      return(TRUE)
    }
  }
  FALSE
}

get_objective_bound = function(bound_vec, index) {
  if (length(bound_vec) < index) {
    return(NA_real_)
  }
  bound_vec[index]
}

ensure_unused_args_absent = function(call, names, context) {
  for (nm in names) {
    if (arg_supplied(call, nm)) {
      stop(sprintf("Argument '%s' is not supported %s.", nm, context))
    }
  }
}
