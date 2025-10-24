test_that("objective_logistic matches manual calculations", {
  x = matrix(c(
    1, -0.5,
    -1.5, 0.75,
    0.25, 1.25,
    -0.75, -0.25
  ), ncol = 2, byrow = TRUE)
  y = c(1, 0, 1, 0)
  weights = c(1, 2, 1.5, 0.5)

  lambda = 0.7
  alpha = 0.3
  obj = objective_logistic(
    x = x,
    y = y,
    weights = weights,
    lambda = lambda,
    alpha = alpha,
    include_intercept = TRUE,
    penalize_intercept = FALSE
  )

  theta = c(0.2, -0.4, 0.1)
  x_aug = cbind(1, x)
  eta = drop(x_aug %*% theta)
  sum_weights = sum(weights)
  loss_scale = 1 / (2 * sum_weights)
  lambda_l1 = lambda * alpha
  lambda_l2 = lambda * (1 - alpha)

  manual_value = loss_scale * sum(weights * (log1p(exp(eta)) - y * eta)) +
    0.5 * lambda_l2 * sum(theta[-1]^2) + lambda_l1 * sum(abs(theta[-1]))
  expect_equal(obj$eval(theta), manual_value, tolerance = 1e-12)

  probs = plogis(eta)
  diff = (probs - y) * weights
  manual_grad = as.numeric(loss_scale * crossprod(x_aug, diff))
  manual_grad[-1] = manual_grad[-1] + lambda_l2 * theta[-1] + lambda_l1 * sign(theta[-1])
  expect_equal(obj$grad(theta), manual_grad, tolerance = 1e-12)

  weight_vec = weights * probs * (1 - probs)
  scaled_design = x_aug * sqrt(weight_vec)
  manual_hess = loss_scale * crossprod(scaled_design, scaled_design)
  dimnames(manual_hess) = NULL
  manual_hess[-1, -1] = manual_hess[-1, -1] + lambda_l2 * diag(length(theta) - 1)
  expect_equal(obj$hess(theta), manual_hess, tolerance = 1e-12)

  expect_s3_class(obj("logreg", x = x, y = y), "Objective")
})

test_that("objective_linear matches manual calculations", {
  x = matrix(c(
    0.5, -1,
    1.5, 0.25,
    -0.75, 0.5,
    0.25, -1.25
  ), ncol = 2, byrow = TRUE)
  y = c(0.5, -1.2, 1.0, 2.5)
  weights = c(1, 0.4, 1.6, 0.8)

  lambda = 0.4
  alpha = 0.75
  obj = objective_linear(
    x = x,
    y = y,
    weights = weights,
    lambda = lambda,
    alpha = alpha,
    include_intercept = FALSE
  )

  theta = c(0.3, -0.6)
  sum_weights = sum(weights)
  loss_scale = 1 / (2 * sum_weights)
  residual = y - drop(x %*% theta)
  lambda_l1 = lambda * alpha
  lambda_l2 = lambda * (1 - alpha)

  manual_value = loss_scale * sum(weights * residual^2) +
    0.5 * lambda_l2 * sum(theta^2) + lambda_l1 * sum(abs(theta))
  expect_equal(obj$eval(theta), manual_value, tolerance = 1e-12)

  manual_grad = -2 * loss_scale * as.numeric(crossprod(x, weights * residual))
  manual_grad = manual_grad + lambda_l2 * theta + lambda_l1 * sign(theta)
  expect_equal(obj$grad(theta), manual_grad, tolerance = 1e-12)

  scaled_design = x * sqrt(weights)
  manual_hess = 2 * loss_scale * crossprod(scaled_design, scaled_design)
  dimnames(manual_hess) = NULL
  manual_hess = manual_hess + lambda_l2 * diag(length(theta))
  expect_equal(obj$hess(theta), manual_hess, tolerance = 1e-12)

  expect_s3_class(obj("linreg", x = x, y = y), "Objective")
})

test_that("dict_objective can be summarized without arguments", {
  tab = data.table::as.data.table(dict_objective)
  tab_df = as.data.frame(tab)
  expect_true("logreg" %in% tab_df$key)
  expect_true("linreg" %in% tab_df$key)
  expect_identical(tab_df$label[tab_df$key == "logreg"], "logistic risk")
  expect_identical(tab_df$label[tab_df$key == "linreg"], "squared error risk")
})
