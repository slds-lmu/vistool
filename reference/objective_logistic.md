# Logistic regression objective

Builds a logistic regression objective with optional elastic net
penalization.

## Usage

``` r
objective_logistic(
  x,
  y,
  weights = NULL,
  lambda = 0,
  alpha = 0,
  lambda_l1 = NULL,
  lambda_l2 = NULL,
  include_intercept = TRUE,
  penalize_intercept = FALSE,
  loss_scale = NULL,
  id = NULL,
  label = NULL,
  transform = objective_transform_identity()
)
```

## Arguments

- x:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html)) Design matrix
  without an intercept column. Rows correspond to observations.

- y:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Binary responses
  taking values in `{0, 1}`.

- weights:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Optional
  non-negative weights with length `nrow(x)`.

- lambda:

  (`numeric(1)`) Overall penalty strength used together with `alpha` for
  elastic net penalties.

- alpha:

  (`numeric(1)`) Elastic net mixing parameter in `[0, 1]`. `0`
  corresponds to ridge, `1` to lasso.

- lambda_l1:

  (`numeric(1)`) Optional direct specification of the L1 penalty weight.
  Overrides `lambda * alpha` when provided.

- lambda_l2:

  (`numeric(1)`) Optional direct specification of the L2 penalty weight.
  Overrides `lambda * (1 - alpha)` when provided.

- include_intercept:

  (`logical(1)`) If `TRUE`, an intercept column is prepended to `x`
  before constructing the objective.

- penalize_intercept:

  (`logical(1)`) If `FALSE` and `include_intercept = TRUE`, the
  intercept term is excluded from the penalties.

- loss_scale:

  (`numeric(1)`) Scaling factor applied to the empirical risk. Defaults
  to `1 / (2 * sum(weights))`.

- id:

  (`character(1)`) Identifier forwarded to
  [Objective](https://slds-lmu.github.io/vistool/reference/Objective.md).
  Defaults to `"logreg"`.

- label:

  (`character(1)`) Label used for the resulting objective. Defaults to
  `"logistic risk"`.

- transform:

  ([`objective_transform()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md))
  Optional scalar transformation applied to the empirical risk before it
  is returned. Defaults to
  [`objective_transform_identity()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md).

## Value

An
[Objective](https://slds-lmu.github.io/vistool/reference/Objective.md)
instance capturing the logistic regression risk.
