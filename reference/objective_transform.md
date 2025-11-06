# Objective transformations

Construct reusable scalar transformations for objective functions. A
transform is described by its value function and first and second
derivatives with respect to the scalar objective value. The returned
object can be supplied to
[`Objective`](https://slds-lmu.github.io/vistool/reference/Objective.md)
instances to obtain transformed values, gradients, and Hessians without
re-deriving model specific logic.

## Usage

``` r
objective_transform(value, d1, d2, id = "custom", domain = NULL)

objective_transform_identity()

objective_transform_log(eps = 0)
```

## Arguments

- value:

  (`function(v)`) Scalar transformation applied to the base objective
  value.

- d1:

  (`function(v)`) First derivative of the transformation with respect to
  `v`.

- d2:

  (`function(v)`) Second derivative of the transformation with respect
  to `v`.

- id:

  (`character(1)`) Identifier used for logging and labels.

- domain:

  (`function(v)`) Optional guard that should throw an informative error
  if the transform is undefined for `v`. Called before evaluating the
  transformation or its derivatives.

- eps:

  (`numeric(1)`) Non-negative smoothing constant added before taking the
  logarithm. Use a positive value to guard against zero crossings.

## Value

A list describing the transform. The list carries class
`"objective_transform"` and contains the entries `value`, `d1`, `d2`,
`id`, and `domain`.

## Examples

``` r
identity_transform = objective_transform_identity()
log_transform = objective_transform_log()

identity_transform$value(10)
#> [1] 10
log_transform$d1(10)
#> [1] 0.1
```
