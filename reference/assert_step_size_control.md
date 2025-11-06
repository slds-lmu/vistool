# Assertion for the main signature of a `step_size_controlXX` function.

Assertion for the main signature of a `step_size_controlXX` function.

## Usage

``` r
assert_step_size_control(x, u, obj, opt)
```

## Arguments

- x:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The "old" point.

- u:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The update added
  to `x` without a step size control.

- obj:

  (`Objective`) The usd objective object.

- opt:

  (`Optimizer`) The optimizer object from which the function is called.
