# Newton-Raphson optimizer

Implements Newton-Raphson updates by combining gradients and Hessians of
the objective with optional Armijo backtracking. Falls back to the
gradient direction when the Hessian is singular or indefinite.

## Super class

[`vistool::Optimizer`](https://slds-lmu.github.io/vistool/reference/Optimizer.md)
-\> `OptimizerNR`

## Methods

### Public methods

- [`OptimizerNR$new()`](#method-OptimizerNR-new)

- [`OptimizerNR$optimize()`](#method-OptimizerNR-optimize)

- [`OptimizerNR$step_size()`](#method-OptimizerNR-step_size)

- [`OptimizerNR$gamma()`](#method-OptimizerNR-gamma)

- [`OptimizerNR$tau()`](#method-OptimizerNR-tau)

- [`OptimizerNR$clone()`](#method-OptimizerNR-clone)

Inherited methods

- [`vistool::Optimizer$prepare_update_for_archive()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-prepare_update_for_archive)
- [`vistool::Optimizer$set_x()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-set_x)
- [`vistool::Optimizer$update_archive()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-update_archive)

------------------------------------------------------------------------

### Method `new()`

Creates a new Newton-Raphson optimizer.

#### Usage

    OptimizerNR$new(
      objective,
      x_start,
      step_size = 1,
      gamma = 0.99,
      tau = 0.5,
      max_backtracks = 20L,
      fallback = c("gradient", "stop"),
      id = "Newton-Raphson",
      print_trace = TRUE
    )

#### Arguments

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `x_start`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Start value of the optimization. Note, after the first call of
  `$optimize()` the last value is used to continue optimization. Get
  this value with `$x`.

- `x_start`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Start value of the optimization. Note, after the first call of
  `$optimize()` the last value is used to continue optimization. Get
  this value with `$x`.

- `step_size`:

  (`numeric(1)`) Initial step size used as the starting value for line
  search. Default `1`.

- `gamma`:

  (`numeric(1)`) Armijo slope parameter in `(0, 1)`.

- `tau`:

  (`numeric(1)`) Multiplicative backtracking factor in `(0, 1)`.

- `max_backtracks`:

  (`integer(1)`) Maximum number of Armijo backtracking attempts.

- `fallback`:

  (`character(1)`) Strategy when the Hessian cannot be inverted. One of
  "gradient" or "stop".

- `id`:

  (`character(1)`)  
  Id of the object.

- `id`:

  (`character(1)`)  
  Id of the object.

- `print_trace`:

  (`logical(1)`)  
  Indicator whether to print the status of `$optimize()`.

- `print_trace`:

  (`logical(1)`)  
  Indicator whether to print the status of `$optimize()`.

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Optimize the associated objective for a number of steps.

#### Usage

    OptimizerNR$optimize(steps = 1L, step_size_control = NULL, minimize = NULL)

#### Arguments

- `steps`:

  (`integer(1)`)  
  Number of steps/iterations.

- `step_size_control`:

  (`function()`) Optional callback with signature `(x, u, obj, opt)`
  returning a scalar step size. Defaults to Armijo backtracking that
  uses the configured `gamma`, `tau`, and `max_backtracks` parameters.

- `minimize`:

  (`logical(1)`) Whether to minimize the objective. Defaults to
  `self$objective$minimize`. Maximization is not supported and triggers
  an error.

------------------------------------------------------------------------

### Method `step_size()`

Access or update the base step size.

#### Usage

    OptimizerNR$step_size(x)

#### Arguments

- `x`:

  (`numeric(1)`) Replacement value. Omit to retrieve the current
  setting.

------------------------------------------------------------------------

### Method [`gamma()`](https://rdrr.io/r/base/Special.html)

Access or update the Armijo slope parameter.

#### Usage

    OptimizerNR$gamma(x)

#### Arguments

- `x`:

  (`numeric(1)`)

------------------------------------------------------------------------

### Method `tau()`

Access or update the Armijo shrink factor.

#### Usage

    OptimizerNR$tau(x)

#### Arguments

- `x`:

  (`numeric(1)`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerNR$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
