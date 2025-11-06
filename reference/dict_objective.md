# Objective dictionary

Lookup table for reusable
[Objective](https://slds-lmu.github.io/vistool/reference/Objective.md)
definitions. The dictionary ships with vistool and comes pre-populated
with regularized regression objectives as well as a selection of
benchmark test functions when the optional `TestFunctions` package is
available. New objectives can be registered at runtime via
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
methods such as `$add()`.

## Usage

``` r
dict_objective
```

## Format

A
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
where each entry is an
[Objective](https://slds-lmu.github.io/vistool/reference/Objective.md)
instance.

## See also

[`obj()`](https://slds-lmu.github.io/vistool/reference/obj.md) for
convenient retrieval.

## Examples

``` r
dict_objective$get("TF_branin")
#> <Objective>
#>   Public:
#>     add_log_fun: function (l, label) 
#>     archive: active binding
#>     assert_x: function (x, ...) 
#>     clear_archive: function () 
#>     clone: function (deep = FALSE) 
#>     eval: function (x) 
#>     eval_store: function (x) 
#>     get_transform: function () 
#>     grad: function (x) 
#>     hess: function (x) 
#>     id: TF_branin
#>     initialize: function (id, fun, label = "f", xdim, lower = NA, upper = NA, 
#>     label: branin
#>     log_funs: active binding
#>     lower: -5 0
#>     minimize: TRUE
#>     set_transform: function (transform, test = TRUE) 
#>     transform_id: active binding
#>     upper: 10 15
#>     xdim: active binding
#>   Private:
#>     check_transform_domain: function (value) 
#>     ensure_transform_initialized: function () 
#>     eval_base_value: function (x) 
#>     get_transform_id: function () 
#>     grad_base_value: function (x) 
#>     hess_base_value: function (x) 
#>     p_archive: data.table, data.frame
#>     p_fargs: list
#>     p_fun: function (x, a = 1, b = 5.1/(4 * pi^2), cc = 5/pi, r = 6, s = 10, 
#>     p_gradient: NULL
#>     p_gradient_fallback: function (x, ...) 
#>     p_hessian: NULL
#>     p_hessian_fallback: function (x, ...) 
#>     p_label_base: branin
#>     p_log_funs: list
#>     p_transform: objective_transform
#>     p_xdim: 2
#>     p_xtest: 0 0
#>     validate_transform: function (transform) 
```
