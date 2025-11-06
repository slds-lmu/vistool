# Conduct linear decay to adjust the update. See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler

Conduct linear decay to adjust the update. See
https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler

## Usage

``` r
step_size_control_decay_linear(iter_zero = 100L)
```

## Arguments

- iter_zero:

  (`integer(1)`) The iteration at which the update is shrinked to zero.

## Value

The step size as number.
