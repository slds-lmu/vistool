# Conduct a step-wise decay to adjust the update. See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler

Conduct a step-wise decay to adjust the update. See
https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler

## Usage

``` r
step_size_control_decay_steps(drop_rate = 0.1, every_iter = 10)
```

## Arguments

- drop_rate:

  (`numeric(1)`) The rate indicating how much the learning rate is
  reduced after each `every_iter`.

- every_iter:

  (`integer(1)`) Number indicates after how many iterations the learning
  rate is reduced by `drop_rate`.

## Value

The step size as number.
