# task validation enforces compatibility and derives limits

    Code
      as_visualizer(task_2d, learner = learner, hypothesis = hyp2d)
    Condition
      Error in `validate_visualizer_task()`:
      ! Provide exactly one of 'learner' or 'hypothesis', not both.

---

    Code
      as_visualizer(task_2d, learner = learner, domain = list(gear = c(0, 1)))
    Condition
      Error in `validate_visualizer_task()`:
      ! Argument 'domain' is not supported when visualizing a Task; limits are derived from task data.

---

    Code
      as_visualizer(task_2d, learner = learner, y_pred = 1)
    Condition
      Error in `ensure_unused_args_absent()`:
      ! Argument 'y_pred' is not supported for task visualizations.

---

    Code
      as_visualizer(task_1d, learner = learner, x2_limits = c(0, 1))
    Condition
      Error in `validate_visualizer_task()`:
      ! Argument 'x2_limits' is not applicable to 1D tasks.

# hypothesis validation requires domain coverage

    Code
      as_visualizer(hyp_no_domain)
    Condition
      Error in `validate_visualizer_hypothesis()`:
      ! 'domain' is required when visualizing a hypothesis without a Task.

---

    Code
      as_visualizer(hyp1d, input_type = "score")
    Condition
      Error in `ensure_unused_args_absent()`:
      ! Argument 'input_type' is not supported for hypothesis visualizations.

---

    Code
      as_visualizer(hyp1d, y_pred = 1)
    Condition
      Error in `ensure_unused_args_absent()`:
      ! Argument 'y_pred' is not supported for hypothesis visualizations.

---

    Code
      as_visualizer(hyp1d, x2_limits = c(0, 1))
    Condition
      Error in `validate_visualizer_hypothesis()`:
      ! Argument 'x2_limits' is not applicable to 1D hypotheses.

# objective validation enforces dimensional constraints

    Code
      as_visualizer(obj_1d, type = "2d")
    Condition
      Error in `resolve_objective_type()`:
      ! 2D and surface visualizations require an objective with exactly 2 dimensions.

---

    Code
      as_visualizer(obj_2d, type = "1d")
    Condition
      Error in `resolve_objective_type()`:
      ! 1D visualization requires an objective with exactly 1 dimension.

---

    Code
      as_visualizer(obj_unknown)
    Condition
      Error in `resolve_objective_type()`:
      ! Auto visualization type detection is not supported for objectives with unknown dimensionality; please specify 'type' explicitly (1d, 2d, or surface).

# loss visualizer validation rejects incompatible inputs

    Code
      as_visualizer(loss_reg, n_points = 5)
    Condition
      Error in `validate_visualizer_loss()`:
      ! Assertion on 'args$n_points' failed: Element 1 is not >= 10.

---

    Code
      as_visualizer(loss_reg, y_curves = "y1")
    Condition
      Error in `validate_visualizer_loss()`:
      ! Argument 'y_curves' is only supported when 'input_type' is 'probability'.

---

    Code
      as_visualizer(loss_reg, y_pred = 1:2, y_true = 1:3)
    Condition
      Error in `validate_visualizer_loss()`:
      ! 'y_pred' and 'y_true' must have the same length when both are supplied.

---

    Code
      as_visualizer(loss_ce, input_type = "probability", y_pred = c(-0.1, 0.5))
    Condition
      Error in `validate_visualizer_loss()`:
      ! 'y_pred' must lie within [0, 1] when 'input_type' is 'probability'.

---

    Code
      as_visualizer(list(loss_reg, 1))
    Condition
      Error in `validate_visualizer_loss()`:
      ! The following elements of 'x' are not LossFunction objects: 2

