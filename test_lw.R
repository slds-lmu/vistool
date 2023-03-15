x = c(1, 1)
y = 3
Xmat = model.matrix(~ x)

obj = rloss_dict$get("RL_cauchy")
obj$reset_fargs(Xmat = Xmat, y = y, epsilon = 5)
obj$eval(c(2, 4))
