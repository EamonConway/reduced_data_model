calculate_standard_curve <- function(plate) {
  # Set up the minimisation problem.
  y <- log(plate$mfi)

  # Define the error function
  error_func <- function(x) {
    f <- log_logistic_5p(plate$dilution, x[1], x[2], x[3], x[4], exp(x[5]))
    return(sum((y - f)^2.0))
  }

  initial_solution <- c(-1.0, 0.0, max(y), 0.0, 0.0)
  proposed_solution <- optim(initial_solution, error_func, control = list(maxit = 20000))
  # if the solver hasnt converged we should throw an error.
  if (proposed_solution$convergence != 0) {
    stop("Solver did not converge: possible fix is to increase max iteration in optim (must modify source code above)")
  }
  sol <- proposed_solution$par
  return(list(b = sol[1], c = sol[2], d = sol[3], e = sol[4], f = exp(sol[5])))
}
