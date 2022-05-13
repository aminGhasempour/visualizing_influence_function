mean_estimator <- function(x, p_x) {
  return(mean(p_x(seq(min(x), max(x), length.out = 1000))))
}
mean_if <- function(x, p_x) {
  estimator_val <- mean_estimator(x, p_x)
  if_val <- p_x(x) - estimator_val
  return(mean(if_val))
}

average_density_estimator <- function(x, p_x) {
  average_density <- function (x) { p_x(x) ^ 2 }
  return(integrate(average_density, min(x), max(x))$value)
}
average_density_if <- function(x, p_x) {
  estimator_val <- average_density_estimator(x, p_x)
  if_val <- 2 * (p_x(x) - estimator_val)
  return(mean(if_val))
}

# To be implemented
# T_covariance <- function() {
#   
# }
# IF_covariance <- function() {
#   
# }
# 
# T_potential_outcome <- function() {
#   
# }
# IF_potential_outcome <- function() {
#   
# }


estimators <- list(mean = list(estimator = mean_estimator, influence_function = mean_if),
                   avgden = list(estimator = average_density_estimator, influence_function = average_density_if)
                  )

calculate_estimator_along_path <- function(x, 
                                           estimator, 
                                           convex_combination, 
                                           epsilon = seq(0, 1, 0.1)) {
  t_vals <- c()
  for (eps in epsilon) {
    p_epsilon <- function(x) {
      convex_combination(x, eps)
    }
    t_vals <- append(t_vals, estimator(x, p_x))
  }
  
  return(list(t_vals = t_vals, t_p_tilde = tail(t_vals, 1)))
}