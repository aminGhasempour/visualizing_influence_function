mean_estimator <- function(x, p_x) {
  return(mean(p_x(seq(min(x), max(x), length.out = 1000))))
}
mean_if <- function(x, p_x) {
  estimator_value <- mean_estimator(x, p_x)
  if_value <- p_x(x) - estimator_value
  return(mean(if_value))
}

avg_d_estimator <- function(x, p_x) {
  avg_den <- function (x) { p_x(x) ^ 2 }
  return(integrate(avg_den, min(x), max(x))$value)
}
avg_d_if <- function(x, p_x) {
  estimator_value <- avg_den_estimator(x, p_x)
  if_value <- 2 * (p_x(x) - estimator_value)
  return(mean(if_value))
}

estimators <-
  list(
    mean = list(estimator = mean_estimator,
                ifn = mean_if),
    avg_den = list(estimator = avg_den_estimator,
                   ifn = avg_den_if)
  )

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


calculate_estimator_along_path <- function(x, 
                                           estimator, 
                                           p_epsilon_fun, 
                                           epsilon) {
  t_epsilon <- c()
  for (eps in epsilon) {
    p_epsilon <- function(x) {
      p_epsilon_fun(x, eps)
    }
    t_epsilon <- append(t_epsilon, estimator(x, p_epsilon))
  }
  
  return(t_epsilon)
}

data_generating_process <-
  function(dnorm_mix,
           rnorm_mix,
           n,
           epsilon = seq(0, 1, 0.01)) {
    
  
  # Generating sample
  sample <- rnorm_mix(n)
  
  # Approximating distribution by kernel estimation
  p_x_tilde_fun <- approxfun(density(sample))
  
  # Creating and evaluating densities
  x <- seq(min(sample), max(sample), length.out = 1000)
  p_x <- dnorm_mix(x)
  p_x_tilde <- p_x_tilde_fun(x)
  
  # Densities along the path
  p_epsilon_fun <- function(x, eps) {
    eps * dnorm_mix(x) + (1 - eps) * p_x_tilde_fun(x)
  }
  
  # Evaluating densities and distances along the path
  p_epsilon <- list()
  # distribution_distances <- list()
  for (eps in epsilon) {
    lbl <- toString(eps)
    p_epsilon[[lbl]] <- p_epsilon_fun(x, eps)
    
    # dist <- function(x) { (dnorm_mix(x) - p_epsilon_fun(x, eps))^2 }
    # distribution_distances[[lbl]] <- integrate(dist, min(x), max(x))
  }
  
  # Calculating estimators and influence functions
  t_values <- list()
  if_values <- list()
  for (t in names(estimators)) {
    t_values[[t]] <- calculate_estimator_along_path(x, estimators[[t]]$estimator, p_epsilon_fun, epsilon)
    if_values[[t]] <- estimators[[t]]$ifn(sample, p_x_tilde_fun)
  }
  
  return(list(x = x,
              epsilon = epsilon,
              # distribution_distances = distribution_distances,
              p_x = p_x,
              p_x_tilde = p_x_tilde,
              p_epsilon = p_epsilon,
              t_values = t_values,
              if_values = if_values
  ))
}