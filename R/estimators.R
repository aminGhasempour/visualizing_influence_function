estimator_mean <- function(x, p_x) {
  avg <- function (x) { x * p_x(x) }
  return(integrate(avg, min(x), max(x))$value)
}
if_mean <- function(x, p_x) {
  estimator_val <- estimator_mean(x, p_x)
  if_val <- x - estimator_val
  return(mean(if_val))
}

estimator_avg_den <- function(x, p_x) {
  avg_den <- function (x) { p_x(x) ^ 2 }
  return(integrate(avg_den, min(x), max(x))$value)
}
if_avg_den <- function(x, p_x) {
  estimator_val <- estimator_avg_den(x, p_x)
  if_val <- 2 * (p_x(x) - estimator_val)
  return(mean(if_val))
}

# To be implemented
# estimator_covariance <- function() {
#   
# }
# if_covariance <- function() {
#   
# }
# 
# estimator_ate <- function() {
#   
# }
# if_ate <- function() {
#   
# }

estimators <- list(
  mean = list(estimator = estimator_mean, ifn = if_mean),
  avg_den = list(estimator = estimator_avg_den, ifn = if_avg_den)
)

calculate_estimator_along_path <- function(x_linspace, estimator, eps, dpath) {
  t_eps <- c()
  for (ep in eps) {
    p_x <- function(x) {
      dpath(x, ep)
    }
    t_eps <- append(t_eps, estimator(x_linspace, p_x))
  }
  
  return(t_eps)
}

# TODO: Separate calculation of influence function from this loop
calculate_estimators_and_ifs <- function(x, eps, dnorm_mix, dtilde, 
                                         dpath) {
  # Values of densities along epsilon path
  p_eps <- list()
  # Distribution distances
  ddistances <- c()
  for (ep in eps) {
    p_eps <- append(p_eps, dpath(x, ep))
    
    dist_fun <- function(x) { (dnorm_mix(x) - dpath(x, ep))^2 }
    dist <- integrate(dist_fun, min(x), max(x))$value
    ddistances <- append(ddistances, dist)
  }
  
  # Calculating estimators and influence functions
  x_linspace <- seq(min(x), max(x), length.out=1000)
  estimator_values <- list()
  if_values <- list()
  for (tn in names(estimators)) {
    t <- estimators[[tn]]
    t_val <- calculate_estimator_along_path(x_linspace, t$estimator, eps, 
                                            dpath)
    estimator_values[[tn]] <- t_val
    
    if_values[[tn]] <- t$ifn(x, dtilde)
  }
  
  return(list(ddistances = ddistances,
              estimators = estimator_values,
              ifs = if_values))
}
