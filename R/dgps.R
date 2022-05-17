dgp <- function(n, dnorm_mix, rnorm_mix) {
  # Generating sample
  x <- rnorm_mix(n)
  
  # Approximating distribution by kernel estimation
  dtilde <- approxfun(density(x))
  
  # Creating and evaluating densities
  x_linspace <- seq(min(x), max(x), length.out = 1000)
  # True distribution values
  p_x <- dnorm_mix(x_linspace)
  # Approximating distribution values
  p_x_tilde <- dtilde(x_linspace)
  
  # Distributions along epsilon
  dpath <- function(x, eps) {
    eps * dnorm_mix(x) + (1 - eps) * dtilde(x)
  }
  
  return(list(x = x,
              x_linspace = x_linspace,
              p_x = p_x,
              p_x_tilde = p_x_tilde,
              dtilde = dtilde,
              dpath = dpath)) 
  }