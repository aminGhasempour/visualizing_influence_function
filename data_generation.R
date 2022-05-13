dgp <- function(dnorm_mix, rnorm_mix, n, epsilon = seq(0, 1, 0.01)) {
  # Generating sample
  x <- rnorm_mix(n)
  
  # Approximating distribution by kernel estimation
  dapprox <- approxfun(density(sample))
  
  # Creating and evaluating densities
  x_linspace <- seq(min(sample), max(sample), length.out = 1000)
  # True distribution values
  p_x <- dnorm_mix(x_linspace)
  # Approximated distribution values
  p_x_tilde <- dapprox(x_linspace)
  
  # Distributions along epsilon
  depsilon <- function(x, eps) {
    eps * dnorm_mix(x) + (1 - eps) * p_x_tilde_distribution(x)
  }
  
  return(list(x = x,
              x_linspace = x_linspace,
              p_x = p_x,
              p_x_tilde = p_x_tilde,
              dapprox = dapprox,
              depsilon = depsilon
  ))
  
  
}