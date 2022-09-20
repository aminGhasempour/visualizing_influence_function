# Generate the DGP functions. dmix is the distribution function and rmix is the 
# sample generator. This should work scope-wise: https://cran.r-project.org/doc/manuals/R-lang.html#Scope-of-variables
generate_dgp <- function(mean_a, std_a, mean_b, std_b, d_mix) {
  dmix <-  function(x) {
    return(d_mix * dnorm(x, mean_a, std_a) + (1 - d_mix) * 
             dnorm(x, mean_b, std_b))
  }
  
  rmix <-  function(n) {
    p <- rbinom(n, 1, d_mix)
    return(p * rnorm(n, mean_a, std_a) + (1 - p) * rnorm(n, mean_b, std_b))
  }
  
  return(list(dmix = dmix, rmix = rmix))
}

generate_dmix_string <- function(mean_a, std_a, mean_b, std_b, d_mix) {
  return(paste0("P = ", d_mix, " * N(", mean_a, ", ", std_a, ") + (1 - ", d_mix, 
                ") * N(", mean_b, ", ", std_b, ")"))
}

generate_data <- function(n, dmix, rmix) {
  # Generating sample
  x <- rmix(n)
  
  # Approximating distribution by kernel estimation
  dtilde <- approxfun(density(x))
  
  # Creating and evaluating densities
  x_linspace <- seq(min(x), max(x), length.out = 1000)
  # True distribution values
  p_x <- dmix(x_linspace)
  # Approximating distribution values
  p_x_tilde <- dtilde(x_linspace)
  
  # Distributions along epsilon
  dpath <- function(x, eps) {
    (1 - eps) * dmix(x) + eps * dtilde(x)
  }
  
  return(list(x = x,
              x_linspace = x_linspace,
              p_x = p_x,
              p_x_tilde = p_x_tilde,
              dtilde = dtilde,
              dpath = dpath)) 
  }