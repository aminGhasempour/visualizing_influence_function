source("R/estimators.R")
# k-fold cross-fitting
k_fold_cross_fitting <- function(sample) {
  
  # Looping over estimators defined in estimators.R
  ifs <- list()
  for (tn in names(estimators)) {
    t <- estimators[[tn]]
    if_values <- rep(0, length(sample))
    for (k_idx in seq_along(sample)) {
      
      # Remove k from sample
      x <- sample[-k_idx]
      # Extract k from sample
      k <- sample[k_idx]
      
      # Approximate density
      dtilde <- approxfun(density(x))
      
      # Storing the value of the influence function for k
      if_values[k_idx] <- t$ifn(x, dtilde)
    }
    # Taking the empirical mean of all the if values
    ifs[[tn]] <- mean(if_values)
  }
  
  return(ifs)
}