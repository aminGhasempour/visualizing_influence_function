source("estimators.R")
# k-fold cross-fitting
k_fold_cross_fitting <- function(sample) {
  ifs <- rep(0, length(sample))
  
  for (k_idx in seq_along(sample)) {
    
    # Remove k from sample
    x <- sample[-k_idx]
    # Extract k from sample
    k <- sample[k_idx]
    
    # Approximate density
    dtilde <- approxfun(density(x))
    
    ifs[k_idx] <- t$ifn(x, dtilde)
    
    
  }
  
}