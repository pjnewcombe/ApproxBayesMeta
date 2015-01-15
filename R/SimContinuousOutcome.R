#' This function simulates a continuous outcome according to a covariate matrix and
#' vector of betas.
#' 
#' @export
#' @title Simulates continuous outcomes
#' @name SimContinuousOutcome
#' @param seed Random number seed
#' @param X A covariate matrix (rows individuals, columns covariates)
#' @param betas A named vector of effects, named with the covariates the
#' effects correspond to in X.
#' @param sd Remaining residual error in outcome
#' @param normalise Center the outcome around it's mean for use with an
#' intercept-free model
#' 
#' @return A N by P+1 data.frame of the outcome Y, and genotypes
SimContinuousOutcome <- function(
  seed=1,
  X,
  betas,
  sd,
  normalise=F) {
  
  # Simulate outcomes
  cat("Simulating outcome vector\n")
  set.seed(seed)
  Y <- rnorm(nrow(X), mean = X[,names(betas), drop=F] %*% betas, sd=sd)
  
  # Normalise covariates and outcome, for intercept-free model
  if (normalise) {
    cat("Normalising for intercept-free model\n")
    Y <- Y - mean(Y)    
  }
  
  return(Y)
}
